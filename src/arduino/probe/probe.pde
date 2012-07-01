#include <string.h>

#include <DateTime.h>
#include <MsTimer2.h>
#include <NewSoftSerial.h>
#include <Wire.h>
#include <XBee.h>

#include "fsm.h"
#include "messages.h"

#define PROBE_COUNTER
#define PROBE_TEMPERATURE
#define PROBE_REL_HUMIDITY
#undef DEBUG

#define COUNTER_SAMPLE_PIN 4     // IR photodiode pin number
#define COUNTER_SAMPLE_PERIOD 5  // 5 milliseconds

#define TMP102_SLAVE_ADDR 0x48   // TMP102 ADDR0 = GND

#define HIH4030_SAMPLE_PIN A0    // Relative humidity sensor ADC pin

#define DISCOVER_TIMEOUT 10000   // 10 seconds
#define IDLE_TIMEOUT 15000       // 15 seconds

// Application state machine
const StateTransition _fsm[STATE_MAX][EVENT_TYPE_MAX] =
{
  {  // STATE_RESET
     { EVENT_TYPE_MODEM_ASSOCIATED, actDiscoverNodes, actStartDiscoverTimer, STATE_DISCOVER },
     { EVENT_TYPE_MODEM_DISASSOCIATED, NULL, NULL, STATE_RESET },
     { EVENT_TYPE_TRANSMIT_FAILURE, NULL, NULL, STATE_RESET }
  },

  {  // STATE_DISCOVER
     { EVENT_TYPE_TIMEOUT, actDiscoverNodes, actStartDiscoverTimer, STATE_DISCOVER },
     { EVENT_TYPE_MODEM_ASSOCIATED, actDiscoverNodes, actStartDiscoverTimer, STATE_DISCOVER },
     { EVENT_TYPE_MODEM_DISASSOCIATED, actResetTimer, actResetNetwork, STATE_RESET },
     { EVENT_TYPE_COORDINATOR_DISCOVERED, actPrintIdleHello, actStartIdleTimer, STATE_IDLE },
     { EVENT_TYPE_POLL_STARTED, actStartPollTimer, NULL, STATE_RUN },
     { EVENT_TYPE_POLL_STOPPED, NULL, NULL, STATE_DISCOVER },
     { EVENT_TYPE_TRANSMIT_FAILURE, NULL, NULL, STATE_DISCOVER }
  },

  {  // STATE_IDLE
     { EVENT_TYPE_TIMEOUT, actPrintIdleHello, actStartIdleTimer, STATE_IDLE },
     { EVENT_TYPE_MODEM_ASSOCIATED, actResetTimer, actResetNetwork, STATE_RESET },
     { EVENT_TYPE_MODEM_DISASSOCIATED, actResetTimer, actResetNetwork, STATE_RESET },
     { EVENT_TYPE_COORDINATOR_DISCOVERED, NULL, NULL, STATE_IDLE },
     { EVENT_TYPE_POLL_STARTED, actStartPollTimer, NULL, STATE_RUN },
     { EVENT_TYPE_POLL_STOPPED, NULL, NULL, STATE_IDLE },
     { EVENT_TYPE_TRANSMIT_FAILURE, actResetTimer, actResetNetwork, STATE_RESET }
  },

  {  // STATE_RUN
     { EVENT_TYPE_TIMEOUT, actTransmitPollNotification, actStartPollTimer, STATE_RUN },
     { EVENT_TYPE_MODEM_ASSOCIATED, actResetTimer, actResetNetwork, STATE_RESET },
     { EVENT_TYPE_MODEM_DISASSOCIATED, actResetTimer, actResetNetwork, STATE_RESET },
     { EVENT_TYPE_COORDINATOR_DISCOVERED, NULL, NULL, STATE_RUN },
     { EVENT_TYPE_POLL_STARTED, actStartPollTimer, NULL, STATE_RUN },
     { EVENT_TYPE_POLL_STOPPED, actStartIdleTimer, NULL, STATE_IDLE },
     { EVENT_TYPE_TRANSMIT_FAILURE, actResetTimer, actResetNetwork, STATE_RESET }
  }
};

// Application globals
State _state = STATE_RESET;
uint32_t _timeout = 0;
int _counterPinReading = LOW;
volatile uint32_t _counterData = 0;
uint32_t _pollInterval = 0;

// A single probe data point
struct ProbeData
{
#ifdef PROBE_COUNTER
  uint32_t counter;
#endif
#ifdef PROBE_TEMPERATURE
  float temperature;
#endif
#ifdef PROBE_REL_HUMIDITY
  float relHumidity;
#endif
};

// Other Globals
NewSoftSerial _nss = NewSoftSerial(3, 2);
XBee _xbee(&_nss);

void setup()
{
  // Initialize serial port and say hello
  Serial.begin(9600);
  Serial.print("Initializing:");
  
#ifdef PROBE_COUNTER
  // Start counter sampling
  pinMode(COUNTER_SAMPLE_PIN, INPUT);
  MsTimer2::set(COUNTER_SAMPLE_PERIOD, counterSampleTimeout);
  MsTimer2::start();
  Serial.print(" counter");
#endif

#ifdef PROBE_TEMPERATURE
  // Join I2C bus
  Wire.begin();
  Serial.print(" temperature");
#endif

#ifdef PROBE_REL_HUMIDITY
  Serial.print(" rel_humidity");
#endif

  // Initialize XBee network interface
  _xbee.begin(9600);
  Serial.println(" xbee");
  actResetNetwork();
}

void loop()
{
  if (_timeout && (millis() - _timeout) < 0x80000000)
  {
    _timeout = 0;
    dispatchEvent(EVENT_TYPE_TIMEOUT);
  }

  handlePacket();
}

#ifdef PROBE_COUNTER
void counterSampleTimeout()
{
  const int counterPinReading = digitalRead(COUNTER_SAMPLE_PIN);

  // Only count LOW->HIGH transitions
  if (_counterPinReading == LOW && counterPinReading == HIGH)
      _counterData++;

  _counterPinReading = counterPinReading;
}
#endif

#ifdef PROBE_TEMPERATURE
float readAmbientTemp()
{
  Wire.requestFrom(TMP102_SLAVE_ADDR, 2);
  while (Wire.available() != 2);

  uint16_t val = Wire.receive() << 4;
  val |= (Wire.receive() >> 4) & 0x0f;

  const float tempC = (val & 0x800) ? ((float) ((~(val - 1)) & 0x7ff) * -0.0625) : ((float) val * 0.0625);
  const float tempF = (tempC * 1.8) + 32;
  
#ifdef DEBUG
  Serial.print("Ambient sensor val = ");
  Serial.print(val, HEX);
  Serial.print(", tempC = ");
  Serial.print(tempC, 2);
  Serial.print(", tempF = ");
  Serial.println(tempF, 2);
#endif

  return tempF;
}
#endif

#ifdef PROBE_REL_HUMIDITY
float readRelHumidity()
{
  const uint16_t val = analogRead(HIH4030_SAMPLE_PIN);
  
  // Calibration values based on +5V supply an 10-bit ADC
  float relHumidity = ((float) val - 196.198) / 6.283;
  if (relHumidity < 0)
    relHumidity = 0;
  else if (relHumidity > 100)
    relHumidity = 100;
  
#ifdef DEBUG
  Serial.print("Rel humidity sensor val = ");
  Serial.print(val, HEX);
  Serial.print(", rel_humidity = "),
  Serial.println(relHumidity, 2);
#endif

  return relHumidity;
}
#endif

void collectProbeData(void *ptr)
{
  // Workaround Arduino auto-prototype generator BS
  ProbeData *data = (ProbeData *) ptr;
  
#ifdef PROBE_COUNTER
  MsTimer2::stop();
  memcpy(&data->counter, (const void *) &_counterData, sizeof(uint32_t));
  MsTimer2::start();
#endif

#ifdef PROBE_TEMPERATURE
  data->temperature = readAmbientTemp();
#endif

#ifdef PROBE_REL_HUMIDITY
  data->relHumidity = readRelHumidity();
#endif
}

void printProbeData(void *ptr)
{
  // Workaround Arduino auto-prototype generator BS
  ProbeData *data = (ProbeData *) ptr;

#ifdef PROBE_COUNTER
  Serial.print("  counter = ");
  Serial.println(data->counter, DEC);
#endif

#ifdef PROBE_TEMPERATURE
  Serial.print("  temperature = ");
  Serial.println(data->temperature, 2);
#endif

#ifdef PROBE_REL_HUMIDITY
  Serial.print("  rel_humidity = ");
  Serial.println(data->relHumidity, 2);
#endif
}

void handleAtCommandResponse(XBeeResponse &resp)
{
  AtCommandResponse atResp;
  resp.getAtCommandResponse(atResp);

  if (atResp.isOk())
  {
    const uint8_t len = atResp.getValueLength();
    const uint8_t *val = atResp.getValue();

    Serial.print("[OK");
    if (len)
    {
      Serial.print(": ");
      for (uint8_t i = 0; i < len; i++)
      {
        if (val[i] < 0x10)
          Serial.print("0");
        Serial.print(val[i], HEX);
      }
    }
    Serial.println("]");

    if (strcasecmp((const char *) atResp.getCommand(), "ND") == 0 && len >= 2 && val[0] == 0 && val[1] == 0)
      dispatchEvent(EVENT_TYPE_COORDINATOR_DISCOVERED);
  }
  else
    Serial.println("[ERROR]");
}

void handleModemStatusResponse(XBeeResponse &resp)
{
  ModemStatusResponse msResp;
  resp.getModemStatusResponse(msResp);

  Serial.print("[Modem status: ");
  switch (msResp.getStatus())
  {
    case 0:
      Serial.println("hardware reset]");
      break;

    case 1:
      Serial.println("watchdog timer reset]");
      break;

    case 2:
      Serial.println("associated]");
      dispatchEvent(EVENT_TYPE_MODEM_ASSOCIATED);
      break;

    case 3:
      Serial.println("disassociated]");
      dispatchEvent(EVENT_TYPE_MODEM_DISASSOCIATED);
      break;

    case 4:
      Serial.println("synchronization lost]");
      break;

    case 5:
      Serial.println("coordinator realignment]");
      break;

    case 6:
      Serial.println("coordinator started]");
      break;

    default:
      Serial.println("(unknown)]");
      break;
  }
}

void handleRxResponse(XBeeResponse &resp)
{
  ZBRxResponse rxResp;
  resp.getZBRxResponse(rxResp);

  const uint8_t len = rxResp.getDataLength();
  const uint16_t addr = rxResp.getRemoteAddress16();

#ifdef DEBUG
  Serial.print("[Received ");
  Serial.print(len, DEC);
  Serial.print(" bytes from address ");
  Serial.print(addr, HEX);
  Serial.println("]");
#endif

  if (!len)
  {
    Serial.println("[Dropping zero-byte message]");
    return;
  }

  if (addr)
  {
    Serial.print("[Dropping message from non-coordinator address ");
    Serial.print(addr, HEX);
    Serial.println("]");
    return;
  }

  const uint8_t *data = rxResp.getData();

  switch (data[0])
  {
    case PROBE_MSG_POLL_REQUEST:
      if (len == sizeof(struct ProbePollRequest))
      {
        struct ProbePollRequest *poll = (struct ProbePollRequest *) data;

        if (poll->sync)
          DateTime.sync(poll->sync);

        _pollInterval = poll->interval * 1000;

        Serial.print("[Received poll request, sync = ");
        Serial.print(poll->sync, DEC);
        Serial.print(", interval = ");
        Serial.print(poll->interval, DEC);
        Serial.println(" sec]");

        dispatchEvent(_pollInterval ? EVENT_TYPE_POLL_STARTED : EVENT_TYPE_POLL_STOPPED);
      }
      else
        Serial.println("[Dropping PROBE_MSG_POLL_REQUEST with mismatched length]");
      break;

    default:
      Serial.print("[Dropping message with unexpected type ");
      Serial.print(data[0], HEX);
      Serial.println("]");
      break;
  }
}

void handleTxStatusResponse(XBeeResponse &resp)
{
  ZBTxStatusResponse txStatusResp;
  resp.getZBTxStatusResponse(txStatusResp);

  Serial.print("[Tx status: ");
  switch (txStatusResp.getDeliveryStatus())
  {
    case SUCCESS:
      Serial.println("success]");
      break;

    default:
      Serial.print("failure ");
      Serial.print(txStatusResp.getDeliveryStatus(), HEX);
      Serial.println("]");
      dispatchEvent(EVENT_TYPE_TRANSMIT_FAILURE);
      break;
  }
}

void handleOtherResponse(XBeeResponse &resp)
{
  const uint8_t len = resp.getFrameDataLength();
  const uint8_t *data = resp.getFrameData();

  Serial.print("[ApiId = ");
  Serial.print(resp.getApiId(), HEX);

  if (len)
  {
    Serial.print(": ");
    for (uint8_t i = 0; i < len; i++)
    {
      if (data[i] < 0x10)
        Serial.print("0");
      Serial.print(data[i], HEX);
    }
  }
  
  Serial.println("]");
}

void handlePacket()
{
  _xbee.readPacket();
  XBeeResponse &resp = _xbee.getResponse();
  if (!resp.isAvailable())
    return;

  switch(resp.getApiId())
  {
    case AT_COMMAND_RESPONSE:
      handleAtCommandResponse(resp);
      break;

    case MODEM_STATUS_RESPONSE:
      handleModemStatusResponse(resp);
      break;

    case ZB_RX_RESPONSE:
      handleRxResponse(resp);
      break;

    case ZB_TX_STATUS_RESPONSE:
      handleTxStatusResponse(resp);
      break;

    default:
      handleOtherResponse(resp);
      break;
  }
}

void dispatchEvent(EventType e)
{
  for (const StateTransition *trans = _fsm[_state]; trans->event != EVENT_TYPE_NONE; trans++)
  {
    if (trans->event == e)
    {
      if (trans->nextState != _state)
      {
#ifdef DEBUG
        Serial.print("Changing from ");
        Serial.print(stateName(_state));
        Serial.print(" to ");
        Serial.print(stateName(trans->nextState));
        Serial.print(" state in response to ");
        Serial.print(eventName(e));
        Serial.println(" event");
#endif
        _state = trans->nextState;
      }
      else
      {
#ifdef DEBUG
        Serial.print("Remaining in ");
        Serial.print(stateName(_state));
        Serial.print(" state in response to ");
        Serial.print(eventName(e));
        Serial.println(" event");
#endif
      }
      if (trans->action1)
        trans->action1();
      if (trans->action2)
        trans->action2();
      return;
    }
  }

  Serial.print("Unhandled event ");
  Serial.print(eventName(e));
  Serial.print(" in state ");
  Serial.println(stateName(_state));
}

void actResetTimer()
{
  _timeout = 0;
}

void actStartDiscoverTimer()
{
  if (!(_timeout = millis() + DISCOVER_TIMEOUT))
    _timeout++;
}

void actStartIdleTimer()
{
  if (!(_timeout = millis() + IDLE_TIMEOUT))
    _timeout++;
}

void actStartPollTimer()
{
  if (!(_timeout = millis() + _pollInterval))
    _timeout++;
}

void actResetNetwork()
{
  AtCommandRequest atCmd;
  atCmd.setCommand((uint8_t *) "FR");
  atCmd.clearCommandValue();

  Serial.println("Reseting network (ATFR)");
  _xbee.send(atCmd);
}

void actDiscoverNodes()
{
  AtCommandRequest atCmd;
  atCmd.setCommand((uint8_t *) "ND");
  atCmd.clearCommandValue();

  Serial.println("Discovering nodes (ATND)");
  _xbee.send(atCmd);
}

void actPrintIdleHello()
{
  ProbeData data;
  collectProbeData(&data);
  
  Serial.println("Idling:");
  printProbeData(&data);
}

void actTransmitPollNotification()
{
  ProbeData data;
  collectProbeData(&data);

  uint8_t bufferLen = sizeof(ProbePollNotification);
#ifdef PROBE_COUNTER
  bufferLen += 2 + sizeof(uint32_t);
#endif
#ifdef PROBE_TEMPERATURE
  bufferLen += 2 + sizeof(float);
#endif
#ifdef PROBE_REL_HUMIDITY
  bufferLen += 2 + sizeof(float);
#endif

  uint8_t buffer[bufferLen];
  ProbePollNotification *notif = (ProbePollNotification *) buffer;
  uint8_t *tlv = (uint8_t *) (notif + 1);

  notif->message = PROBE_MSG_POLL_NOTIFICATION;
  notif->sync = DateTime.now();
  notif->count = 0;

#ifdef PROBE_COUNTER
  *tlv++ = PROBE_DATA_TYPE_COUNTER;
  *tlv++ = sizeof(uint32_t);
  memcpy(tlv, &data.counter, sizeof(uint32_t));
  tlv += sizeof(uint32_t);
  notif->count++;
#endif

#ifdef PROBE_TEMPERATURE
  *tlv++ = PROBE_DATA_TYPE_TEMPERATURE;
  *tlv++ = sizeof(float);
  memcpy(tlv, &data.temperature, sizeof(float));
  tlv += sizeof(float);
  notif->count++;
#endif

#ifdef PROBE_REL_HUMIDITY
  *tlv++ = PROBE_DATA_TYPE_REL_HUMIDITY;
  *tlv++ = sizeof(float);
  memcpy(tlv, &data.relHumidity, sizeof(float));
  tlv += sizeof(float);
  notif->count++;
#endif

  Serial.print("Sending poll notification (sync = ");
  Serial.print(notif->sync, DEC);
  Serial.println("):");
  printProbeData(&data);

  XBeeAddress64 zeroAddr(0, 0);
  ZBTxRequest txReq(zeroAddr, 0, ZB_BROADCAST_RADIUS_MAX_HOPS, ZB_TX_UNICAST, buffer, bufferLen, DEFAULT_FRAME_ID);
  _xbee.send(txReq);
}

