enum ProbeMessageType {
  PROBE_MSG_UNKNOWN,
  PROBE_MSG_POLL_REQUEST,
  PROBE_MSG_POLL_NOTIFICATION
};

struct ProbePollRequest {
  int8_t message;      // PROBE_MSG_POLL_REQUEST
  uint32_t sync;
  uint8_t interval;
};

struct ProbePollNotification {
  uint8_t message;      // PROBE_MSG_POLL_NOTIFCATION
  uint32_t sync;
  uint8_t count;
  uint8_t data[0];      // array of TLV's: one byte type, one byte length, "length" bytes of value
};

enum ProbeDataType {
  PROBE_DATA_TYPE_UNKNOWN,
  PROBE_DATA_TYPE_COUNTER,
  PROBE_DATA_TYPE_TEMPERATURE
};

