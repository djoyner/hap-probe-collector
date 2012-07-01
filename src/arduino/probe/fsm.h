enum State
{
  STATE_RESET,
  STATE_DISCOVER,
  STATE_IDLE,
  STATE_RUN,
  STATE_MAX
};

static inline const char *stateName(State s)
{
  switch (s)
  {
    case STATE_RESET:
      return "RESET";
    case STATE_DISCOVER:
      return "DISCOVER";
    case STATE_IDLE:
      return "IDLE";
    case STATE_RUN:
      return "RUN";
    default:
      return "(unknown)";
  }
}

enum EventType
{
  EVENT_TYPE_NONE,
  EVENT_TYPE_TIMEOUT,
  EVENT_TYPE_MODEM_ASSOCIATED,
  EVENT_TYPE_MODEM_DISASSOCIATED,
  EVENT_TYPE_COORDINATOR_DISCOVERED,
  EVENT_TYPE_POLL_STARTED,
  EVENT_TYPE_POLL_STOPPED,
  EVENT_TYPE_TRANSMIT_FAILURE,
  EVENT_TYPE_MAX
};

static inline const char *eventName(EventType e)
{
  switch (e)
  {
    case EVENT_TYPE_TIMEOUT:
      return "TIMEOUT";
    case EVENT_TYPE_MODEM_ASSOCIATED:
      return "MODEM_ASSOCIATED";
    case EVENT_TYPE_MODEM_DISASSOCIATED:
      return "MODEM_DISASSOCIATED";
    case EVENT_TYPE_COORDINATOR_DISCOVERED:
      return "COORDINATOR_DISCOVERED";
    case EVENT_TYPE_POLL_STARTED:
      return "POLL_STARTED";
    case EVENT_TYPE_POLL_STOPPED:
      return "POLL_STOPPED";
    case EVENT_TYPE_TRANSMIT_FAILURE:
      return "TRANSMIT_FAILURE";
    default:
      return "(unknown)";
  }
}

struct StateTransition
{
  EventType event;
  void (*action1)(void);
  void (*action2)(void);
  State nextState;
};



