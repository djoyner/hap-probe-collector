# Overview

A personal project involving an Arduino-based environmental and power-usage
probe, ZigBee wireless interfaces, and Haskell.

The Arduino firmware assumes the presence of a Texas Instruments TMP102 
digital temperature sensor, a Honeywell HIH-4030 humidity sensor,
and an I/R photodiode used to count pulses from
the power meter on the side of my house.  A soft UART is used to communicate
with an XBee ZNet 2.5 (ZigBee) RF module.  The probe runs a simple state machine
to manage the XBee interface and to allow a server-based collector program to
register a poll request.  Once that is accomplished the probe will perodically
broadcast sensor readings.

The collector is a simple Haskell program that discovers probes on
the XBee wireless network and then initiates data polling.  As poll
notifications are received from the probes they are serialized as JSON
messages and streamed to a ZeroMQ PUB socket.

# Author

David Joyner, <david@joynerhome.net>

