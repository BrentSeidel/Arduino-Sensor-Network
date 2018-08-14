// include the library code:
#include <RS-485.h>

//
// Change this to whichever port is hooked up to RS-485.  For the Uno, use the only
// serial port.  For the Mega and Due, use Serial3
//
#if defined (__AVR_ATmega328P__)
HardwareSerial *rs485 = &Serial;
#else
HardwareSerial *rs485 = &Serial3;
#endif
//#define NUM_NODES 8
#define NODE_NEVER 32767
const bool DEBUG = true;

void setup()
{
  Serial.begin(BAUD_RATE);
  rs485->begin(BAUD_RATE);
  Serial.println("Analyzer initialization complete.");
}

void loop()
{
  int data;

  if (rs485->available())
  {
    data = rs485->read();
    Serial.write(data);
  }
}


