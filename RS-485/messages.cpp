#include <RS-485.h>
#include <Arduino.h>
//
// This module contains routines to send some of the standard messages.
//
//const int MSG_TYPE_INFO = 3; // Address 0 information message
//
void rs485_msg_unknown(HardwareSerial *rs485, unsigned int device, unsigned int address)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_UNKNOWN, HEX);
  rs485->print("%FF");
}
//
void rs485_msg_empty(HardwareSerial *rs485, unsigned int device, unsigned int address)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_EMPTY, HEX);
  rs485->print("%FF");
}
//
void rs485_msg_nak(HardwareSerial *rs485, unsigned int device, unsigned int address)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_NAK, HEX);
  rs485->print("%FF");
}
//
void rs485_msg_info(HardwareSerial *rs485, unsigned int device, unsigned int address,
	unsigned int num_addr, const char *name)
{
  int x;
  int ptr;

  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_INFO, HEX);
  rs485->print("&");
  rs485->print(num_addr, HEX);
  for (x = 0, ptr = 0; x < 8; x++, ptr += 4)
  {
    rs485->print("&");
    rs485->print(str_to_uint(&name[ptr]), HEX);
  }
  rs485->print("%FF");
}
