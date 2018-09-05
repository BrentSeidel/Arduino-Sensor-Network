#include <RS-485.h>
#include <Arduino.h>
//
// This module contains routines to send some of the standard messages.
//
void rs485_msg_unknown(HardwareSerial *rs485, uint32_t device, uint32_t address)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_UNKNOWN, HEX);
  rs485->println("%FF");
}
//
void rs485_msg_empty(HardwareSerial *rs485, uint32_t device, uint32_t address)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_EMPTY, HEX);
  rs485->println("%FF");
}
//
void rs485_msg_nak(HardwareSerial *rs485, uint32_t device, uint32_t address)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_NAK, HEX);
  rs485->println("%FF");
}
//
void rs485_msg_info(HardwareSerial *rs485, uint32_t device, uint32_t address,
	uint32_t num_addr, const char *name)
{
  uint8_t x;
  uint8_t ptr;

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
  rs485->println("%FF");
}
//
void rs485_msg_disc(HardwareSerial *rs485, uint32_t device, uint32_t address,
	uint32_t disc_type, uint32_t disc_value)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_DISCRETE, HEX);
  rs485->print("&");
  rs485->print(disc_type, HEX);
  rs485->print("&");
  rs485->print(disc_value, HEX);
  rs485->println("%FF");
}
