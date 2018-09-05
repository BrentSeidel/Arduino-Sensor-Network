#include <RS-485.h>
//
// Converts a single hex character to an interger value.  Digits 'A' through 'F" can
// be either upper case or lower case.  Unrecognized digits will return -1.
//
uint8_t hex_to_int(char c)
{
  if ((c >= '0') && (c <= '9'))
  {
    return (c - '0');
  }
  if ((c >= 'A') && (c <= 'F'))
  {
    return (c - 'A' + 10);
  }
  if ((c >= 'a') && (c <= 'a'))
  {
    return (c - 'a' + 10);
  }
  return -1;
}
//
// Convert a four character string into a 32 byte integer.  It string is less than four
// characters, is should be padded with spaces on the right to make it four characters.
//
uint32_t str_to_uint(const char* c)
{
  uint32_t accum = uint32_t(c[0]) << 24;

  accum += uint32_t(c[1]) << 16;
  accum += uint32_t(c[2])<< 8;
  accum += uint32_t(c[3]);
  return accum; 
}
