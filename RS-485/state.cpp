#include <RS-485.h>
#include <Arduino.h>
//
// State machine states
//
const uint8_t STATE_START = 0;
const uint8_t STATE_GET_MSG_DEV = 1;
const uint8_t STATE_GET_MSG_ADDR = 2;
const uint8_t STATE_GET_MSG_TYPE = 3;
const uint8_t STATE_START_BUFFER = 4;
const uint8_t STATE_BUFFER_ENTRY = 5;
const uint8_t STATE_BUFFER_END = 6;
const uint8_t STATE_WAIT_MSG_LF = 8;
const uint8_t STATE_GET_CMD_DEV = 10;
const uint8_t STATE_GET_CMD_ADDR = 11;
const uint8_t STATE_GET_CMD_CMD = 12;
const uint8_t STATE_GET_CMD_ARG = 13;
const uint8_t STATE_WAIT_CMD_LF = 14;
uint8_t rs485_state = STATE_START;
//
// Data from bus
//
uint32_t device;
uint32_t address;
uint32_t data_type;
uint32_t ages[NUM_NODES] = {0, 0, 0, 0, 0, 0, 0};
uint32_t cmd_cmd;
uint32_t cmd_arg;
//
// RS-485 data buffer
//
uint32_t data_buffer[BUFFER_SIZE];
uint8_t buffer_ptr = 0;
//
// For some reason, this doesn't seem to get defined.
//
extern unsigned long millis(void);
//
// Utility function to perform a soft reset 
//
void softReset()
{
  asm volatile("jmp 0");
}
//
int get_hex_digit(int data)
{
  switch (data)
  {
    case '0': // Process hex digits
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return (data - '0');
      break;
    case 'A':
    case 'a':
      return 10;
      break;
    case 'B':
    case 'b':
      return 11;
      break;
    case 'C':
    case 'c':
      return 12;
      break;
    case 'D':
    case 'd':
      return 13;
      break;
    case 'E':
    case 'e':
      return 14;
      break;
    case 'F':
    case 'f':
      return 15;
      break;
    default: // Any other character
      return -1;
      break;
  }
}
//
// Utility functions to support the state machine.  These should not be
// used by other software.
//
static void state_wait_for_lf(int data, uint8_t *state, uint8_t state_value)
{
  if (data == '\n')
  {
    *state = state_value;
  }
}
//
// This function implements a state machine to read commands and
// messages off of the RS-485 bus.  While the state machine is
// processing, the return value is STATE_RS485_PROCESSING and none of
// the global variables should be considered to be valid.  If a command
// or message is completed, the return value is set appropriately and
// the data values can be used.
//
uint8_t rs485_state_machine(HardwareSerial *rs485, HardwareSerial *dbg_port, bool debug)
{
  int data;
  int temp;
  uint8_t status = STATE_RS485_PROCESSING;

  data = rs485->read();
  if (data <= 0)
  {
	  return status;
  }
  if (debug)
  {
      dbg_port->write(data);
  }
  switch (rs485_state)
  {
    case STATE_START:
      if (data == '@')
      {
        device = 0;
        address = 0;
        cmd_cmd = 0;
        cmd_arg = 0;
        data_type = MSG_TYPE_UNKNOWN;
        rs485_state = STATE_GET_CMD_DEV;
      }
      if (data == '#')
      {
        device = 0;
        address = 0;
        rs485_state = STATE_GET_MSG_DEV;
      }
      break;
    case STATE_GET_MSG_DEV: // Message processing
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
		  case '/':  // Device ID is always terminated by a slash, '/'
		    rs485_state = STATE_GET_MSG_ADDR;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        device = (device << 4) + temp;
	  }
      break;
    case STATE_GET_MSG_ADDR:
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
		  case '/':  // Device ID is always terminated by a slash, '/'
		    rs485_state = STATE_GET_MSG_TYPE;
            break;
          case '&': // Address may be terminated by an ampersand if
                    // message address and the next field is data.
            rs485_state = STATE_START_BUFFER;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        address = (address << 4) + temp;
	  }
      break;
    case STATE_GET_MSG_TYPE:
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
          case '&': // Message type is followed by an ampersand if there is data
            rs485_state = STATE_START_BUFFER;
            break;
          case '%': // Message type is followed by an percent if there is no data
            rs485_state = STATE_BUFFER_END;
            buffer_ptr = 0;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        data_type = (data_type << 4) + temp;
	  }
      break;
    case STATE_START_BUFFER:
      for (buffer_ptr = 0; buffer_ptr < BUFFER_SIZE; buffer_ptr++)
      {
        data_buffer[buffer_ptr] = 0;
      }
      buffer_ptr = 0;
      rs485_state = STATE_BUFFER_ENTRY;
//      break; // Yes, we do intend to fall through to the next state here.
    case STATE_BUFFER_ENTRY:
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
          case '&': // Field separator
            buffer_ptr++;
            if (buffer_ptr >= BUFFER_SIZE)
            {
              rs485_state = STATE_BUFFER_END;
            }
            break;
          case '%': // End of fields, start of checksum
            rs485_state = STATE_BUFFER_END;
            break;
          case '#': // Start of new message
            rs485_state = STATE_GET_MSG_DEV;
            break;
          case '@': // Start of new command
            rs485_state = STATE_GET_CMD_DEV;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + temp;
	  }
      break;
    case STATE_BUFFER_END:
      ages[device] = millis();
      rs485_state = STATE_WAIT_MSG_LF;
      break;
    case STATE_WAIT_MSG_LF:
      state_wait_for_lf(data, &rs485_state, STATE_START);
      if (rs485_state == STATE_START)
      {
        status = STATE_RS485_GOT_MSG;
      }
      break;
    case STATE_GET_CMD_DEV: // Command processing
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
		  case '/':  // Device ID is always terminated by a slash, '/'
		    rs485_state = STATE_GET_CMD_ADDR;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        device = (device << 4) + temp;
	  }
      break;
    case STATE_GET_CMD_ADDR:
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
          case '%': // Address may be terminated by a percent sign.
            rs485_state = STATE_WAIT_CMD_LF;
            break;
          case '&': // Address may be followed by an ampersand and command
            rs485_state = STATE_GET_CMD_CMD;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        address = (address << 4) + temp;
	  }
      break;
	case STATE_GET_CMD_CMD:
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
          case '&': // Command code is followed by an argument value
            rs485_state = STATE_GET_CMD_ARG;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        cmd_cmd = (cmd_cmd << 4) + temp;
	  }
	  break;
	case STATE_GET_CMD_ARG:
      temp = get_hex_digit(data);
      if (temp < 0)
      {
		switch (data)
		{
          case 0: // No data
            break;
          case '%': // Command argument is terminated by a percent sign.
            rs485_state = STATE_WAIT_CMD_LF;
            break;
          default:  // Any other character is an error so restart the state machine
            rs485_state = STATE_START;
            break;
		}
	  }
	  else
	  {
        cmd_arg = (cmd_arg << 4) + temp;
	  }
      break;
    case STATE_WAIT_CMD_LF:
      state_wait_for_lf(data, &rs485_state, STATE_START);
      if (rs485_state == STATE_START)
      {
        status = STATE_RS485_GOT_CMD;
      }
      break;
    default: // Should never get here.  If we do, restart the state machine.
      rs485_state = STATE_START;
      break;
  }
  return status;
}
