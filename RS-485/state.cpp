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
const uint8_t STATE_WAIT_CMD_LF = 13;
uint8_t rs485_state = STATE_START;
//
// Data from bus
//
uint32_t device;
uint32_t address;
uint32_t data_type;
uint32_t ages[NUM_NODES] = {0, 0, 0, 0, 0, 0, 0};
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
void state_wait_for_lf(int data, uint8_t *state, uint8_t state_value)
{
  if (data == '\n')
  {
    *state = state_value;
  }
}
//
void state_get_device(int data, uint8_t *state)
{
  switch (data)
  {
    case 0: // No data
      break;
    case '/':  // Device ID is always terminated by a slash, '/'
      if (*state == STATE_GET_MSG_DEV)
      {
        *state = STATE_GET_MSG_ADDR;
      }
      else
      {
        *state = STATE_GET_CMD_ADDR;
      }
      break;
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
      device = (device << 4) + (data - '0');
      break;
    case 'A':
    case 'a':
      device = (device << 4) + 10;
      break;
    case 'B':
    case 'b':
      device = (device << 4) + 11;
      break;
    case 'C':
    case 'c':
      device = (device << 4) + 12;
      break;
    case 'D':
    case 'd':
      device = (device << 4) + 13;
      break;
    case 'E':
    case 'e':
      device = (device << 4) + 14;
      break;
    case 'F':
    case 'f':
      device = (device << 4) + 15;
      break;
    default:  // Any other character is an error so restart the state machine
      *state = STATE_START;
      break;
  }
}
//
void state_get_address(int data, uint8_t *state)
{
  switch (data)
  {
    case 0: // No data
      break;
    case '/':  // Address may be terminated by a slash, if message address and the next
               // field is the message type.
      if (*state == STATE_GET_MSG_ADDR)
      {
        *state = STATE_GET_MSG_TYPE;
      }
      else
      {
        *state = STATE_START;
      }
      break;
    case '&': // Address may be terminated by an ampersand if message address and the
              // next field is data.
      if (*state == STATE_GET_MSG_ADDR)
      {
        *state = STATE_START_BUFFER;
      }
      else
      {
        *state = STATE_START;
      }
      break;
    case '%': // Address may be terminated by a percent sign if command address.
      if (*state == STATE_GET_CMD_ADDR)
      {
        *state = STATE_WAIT_CMD_LF;
      }
      else
      {
        *state = STATE_START;
      }
      break;
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
      address = (address << 4) + (data - '0');
      break;
    case 'A':
    case 'a':
      address = (address << 4) + 10;
      break;
    case 'B':
    case 'b':
      address = (address << 4) + 11;
      break;
    case 'C':
    case 'c':
      address = (address << 4) + 12;
      break;
    case 'D':
    case 'd':
      address = (address << 4) + 13;
      break;
    case 'E':
    case 'e':
      address = (address << 4) + 14;
      break;
    case 'F':
    case 'f':
      address = (address << 4) + 15;
      break;
    default:  // Any other character is an error so restart the state machine
      *state = STATE_START;
      break;
  }
}
//
void state_get_type(int data, uint8_t *state)
{
  switch (data)
  {
    case 0: // No data
      break;
    case '&': // Message type is followed by an ampersand if there is data
      *state = STATE_START_BUFFER;
      break;
    case '%': // Message type is followed by an percent if there is no data
      *state = STATE_BUFFER_END;
      buffer_ptr = 0;
      break;
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
      data_type = (data_type << 4) + (data - '0');
      break;
    case 'A':
    case 'a':
      data_type = (data_type << 4) + 10;
      break;
    case 'B':
    case 'b':
      data_type = (data_type << 4) + 11;
      break;
    case 'C':
    case 'c':
      data_type = (data_type << 4) + 12;
      break;
    case 'D':
    case 'd':
      data_type = (data_type << 4) + 13;
      break;
    case 'E':
    case 'e':
      data_type = (data_type << 4) + 14;
      break;
    case 'F':
    case 'f':
      data_type = (data_type << 4) + 15;
      break;
    default:  // Any other character is an error so restart the state machine
      *state = STATE_START;
      break;
  }
}
//
void state_process_data(char data, uint8_t *state)
{
  switch (data)
  {
    case 0: // No data
      break;
    case '&': // Field separator
      buffer_ptr++;
      if (buffer_ptr >= BUFFER_SIZE)
      {
        *state = STATE_BUFFER_END;
      }
      break;
    case '%': // End of fields, start of checksum
      *state = STATE_BUFFER_END;
      break;
    case '#': // Start of new message
      *state = STATE_GET_MSG_DEV;
      break;
    case '@': // Start of new command
      *state = STATE_GET_CMD_DEV;
      break;
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
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + (data - '0');
      break;
    case 'A':
    case 'a':
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + 10;
      break;
    case 'B':
    case 'b':
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + 11;
      break;
    case 'C':
    case 'c':
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + 12;
      break;
    case 'D':
    case 'd':
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + 13;
      break;
    case 'E':
    case 'e':
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + 14;
      break;
    case 'F':
    case 'f':
      data_buffer[buffer_ptr] = (data_buffer[buffer_ptr] << 4) + 15;
      break;
    default:  // Any other character is an error so restart the state machine
      *state = STATE_START;
      break;
  }
}
//
// This function implements a state machine to read commands and messages off of the
// RS-485 bus.  While the state machine is processing, the return value is
// STATE_RS485_PROCESSING and none of the global variables should be considered to be
// valid.  If a command or message is completed, the return value is set appropriately
// and the data values can be used.
//
uint8_t rs485_state_machine(HardwareSerial *rs485, HardwareSerial *dbg_port, bool debug)
{
  int data;
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
        rs485_state = STATE_GET_CMD_DEV;
      }
      if (data == '#')
      {
        rs485_state = STATE_GET_MSG_DEV;
      }
      device = 0;
      break;
    case STATE_GET_MSG_DEV:
      state_get_device(data, &rs485_state);
      address = 0;
      break;
    case STATE_GET_MSG_ADDR:
      state_get_address(data, &rs485_state);
      data_type = MSG_TYPE_UNKNOWN;
      break;
    case STATE_GET_MSG_TYPE:
      state_get_type(data, &rs485_state);
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
      state_process_data((char) data, & rs485_state);
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
    case STATE_GET_CMD_DEV:
      state_get_device(data, &rs485_state);
      address = 0;
      break;
    case STATE_GET_CMD_ADDR:
      state_get_address(data, &rs485_state);
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
