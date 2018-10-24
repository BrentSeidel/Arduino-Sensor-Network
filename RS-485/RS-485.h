#include <Arduino.h>
//
// Some common definitions for the RS-485 project
//
/*
According to selected target processor, compiler defined constant by processor name.

Constant  CPU Board
__AVR_ATmega168__   ATmega 168  Arduino Decimilia and older
__AVR_ATmega328P__  ATmega 328P Arduino Duemilanove and Uno
__AVR_ATmega1280__  ATmega 1280 Arduino Mega
__AVR_ATmega2560__  ATmega 2560 Arduino Mega 2560
__AVR_ATmega32U4__  ATmega 32U4 Arduino Leonardo
__SAM3X8E__ AT91SAM3X8E Arduino Due
*/
//
// Header file for the RS-485 interface library.  This defines constants, types,
// functions, and globals used by the interface.
//
//
// If on an Arduino Due, some definitions need to be included.
//
#if defined __SAM3X8E__
#include <sam3xa/include/sam3x8e.h>
#endif

#include <HardwareSerial.h>
#include <wiring_private.h>
//
// The serial port definitions are a bit different between the AVR and
// SAM based Arduinos.
//
#if defined __SAM3X8E__
#define SERIAL0_TX_COMPLETE (REG_UART_SR & UART_SR_TXEMPTY)
#define SERIAL1_TX_COMPLETE (REG_USART0_CSR & US_CSR_TXEMPTY)
#define SERIAL2_TX_COMPLETE (REG_USART1_CSR & US_CSR_TXEMPTY)
//
// It's not clear what USART2 is being used for.
//
#define SERIAL3_TX_COMPLETE (REG_USART3_CSR & US_CSR_TXEMPTY)

#else

#define SERIAL0_TX_COMPLETE (UCSR0A & _BV(TXC0))
#define SERIAL1_TX_COMPLETE (UCSR1A & _BV(TXC1))
#define SERIAL2_TX_COMPLETE (UCSR2A & _BV(TXC2))
#define SERIAL3_TX_COMPLETE (UCSR3A & _BV(TXC3))
#endif
//
// Since all devices on the bus should be using the same baud rate, it can be
// defined here.  From the Arduino page, for communicating with a computer, the
// following should be used (they are fairly standard rates):
// 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 28800, 38400, 57600, or 115200.
// Other rates can be used, but make sure that all your devices support them.
// For devices like the Arduino Mega 2560 and the Arduino Due, each serial port can
// have a different baud rate.
//
#define BAUD_RATE 115200
//
// Controller node ID
//
#define CTRL_NODE 0
//
// Error codes for state machine
//
const uint8_t STATE_ERROR_NONE = 0; // no error
const uint8_t STATE_ERROR_RESTART = 1; // Start of record found while processing record
const uint8_t STATE_ERROR_END = 2; // Premature end of record found
const uint8_t STATE_ERROR_UNEXPECTED = 3; // Other unexpected character found during processing
//
// RS-485 state machine return values
//
const uint8_t STATE_RS485_PROCESSING = 0; // Waiting for the end of cmd or msg
const uint8_t STATE_RS485_GOT_CMD = 1; // End of command reached
const uint8_t STATE_RS485_GOT_MSG = 2; // End of message reached
//
// Data from bus
//
#define NUM_NODES 32
extern uint32_t device;
extern uint32_t address;
extern uint32_t data_type;
extern uint32_t ages[NUM_NODES];
extern uint32_t cmd_cmd;
extern uint32_t cmd_arg;
//
// RS-485 data buffer
//
#define BUFFER_SIZE 32
extern uint32_t data_buffer[BUFFER_SIZE];
extern uint8_t buffer_ptr;
//
// Defined commands from controller
//
const uint32_t CMD_READ  = 0; // The default command
const uint32_t CMD_RESET = 1; // Perform a soft reset
//
// Defined message types
//
const uint32_t MSG_TYPE_UNKNOWN = 0;  // Undefined or not present.
const uint32_t MSG_TYPE_EMPTY = 1;    // Everything is OK, but no data to send
const uint32_t MSG_TYPE_NAK = 2;      // Address not supported
const uint32_t MSG_TYPE_INFO = 3;     // Address 0 information message
const uint32_t MSG_TYPE_BME280 = 4;   // BME280 sensor values
const uint32_t MSG_TYPE_DISCRETE = 5; // Discretes
const uint32_t MSG_TYPE_ANALOG = 6;   // Analog values
const uint32_t MSG_TYPE_VERSION = 7;  // Version/Software ID
const uint32_t MSG_TYPE_CCS811 = 8;   // CCS811 sensor values
const uint32_t MSG_TYPE_TSL2561 = 9;  // TSL2651 sensor values
//
const uint32_t DISCRETE_UNKNOWN = 0; // Unknown discrete type
const uint32_t DISCRETE_CMD = 1;     // Command discretes from controller
const uint32_t DISCRETE_MIXED = 2;   // Mixed discrete types
const uint32_t DISCRETE_SWITCH = 3;  // Discretes from switches
const uint32_t DISC_CMD_LED = 0x00000001; // Turn LED on
const uint32_t DISC_CMD_1  = 0x00000002;
const uint32_t DISC_CMD_2  = 0x00000004;
const uint32_t DISC_CMD_3  = 0x00000008;
const uint32_t DISC_CMD_4  = 0x00000010;
const uint32_t DISC_CMD_5  = 0x00000020;
const uint32_t DISC_CMD_6  = 0x00000040;
const uint32_t DISC_CMD_7  = 0x00000080;
const uint32_t DISC_CMD_8  = 0x00000100;
const uint32_t DISC_CMD_9  = 0x00000200;
const uint32_t DISC_CMD_10 = 0x00000400;
const uint32_t DISC_CMD_11 = 0x00000800;
const uint32_t DISC_CMD_12 = 0x00001000;
const uint32_t DISC_CMD_13 = 0x00002000;
const uint32_t DISC_CMD_14 = 0x00004000;
const uint32_t DISC_CMD_15 = 0x00008000;
const uint32_t DISC_CMD_16 = 0x00010000;
const uint32_t DISC_CMD_17 = 0x00020000;
const uint32_t DISC_CMD_18 = 0x00040000;
const uint32_t DISC_CMD_19 = 0x00080000;
const uint32_t DISC_CMD_20 = 0x00100000;
const uint32_t DISC_CMD_21 = 0x00200000;
const uint32_t DISC_CMD_22 = 0x00400000;
const uint32_t DISC_CMD_23 = 0x00800000;
const uint32_t DISC_CMD_24 = 0x01000000;
const uint32_t DISC_CMD_25 = 0x02000000;
const uint32_t DISC_CMD_26 = 0x04000000;
const uint32_t DISC_CMD_27 = 0x08000000;
const uint32_t DISC_CMD_28 = 0x10000000;
const uint32_t DISC_CMD_29 = 0x20000000;
const uint32_t DISC_CMD_30 = 0x40000000;
const uint32_t DISC_CMD_31 = 0x80000000;
//
// Analog values - the number of values is in the 5 LSBs of the type
//
const uint32_t ANALOG_UNKNOWN = 0; // Unknown analog types
const uint32_t ANALOG_MIXED = 32;  // Mixed analog types
const uint32_t ANALOG_POT = 64;    // Analog values from potentiometers
//
//
// Status codes for data
//
const uint8_t DATA_VALID = 0; // Data is fresh and valid
const uint8_t DATA_STALE = 1; // No new data since last trasnmission
const uint8_t DATA_INIT = 2;  // Data not ready due to initialization
const uint8_t DATA_SENSOR = 3; // Sensor failure
const uint8_t DATA_NO_COMPUTED = 4; // Data not computed due to invalid inputs
//
// Conversion functions
//
extern uint8_t hex_to_int(char c);
extern uint32_t str_to_uint(const char* c);
//
// Utility function to perform a soft reset
//
extern void softReset();
//
// State machine function
//
extern uint8_t rs485_state_machine(HardwareSerial *rs485, HardwareSerial *dbg_port, bool debug);
//
// Functions for predefined messages
//
extern void rs485_msg_unknown(HardwareSerial *rs485, uint32_t device, uint32_t address);
extern void rs485_msg_empty(HardwareSerial *rs485, uint32_t device, uint32_t address);
extern void rs485_msg_nak(HardwareSerial *rs485, uint32_t device, uint32_t address);
extern void rs485_msg_info(HardwareSerial *rs485, uint32_t device, uint32_t address,
	uint32_t num_addr, const char *name);
extern void rs485_msg_disc(HardwareSerial *rs485, uint32_t device, uint32_t address,
	uint32_t disc_type, uint32_t disc_value);
extern void rs485_msg_analog(HardwareSerial *rs485, uint32_t device, uint32_t address,
	uint32_t analog_type, const uint32_t *analog_values);
