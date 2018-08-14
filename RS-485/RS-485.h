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
// Define a node name type.
//
//typedef char[32] node_name;
//
// The serial port definitions are a bit different between the AVR and SAM based Arduinos.
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
// Error codes for state machine
//
const int STATE_ERROR_NONE = 0; // no error
const int STATE_ERROR_RESTART = 1; // Start of record found while processing record
const int STATE_ERROR_END = 2; // Premature end of record found
const int STATE_ERROR_UNEXPECTED = 3; // Other unexpected character found during processing
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
extern unsigned int device;
extern unsigned int address;
extern unsigned int data_type;
extern unsigned long ages[NUM_NODES];
//
// RS-485 data buffer
//
#define BUFFER_SIZE 32
extern uint32_t data_buffer[BUFFER_SIZE];
extern int buffer_ptr;
//
// Defined message types
//
const int MSG_TYPE_UNKNOWN = 0; // Undefined or not present.
const int MSG_TYPE_EMPTY = 1; // Everything is OK, but no data to send
const int MSG_TYPE_NAK = 2; // Address not supported
const int MSG_TYPE_INFO = 3; // Address 0 information message
//
// Status codes for data
//
const int DATA_VALID = 0; // Data is fresh and valid
const int DATA_STALE = 1; // No new data since last trasnmission
const int DATA_INIT = 2;  // Data not ready due to initialization
const int DATA_SENSOR = 3; // Sensor failure
const int DATA_NO_COMPUTED = 4; // Data not computed due to invalid inputs
//
// Conversion functions
//
extern unsigned int hex_to_int(char c);
extern uint32_t str_to_uint(const char* c);
//
// State machine function
//
extern uint8_t rs485_state_machine(HardwareSerial *rs485, HardwareSerial *dbg_port, bool debug);
//
// Functions for predefined messages
//
extern void rs485_msg_unknown(HardwareSerial *rs485, unsigned int device, unsigned int address);
extern void rs485_msg_empty(HardwareSerial *rs485, unsigned int device, unsigned int address);
extern void rs485_msg_nak(HardwareSerial *rs485, unsigned int device, unsigned int address);
void rs485_msg_info(HardwareSerial *rs485, unsigned int device, unsigned int address,
	unsigned int num_addr, const char *name);
