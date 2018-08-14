#include <RS-485.h>
//
// pin 7 - Switch input
// pin 6 - Switch input
// pin 5 - RS-485 direction output
//
//
// Change this to whichever port is hooked up to RS-485.  For the Uno, use the only
// serial port.  For the Mega and Due, use Serial3
//
#if defined (__AVR_ATmega328P__)
HardwareSerial *rs485 = &Serial;
#define RS485_TX_COMPLETE (SERIAL0_TX_COMPLETE)
#else
HardwareSerial *rs485 = &Serial3;
#define RS485_TX_COMPLETE (SERIAL3_TX_COMPLETE)
#endif
const int TX_RX = 2; // Pin 2 is used to control direction.
//
// The following two pins are used to generate signals that are useful for triggering
// an oscilloscope or other such device.
//
const int CMD_PIN = 23; // Set high while a command is being sent
const int MSG_PIN = 22; // Set high while a message is being sent
//
// States
// 0 - Sending command
// 1 - Waiting for transmission to complete
// 2 - Waiting for '\r' indicating end of reply.
//
const uint8_t STATE_START = 0;
const uint8_t STATE_WAIT_TX = 12;
const uint8_t STATE_WAIT_RX = 13;
uint8_t state = STATE_START;
//
// Command information
//
const uint8_t CMD_NULL = 0;       // Do nothing special
const uint8_t CMD_LED_OFF = 1;    // Command all sensors to turn their LED off
const uint8_t CMD_LED_ON = 2;     // Command all sensors to turn their LED on
const uint8_t CMD_LED_TOGGLE = 3; // Command all sensors to toggle their LED (default case)
const uint8_t CMD_ID = 4;         // Request ID from all sensor units
//
// Command pins
//
const uint8_t CMD_PIN_D0 = 4;
const uint8_t CMD_PIN_D1 = 5;
const uint8_t CMD_PIN_D2 = 6;
const uint8_t CMD_PIN_D3 = 7;
const uint8_t CMD_PIN_ENABLE = 3; // Set high to indicate a command is valid
const uint8_t cmd_pins[] = {CMD_PIN_ENABLE, CMD_PIN_D0, CMD_PIN_D1, CMD_PIN_D2, CMD_PIN_D3};
const uint8_t NUM_PINS = 5;
//
uint8_t read_cmd();
//
//
// Delays
//
#define XMIT_DELAY 1
#define TIMEOUT_DELAY 500
//
// Device ID and name
//
// Controller node ID (this node)
//
#define CTRL_NODE 0
//
// Name must be 32 characters padded with spaces on the end.
//                  12345678901234567890123456789012
const char* name = "Control, not KAOS 1             ";
//
uint32_t disc_value = 0;
//
// Supported number of nodes on network
//
//#define NUM_NODES 32 // Defined in header file.
int node_state[NUM_NODES];
int max_addr[NUM_NODES];
unsigned int max_node = 7;
unsigned int timeout_delay = TIMEOUT_DELAY;
unsigned long end_time;
unsigned int node_id = NUM_NODES;

const bool DEBUG = true;

void setup()
{
  int x;

  rs485->begin(BAUD_RATE);
  Serial.begin(BAUD_RATE);
  pinMode(TX_RX, OUTPUT);  // RS-485 direction control
  pinMode(CMD_PIN, OUTPUT);
  pinMode(MSG_PIN, OUTPUT);
  for (x = 0; x < NUM_NODES; x++)
  {
    node_state[x] = 0;
    max_addr[x] = 0;
  }
  for (x = 0; x < NUM_PINS; x++)
  {
    pinMode(cmd_pins[x], INPUT);
  }
}

void loop()
{
  int x;
  int data;
  uint8_t reply_state;
  uint8_t command;
  static uint8_t led_state = 0;
  static uint8_t led_command = CMD_LED_OFF;
//
// Process commands coming in on the discretes.
//
  command = read_cmd();
  if (command == CMD_LED_OFF)
  {
    led_command = CMD_LED_OFF;
    led_state = 0;
  }
  if (command == CMD_LED_ON)
  {
    led_command = CMD_LED_ON;
    led_state = 1;
  }
  if (command == CMD_LED_TOGGLE)
  {
    led_command = CMD_LED_TOGGLE;
  }
  if (command == CMD_ID)
  {
    for (x = 0; x < NUM_NODES; x++)
    {
      node_state[x] = 0;
      max_addr[x] = 0;
    }
    command = CMD_NULL;
  }
//
// Main state machine
//
  switch (state)
  {
    case STATE_START:
      node_id++;
      if (node_id > max_node)
      {
        node_id = CTRL_NODE;
      }
      delay(XMIT_DELAY);
      if (node_id == CTRL_NODE)
      {
        Serial.println("Control node transmission");
      }
      else
      {
        Serial.print("Requesting device ");
        Serial.print(node_id, DEC);
        Serial.print(" address ");
        Serial.println(node_state[node_id]);
      }
      digitalWrite(TX_RX, HIGH);  // Prepare to transmit
      if (node_id == CTRL_NODE)
      {
        digitalWrite(MSG_PIN, HIGH);
        if (node_state[CTRL_NODE] == 0)
        {
          rs485_msg_info(rs485, CTRL_NODE, 0, 1, name);
//          rs485->println(" "); // For some reason, the previous message doesn't always have a
//                               // cr-lf terminating it.
//          delay(1);
        }
        else
        {
          if (led_command == CMD_LED_TOGGLE)
          {
            led_state++;
            led_state = led_state % 2;
          }
          rs485_msg_disc(rs485, CTRL_NODE, 1, DISCRETE_CMD, disc_value*2 + led_state);
//          rs485->println(" "); // For some reason, the previous message doesn't always have a
//                               // cr-lf terminating it.
//          delay(1);
          //
          // For test purposes, just increment the discrete value.
          //
          disc_value++;
        }
        digitalWrite(MSG_PIN, LOW);
        node_state[CTRL_NODE] = 1;
      }
      else
      {
        digitalWrite(CMD_PIN, HIGH);
        rs485->print("@");
        rs485->print(node_id, HEX);
        rs485->print("/");
        rs485->print(node_state[node_id], HEX);
        rs485->println("%FF");
        digitalWrite(CMD_PIN, LOW);
      }
      state = STATE_WAIT_TX;
      break;
    case STATE_WAIT_TX: // Wait for transmission to complete
      if (RS485_TX_COMPLETE)
      {
        digitalWrite(TX_RX, LOW);  // End transmission
        if (node_id == 0)
        {
//          delay(1); // A little wait to allow the transmission to clear out
          state = STATE_START; // There is no response to the message from the controller
        }
        else
        {
          state = STATE_WAIT_RX;
          end_time = millis() + TIMEOUT_DELAY; // Compute timeout value
        }
      }
      break;
    case STATE_WAIT_RX: // Wait for response or timemout
      reply_state = rs485_state_machine(rs485, &Serial, false);
      if (reply_state == STATE_RS485_GOT_MSG)
      {
        Serial.print("Got reply from node ");
        Serial.println(device, DEC);
        if (device == node_id)
        {
          if  (address == 0)
          {
            max_addr[node_id] = data_buffer[0];
          }
          node_state[node_id]++;
          if (node_state[node_id] > max_addr[node_id])
          {
            node_state[node_id] = 1;
          }
        }
        else
        {
          Serial.print("Node ");
          Serial.print(device, DEC);
          Serial.print(" responded, expected node ");
          Serial.println(node_id, DEC);
        }
        delay(5); // A little delay to wait for sender to release the bus
        state = STATE_START;
      }
      //
      // The timeout test here will need to be updated to account for wraparound
      // of the timer.
      //
      if (millis() > end_time)
      {
        node_state[node_id] = 0;
        state = STATE_START;
      }
      break;
    default:
      break;
  }
}

uint8_t read_cmd()
{
  uint8_t enabled = digitalRead(CMD_PIN_ENABLE);
  uint8_t command = 1*digitalRead(CMD_PIN_D0) + 2*digitalRead(CMD_PIN_D1) +
          4*digitalRead(CMD_PIN_D2) + 8*digitalRead(CMD_PIN_D3);

  if (enabled)
  {
    return command;
  }
  else
  {
    return CMD_NULL;
  }
}

