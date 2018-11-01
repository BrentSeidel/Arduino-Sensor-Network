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
// To allow commands from a BeagleBone Black, a Mega or Due is required.  In this case,
// the main RS-485 network is on Serial3 and the commands are input on Serial2.
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
// Command definitions for general commands.  Directed commands are defined in the
// rs485.h header file.
//
const uint8_t CMD_NULL = 0;       // Do nothing special
const uint8_t CMD_LED_OFF = 1;    // Command all sensors to turn their LED off
const uint8_t CMD_LED_ON = 2;     // Command all sensors to turn their LED on
const uint8_t CMD_LED_TOGGLE = 3; // Command all sensors to toggle their LED (default case)
const uint8_t CMD_ID = 4;         // Request ID from all sensor units
//
//  Command states
//
const uint8_t CMD_STATE_START = 0;
const uint8_t CMD_STATE_CMD = 1;
const uint8_t CMD_STATE_GET_CMD = 2;
const uint8_t CMD_STATE_GET_NODE = 3;
const uint8_t CMD_STATE_GET_ARG = 4;
const uint8_t CMD_STATE_DONE = 5;
uint8_t read_cmd();
struct cmd_state_type
{
  uint8_t state;
  uint32_t cmd;
  uint32_t args;
  uint32_t node;
  boolean directed;
  boolean need_node;
  boolean need_cmd;
  boolean need_arg;
};
cmd_state_type cmd_data = {.state = CMD_STATE_START};
struct directed_cmd_type
{
  uint32_t cmd;
  uint32_t arg;
};
directed_cmd_type directed_cmd[NUM_NODES];
//
// Delays
//
#define XMIT_DELAY 1
#define TIMEOUT_DELAY 500
//
// Device ID and name
//
// Controller node ID (this node).  Controller is always node 0.
//
#define CTRL_NODE 0
//
// Name must be 32 characters padded with spaces on the end.
//                  12345678901234567890123456789012
const char* name = "Control, not KAOS               ";
//
uint32_t disc_value = 0;
//
// Supported number of nodes on network
//
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
  Serial2.begin(BAUD_RATE);
  Serial.begin(BAUD_RATE);
  pinMode(TX_RX, OUTPUT);  // RS-485 direction control
  pinMode(CMD_PIN, OUTPUT);
  pinMode(MSG_PIN, OUTPUT);
  for (x = 0; x < NUM_NODES; x++)
  {
    node_state[x] = 0;
    max_addr[x] = 0;
    directed_cmd[x].cmd = CMD_NULL;
    directed_cmd[x].arg = 0;
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
  switch (read_cmd())
  {
    case CMD_LED_OFF:
      led_command = CMD_LED_OFF;
      led_state = 0;
      break;
    case CMD_LED_ON:
      led_command = CMD_LED_ON;
      led_state = 1;
      break;
    case CMD_LED_TOGGLE:
      led_command = CMD_LED_TOGGLE;
      break;
    case CMD_ID:
      for (x = 0; x < NUM_NODES; x++)
      {
        node_state[x] = 0;
        max_addr[x] = 0;
      }
      break;
    default:
      break;
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
//      else
//      {
//        Serial.print("Requesting device ");
//        Serial.print(node_id, DEC);
//        Serial.print(" address ");
//        Serial.println(node_state[node_id]);
//      }
      digitalWrite(TX_RX, HIGH);  // Prepare to transmit
      if (node_id == CTRL_NODE)
      {
        digitalWrite(MSG_PIN, HIGH);
        if (node_state[CTRL_NODE] == 0)
        {
          rs485_msg_info(rs485, CTRL_NODE, 0, 1, name);
        }
        else
        {
          if (led_command == CMD_LED_TOGGLE)
          {
            led_state++;
            led_state = led_state % 2;
          }
          rs485_msg_disc(rs485, CTRL_NODE, 1, DISCRETE_CMD, disc_value*2 + led_state);
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
        if (directed_cmd[node_id].cmd == CMD_READ)
        {
          Serial.print("Requesting read of node ");
          Serial.println(node_id, DEC);
          digitalWrite(CMD_PIN, HIGH);
          rs485->print("@");
          rs485->print(node_id, HEX);
          rs485->print("/");
          rs485->print(node_state[node_id], HEX);
          rs485->println("%FF");
          digitalWrite(CMD_PIN, LOW);
        }
        else
        {
          Serial.print("Directed command to node ");
          Serial.println(node_id, DEC);
          digitalWrite(CMD_PIN, HIGH);
          rs485->print("@");
          rs485->print(node_id, HEX);
          rs485->print("/");
          rs485->print(node_state[node_id], HEX);
          rs485->print("&");
          rs485->print(directed_cmd[node_id].cmd, HEX);
          rs485->print("&");
          rs485->print(directed_cmd[node_id].arg, HEX);
          rs485->println("%FF");
          digitalWrite(CMD_PIN, LOW);
          directed_cmd[node_id].cmd = CMD_READ;
          directed_cmd[node_id].arg = 0;
        }
      }
      state = STATE_WAIT_TX;
      break;
    case STATE_WAIT_TX: // Wait for transmission to complete
      if (RS485_TX_COMPLETE)
      {
        digitalWrite(TX_RX, LOW);  // End transmission
        if (node_id == 0)
        {
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
//        Serial.print("Got reply from node ");
//        Serial.println(device, DEC);
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
  int data = Serial2.read();
  uint8_t command = CMD_NULL;

  if (data > 0)
  {
    switch (cmd_data.state)
    {
      case CMD_STATE_START:
        if (data == '!')
        {
          cmd_data.state = CMD_STATE_CMD;
        }
        cmd_data.cmd = CMD_NULL;
        cmd_data.args = 0;
        cmd_data.node = 0;
        cmd_data.directed = false;
        cmd_data.need_node = false;
        cmd_data.need_cmd = false;
        cmd_data.need_arg = false;
        break;
      case CMD_STATE_CMD:
        switch (data)
        {
          case 'A':
            Serial.println("Command LED Off");
            cmd_data.cmd = CMD_LED_OFF;
            cmd_data.state = CMD_STATE_DONE;
            break;
          case 'B':
            Serial.println("Command LED On");
            cmd_data.cmd = CMD_LED_ON;
            cmd_data.state = CMD_STATE_DONE;
            break;
          case 'C':
            Serial.println("Command LED Toggle");
            cmd_data.cmd = CMD_LED_TOGGLE;
            cmd_data.state = CMD_STATE_DONE;
            break;
          case 'D':
            Serial.println("Command Identify");
            cmd_data.cmd = CMD_ID;
            cmd_data.state = CMD_STATE_DONE;
            break;
          case 'R':
            Serial.println("Directed reset command");
            cmd_data.cmd = CMD_RESET;
            cmd_data.state = CMD_STATE_GET_NODE;
            cmd_data.directed = true;
            cmd_data.need_node = true;
            break;
          case 'S':
            Serial.println("Directed arbitrary command");
            cmd_data.state = CMD_STATE_GET_CMD;
            cmd_data.directed = true;
            cmd_data.need_node = true;
            cmd_data.need_arg = true;
            break;
          default:
            cmd_data.state = CMD_STATE_START;
            break;
        }
        break;
      case CMD_STATE_GET_CMD:
//        Serial.println("State get command.");
        if ((data >= '0') && (data <= '9'))
        {
          cmd_data.cmd = cmd_data.cmd*10 + (data - '0');
        }
        else
        {
          if ((data == ',') && (cmd_data.need_node))
          {
            cmd_data.state = CMD_STATE_GET_NODE;
          }
          else if ((data == ',') && (cmd_data.need_arg)) 
          {
            cmd_data.state = CMD_STATE_GET_ARG;
          }
          else if (data == ',')
          {
            cmd_data.state = CMD_STATE_DONE;
          }
          else
          {
            cmd_data.state = CMD_STATE_START;
          }
        }
        break;
      case CMD_STATE_GET_NODE:
//        Serial.println("State get node.");
        if ((data >= '0') && (data <= '9'))
        {
          cmd_data.node = cmd_data.node*10 + (data - '0');
        }
        else
        {
          if ((data == ',') && (cmd_data.need_arg)) 
          {
            cmd_data.state = CMD_STATE_GET_ARG;
          }
          else if (data == ',')
          {
            cmd_data.state = CMD_STATE_DONE;
          }
          else
          {
            cmd_data.state = CMD_STATE_START;
          }
        }
        break;
      case CMD_STATE_GET_ARG:
//        Serial.println("State get arg.");
        if ((data >= '0') && (data <= '9'))
        {
          cmd_data.args = cmd_data.args*10 + (data - '0');
        }
        else
        {
          if (data == ',')
          {
            cmd_data.state = CMD_STATE_DONE;
          }
          else
          {
            cmd_data.state = CMD_STATE_START;
          }
        }

        cmd_data.state = CMD_STATE_DONE;
        break;
      case CMD_STATE_DONE:
//        Serial.println("State done.");
        cmd_data.state = CMD_STATE_START;
        if (cmd_data.directed)
        {
          Serial.print("Sending directed command ");
          Serial.print(cmd_data.cmd, DEC);
          Serial.print(" to node ");
          Serial.print(cmd_data.node, DEC);
          Serial.print(" with arguments ");
          Serial.println(cmd_data.args, DEC);
          if ((cmd_data.node > 0) && (cmd_data.node < NUM_NODES))
          {
            directed_cmd[cmd_data.node].cmd = cmd_data.cmd;
            directed_cmd[cmd_data.node].arg = cmd_data.args;
          }
        }
        else
        {
          command = cmd_data.cmd;
        }
        break;
      default:
        cmd_data.state = CMD_STATE_START;
        break;
    }
    Serial.write(data);
  }
  return command;
}

