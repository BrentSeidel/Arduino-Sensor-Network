#include <RS-485.h>
#include <Wire.h>
//
// Display debugging messages?  If set true, the node should either be an Arduino
// Mega 2560 (or similar with multiple serial ports) or disconnected from the network.
// On the basic Arduino the RS485 and the debug serial port are shared.
//
const bool DEBUG = false;
//
// Pick which serial device to use for the RS-485 interface.
//
#if defined (__AVR_ATmega328P__)
HardwareSerial *rs485 = &Serial;
#define RS485_TX_COMPLETE (SERIAL0_TX_COMPLETE)
#else
HardwareSerial *rs485 = &Serial3;
#define RS485_TX_COMPLETE (SERIAL3_TX_COMPLETE)
#endif
//
// Configuration values.  These need to be set individually for each node.
//
#define DEVICE_ID 4
// Name must be 32 characters padded with spaces on the end.
//                  12345678901234567890123456789012
#if DEVICE_ID == 1
const char* name = "Bedroom Sensor Unit             "; // 1
#else
#if DEVICE_ID == 2
const char* name = "Hall Monitor Unit               "; // 2
#else
#if DEVICE_ID == 3
const char* name = "Office Sensor Unit              "; // 3
#else
#if DEVICE_ID == 4
const char* name = "Development and Test Unit       "; // 4
#else
#if DEVICE_ID == 5
const char* name = "Guest Room Sensor Unit          "; // 5
#else
#if DEVICE_ID == 6
const char* name = "Another Sensor Unit             "; // 6
#else
const char* name = "Unknown Sensor Unit             "; // Other ID
#endif
#endif
#endif
#endif
#endif
#endif
//
// System configuration.  This is used to enable or disable transmission of various
// messages depending on the nodes configuration.  In some cases (i.e. discretes or
// analogs), additional code will need to be added to process the inputs and produce
// the outputs.
//
// Configuration constants.  When this list gets updated, the determine_transmission
// and determine_config functions also needs to be updated.
//
const uint8_t CONFIG_NAK = 0; // No configuration item
const uint8_t CONFIG_DISCRETE = 1; // Discretes item
const uint8_t CONFIG_ANALOG = 2; // Analog item
const uint8_t CONFIG_BME280 = 3; // BME280 environment sensor
const uint8_t CONFIG_TSL2561 = 4; // TSL2561 light sensor
const uint8_t CONFIG_CCS811 = 5; // CCS811 air quality sensor

const uint8_t NUM_CONFIG = 5; // Number of configuration items
//
// Configuration array
//
uint8_t config_array[NUM_CONFIG];
//
// Set true if the node is to transmit discrete data.
//
const bool has_discretes = true;
//
// Set true if the node is to transmit analog data.
//
const bool has_analogs = true;
//
// Set true if a PING sonar unit is installed
//
const bool has_sonar = false;
//
// Check for the presence of I2C based sensors.  These are set during the I2C
// initialization.
//
extern bool found_BME280;
extern bool found_TSL2561;
extern bool found_CCS811;
//
// Build configuration array
//
uint8_t determine_config();
//
// Determine what to transmit
//
void determine_transmission(uint8_t value);
//
// This can be changed to whatever pin is being used.
//
const uint8_t TX_RX = 5; // Pin 5 is used to control direction.
//
// Values for transmission
//
extern void send_BME280(HardwareSerial *);
extern void send_TSL2561(HardwareSerial *);
extern void send_CCS811(HardwareSerial *);
//
// Sample discrete value
//
uint32_t silly_counter = 0;
//
// Declare functions.
//
//
extern void i2c_init();
extern void i2c_state_machine();
//
// Do whatever local initialization is needed, then call the i2c initializatio
// function.  This will initialize the I2C bus and all the known devices on it.
//
void setup()
{
#if !defined (__AVR_ATmega328P__)
    Serial.begin(BAUD_RATE);
#endif
  rs485->begin(BAUD_RATE);
  if (DEBUG)
  {
    Serial.println(F("Starting initialization."));
    Serial.println(name);
  }
#if DEVICE_ID == 4
  analogReference(DEFAULT);
  pinMode(8, INPUT_PULLUP);
  pinMode(9, INPUT_PULLUP);
  pinMode(10, INPUT_PULLUP);
  pinMode(11, INPUT_PULLUP);
#endif
  pinMode(TX_RX, OUTPUT);
  pinMode(LED_BUILTIN, OUTPUT);
  digitalWrite(TX_RX, LOW);
  digitalWrite(LED_BUILTIN, LOW);
  i2c_init();
  if (DEBUG)
  {
    Serial.println(F("Initialization complete.  Waiting for data."));
  }
  determine_config();
}
//
// The loop function calls two state machines, so each time through the loop function,
// one iteration of each state machine in processed.  This is essentially a simple form
// of multitasking.  To ensure that are properly processed, the amount of time spent in
// each state should be minimized.  Probably the most time critical item is ensuring
// that the RS-485 bus is released when transmission is completed.
//
// The first state machine is the RS485 processing.  This monitors the RS485 bus for
// commands and messages.  If a command or message is received, an appropriate value
// is returned and the loop code generates a response, if necessary.
//
// The second state machine is for the I2C processing.  The processing done depends on
// which devices are detected on the I2C bus.  In general, data from each device is read
// and the results are stored for transmission.  Since the I2C state machine is decoupled
// from the RS485 state machine, it is possible for I2C values to be read any number of
// times before transmission is requested.  The routines for each I2C device are
// responsible for storage and transmission of the data.  Typically, the data is not
// queued and only the lastest value is transmitted.
//
void loop()
{
  uint8_t total_addresses;

  switch (rs485_state_machine(rs485, &Serial, DEBUG))
  {
    case STATE_RS485_PROCESSING: // Do nothing for this
      break;
    case STATE_RS485_GOT_CMD:  // Responder.  Check for commands.
      if (DEBUG)
      {
        Serial.print(F("Device is: "));
        Serial.print(device, HEX);
        Serial.print(F(", address is: "));
        Serial.println(address, HEX);
      }
      if (device == DEVICE_ID)
      {
        if (DEBUG)
        {
          Serial.print(F("Responding to device "));
          Serial.print(device, DEC);
          Serial.print(F(", address "));
          Serial.println(address, DEC);
        }
        digitalWrite(TX_RX, HIGH);  // Prepare to transmit
        switch (address)
        {
          case 0: // Build configuration and return info record
            total_addresses = determine_config();
            rs485_msg_info(rs485, DEVICE_ID, 0, total_addresses, name);
            break;
          default: // Figure out what to do for other addresses
            if (address < NUM_CONFIG)
            {
              determine_transmission(config_array[address - 1]);
            }
            else
            {
              rs485_msg_nak(rs485, DEVICE_ID, address);
            }
            break;
        }
      }
      break;
    case STATE_RS485_GOT_MSG: // Look for commands from the controller
      if ((device == CTRL_NODE) && (data_type == MSG_TYPE_DISCRETE) &&
          (data_buffer[0] == DISCRETE_CMD))
      {
        if (data_buffer[1] & DISC_CMD_LED)
        {
          digitalWrite(LED_BUILTIN, HIGH);
        }
        else
        {
          digitalWrite(LED_BUILTIN, LOW);
        }
      }
      break;
    default: // Ignore anything else.
      break;
  }
  //
  // Check if transmitter is clear.  If so, release the bus.  The controller node waits
  // 5mS after receiving a response message before sending the next command or message.
  // Thus, no combination of RS485 and I2C state machine states should take more than
  // 5mS.  If this is not possible, this test can also be done after the I2C state
  // machine.
  //
  if (RS485_TX_COMPLETE)
  {
    digitalWrite(TX_RX, LOW);  // Release bus
  }
  //
  // Run the I2C state machine
  //
  i2c_state_machine();
}
//
// Determine what record to transmit depending on requested address and
// configuration array.  This needs to be updated as more data types are
// added.
//
void determine_transmission(uint8_t value)
{
  int temp;
  uint32_t analogs[1];

  switch (value)
  {
    case CONFIG_NAK:
      rs485_msg_nak(rs485, DEVICE_ID, address);
      break;
    case CONFIG_DISCRETE:
#if DEVICE_ID == 4
      temp = digitalRead(8) == HIGH   ? 1 : 0;
      temp += digitalRead(9) == HIGH  ? 2 : 0;
      temp += digitalRead(10) == HIGH ? 4 : 0;
      temp += digitalRead(11) == HIGH ? 8 : 0;
      rs485_msg_disc(rs485, DEVICE_ID, address, DISCRETE_SWITCH, temp);
#else
      rs485_msg_disc(rs485, DEVICE_ID, address, 0, silly_counter++);
#endif
      break;
    case CONFIG_ANALOG: // Analog transmission is not yet implemented
#if DEVICE_ID == 4
        analogs[0] = analogRead(0);
        rs485_msg_analog(rs485, DEVICE_ID, address, ANALOG_POT + 1, analogs);
#else
        rs485_msg_nak(rs485, DEVICE_ID, address);
#endif
      break;
    case CONFIG_BME280:
      send_BME280(rs485);
      break;
    case CONFIG_TSL2561:
      send_TSL2561(rs485);
      break;
    case CONFIG_CCS811:
      send_CCS811(rs485);
      break;
    default: // Some other unknown value
      rs485_msg_nak(rs485, DEVICE_ID, address);
      break;
  }
}
//
// This builds the configuration array and returns the number of addresses
// supported by the device.
//
uint8_t determine_config()
{
  uint8_t item;
  uint8_t x;

  for (x = 0; x < NUM_CONFIG; x++)
  {
    config_array[x] = CONFIG_NAK;
  }
  item = 0;
  if (has_discretes)
  {
    config_array[item] = CONFIG_DISCRETE;
    item++;
  }
  if (has_analogs)
  {
    config_array[item] = CONFIG_ANALOG;
    item++;
  }
  if (found_BME280)
  {
    config_array[item] = CONFIG_BME280;
    item++;
  }
  if (found_TSL2561)
  {
    config_array[item] = CONFIG_TSL2561;
    item++;
  }
  if (found_CCS811)
  {
    config_array[item] = CONFIG_CCS811;
    item++;
  }
  if (DEBUG)
  {
    for (x = 0; x < NUM_CONFIG; x++)
    {
      Serial.print("Config entry ");
      Serial.print(x, DEC);
      Serial.print(" is set to ");
      Serial.println(config_array[x], DEC);
    }
  }
  return item;
}

