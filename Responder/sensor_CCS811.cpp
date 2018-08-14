#include <RS-485.h>
#include <wiring_private.h>
#include <Wire.h>

static const bool DEBUG = false;
//
// I2C Stuff
//
const uint32_t I2C_TIMEOUT = 500;
extern uint8_t i2c_buffer[BUFFER_SIZE];
extern uint8_t buff_ptr;
extern uint8_t bytes_pending;
extern uint32_t end_time;
//
extern uint8_t i2c_start_read(uint8_t device, uint8_t reg, uint8_t bytes);
extern uint8_t i2c_read_pending();
//
extern bool found_CCS811;
//
// Status for sub-state machines
//
static const uint8_t MACHINE_WORKING = 1;
static const uint8_t MACHINE_DONE = 2;
//
static const uint8_t STATE_CCS811_ASK_STATUS = 1;
static const uint8_t STATE_CCS811_CHECK_STATUS = 2;
static const uint8_t STATE_CCS811_IS_DATA_READY = 3;
static const uint8_t STATE_CCS811_START_READ = 4;
static const uint8_t STATE_CCS811_CHECK_READ = 5;
static const uint8_t STATE_CCS811_ASK_ERROR = 6;
static const uint8_t STATE_CCS811_CHECK_ERROR = 7;
//
static uint8_t CCS811_state = STATE_CCS811_ASK_STATUS;
//
// Sensor values for CCS811
//
static uint8_t CCS811_data_status = DATA_INIT;
static uint8_t CCS811_age = 0;
static uint32_t CCS811_eCO2 = 0;
static uint32_t CCS811_TVOC = 0;
//
// CCS811 Constant values
//
static const uint8_t ADDR_CCS811 = 0x5A;
//
// Registers
//
static const uint8_t CCS811_STATUS = 0x00;           // Both boot and app mode
static const uint8_t CCS811_CTRL_MEAS_MODE = 0x01;   // App mode only
static const uint8_t CCS811_ALG_RESULT_DATA = 0x02;  // App mode only
static const uint8_t CCS811_RAW_DATA = 0x03;         // App mode only
static const uint8_t CCS811_ENV_DATA = 0x05;         // App mode only
static const uint8_t CCS811_NTC = 0x06;              // App mode only
static const uint8_t CCS811_THRESHOLDS = 0x10;       // App mode only
static const uint8_t CCS811_BASELINE = 0x11;         // App mode only
static const uint8_t CCS811_HW_ID = 0x20;            // Both boot and app mode
static const uint8_t CCS811_HW_VERSION = 0x21;       // Both boot and app mode
static const uint8_t CCS811_FW_BOOT_VERSION = 0x23;  // Both boot and app mode
static const uint8_t CCS811_FW_APP_VERSION = 0x24;   // Both boot and app mode
static const uint8_t CCS811_ERROR_ID = 0xE0;         // Both boot and app mode
static const uint8_t CCS811_APP_START = 0xF4;        // Boot mode only
static const uint8_t CCS811_SW_RESET = 0xFF;         // Both boot and app mode
//
// Register values
//
static const uint8_t CCS811_ID = 0x81;
//
static const uint8_t CCS811_MODE_IDLE =0x00;
static const uint8_t CCS811_MODE_CONST1 = 0x10;
static const uint8_t CCS811_MODE_PULSE10 = 0x20;
static const uint8_t CCS811_MODE_LOW60 = 0x30;
static const uint8_t CCS811_MODE_CONST250 = 0x40;
static const uint8_t CCS811_MODE_INT_ON = 0x08;
static const uint8_t CCS811_MODE_THRESH_ON = 0x04;
//
// Status values
//
static const uint8_t CCSS811_STATUS_FW_MODE = 0x80;
static const uint8_t CCSS811_STATUS_APP_VALID = 0x10;
static const uint8_t CCSS811_STATUS_DATA_READY = 0x08;
static const uint8_t CCSS811_STATUS_ERROR = 0x01;

void CCS811_request_ID()
{
  i2c_start_read(ADDR_CCS811, CCS811_HW_ID, 1);
}

void send_CCS811(HardwareSerial *rs485)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_CCS811, HEX);
  rs485->print("&");
  rs485->print(CCS811_data_status, HEX);
  rs485->print("&");
  rs485->print(CCS811_age, HEX);
  rs485->print("&");
  rs485->print(CCS811_eCO2, HEX);
  rs485->print("&");
  rs485->print(CCS811_TVOC, HEX);
  rs485->println("%FF");
  if ((CCS811_age > 10) && (CCS811_data_status == DATA_VALID))
  {
    CCS811_data_status = DATA_STALE;
  }
  CCS811_age++;
}
//
// Functions for CCS811 sensor.
//
void CCS811_init()
{
  uint8_t success;

  CCS811_request_ID();
  if (DEBUG)
  {
    Serial.println(F("Waiting for CCS811 to respond."));
  }
  end_time = millis() + I2C_TIMEOUT; // Compute timeout value
  while ((i2c_read_pending() > 0) && (millis() > end_time));
  if (i2c_buffer[0] != CCS811_ID)
  {
    if (DEBUG)
    {
      Serial.println(F("Unable to find CCS811."));
    }
    found_CCS811 = false;
    CCS811_data_status = DATA_SENSOR;
  }
  else
  {
    if (DEBUG)
    {
      Serial.println(F("CCS811 Found."));
    }
    found_CCS811 = true;
  }
  //
  // No point continuing if the device was not found.
  //
  if (!found_CCS811)
  {
    return;
  }
  //
  // Now that the device exists, we can initialize it.
  //
  // First, reset the device.
  //
  Wire.beginTransmission(ADDR_CCS811);
  Wire.write(CCS811_SW_RESET);
  Wire.write(0x11);
  Wire.write(0xE5);
  Wire.write(0x72);
  Wire.write(0x8A);
  success = Wire.endTransmission();
  if (DEBUG)
  {
    Serial.print(F("CCS811 software reset returns "));
    Serial.println(success, DEC);
  }
  //
  // There apparently needs to be a bit of delay between doing a software reset and configuring
  // the device.  With DEBUG set true, printing the debug message above would usually provide
  // enough delay - so the device would work with debugging messages but not without.  Adding a
  // 1mS delay here seems to do the trick.  I haven' found that this is documented anywhere,
  //
  delay(1);
  //
  // We assume that there is valid application software on the device.  Otherwise this gets more
  // complicated.  Start the application.
  //
  Wire.beginTransmission(ADDR_CCS811);
  Wire.write(CCS811_APP_START);
  success = Wire.endTransmission();
  if (DEBUG)
  {
    Serial.print(F("CCS811 application start returns "));
    Serial.println(success, DEC);
  }
  //
  // Next, set the desired operating mode.
  // Mode 1 - Constant power mode, IAQ measurement every second
  // Interrupts disabled
  // Thresholds disabled
  //
  Wire.beginTransmission(ADDR_CCS811);
  Wire.write(CCS811_CTRL_MEAS_MODE);
  Wire.write(CCS811_MODE_CONST1);
  success = Wire.endTransmission();
  if (DEBUG)
  {
    Serial.print(F("CCS811 mode set returns "));
    Serial.println(success, DEC);
  }
}

uint8_t CCS811_state_machine()
{
  uint8_t machine_status = MACHINE_WORKING;
  uint8_t device_status;

  switch (CCS811_state)
  {
    case STATE_CCS811_ASK_STATUS:
      i2c_start_read(ADDR_CCS811, CCS811_STATUS, 1);
      CCS811_state = STATE_CCS811_CHECK_STATUS;
      break;
    case STATE_CCS811_CHECK_STATUS:
      if (i2c_read_pending() == 0)
      {
        CCS811_state = STATE_CCS811_IS_DATA_READY;
        device_status = i2c_buffer[0];
        if ((device_status & CCSS811_STATUS_DATA_READY) == CCSS811_STATUS_DATA_READY)
        {
          CCS811_state = STATE_CCS811_START_READ;
        }
        else if ((device_status & CCSS811_STATUS_ERROR) == CCSS811_STATUS_ERROR)
        {
          CCS811_state = STATE_CCS811_ASK_ERROR;
        }
//
// If data is not ready, go back to start and read other sensors.  Data only
// comes in once per second and we don't want to delay others.
//
        else
        {
          CCS811_state = STATE_CCS811_ASK_STATUS;
          machine_status = MACHINE_DONE;
        }
      }
      break;
    case STATE_CCS811_START_READ:
      i2c_start_read(ADDR_CCS811, CCS811_ALG_RESULT_DATA, 4);
      CCS811_state = STATE_CCS811_CHECK_READ;
      break;
    case STATE_CCS811_CHECK_READ:
      if (i2c_read_pending() == 0)
      {
        CCS811_eCO2 = (i2c_buffer[0] << 8) + i2c_buffer[1];
        CCS811_TVOC = (i2c_buffer[2] << 8) + i2c_buffer[3];
        CCS811_data_status = DATA_VALID;
        CCS811_age = 0;
        CCS811_state = STATE_CCS811_ASK_STATUS;
        machine_status = MACHINE_DONE;
        if (DEBUG)
        {
          Serial.print(F("CCS811 data eCO2 is "));
          Serial.print(CCS811_eCO2, DEC);
          Serial.print(F(", TVOC is "));
          Serial.println(CCS811_TVOC, DEC);
        }
      }
      break;
    case STATE_CCS811_ASK_ERROR:
      i2c_start_read(ADDR_CCS811, CCS811_ERROR_ID, 1);
      if (DEBUG)
      {
        Serial.println(F("Requesting CCS811 error code"));
      }
      CCS811_state = STATE_CCS811_CHECK_ERROR;
      break;
    case STATE_CCS811_CHECK_ERROR:
      if (i2c_read_pending() == 0)
      {
        if (DEBUG)
        {
          Serial.print(F("Error code is "));
          Serial.println(i2c_buffer[0], HEX);
        }
        CCS811_state = STATE_CCS811_ASK_STATUS;
        machine_status = MACHINE_DONE;
      }
    default:
      CCS811_data_status = DATA_NO_COMPUTED;
      machine_status = MACHINE_DONE;
      break;
  }
  return machine_status;
}

