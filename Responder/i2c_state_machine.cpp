#include <Wire.h>
#include <wiring_private.h>
#include <RS-485.h>
//
const bool DEBUG = false;
//
// Locally defined and used functions
//
uint8_t i2c_start_read(uint8_t device, uint8_t reg, uint8_t bytes);
uint8_t i2c_read_pending();
//
// External functions for the I2C devices.
//
extern void BME280_init();
extern uint8_t BME280_state_machine();
//
extern void TSL2561_init();
extern uint8_t TSL2561_state_machine();
//
extern void CCS811_init();
extern uint8_t CCS811_state_machine();
//
extern void PCA9685_init();
//
// File contains processing for I2C sensors.
// Currently the BME280, TSL2561, and CCS811 sensors are supported.
//
// Note that since other periodic proceesing may be occuring, the
// software needs to be written to avoid blocking.  Instead of simple
// sequential code that waits for I/O to complete before continuing,
// a state machine is used.
//
// To simplify things and improve encapsulation, each device has its
// own state machine that does the processing needed for that device.
// The main state machine selects which device to process and when it
// is finished, selects the next device based on which devices were
// found during initialization.
//
//
// I2C state machine
//
static const uint8_t STATE_START = 0;
static const uint8_t STATE_BME280_STATE_MACHINE = 1;
static const uint8_t STATE_TSL2561_MACHINE = 2;
static const uint8_t STATE_CCS811_MACHINE = 3;
//static const uint8_t STATE_PCA9685_MACHINE;  // No state machine for the PCA9685.
//
static uint8_t i2c_state = STATE_START;
//
// Status for sub-state machines
//
static const uint8_t MACHINE_WORKING = 1;
static const uint8_t MACHINE_DONE = 2;
//
bool found_BME280  = false;
bool found_TSL2561 = false;
bool found_CCS811  = false;
bool found_PCA9685 = false;
//
// Buffer for i2c data
//
const uint32_t I2C_TIMEOUT = 500;
uint8_t i2c_buffer[BUFFER_SIZE];
uint8_t buff_ptr = 0;
uint8_t bytes_pending = 0;
uint32_t end_time;
//
// Called from the main init() routine to initialize devices on the I2C bus.
//
void i2c_init()
{
  Wire.begin();
  //
  // Initialize the BME280
  //
  BME280_init();
  if (DEBUG)
  {
    if (found_BME280)
    {
      Serial.println(F("BME280 Found."));
    }
    else
    {
      Serial.println(F("BME280 Not found."));
    }
  }
  //
  // Initialize the TSL2561
  //
  TSL2561_init();
  if (DEBUG)
  {
    if (found_TSL2561)
    {
      Serial.println(F("TSL2561 Found."));
    }
    else
    {
      Serial.println(F("TSL2561 Not found."));
    }
  }
  //
  // Initialize the CCS811
  //
  CCS811_init();
  if (DEBUG)
  {
    if (found_CCS811)
    {
      Serial.println(F("CCS811 Found."));
    }
    else
    {
      Serial.println(F("CCS811 Not found."));
    }
  }
  //
  // Initialize the PCA9685
  //
  PCA9685_init();
  if (DEBUG)
  {
    if (found_PCA9685)
    {
      Serial.println(F("PCA9685 Found."));
    }
    else
    {
      Serial.println(F("PCA9685 Not found."));
    }
  }
}
//
// I2C state machine to read and process data from devices on the I2C bus.
//
void i2c_state_machine()
{
  switch (i2c_state)
  {
    case STATE_START:
      //
      // Based on devices found, determine which device to start with.
      //
      if (found_BME280)
      {
        i2c_state = STATE_BME280_STATE_MACHINE;
      }
      else if (found_TSL2561)
      {
        i2c_state = STATE_TSL2561_MACHINE;
      }
      else if (found_CCS811)
      {
        i2c_state = STATE_CCS811_MACHINE;
      }
      else
      {
        i2c_state = STATE_START;
      }
      break;
    case STATE_BME280_STATE_MACHINE:
      if (BME280_state_machine() == MACHINE_DONE)
      {
        if (found_TSL2561)
        {
          i2c_state = STATE_TSL2561_MACHINE;
        }
        else if (found_CCS811)
        {
          i2c_state = STATE_CCS811_MACHINE;
        }
        else
        {
          i2c_state = STATE_START;
        }
      }
      break;
    case STATE_TSL2561_MACHINE:
      if (TSL2561_state_machine() == MACHINE_DONE)
      {
        if (found_CCS811)
        {
          i2c_state = STATE_CCS811_MACHINE;
        }
        else
        {
          i2c_state = STATE_START;
        }
      }
      break;
    case STATE_CCS811_MACHINE:
      if (CCS811_state_machine() == MACHINE_DONE)
      {
        i2c_state = STATE_START;
      }
    default:
      break;
  }
}

uint8_t i2c_start_read(uint8_t device, uint8_t reg, uint8_t bytes)
{
  uint8_t success;

  if (bytes > BUFFER_SIZE)
  {
    return 5;
  }
  Wire.beginTransmission(device);
  Wire.write(reg);
  success = Wire.endTransmission();
  Wire.requestFrom(device, bytes);
  bytes_pending = bytes;
  buff_ptr = 0;
  if (DEBUG)
  {
    if (success != 0)
    {
      Serial.print(F("I2C read request returned failure "));
      Serial.println(success, DEC);
    }
  }
  return success;
}

uint8_t i2c_read_pending()
{
  uint8_t temp;
  while (Wire.available() > 0)
  {
    temp = Wire.read();
    i2c_buffer[buff_ptr] = temp;
    buff_ptr++;
    bytes_pending--;
  }
  return bytes_pending;
}


