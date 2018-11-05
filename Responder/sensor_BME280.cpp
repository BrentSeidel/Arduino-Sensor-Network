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
extern bool found_BME280;
//
// Status for sub-state machines
//
static const uint8_t MACHINE_WORKING = 1;
static const uint8_t MACHINE_DONE = 2;
//
static const uint8_t STATE_BME280_START_CONVERSION = 1;
static const uint8_t STATE_BME280_ASK_DATA_READY = 2;
static const uint8_t STATE_BME280_CHECK_DATA_READY = 3;
static const uint8_t STATE_BME280_START_READ = 4;
static const uint8_t STATE_BME280_CHECK_READ = 5;
static const uint8_t STATE_BME280_END_READ = 6;
static const uint8_t STATE_BME280_PROCESS1 = 7;  // Split into three steps to avoid too much time in each loop
static const uint8_t STATE_BME280_PROCESS2 = 8;
static const uint8_t STATE_BME280_PROCESS3 = 9;
//
static uint8_t BME280_state = STATE_BME280_START_CONVERSION;
//
// Calibration values for BME280
//
static uint16_t BME280_cal_T1 = 0;
static int16_t BME280_cal_T2 = 0;
static int16_t BME280_cal_T3 = 0;
static uint16_t BME280_cal_P1 = 0;
static int16_t BME280_cal_P2 = 0;
static int16_t BME280_cal_P3 = 0;
static int16_t BME280_cal_P4 = 0;
static int16_t BME280_cal_P5 = 0;
static int16_t BME280_cal_P6 = 0;
static int16_t BME280_cal_P7 = 0;
static int16_t BME280_cal_P8 = 0;
static int16_t BME280_cal_P9 = 0;
static uint8_t BME280_cal_H1 = 0;
static int16_t BME280_cal_H2 = 0;
static uint8_t BME280_cal_H3 = 0;
static int16_t BME280_cal_H4 = 0;
static int16_t BME280_cal_H5 = 0;
static uint8_t BME280_cal_H6 = 0;
//
// Sensor values for BME280
//
static uint8_t BME280_status = DATA_INIT;
static uint8_t BME280_age = 0;
static uint32_t raw_press;
static uint32_t raw_temp;
static uint32_t raw_hum;
static int32_t  t_fine;    // Calibrated temperature
static uint32_t cal_press; // Calibrated pressure (Pa/256)
static uint32_t cal_hum;   // Calibrated humidity (%/1024)
//
// BME280 Constant values
//
static const uint8_t ADDR_BME280 = 0x77;
//
// Registers
//
static const uint8_t BME280_CTRL_HUM = 0xF2;
static const uint8_t BME280_STATUS = 0xF3;
static const uint8_t BME280_CTRL_MEAS = 0xF4;
static const uint8_t BME280_CONFIG = 0xF5;
static const uint8_t BME280_DATA_START = 0xF7;
static const uint8_t BME280_RESET = 0xE0;
static const uint8_t BME280_ID_ADDR = 0xD0;
//
// Register values
//
static const uint8_t BME280_MODE_SLEEP = 0x00;
static const uint8_t BME280_MODE_FORCE = 0x02;
static const uint8_t BME280_MODE_NORMAL = 0x03;
static const uint8_t BME280_HUM_OVER_1 =  0x01;
static const uint8_t BME280_PRESS_OVER_1 = 0x20;
static const uint8_t BME280_TEMP_OVER_1 = 0x04;
static const uint8_t BME280_ID_VALUE = 0x60;
//
// Status values
//
static const uint8_t BME280_STAT_MEASURING = 0x08;
static const uint8_t BME280_STAT_IM_UPDATE = 0x01;
//
// Local function definitions
//
static void BME280_get_raw_values();
static int32_t BME280_cal_temp();
static uint32_t BME280_cal_press();
static uint32_t BME280_cal_hum();
//
static void BME280_start_conversion()
{
  Wire.beginTransmission(ADDR_BME280);
  Wire.write(BME280_CTRL_MEAS);
  Wire.write(BME280_MODE_FORCE + BME280_TEMP_OVER_1 + BME280_PRESS_OVER_1);
}

static void BME280_request_ID()
{
  i2c_start_read(ADDR_BME280, BME280_ID_ADDR, 1);
}

static void BME280_request_status()
{
  i2c_start_read(ADDR_BME280, BME280_STATUS, 1);
}

static void BME280_request_data()
{
  i2c_start_read(ADDR_BME280, BME280_DATA_START, 8);
}

static bool BME280_check_data_ready()
{
  return ((i2c_buffer[0] & BME280_STAT_MEASURING) == BME280_STAT_MEASURING);
}

void send_BME280(HardwareSerial *rs485)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_BME280, HEX);
  rs485->print("&");
  rs485->print(BME280_status, HEX);
  rs485->print("&");
  rs485->print(BME280_age, HEX);
  rs485->print("&");
  rs485->print(t_fine, HEX);
  rs485->print("&");
  rs485->print(cal_press, HEX);
  rs485->print("&");
  rs485->print(cal_hum, HEX);
  rs485->println("%FF");
  BME280_status = DATA_STALE;
  BME280_age++;
}
//
// Functions for BME280 sensor.  This code is based on some Ada code that I wrote that was based
// on some C code from the data sheet.
//
void BME280_init()
{
  //
  // Check if BME280 is present
  //
  BME280_request_ID();
  if (DEBUG)
  {
    Serial.println(F("Waiting for BME280 to respond."));
  }
  end_time = millis() + I2C_TIMEOUT; // Compute timeout value
  while ((i2c_read_pending() > 0) && (millis() > end_time));
  if (i2c_buffer[0] != BME280_ID_VALUE)
  {
    if (DEBUG)
    {
      Serial.println(F("Unable to find BME280."));
      Serial.print(F("Found ID "));
      Serial.println(i2c_buffer[0], HEX);
    }
    BME280_status = DATA_SENSOR;
  }
  else
  {
    found_BME280 = true;
    if (DEBUG)
    {
      Serial.println(F("BME280 Found."));
    }
  }
  //
  // No point continuing if the device was not found.
  //
  if (!found_BME280)
  {
    return;
  }
  //
  // Read calibration constants;
  //
  i2c_start_read(ADDR_BME280, 0x88, 24);
  while (i2c_read_pending() > 0);
  BME280_cal_T1 = i2c_buffer[0] + (i2c_buffer[1] << 8);
  BME280_cal_T2 = i2c_buffer[2] + (i2c_buffer[3] << 8);
  BME280_cal_T3 = i2c_buffer[4] + (i2c_buffer[5] << 8);
  BME280_cal_P1 = i2c_buffer[6] + (i2c_buffer[7] << 8);
  BME280_cal_P2 = i2c_buffer[8] + (i2c_buffer[9] << 8);
  BME280_cal_P3 = i2c_buffer[10] + (i2c_buffer[11] << 8);
  BME280_cal_P4 = i2c_buffer[12] + (i2c_buffer[13] << 8);
  BME280_cal_P5 = i2c_buffer[14] + (i2c_buffer[15] << 8);
  BME280_cal_P6 = i2c_buffer[16] + (i2c_buffer[17] << 8);
  BME280_cal_P7 = i2c_buffer[18] + (i2c_buffer[19] << 8);
  BME280_cal_P8 = i2c_buffer[20] + (i2c_buffer[21] << 8);
  BME280_cal_P9 = i2c_buffer[22] + (i2c_buffer[23] << 8);
  i2c_start_read(ADDR_BME280, 0xA1, 1);
  while (i2c_read_pending() > 0);
  BME280_cal_H1 = i2c_buffer[0];

  i2c_start_read(ADDR_BME280, 0xE1, 7);
  while (i2c_read_pending() > 0);
  BME280_cal_H2 = i2c_buffer[0] + (i2c_buffer[1] << 8);
  BME280_cal_H3 = i2c_buffer[2];
  BME280_cal_H4 = (i2c_buffer[3] << 4) + (i2c_buffer[4] & 0xF);
  BME280_cal_H5 = (i2c_buffer[5] << 4) + (i2c_buffer[4] >> 4);
  BME280_cal_H6 = i2c_buffer[6];
  if (DEBUG)
  {
    Serial.println(F("Read calibration constants."));
    Serial.print(F("BME280_cal_T1 = "));
    Serial.println(BME280_cal_T1, HEX);
    Serial.print(F("BME280_cal_T2 = "));
    Serial.println(BME280_cal_T2, HEX);
    Serial.print(F("BME280_cal_T3 = "));
    Serial.println(BME280_cal_T3, HEX);
    Serial.print(F("BME280_cal_P1 = "));
    Serial.println(BME280_cal_P1, HEX);
    Serial.print(F("BME280_cal_P2 = "));
    Serial.println(BME280_cal_P2, HEX);
    Serial.print(F("BME280_cal_P3 = "));
    Serial.println(BME280_cal_P3, HEX);
    Serial.print(F("BME280_cal_P4 = "));
    Serial.println(BME280_cal_P4, HEX);
    Serial.print(F("BME280_cal_P5 = "));
    Serial.println(BME280_cal_P5, HEX);
    Serial.print(F("BME280_cal_P6 = "));
    Serial.println(BME280_cal_P6, HEX);
    Serial.print(F("BME280_cal_P7 = "));
    Serial.println(BME280_cal_P7, HEX);
    Serial.print(F("BME280_cal_P8 = "));
    Serial.println(BME280_cal_P8, HEX);
    Serial.print(F("BME280_cal_P9 = "));
    Serial.println(BME280_cal_P9, HEX);
    Serial.print(F("BME280_cal_H1 = "));
    Serial.println(BME280_cal_H1, HEX);
    Serial.print(F("BME280_cal_H2 = "));
    Serial.println(BME280_cal_H2, HEX);
    Serial.print(F("BME280_cal_H3 = "));
    Serial.println(BME280_cal_H3, HEX);
    Serial.print(F("BME280_cal_H4 = "));
    Serial.println(BME280_cal_H4, HEX);
    Serial.print(F("BME280_cal_H5 = "));
    Serial.println(BME280_cal_H5, HEX);
    Serial.print(F("BME280_cal_H6 = "));
    Serial.println(BME280_cal_H6, HEX);
  }
//-- Now set the mode.  Use forced mode to keep the interface similar to
//-- the BMP180.
//--
//-- First put into sleep more so configuration can be set.  Oversampling
//-- is set to 1 for each parameter.
//--
  Wire.beginTransmission(ADDR_BME280);
  Wire.write(BME280_CTRL_MEAS);
  Wire.write(BME280_MODE_SLEEP);
  Wire.endTransmission();
//self.port.write(self.address, ctrl_meas, mode_sleep, error);
//--
//-- Set humidity oversampling
//--
  Wire.beginTransmission(ADDR_BME280);
  Wire.write(BME280_CTRL_HUM);
  Wire.write(BME280_HUM_OVER_1);
  Wire.endTransmission();
//self.port.write(self.address, ctrl_hum, hum_over_1, error);
//--
//-- Temperature, pressure, and mode are in the same register so set them
//-- all at once.  This also apparently starts conversion.
//--
  Wire.beginTransmission(ADDR_BME280);
  Wire.write(BME280_CTRL_MEAS);
  Wire.write(BME280_MODE_FORCE + BME280_TEMP_OVER_1 + BME280_PRESS_OVER_1);
  Wire.endTransmission();
//self.port.write(self.address, ctrl_meas, temp_over_1 + press_over_1 +
//                        mode_force, error);
}
//
uint8_t BME280_state_machine()
{
  uint8_t machine_status = MACHINE_WORKING;
  uint8_t success;

  switch (BME280_state)
  {
    case STATE_BME280_START_CONVERSION:
      BME280_start_conversion();
      success = Wire.endTransmission();
      if (success != 0)
      {
        machine_status = MACHINE_DONE;
        BME280_status = DATA_SENSOR;
      }
      else
      {
        BME280_state = STATE_BME280_ASK_DATA_READY;
      }
      break;
    case STATE_BME280_ASK_DATA_READY:
      BME280_request_status();
      BME280_state = STATE_BME280_CHECK_DATA_READY;
      break;
    case STATE_BME280_CHECK_DATA_READY:
      if (i2c_read_pending() == 0)
      {
        BME280_state = BME280_check_data_ready() ?
                      STATE_BME280_ASK_DATA_READY : STATE_BME280_START_READ;
      }
      break;
    case STATE_BME280_START_READ:
      BME280_request_data();
      BME280_state = STATE_BME280_CHECK_READ;
      break;
    case STATE_BME280_CHECK_READ:
      if (i2c_read_pending() == 0)
      {
        BME280_state = STATE_BME280_END_READ;
      }
      break;
    case STATE_BME280_END_READ:
      BME280_get_raw_values();
      BME280_state = STATE_BME280_PROCESS1;
      break;
    case STATE_BME280_PROCESS1:
      t_fine = BME280_cal_temp();
      BME280_state = STATE_BME280_PROCESS2;
      break;
    case STATE_BME280_PROCESS2:
      cal_press = BME280_cal_press();
      BME280_state = STATE_BME280_PROCESS3;
      break;
    case STATE_BME280_PROCESS3:
      cal_hum = BME280_cal_hum();
      if (DEBUG)
      {
        Serial.print(F("Raw temperature is "));
        Serial.println(raw_temp, HEX);
        Serial.print(F("Raw pressure is "));
        Serial.println(raw_press, HEX);
        Serial.print(F("Raw humidity is "));
        Serial.println(raw_hum, HEX);
        Serial.println(F("----------"));
        Serial.print(F("Calibrated temperature is "));
        Serial.println(((t_fine*5 + 128) >> 8)/100.0, DEC);
        Serial.print(F("Calibrated pressure is "));
        Serial.println(cal_press/256.0, DEC);
        Serial.print(F("Calibrated humidity is "));
        Serial.println(cal_hum/1024.0, DEC);
        Serial.println(F("================"));
      }
      BME280_state = STATE_BME280_START_CONVERSION;
      BME280_status = DATA_VALID;
      BME280_age = 0;
      machine_status = MACHINE_DONE;
      break;
    default:
      BME280_status = DATA_NO_COMPUTED;
      machine_status = MACHINE_DONE;
      break;
  }
  return machine_status;
}
//
static void BME280_get_raw_values()
{
  raw_press = ((((uint32_t)i2c_buffer[0]) << 16) + (((uint32_t)i2c_buffer[1]) << 8) + i2c_buffer[2]) >> 4;
  raw_temp  = ((((uint32_t)i2c_buffer[3]) << 16) + (((uint32_t)i2c_buffer[4]) << 8) + i2c_buffer[5]) >> 4;
  raw_hum   = (((uint32_t)i2c_buffer[6]) << 8)  + i2c_buffer[7];
}
//
//      function cal_temp(self : not null access BME280_record'class) return int32 is
//         var1 : int32;
//         var2 : int32;
//      begin
//         var1 := (int32(self.raw_temp)/2**3 - int32(self.T1)*2)*int32(self.T2)/2**11;
//         var2 := (int32(self.raw_temp)/2**4 - int32(self.T1))*
//           (int32(self.raw_temp)/2**4 - int32(self.T1))/2**12*int32(self.T3)/2**14;
//         return var1 + var2;
//      end;
static int32_t BME280_cal_temp()
{
  int32_t var1;
  int32_t var2;

  var1 = ((raw_temp >> 3) - int32_t(BME280_cal_T1)*2)*(int32_t(BME280_cal_T2) >> 11);
  var2 = ((raw_temp >> 4) - int32_t(BME280_cal_T1))*
           (((raw_temp >> 4) - int32_t(BME280_cal_T1)) >> 12)*(int32_t(BME280_cal_T3) >> 14);
  return var1 + var2;
}

//      function cal_press(self : not null access BME280_record'class) return uint32 is
//         var1 : int64;
//         var2 : int64;
//         p : int64;
//      begin
//         var1 := int64(self.t_fine) - 128000;
//         var2 := var1*var1*int64(self.P6);
//         var2 := var2 + var1*int64(self.P5)*2**17;
//         var2 := var2 + int64(self.P4)*2**35;
//         var1 := var1*var1*int64(self.P3)/2**8 + var1*int64(self.P2)*2**12;
//         var1 := (2**47 + var1)*int64(self.P1)/2**33;
//         if (var1 = 0) then
//            return 0;
//         end if;
//         p := 1_048_576 - int64(self.raw_press);
//         p := (p*2**31 - var2)*3125/var1;
//         var1 := int64(self.P9)*(p/2**13)*(p/2**13)/2**25;
//         var2 := int64(self.P8)*p/2**19;
//         p := (p + var1 + var2)/2**8 + int64(self.P7)*2**4;
//         return uint32(p);
//      end;
static uint32_t BME280_cal_press()
{
  int64_t var1;
  int64_t var2;
  int64_t p;

  var1 = int64_t(t_fine) - 128000;
  var2 = var1*var1*int64_t(BME280_cal_P6);
  var2 = var2 + (var1*int64_t(BME280_cal_P5) << 17);
  var2 = var2 + (int64_t(BME280_cal_P4) << 35);
  var1 = ((var1*var1*int64_t(BME280_cal_P3)) >> 8) + ((var1*int64_t(BME280_cal_P2)) << 12);
  var1 = (((int64_t(1) << 47) + var1)*int64_t(BME280_cal_P1)) >> 33;
  if (var1 == 0)
  {
    return 0;
  }
  p = 1048576 - int64_t(raw_press);
  p = ((p << 31) - var2)*3125/var1;
  var1 = (int64_t(BME280_cal_P9)*(p >> 13)*(p >> 13)) >> 25;
  var2 = (int64_t(BME280_cal_P8)*p) >>19;
  p = ((p + var1 + var2) >> 8) + (int64_t(BME280_cal_P7) << 4);
  return uint32_t(p);
}

//      function cal_hum(self : not null access BME280_record'class) return uint32 is
//         v_x1 : int32;
//      begin
//         v_x1 := self.t_fine - 76_800;
//         v_x1 := ((int32(self.raw_hum)*2**14 - int32(self.H4)*2**20 - int32(self.H5)*v_x1 + 16_384)/2**15)*
//           ((((v_x1*int32(self.H6)/2**10)*
//            (v_x1*int32(self.H3)/2**11 + 32_768)/2**10 + 2_097_152)*int32(self.H2) + 8192)/2**14);
//         v_x1 := v_x1 - (v_x1/2**15)*(v_x1/2**15)/2**7*int32(self.H1)/2**4;
//         if (v_x1 < 0) then
//            v_x1 := 0;
//         elsif (v_x1 > 419_430_400) then
//            v_x1 := 419_430_400;
//         end if;
//         return uint32(v_x1/2**12);
//      end;
static uint32_t BME280_cal_hum()
{
  int32_t v_x1;

  v_x1 = t_fine - 76800;
  v_x1 = (((int32_t(raw_hum) << 14) - (int32_t(BME280_cal_H4) << 20) - int32_t(BME280_cal_H5)*v_x1 + 16384) >> 15)*
           ((((v_x1*int32_t(BME280_cal_H6) >> 10)*
            ((v_x1*(int32_t(BME280_cal_H3) >> 11) + 32768) >> 10) + 2097152)*int32_t(BME280_cal_H2) + 8192) >> 14);
  v_x1 = v_x1 - ((v_x1 >> 15)*(v_x1 >> 15) >> 7)*(int32_t(BME280_cal_H1) >> 4);
  if (v_x1 < 0)
  {
    v_x1 = 0;
  }
  else if (v_x1 > 419430400)
  {
    v_x1 = 419430400;
  }
  return uint32_t(v_x1 >> 12);
}


