#include <RS-485.h>
#include <wiring_private.h>
#include <Wire.h>

static const bool DEBUG = true;
//
// I2C Stuff
//
const uint32_t I2C_TIMEOUT = 500;
extern uint8_t i2c_buffer[BUFFER_SIZE];
extern uint8_t buff_ptr;
extern uint8_t bytes_pending;
extern uint32_t end_time;

extern uint8_t i2c_start_read(uint8_t device, uint8_t reg, uint8_t bytes);
extern uint8_t i2c_read_pending();
//
extern bool found_TSL2561;
//
// Status for sub-state machines
//
static const uint8_t MACHINE_WORKING = 1;
static const uint8_t MACHINE_DONE = 2;
//
static const uint8_t STATE_TSL2561_START_READ0 = 1;
static const uint8_t STATE_TSL2561_CHECK_READ0 = 2;
static const uint8_t STATE_TSL2561_END_READ0 = 3;
static const uint8_t STATE_TSL2561_START_READ1 = 4;
static const uint8_t STATE_TSL2561_CHECK_READ1 = 5;
static const uint8_t STATE_TSL2561_END_READ1 = 6;
static const uint8_t STATE_TSL2561_FIAT_LUX = 7;
//
static uint8_t TSL2561_state = STATE_TSL2561_START_READ0;
//
static void TSL2561_request_data0();
static void TSL2561_request_data1();
static unsigned int CalculateLux(unsigned int iGain, unsigned int tInt, unsigned int ch0,
                          unsigned int ch1, int iType);

//
// Values for TSL2561 light sensor
//
static uint8_t TSL2561_status = DATA_INIT;
static uint16_t TSL2561_data0;
static uint16_t TSL2561_data1;
static uint16_t TSL2561_lux;
//
static const uint8_t ADDR_TSL2561 = 0x39; // Could also be 0x29 or 0x49
//
// Register addresses
//
static const uint8_t TSL2561_CTRL = 0x80;
static const uint8_t TSL2561_TIMING = 0x81;
static const uint8_t TSL2561_THRESH_LL = 0x82;
static const uint8_t TSL2561_THRESH_LH = 0x83;
static const uint8_t TSL2561_THRESH_HL = 0x84;
static const uint8_t TSL2561_THRESH_HH = 0x85;
static const uint8_t TSL2561_INT = 0x86;
static const uint8_t TSL2561_CRC = 0x88;
static const uint8_t TSL2561_ID = 0x8A;
static const uint8_t TSL2561_DATA_0 = 0xAC;
static const uint8_t TSL2561_DATA_0L = 0x8C;
static const uint8_t TSL2561_DATA_0H = 0x8D;
static const uint8_t TSL2561_DATA_1 = 0xAE;
static const uint8_t TSL2561_DATA_1L = 0x8E;
static const uint8_t TSL2561_DATA_1H = 0x8F;
//
// Register values
//
static const uint8_t TSL2561_CTRL_PWR = 0x03; // Power device up
static const uint8_t TSL2561_TIMING_HIGH_GAIN = 0x10;
static const uint8_t TSL2561_TIMING_MANUAL = 0x08;
static const uint8_t TSL2561_TIMING_13MS = 0;
static const uint8_t TSL2561_TIMING_101MS = 1;
static const uint8_t TSL2561_TIMING_402MS = 2;
//
void TSL2561_init()
{
  uint8_t success;

  //
  // Initialize the TSL2561
  //
  i2c_start_read(ADDR_TSL2561, TSL2561_ID, 1);
  if (DEBUG)
  {
    Serial.println(F("Waiting for TSL2561 to respond."));
  }
  end_time = millis() + I2C_TIMEOUT; // Compute timeout value
  while ((i2c_read_pending() > 0) && (millis() > end_time));
  if (i2c_read_pending() == 0)
  {
    found_TSL2561 = true;
  }
  else
  {
    found_TSL2561 = false;
    TSL2561_status = DATA_SENSOR;
  }
  if (DEBUG)
  {
    Serial.print(F("TSL2561 ID value is "));
    Serial.println(i2c_buffer[0], HEX); // My unit returns 3A (or 66)
  }
  //
  // No point continuing if the device was not found.
  //
  if (!found_TSL2561)
  {
    return;
  }
  //
  // Rest of initialization
  //
  Wire.beginTransmission(ADDR_TSL2561);
  Wire.write(TSL2561_CTRL);
  Wire.write(TSL2561_CTRL_PWR);
  success = Wire.endTransmission();
  Wire.beginTransmission(ADDR_TSL2561);
  Wire.write(TSL2561_TIMING);
  Wire.write(TSL2561_TIMING_402MS);
  success = Wire.endTransmission();
  if (DEBUG)
  {
    Serial.print("Initializing TSL2651 returns ");
    Serial.println(success, DEC);
  }
}

static void TSL2561_request_data0()
{
  i2c_start_read(ADDR_TSL2561, TSL2561_DATA_0, 2);
}

static void TSL2561_request_data1()
{
  i2c_start_read(ADDR_TSL2561, TSL2561_DATA_1, 2);
}

void send_TSL2561(HardwareSerial *rs485)
{
  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_TSL2561, HEX);
  rs485->print("&");
  rs485->print(TSL2561_status, HEX);
  rs485->print("&");
  rs485->print(TSL2561_data0, HEX);
  rs485->print("&");
  rs485->print(TSL2561_data1, HEX);
  rs485->print("&");
  rs485->print(TSL2561_lux, HEX);
  rs485->println("%FF");
  TSL2561_status = DATA_STALE;
}

uint8_t TSL2561_state_machine()
{
  uint8_t machine_status = MACHINE_WORKING;

  switch (TSL2561_state)
  {
    case STATE_TSL2561_START_READ0:
      TSL2561_request_data0();
      TSL2561_state = STATE_TSL2561_CHECK_READ0;
      break;
    case STATE_TSL2561_CHECK_READ0:
      if (i2c_read_pending() == 0)
      {
        TSL2561_state = STATE_TSL2561_END_READ0;
      }
      break;
    case STATE_TSL2561_END_READ0:
      TSL2561_data0 = i2c_buffer[0] + i2c_buffer[1]*256;
      if (DEBUG)
      {
        Serial.print(F("TSL2561 Data 0 = "));
        Serial.println(TSL2561_data0, DEC);
      }
      TSL2561_state = STATE_TSL2561_START_READ1;
      break;
    case STATE_TSL2561_START_READ1:
      TSL2561_request_data1();
      TSL2561_state = STATE_TSL2561_CHECK_READ1;
      break;
    case STATE_TSL2561_CHECK_READ1:
      if (i2c_read_pending() == 0)
      {
        TSL2561_state = STATE_TSL2561_END_READ1;
      }
      break;
    case STATE_TSL2561_END_READ1:
      TSL2561_data1 = i2c_buffer[0] + i2c_buffer[1]*256;
      if (DEBUG)
      {
        Serial.print(F("TSL2561 Data 1 = "));
        Serial.println(TSL2561_data1, DEC);
      }
      TSL2561_status = DATA_VALID;
      TSL2561_state = STATE_TSL2561_FIAT_LUX;
      break;
    case STATE_TSL2561_FIAT_LUX:
      TSL2561_lux = CalculateLux(0, 2, TSL2561_data0, TSL2561_data1, 0);
      TSL2561_state = STATE_TSL2561_START_READ0;
      machine_status = MACHINE_DONE;
      break;
    default:
      TSL2561_status = DATA_NO_COMPUTED;
      machine_status = MACHINE_DONE;
      break;
  }
  return machine_status;
}

//
// The code for calculating the LUX value is based on the "as is" software in the
// TSL2561 data sheet.
//
//**************************************************************************** //
// Copyright E 2004−2005 TAOS, Inc.
//
//  THIS CODE AND INFORMATION IS PROVIDED ”AS IS” WITHOUT WARRANTY OF ANY
//  KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
//  IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
//  PURPOSE.
//
// Module Name:
// lux.cpp
//
//****************************************************************************

#define LUX_SCALE 14 // scale by 2^14
#define RATIO_SCALE 9 // scale ratio by 2^9

//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
// Integration time scaling factors
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

#define CH_SCALE      10 // scale channel values by 2^10
#define CHSCALE_TINT0 0x7517 // 322/11 * 2^CH_SCALE
#define CHSCALE_TINT1 0x0fe7 // 322/81 * 2^CH_SCALE

//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
// T, FN, and CL Package coefficients
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
// For Ch1/Ch0=0.00 to 0.50
//    Lux/Ch0=0.0304−0.062*((Ch1/Ch0)^1.4)
//    piecewise approximation
//      For Ch1/Ch0=0.00 to 0.125:
//        Lux/Ch0=0.0304−0.0272*(Ch1/Ch0)
//      For Ch1/Ch0=0.125 to 0.250:
//        Lux/Ch0=0.0325−0.0440*(Ch1/Ch0)
//      For Ch1/Ch0=0.250 to 0.375:
//        Lux/Ch0=0.0351−0.0544*(Ch1/Ch0)
//      For Ch1/Ch0=0.375 to 0.50:
//        Lux/Ch0=0.0381−0.0624*(Ch1/Ch0)//
//
// For Ch1/Ch0=0.50 to 0.61:
//     Lux/Ch0=0.0224−0.031*(Ch1/Ch0)
//
// For Ch1/Ch0=0.61 to 0.80:
//     Lux/Ch0=0.0128−0.0153*(Ch1/Ch0)
//
// For Ch1/Ch0=0.80 to 1.30:
//     Lux/Ch0=0.00146−0.00112*(Ch1/Ch0)
//
// For Ch1/Ch0>1.3:
//     Lux/Ch0=0
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
#define K1T  0x0040  // 0.125 * 2^RATIO_SCALE
#define B1T  0x01f2  // 0.0304 * 2^LUX_SCALE
#define M1T  0x01be  // 0.0272 * 2^LUX_SCALE

#define K2T  0x0080  // 0.250 * 2^RATIO_SCALE
#define B2T  0x0214  // 0.0325 * 2^LUX_SCALE
#define M2T  0x02d1  // 0.0440 * 2^LUX_SCALE

#define K3T  0x00c0 // 0.375 * 2^RATIO_SCALE
#define B3T  0x023f // 0.0351 * 2^LUX_SCALE
#define M3T  0x037b // 0.0544 * 2^LUX_SCALE

#define K4T  0x0100 // 0.50 * 2^RATIO_SCALE
#define B4T  0x0270 // 0.0381 * 2^LUX_SCALE
#define M4T  0x03fe // 0.0624 * 2^LUX_SCALE

#define K5T  0x0138 // 0.61 * 2^RATIO_SCALE
#define B5T  0x016f // 0.0224 * 2^LUX_SCALE
#define M5T  0x01fc // 0.0310 * 2^LUX_SCALE

#define K6T  0x019a // 0.80 * 2^RATIO_SCALE
#define B6T  0x00d2 // 0.0128 * 2^LUX_SCALE
#define M6T  0x00fb // 0.0153 * 2^LUX_SCALE

#define K7T  0x029a // 1.3 * 2^RATIO_SCALE
#define B7T  0x0018 // 0.00146 * 2^LUX_SCALE
#define M7T  0x0012 // 0.00112 * 2^LUX_SCALE

#define K8T  0x029a // 1.3 * 2^RATIO_SCALE
#define B8T  0x0000 // 0.000 * 2^LUX_SCALE
#define M8T  0x0000 // 0.000 * 2^LUX_SCALE

//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
// CS package coefficients
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
// For 0 <= Ch1/Ch0 <= 0.52
//   Lux/Ch0 = 0.0315−0.0593*((Ch1/Ch0)^1.4)
//   piecewise approximation
//     For 0 <= Ch1/Ch0 <= 0.13
//       Lux/Ch0 = 0.0315−0.0262*(Ch1/Ch0)
//     For 0.13 <= Ch1/Ch0 <= 0.26
//       Lux/Ch0 = 0.0337−0.0430*(Ch1/Ch0)
//     For 0.26 <= Ch1/Ch0 <= 0.39
//       Lux/Ch0 = 0.0363−0.0529*(Ch1/Ch0)
//     For 0.39 <= Ch1/Ch0 <= 0.52
//       Lux/Ch0 = 0.0392−0.0605*(Ch1/Ch0)
//
// For 0.52 < Ch1/Ch0 <= 0.65
//   Lux/Ch0 = 0.0229−0.0291*(Ch1/Ch0)
// For 0.65 < Ch1/Ch0 <= 0.80
//   Lux/Ch0 = 0.00157−0.00180*(Ch1/Ch0)
// For 0.80 < Ch1/Ch0 <= 1.30
//   Lux/Ch0 = 0.00338−0.00260*(Ch1/Ch0)
// For Ch1/Ch0 > 1.30
//   Lux = 0
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
#define K1C  0x0043 // 0.130 * 2^RATIO_SCALE
#define B1C  0x0204 // 0.0315 * 2^LUX_SCALE
#define M1C  0x01ad // 0.0262 * 2^LUX_SCALE

#define K2C  0x0085 // 0.260 * 2^RATIO_SCALE
#define B2C  0x0228 // 0.0337 * 2^LUX_SCALE
#define M2C  0x02c1 // 0.0430 * 2^LUX_SCALE

#define K3C  0x00c8 // 0.390 * 2^RATIO_SCALE
#define B3C  0x0253 // 0.0363 * 2^LUX_SCALE
#define M3C  0x0363 // 0.0529 * 2^LUX_SCALE

#define K4C  0x010a // 0.520 * 2^RATIO_SCALE
#define B4C  0x0282 // 0.0392 * 2^LUX_SCALE
#define M4C  0x03df // 0.0605 * 2^LUX_SCALE

#define K5C  0x014d // 0.65 * 2^RATIO_SCALE
#define B5C  0x0177 // 0.0229 * 2^LUX_SCALE
#define M5C  0x01dd // 0.0291 * 2^LUX_SCALE

#define K6C  0x019a // 0.80 * 2^RATIO_SCALE
#define B6C  0x0101 // 0.0157 * 2^LUX_SCALE
#define M6C  0x0127 // 0.0180 * 2^LUX_SCALE

#define K7C  0x029a // 1.3 * 2^RATIO_SCALE
#define B7C  0x0037 // 0.00338 * 2^LUX_SCALE
#define M7C  0x002b // 0.00260 * 2^LUX_SCALE

#define K8C  0x029a // 1.3 * 2^RATIO_SCALE
#define B8C  0x0000 // 0.000 * 2^LUX_SCALE
#define M8C  0x0000 // 0.000 * 2^LUX_SCALE

// lux equation approximation without floating point calculations
//////////////////////////////////////////////////////////////////////////////
// Routine: unsigned int CalculateLux(unsigned int ch0, unsigned int ch0, int iType)
//
// Description: Calculate the approximate illuminance (lux) given the raw
//              channel values of the TSL2560. The equation is implemented
//              as a piece−wise linear approximation.
//
// Arguments:   unsigned int iGain − gain, where 0:1X, 1:16X
//              unsigned int tInt − integration time, where 0:13.7mS, 1:100mS, 2:402mS,
//                                  3:Manual
//              unsigned int ch0 − raw channel value from channel 0 of TSL2560
//              unsigned int ch1 − raw channel value from channel 1 of TSL2560
//              unsigned int iType − package type (0:T, FN, or CL; or 1:CS)
//
// Return:      unsigned int − the approximate illuminance (lux)
//
//////////////////////////////////////////////////////////////////////////////
static unsigned int CalculateLux(unsigned int iGain, unsigned int tInt, unsigned int ch0,
                          unsigned int ch1, int iType)
{
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
// first, scale the channel values depending on the gain and integration time
// 16X, 402mS is nominal.
// scale if integration time is NOT 402 msec
  unsigned long chScale;
  unsigned long channel1;
  unsigned long channel0;

  switch (tInt)
  {
    case 0: // 13.7 msec
      chScale = CHSCALE_TINT0;
      break;
    case 1: // 101 msec
       chScale = CHSCALE_TINT1;
       break;
    default: // assume no scaling
       chScale = (1 << CH_SCALE);
       break;
  }

// scale if gain is NOT 16X
  if (!iGain) chScale = chScale << 4;   // scale 1X to 16X

// scale the channel values
  channel0 = (ch0 * chScale) >> CH_SCALE;
  channel1 = (ch1 * chScale) >> CH_SCALE;
//−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

// find the ratio of the channel values (Channel1/Channel0)
// protect against divide by zero
  unsigned long ratio1 = 0;
  if (channel0 != 0) ratio1 = (channel1 << (RATIO_SCALE+1)) / channel0;

// round the ratio value
  unsigned long ratio = (ratio1 + 1) >> 1;
// is ratio <= eachBreak ?
  unsigned int b, m;
  switch (iType)
  {
    case 0: // T, FN and CL package
           if ((ratio >= 0) && (ratio <= K1T))
                  {b=B1T; m=M1T;}
           else if (ratio <= K2T)
                  {b=B2T; m=M2T;}
           else if (ratio <= K3T)
                  {b=B3T; m=M3T;}
           else if (ratio <= K4T)
                  {b=B4T; m=M4T;}
           else if (ratio <= K5T)
                  {b=B5T; m=M5T;}
           else if (ratio <= K6T)
                  {b=B6T; m=M6T;}
           else if (ratio <= K7T)
                  {b=B7T; m=M7T;}
           else if (ratio > K8T)
                  {b=B8T; m=M8T;}
           break;
    case 1:// CS package
           if ((ratio >= 0) && (ratio <= K1C))
                  {b=B1C; m=M1C;}
           else if (ratio <= K2C)
                  {b=B2C; m=M2C;}
           else if (ratio <= K3C)
                  {b=B3C; m=M3C;}
           else if (ratio <= K4C)
                  {b=B4C; m=M4C;}
           else if (ratio <= K5C)
                  {b=B5C; m=M5C;}
           else if (ratio <= K6C)
                  {b=B6C; m=M6C;}
           else if (ratio <= K7C)
                  {b=B7C; m=M7C;}
           else if (ratio > K8C)
                  {b=B8C; m=M8C;}
           break;
  }
  unsigned long temp;
  temp = ((channel0 * b) - (channel1 * m));

// do not allow negative lux value
  if (temp < 0) temp = 0;

// round lsb (2^(LUX_SCALE−1))
  temp += (1 << (LUX_SCALE - 1));

// strip off fractional portion
  unsigned long lux = temp >> LUX_SCALE;
  return(lux);
}

