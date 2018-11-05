#include <RS-485.h>
#include <wiring_private.h>
#include <Wire.h>
//
// Interface to PCA9685 16 channel, 12 bit PWM driver.
//
/*
    Define constants and types used by the PCA9685 Pulse Width Modulation LED
    controller (16-channels, 12-bit PWM, I2C Bus).

    In addition to controlling LEDs, it can control other PWM devices such as
    servo motors.  Note that LED brightness is controlled by the duty cycle and
    any duty cycle is valid, servos are controlled by the pulse width which
    should range from 1.5 to 2.5 mS.

    Measured values for some servos in my testing are:
    Servo       Min   Max
    SG90        500  2100
    SG92        450  2050
    SG5010      500  2100

    For a continuous rotation servo from Parallax, I got the following:
    Null:  1290 - 1350
    CW:   <1290
    CCW:  >1350

    Note that all measured numbers are approximate.  There are probably a few
    counts left before hitting full scale movement.  It's also entirely possible
    that these values may vary with time, temperature, or other factors.

    There are two things to keep in mind:
    1. Test your own servos to determin their appropriate values.
    2. If you want any sort of precision, you need some sort of position feed-
       back to the program.
    3. The documentation that says that the pulse width for servos should range
       from 1.5 to 2.5 mS may not be accurate.

    PWM channels are 0 to 15.  Channel 16 is the all call channel.
*/
static const bool DEBUG = false;
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
extern bool found_PCA9685;

//
// Values for PCA9685 PWM controller
//
static uint16_t PCA9685_servos[16];
static boolean PCA9685_set[16];
static uint8_t PCA9685_status = DATA_INIT;
//
static const uint8_t ADDR_PCA9685 = 0x40; // Could also be 0x41, 0x42, or 0x43
//
// Register addresses
//
static const uint8_t MODE1       = 0x00;
static const uint8_t MODE2       = 0x01;
static const uint8_t SUBADR1     = 0x02;
static const uint8_t SUBADR2     = 0x03;
static const uint8_t SUBADR3     = 0x04;
static const uint8_t ALLCALLADDR = 0x05;
static const uint8_t PRESCALE    = 0xFE;
static const uint8_t RESERVED    = 0xFF;
//
// Channel addresses.  Each channel has 4 addresses.  First define
// the offsets for the individual addresses.
//
static const uint8_t ON_L        = 0x00; // Offset of low byte of on time
static const uint8_t ON_H        = 0x01;
static const uint8_t OFF_L       = 0x02;
static const uint8_t OFF_H       = 0x03;
//
// Channel addresses.  The desired channel address is equal to
//   Addr = ch*4 + PCA9685_CH + offset
// Where
//   Addr is the desired address
//   ch is the channel number
//   offset is the offset value dfined above.
//
static const uint8_t PCA9685_CH = 0x06;
//
// Register values
//
static const uint8_t PCA9685_SLEEP = 0x10;
static const uint8_t PCA9685_AI = 0x20;
//
void PCA9685_init()
{
  uint8_t success;

  Wire.beginTransmission(ADDR_PCA9685);
  Wire.write(MODE1);
  Wire.write(PCA9685_SLEEP);
  success = Wire.endTransmission();
  if (success == 0)
  {
    found_PCA9685 = true;
  }
  else
  {
    found_PCA9685 = false;
    PCA9685_status = DATA_SENSOR;
    return;
  }
  Wire.beginTransmission(ADDR_PCA9685);
  Wire.write(PRESCALE);
  Wire.write(0x1E);
  success = Wire.endTransmission();
  if (success == 0)
  {
    found_PCA9685 = true;
  }
  else
  {
    found_PCA9685 = false;
    PCA9685_status = DATA_SENSOR;
    return;
  }
  Wire.beginTransmission(ADDR_PCA9685);
  Wire.write(MODE1);
  Wire.write(PCA9685_AI); // Set auto-increment
  success = Wire.endTransmission();
  if (success == 0)
  {
    found_PCA9685 = true;
  }
  else
  {
    found_PCA9685 = false;
    PCA9685_status = DATA_SENSOR;
    return;
  }
}
//
// Send a word containing the values that have been sent to the PCA9685.  The unit actually
// isn't read.
//
void send_PCA9685(HardwareSerial *rs485)
{
  int x;

  rs485->print("#");
  rs485->print(device, HEX);
  rs485->print("/");
  rs485->print(address, HEX);
  rs485->print("/");
  rs485->print(MSG_TYPE_PCA9685, HEX);
  for (x = 0; x < 16; x++)
  {
    rs485->print("&");
    rs485->print(PCA9685_servos[x] + (PCA9685_set[x] ? 1 : 0) << 16, HEX);
  }
  rs485->println("%FF");
  PCA9685_status = DATA_STALE;
}
//
// Set channel values
//
uint8_t PCA9685_set_chan(uint8_t chan, uint16_t on, uint16_t off)
{
  uint8_t success;

  //
  // Check for channel in range
  //
  if ((chan < 0) || (chan > 15))
  {
    return -1;
  }
  Wire.beginTransmission(ADDR_PCA9685);
  Wire.write(chan*4 + PCA9685_CH);
  Wire.write(on & 0xFF);  // ON_L
  Wire.write(on >> 8);    // ON_H
  Wire.write(off & 0xFF); // OFF_L
  Wire.write(off >> 8);   // OFF_H
  success = Wire.endTransmission();
  return success;
}


