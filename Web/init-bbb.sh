#!/bin/sh
#
# Initialization script for the BeagleBone Black
#
# Run this shell script as root to set protections on some of the device control files
# so that non-root users can access them.  It seems that the protection on these gets
# set back to 644 every time the BeagleBone Black boots.
#
#
# LED Control files
#
echo "LED Control files"
chmod 666 /sys/class/leds/beaglebone:green:usr0/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr1/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr2/brightness
chmod 666 /sys/class/leds/beaglebone:green:usr3/brightness
#
# Pin control files.  These are used to set what the particular pin is used
# for.
#
echo "Pin control files"
chmod 666 /sys/devices/platform/ocp/ocp:P*_pinmux/state
#
# Options for each pin can be found in the of_node/pinctrl-names file in the
# *_pinmux directory.  It appears that delimiters are not used between the
# options so when you cat the file you might see something like this:
# defaultgpiogpio_pugpio_pduartcani2cpruin
#
# Pin Control files for I2C bus 1
#
# Write "i2c" to the state file for P9_24 and P9_26 to enable the I2C bus.
# Then communicate on the bus using the device file /dev/i2c-?, where ? is
# probably 1, but apparently this is not fixed and may change.
#
# Some GPIOs are not naturally exported.  These are generally on pins that have
# only a single use.  The following exports them and creates the /sys/class/gpio*
# file structure for them.
#
echo "Additional GPIO"
echo 110 > /sys/class/gpio/export
echo 111 > /sys/class/gpio/export
echo 113 > /sys/class/gpio/export
echo 117 > /sys/class/gpio/export
#
# There are a number of GPIO pins on connector P8.  These are used by some of
# on board functions (MMC and HDMI).  Don't fiddle with them unless you know
# what you're doing and really need to.
#
# You can use the board diagrams at the end
# of http://beagleboard.org/support/bone101 to determine which ones to
# use.  To ensure that gpio is selected, write "gpio" to the desired
# state file above.
#
# Set direction to "in" or "out" for input or output.
# Set value to "0" or "1" for output or read "0" or "1" for input.
#
echo "GPIO control files"
chmod 666 /sys/class/gpio/*/direction
chmod 666 /sys/class/gpio/*/value
#
# Build the directory structures for PWMs.  As the PWMs can be rearranged each
# time the unit is booted, this script has to figure out which pwm is where and
# create some symbolic links that can be used.
#
# First see if /links is present and create it if not.  If it does exist, empty
# it.
#
if [ -d "/links" ]
then
  rm /links/*
else
  mkdir /links
fi
#
echo "PWM Directory structures"
echo "PWM0 and PWM1"
for f in /sys/devices/platform/ocp/48300000.epwmss/48300200.ehrpwm/pwm/pwmchip*
do
  echo 0 > $f/export
  echo 1 > $f/export
  ln -s $f/pwm0 /links/pwm0 # EHRPWM0A
  ln -s $f/pwm1 /links/pwm1 # EHRPWM0B
done
#
echo "PWM2 and PWM3"
for f in /sys/devices/platform/ocp/48302000.epwmss/48302200.ehrpwm/pwm/pwmchip*
do
  echo 0 > $f/export
  echo 1 > $f/export
  ln -s $f/pwm0 /links/pwm2 # EHRPWM1A
  ln -s $f/pwm1 /links/pwm3 # EHRPWM1B
done
#
echo "PWM4 and PWM5"
for f in /sys/devices/platform/ocp/48304000.epwmss/48304200.ehrpwm/pwm/pwmchip*
do
  echo 0 > $f/export
  echo 1 > $f/export
  ln -s $f/pwm0 /links/pwm4 # EHRPWM2A
  ln -s $f/pwm1 /links/pwm5 # EHRPWM2B
done
#
echo "PWM6"
for f in /sys/devices/platform/ocp/48300000.epwmss/48300100.ecap/pwm/pwmchip*
do
  echo 0 > $f/export
  ln -s $f/pwm0 /links/pwm6 # ECAPPWM0
done
#
echo "PWM7"
for f in /sys/devices/platform/ocp/48304000.epwmss/48304100.ecap/pwm/pwmchip*
do
  echo 0 > $f/export
  ln -s $f/pwm0 /links/pwm7 # ECAPPWM2
done
#
# Note that ECAPPWM2 is on P9_28 and there is no *pinumx/state file for this
# pin.  So this PWM currently can't be used.
#
# Set protections for the PWM control files
#
echo "Set PWM protections"
chmod 666 /links/pwm*/enable
chmod 666 /links/pwm*/duty_cycle
chmod 666 /links/pwm*/period
#
# Enable analog inputs.  This requires adding a device tree overlay.  Fortunately,
# everything exists, so the following should be all that is needed:
#
echo "BB-ADC" > /sys/devices/platform/bone_capemgr/slots
#
# Set baud rate on serial inputs
#
chmod 666 /dev/ttyS*
#
# ttyO1 rd pin P9-26, tx pin P9-24, rtsn pin P9-19, ctsn pin P9-20
config-pin P9.26 uart
config-pin P9.24 uart
stty -F /dev/ttyO1 raw speed 115200
#
# ttyO2 rd pin P9-22, tx pin P9-22
stty -F /dev/ttyO2 raw speed 115200
#
# ttyO3 tx pin P9-42, rtsn pin P8-34, ctsn P8-36
#stty -F /dev/ttyO3 raw speed 115200
#
# ttyO4 rd pin P9-11, tx pin P9-13, rtsn pin P8-33, ctsn pin P8-35
config-pin P9.11 uart
config-pin P9.13 uart
#
# Set the uart speed and modes.  Want raw input and nothing echoed.  The output
# will be a completely different channel.
#
stty -F /dev/ttyO4 raw speed 115200 -echo -echok -echoe -echoctl -echoke
#
# ttyO5 rd pin P8-38, tx pin P8-37, rtsn P8-32, ctsn P8-31
#stty -F /dev/ttyO5 raw speed 115200
#
# Disable current services that use port 80.  Since "stop" is used instead of
# "disable", they will be restarted when the system reboots.  So if you want them
# back, just reboot the system.
#
systemctl stop bonescript.socket
systemctl stop bonescript.service
systemctl stop bonescript-autorun.service
