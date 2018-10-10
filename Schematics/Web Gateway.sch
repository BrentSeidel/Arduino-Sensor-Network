EESchema Schematic File Version 2
LIBS:Arduino-Sensors-rescue
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:PartsLibrary
LIBS:Arduino-Sensors-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 4 4
Title ""
Date ""
Rev ""
Comp "Modest Consulting"
Comment1 "by Brent Seidel"
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
Text Notes 7400 7500 0    60   ~ 0
Web Gateway Processor
Text Notes 7050 6700 0    60   ~ 0
External power and ethernet connections not shown.
$Comp
L BeagleBone_Black U1
U 1 1 5BB7A561
P 2600 3600
F 0 "U1" H 2600 3700 60  0000 C CNN
F 1 "BeagleBone_Black" H 2600 3600 60  0000 C CNN
F 2 "" H 2600 3600 60  0001 C CNN
F 3 "" H 2600 3600 60  0001 C CNN
	1    2600 3600
	1    0    0    -1  
$EndComp
$Comp
L RS-485 U3
U 1 1 5BB7A58E
P 6300 2000
F 0 "U3" H 6300 1900 60  0000 C CNN
F 1 "RS-485" H 6300 2000 60  0000 C CNN
F 2 "" H 6300 2000 60  0001 C CNN
F 3 "" H 6300 2000 60  0001 C CNN
	1    6300 2000
	1    0    0    -1  
$EndComp
$Comp
L RS-485 U2
U 1 1 5BB7A5B5
P 6300 3700
F 0 "U2" H 6300 3600 60  0000 C CNN
F 1 "RS-485" H 6300 3700 60  0000 C CNN
F 2 "" H 6300 3700 60  0001 C CNN
F 3 "" H 6300 3700 60  0001 C CNN
	1    6300 3700
	1    0    0    -1  
$EndComp
Text GLabel 7600 4100 2    60   BiDi ~ 0
RS-485A
Text GLabel 7600 3900 2    60   BiDi ~ 0
RS-485B
Text GLabel 7500 2400 2    60   BiDi ~ 0
Back_A
Text GLabel 7500 2200 2    60   BiDi ~ 0
Back_B
Wire Wire Line
	1600 1400 1500 1400
Wire Wire Line
	1500 1400 1500 1200
Wire Wire Line
	1500 1200 5300 1200
Wire Wire Line
	5300 1200 5300 2450
Wire Wire Line
	5300 2200 5700 2200
Wire Wire Line
	5300 2450 5700 2450
Connection ~ 5300 2200
Wire Wire Line
	1600 2600 1400 2600
Wire Wire Line
	1400 2600 1400 1100
Wire Wire Line
	1400 1100 5100 1100
Wire Wire Line
	5100 1100 5100 2700
Wire Wire Line
	5100 2700 5700 2700
Wire Wire Line
	1600 1700 1300 1700
Wire Wire Line
	1300 1700 1300 1000
Wire Wire Line
	1300 1000 5000 1000
Wire Wire Line
	5000 1000 5000 4150
Wire Wire Line
	5000 3900 5700 3900
Wire Wire Line
	5000 4150 5700 4150
Connection ~ 5000 3900
Wire Wire Line
	1600 2400 1200 2400
Wire Wire Line
	1200 2400 1200 900 
Wire Wire Line
	1200 900  4800 900 
Wire Wire Line
	4800 900  4800 4000
Wire Wire Line
	4800 4000 5700 4000
Wire Wire Line
	6950 4100 7600 4100
Wire Wire Line
	6950 3900 7600 3900
Wire Wire Line
	6950 2400 7500 2400
Wire Wire Line
	6950 2200 7500 2200
$EndSCHEMATC
