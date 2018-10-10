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
Sheet 3 4
Title ""
Date ""
Rev ""
Comp "Modest Consulting"
Comment1 "by Brent Seidel"
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L ArduinoUno U1
U 1 1 5BB78B80
P 5250 2900
F 0 "U1" H 5250 3000 60  0000 C CNN
F 1 "ArduinoUno" H 5250 2900 60  0000 C CNN
F 2 "" H 5250 2900 60  0001 C CNN
F 3 "" H 5250 2900 60  0001 C CNN
	1    5250 2900
	1    0    0    -1  
$EndComp
$Comp
L RS-485 U2
U 1 1 5BB78BA9
P 7350 2900
F 0 "U2" H 7350 2800 60  0000 C CNN
F 1 "RS-485" H 7350 2900 60  0000 C CNN
F 2 "" H 7350 2900 60  0001 C CNN
F 3 "" H 7350 2900 60  0001 C CNN
	1    7350 2900
	1    0    0    -1  
$EndComp
$Comp
L BME280_Breakout U3
U 1 1 5BB78BCE
P 3050 3350
F 0 "U3" H 3050 3450 60  0000 C CNN
F 1 "BME280_Breakout" H 3050 3350 60  0000 C CNN
F 2 "" H 3050 3350 60  0001 C CNN
F 3 "" H 3050 3350 60  0001 C CNN
	1    3050 3350
	-1   0    0    -1  
$EndComp
Wire Wire Line
	5950 3700 6650 3700
Wire Wire Line
	6650 3700 6650 3200
Wire Wire Line
	6650 3200 6750 3200
Wire Wire Line
	5950 3600 6750 3600
Wire Wire Line
	5950 3200 6500 3200
Wire Wire Line
	6500 3100 6500 3350
Wire Wire Line
	6500 3100 6750 3100
Wire Wire Line
	6500 3350 6750 3350
Connection ~ 6500 3200
Text Notes 7350 7500 0    60   ~ 0
Basic Responder Unit With BME280 Sensor
Text Notes 7100 6700 0    60   ~ 0
External power and USB connections not shown
Text GLabel 8400 3300 2    60   BiDi ~ 0
RS-485A
Text GLabel 8400 3100 2    60   BiDi ~ 0
RS-485B
Wire Wire Line
	8000 3100 8400 3100
Wire Wire Line
	8000 3300 8400 3300
$Comp
L CCS811 U?
U 1 1 5BBE1A0C
P 3100 4550
F 0 "U4" H 3100 4650 60  0000 C CNN
F 1 "CCS811" H 3100 4550 60  0000 C CNN
F 2 "" H 3100 4550 60  0001 C CNN
F 3 "" H 3100 4550 60  0001 C CNN
	1    3100 4550
	1    0    0    -1  
$EndComp
$Comp
L TSL2561 U?
U 1 1 5BBE1A37
P 3100 2250
F 0 "U5" H 3100 2350 60  0000 C CNN
F 1 "TSL2561" H 3100 2250 60  0000 C CNN
F 2 "" H 3100 2250 60  0001 C CNN
F 3 "" H 3100 2250 60  0001 C CNN
	1    3100 2250
	1    0    0    -1  
$EndComp
Wire Wire Line
	3800 3500 4400 3500
Wire Wire Line
	3900 3500 3900 3600
Wire Wire Line
	3900 3600 3800 3600
Wire Wire Line
	3800 2500 4000 2500
Wire Wire Line
	4000 2500 4000 4900
Wire Wire Line
	4000 4900 3800 4900
Wire Wire Line
	4000 3100 3800 3100
Connection ~ 4000 3100
Wire Wire Line
	4600 2750 4000 2750
Connection ~ 4000 2750
Wire Wire Line
	3800 2400 4100 2400
Wire Wire Line
	4100 2400 4100 4700
Wire Wire Line
	4100 4700 3800 4700
Wire Wire Line
	3800 3300 4100 3300
Connection ~ 4100 3300
Wire Wire Line
	4600 2950 4100 2950
Connection ~ 4100 2950
Wire Wire Line
	3800 1900 4300 1900
Wire Wire Line
	4300 1900 4300 4500
Wire Wire Line
	4300 4500 3800 4500
Wire Wire Line
	4600 3700 4300 3700
Connection ~ 4300 3700
Wire Wire Line
	3800 3400 4300 3400
Connection ~ 4300 3400
Wire Wire Line
	3800 2000 4400 2000
Wire Wire Line
	4400 2000 4400 4600
Wire Wire Line
	4400 4600 3800 4600
Wire Wire Line
	4600 3600 4400 3600
Connection ~ 4400 3600
Connection ~ 3900 3500
Connection ~ 4400 3500
$EndSCHEMATC
