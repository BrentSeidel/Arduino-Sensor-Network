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
Sheet 2 4
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
L Arduino_Mega_2560 U1
U 1 1 5BB7BF78
P 2900 4600
F 0 "U1" H 2900 5750 60  0000 C CNN
F 1 "Arduino_Mega_2560" H 3000 5600 60  0000 C CNN
F 2 "" H 3550 4600 60  0001 C CNN
F 3 "" H 3550 4600 60  0001 C CNN
	1    2900 4600
	1    0    0    -1  
$EndComp
$Comp
L RS-485 U2
U 1 1 5BB7BFEA
P 7300 2500
F 0 "U2" H 7300 2400 60  0000 C CNN
F 1 "RS-485" H 7300 2500 60  0000 C CNN
F 2 "" H 7300 2500 60  0001 C CNN
F 3 "" H 7300 2500 60  0001 C CNN
	1    7300 2500
	1    0    0    -1  
$EndComp
$Comp
L RS-485 U3
U 1 1 5BB7C013
P 7300 4200
F 0 "U3" H 7300 4100 60  0000 C CNN
F 1 "RS-485" H 7300 4200 60  0000 C CNN
F 2 "" H 7300 4200 60  0001 C CNN
F 3 "" H 7300 4200 60  0001 C CNN
	1    7300 4200
	1    0    0    -1  
$EndComp
Text Notes 7400 7500 0    60   ~ 0
Network Controller
Text Notes 7100 6700 0    60   ~ 0
Power and USB connections not shown
Text GLabel 8450 4600 2    60   BiDi ~ 0
RS-485A
Text GLabel 8450 4400 2    60   BiDi ~ 0
RS-485B
Text GLabel 8450 2900 2    60   BiDi ~ 0
Back_A
Text GLabel 8450 2700 2    60   BiDi ~ 0
Back_B
Wire Wire Line
	7950 4600 8450 4600
Wire Wire Line
	7950 4400 8450 4400
Wire Wire Line
	7950 2900 8450 2900
Wire Wire Line
	7950 2700 8450 2700
Wire Wire Line
	5100 4900 6700 4900
Wire Wire Line
	6700 4500 6500 4500
Wire Wire Line
	6500 4500 6500 5100
Wire Wire Line
	6500 5100 5100 5100
Wire Wire Line
	6100 4650 6700 4650
Wire Wire Line
	6100 4100 6100 4650
Wire Wire Line
	6100 4100 5100 4100
Wire Wire Line
	6700 4400 6100 4400
Connection ~ 6100 4400
Wire Wire Line
	6700 2800 6300 2800
Wire Wire Line
	6300 2800 6300 5500
Wire Wire Line
	6300 5500 5100 5500
Wire Wire Line
	850  2500 600  2500
Wire Wire Line
	600  2500 600  600 
Wire Wire Line
	600  600  6500 600 
Wire Wire Line
	6500 600  6500 2950
Wire Wire Line
	6500 2700 6700 2700
Wire Wire Line
	6500 2950 6700 2950
Connection ~ 6500 2700
$Comp
L R R?
U 1 1 5BB7ABC8
P 8200 4050
F 0 "R?" V 8280 4050 50  0000 C CNN
F 1 "R" V 8200 4050 50  0000 C CNN
F 2 "" V 8130 4050 50  0000 C CNN
F 3 "" H 8200 4050 50  0000 C CNN
	1    8200 4050
	1    0    0    -1  
$EndComp
$Comp
L R R?
U 1 1 5BB7ABE8
P 8200 4950
F 0 "R?" V 8280 4950 50  0000 C CNN
F 1 "R" V 8200 4950 50  0000 C CNN
F 2 "" V 8130 4950 50  0000 C CNN
F 3 "" H 8200 4950 50  0000 C CNN
	1    8200 4950
	1    0    0    -1  
$EndComp
Wire Wire Line
	8200 4200 8200 4400
Connection ~ 8200 4400
Wire Wire Line
	8200 4800 8200 4600
Connection ~ 8200 4600
$Comp
L GND #PWR?
U 1 1 5BB7AC9B
P 8200 5300
F 0 "#PWR?" H 8200 5050 50  0001 C CNN
F 1 "GND" H 8200 5150 50  0000 C CNN
F 2 "" H 8200 5300 50  0000 C CNN
F 3 "" H 8200 5300 50  0000 C CNN
	1    8200 5300
	1    0    0    -1  
$EndComp
$Comp
L VCC #PWR?
U 1 1 5BB7ACB9
P 8200 3700
F 0 "#PWR?" H 8200 3550 50  0001 C CNN
F 1 "VCC" H 8200 3850 50  0000 C CNN
F 2 "" H 8200 3700 50  0000 C CNN
F 3 "" H 8200 3700 50  0000 C CNN
	1    8200 3700
	1    0    0    -1  
$EndComp
Wire Wire Line
	8200 3700 8200 3900
Wire Wire Line
	8200 5100 8200 5300
$EndSCHEMATC
