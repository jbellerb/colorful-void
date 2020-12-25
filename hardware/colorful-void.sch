EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "Colorful Void Controller"
Date "2020-12-12"
Rev "1"
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Connector:Raspberry_Pi_2_3 J1
U 1 1 5FD53E6E
P 4950 3900
F 0 "J1" H 4300 5150 50  0000 C CNN
F 1 "Raspberry_Pi_Zero" H 5550 5150 50  0000 C CNN
F 2 "" H 4950 3900 50  0001 C CNN
F 3 "https://www.raspberrypi.org/documentation/hardware/raspberrypi/schematics/rpi_SCH_3bplus_1p0_reduced.pdf" H 4950 3900 50  0001 C CNN
	1    4950 3900
	1    0    0    -1  
$EndComp
$Comp
L power:+5V #PWR02
U 1 1 5FD55063
P 7250 2800
F 0 "#PWR02" H 7250 2650 50  0001 C CNN
F 1 "+5V" H 7265 2973 50  0000 C CNN
F 2 "" H 7250 2800 50  0001 C CNN
F 3 "" H 7250 2800 50  0001 C CNN
	1    7250 2800
	1    0    0    -1  
$EndComp
$Comp
L misc_hardware:74AHCT125 U1
U 1 1 5FD63880
P 7250 3900
F 0 "U1" H 7000 4750 50  0000 C CNN
F 1 "74AHCT125" H 7500 4750 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm" H 7250 3900 50  0001 C CNN
F 3 "" H 7250 4300 50  0001 C CNN
	1    7250 3900
	1    0    0    -1  
$EndComp
Wire Wire Line
	7250 4800 7250 4900
$Comp
L power:GND #PWR05
U 1 1 5FD65A50
P 7250 5000
F 0 "#PWR05" H 7250 4750 50  0001 C CNN
F 1 "GND" H 7255 4827 50  0000 C CNN
F 2 "" H 7250 5000 50  0001 C CNN
F 3 "" H 7250 5000 50  0001 C CNN
	1    7250 5000
	1    0    0    -1  
$EndComp
Wire Wire Line
	7650 3400 7750 3400
Text GLabel 7750 3400 2    50   Input ~ 0
LED_D
Wire Wire Line
	7650 3700 7750 3700
Wire Wire Line
	7750 4000 7650 4000
Wire Wire Line
	7650 4300 7750 4300
Wire Wire Line
	6750 4300 6850 4300
Wire Wire Line
	6750 4000 6850 4000
Wire Wire Line
	6750 3700 6850 3700
NoConn ~ 7750 3700
NoConn ~ 7750 4000
NoConn ~ 7750 4300
Wire Wire Line
	7250 4900 7450 4900
Wire Wire Line
	7450 4900 7450 5000
Connection ~ 7250 4900
Text GLabel 7450 5000 3    50   Input ~ 0
LED_GND
Wire Wire Line
	7250 5000 7250 4900
Wire Wire Line
	7250 2800 7250 2900
Wire Wire Line
	7250 2900 7450 2900
Wire Wire Line
	7450 2900 7450 2800
Connection ~ 7250 2900
Wire Wire Line
	7250 2900 7250 3000
Text GLabel 7450 2800 1    50   Input ~ 0
LED_VCC
Wire Wire Line
	6650 3850 6850 3850
Wire Wire Line
	6650 4150 6850 4150
Wire Wire Line
	6650 4450 6850 4450
Wire Wire Line
	6750 3550 6750 3700
Wire Wire Line
	6750 4900 7250 4900
Wire Wire Line
	6750 3550 6850 3550
Connection ~ 6750 3700
Wire Wire Line
	6750 3700 6750 4000
Connection ~ 6750 4000
Wire Wire Line
	6750 4000 6750 4300
Connection ~ 6750 4300
Wire Wire Line
	6750 4300 6750 4900
Wire Wire Line
	6650 4450 6650 4150
Wire Wire Line
	6650 2900 7250 2900
Connection ~ 6650 3850
Wire Wire Line
	6650 3850 6650 2900
Connection ~ 6650 4150
Wire Wire Line
	6650 4150 6650 3850
Wire Wire Line
	4550 5200 4550 5300
$Comp
L power:GND #PWR06
U 1 1 5FD70DC2
P 4550 5300
F 0 "#PWR06" H 4550 5050 50  0001 C CNN
F 1 "GND" H 4555 5127 50  0000 C CNN
F 2 "" H 4550 5300 50  0001 C CNN
F 3 "" H 4550 5300 50  0001 C CNN
	1    4550 5300
	1    0    0    -1  
$EndComp
Wire Wire Line
	4750 2600 4750 2500
$Comp
L power:+5V #PWR01
U 1 1 5FD7188B
P 4750 2500
F 0 "#PWR01" H 4750 2350 50  0001 C CNN
F 1 "+5V" H 4765 2673 50  0000 C CNN
F 2 "" H 4750 2500 50  0001 C CNN
F 3 "" H 4750 2500 50  0001 C CNN
	1    4750 2500
	1    0    0    -1  
$EndComp
Wire Wire Line
	4650 5200 4650 5300
NoConn ~ 4650 5300
Wire Wire Line
	4750 5200 4750 5300
NoConn ~ 4750 5300
Wire Wire Line
	4850 5200 4850 5300
NoConn ~ 4850 5300
Wire Wire Line
	4950 5200 4950 5300
NoConn ~ 4950 5300
Wire Wire Line
	5050 5200 5050 5300
NoConn ~ 5050 5300
Wire Wire Line
	5150 5200 5150 5300
NoConn ~ 5150 5300
Wire Wire Line
	5250 5200 5250 5300
NoConn ~ 5250 5300
Wire Wire Line
	5750 4700 5850 4700
NoConn ~ 5850 4700
Wire Wire Line
	5750 4600 5850 4600
NoConn ~ 5850 4600
Wire Wire Line
	5750 4400 5850 4400
NoConn ~ 5850 4400
Wire Wire Line
	5750 4200 5850 4200
NoConn ~ 5850 4200
Wire Wire Line
	5750 4100 5850 4100
NoConn ~ 5850 4100
Wire Wire Line
	5750 4000 5850 4000
NoConn ~ 5850 4000
Wire Wire Line
	5750 3800 5850 3800
NoConn ~ 5850 3800
Wire Wire Line
	5750 3700 5850 3700
NoConn ~ 5850 3700
Wire Wire Line
	5750 3600 5850 3600
NoConn ~ 5850 3600
Wire Wire Line
	5750 3400 5850 3400
NoConn ~ 5850 3400
Wire Wire Line
	5750 3300 5850 3300
NoConn ~ 5850 3300
Wire Wire Line
	5750 3100 5850 3100
NoConn ~ 5850 3100
Wire Wire Line
	5750 3000 5850 3000
NoConn ~ 5850 3000
Wire Wire Line
	4150 3000 4050 3000
NoConn ~ 4050 3000
Wire Wire Line
	4150 3100 4050 3100
NoConn ~ 4050 3100
Wire Wire Line
	4150 3300 4050 3300
NoConn ~ 4050 3300
Wire Wire Line
	4150 3400 4050 3400
NoConn ~ 4050 3400
Wire Wire Line
	4150 3500 4050 3500
NoConn ~ 4050 3500
Wire Wire Line
	4150 3700 4050 3700
NoConn ~ 4050 3700
Wire Wire Line
	4150 3800 4050 3800
NoConn ~ 4050 3800
Wire Wire Line
	4150 3900 4050 3900
NoConn ~ 4050 3900
Wire Wire Line
	4150 4100 4050 4100
NoConn ~ 4050 4100
Wire Wire Line
	4150 4200 4050 4200
NoConn ~ 4050 4200
Wire Wire Line
	4150 4300 4050 4300
NoConn ~ 4050 4300
Wire Wire Line
	4150 4400 4050 4400
NoConn ~ 4050 4400
Wire Wire Line
	4150 4500 4050 4500
NoConn ~ 4050 4500
Wire Wire Line
	4150 4600 4050 4600
NoConn ~ 4050 4600
Wire Wire Line
	4850 2600 4850 2500
Wire Wire Line
	5150 2600 5150 2500
Wire Wire Line
	5050 2500 5050 2600
NoConn ~ 4850 2500
NoConn ~ 5050 2500
NoConn ~ 5150 2500
Wire Wire Line
	5750 4300 6250 4300
Wire Wire Line
	6250 3400 6850 3400
Wire Wire Line
	6250 3400 6250 4300
$Comp
L Connector:Barrel_Jack J2
U 1 1 5FDE4F93
P 2750 3900
F 0 "J2" H 2807 4225 50  0000 C CNN
F 1 "Barrel_Jack" H 2807 4134 50  0000 C CNN
F 2 "" H 2800 3860 50  0001 C CNN
F 3 "~" H 2800 3860 50  0001 C CNN
	1    2750 3900
	1    0    0    -1  
$EndComp
Wire Wire Line
	3050 3800 3150 3800
Wire Wire Line
	3150 3800 3150 3700
Wire Wire Line
	3050 4000 3150 4000
Wire Wire Line
	3150 4000 3150 4100
$Comp
L power:+5V #PWR03
U 1 1 5FDEBA13
P 3450 3700
F 0 "#PWR03" H 3450 3550 50  0001 C CNN
F 1 "+5V" H 3465 3873 50  0000 C CNN
F 2 "" H 3450 3700 50  0001 C CNN
F 3 "" H 3450 3700 50  0001 C CNN
	1    3450 3700
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR04
U 1 1 5FDEC1C7
P 3450 4100
F 0 "#PWR04" H 3450 3850 50  0001 C CNN
F 1 "GND" H 3455 3927 50  0000 C CNN
F 2 "" H 3450 4100 50  0001 C CNN
F 3 "" H 3450 4100 50  0001 C CNN
	1    3450 4100
	1    0    0    -1  
$EndComp
$Comp
L power:PWR_FLAG #FLG01
U 1 1 5FDF5B6E
P 3150 3700
F 0 "#FLG01" H 3150 3775 50  0001 C CNN
F 1 "PWR_FLAG" H 3150 3873 50  0000 C CNN
F 2 "" H 3150 3700 50  0001 C CNN
F 3 "~" H 3150 3700 50  0001 C CNN
	1    3150 3700
	1    0    0    -1  
$EndComp
Wire Wire Line
	3450 3700 3450 3800
Wire Wire Line
	3450 3800 3150 3800
Connection ~ 3150 3800
Wire Wire Line
	3450 4100 3450 4000
Wire Wire Line
	3450 4000 3150 4000
Connection ~ 3150 4000
$Comp
L power:PWR_FLAG #FLG02
U 1 1 5FDFB526
P 3150 4100
F 0 "#FLG02" H 3150 4175 50  0001 C CNN
F 1 "PWR_FLAG" H 3150 4273 50  0000 C CNN
F 2 "" H 3150 4100 50  0001 C CNN
F 3 "~" H 3150 4100 50  0001 C CNN
	1    3150 4100
	-1   0    0    1   
$EndComp
$Comp
L Connector_Generic:Conn_01x03 J3
U 1 1 5FE0F243
P 9150 3900
F 0 "J3" H 9230 3942 50  0000 L CNN
F 1 "Conn_01x03" H 9230 3851 50  0000 L CNN
F 2 "" H 9150 3900 50  0001 C CNN
F 3 "~" H 9150 3900 50  0001 C CNN
	1    9150 3900
	1    0    0    -1  
$EndComp
Wire Wire Line
	8950 3800 8850 3800
Wire Wire Line
	8850 3900 8950 3900
Wire Wire Line
	8950 4000 8850 4000
Text GLabel 8850 3800 0    50   Input ~ 0
LED_VCC
Text GLabel 8850 3900 0    50   Input ~ 0
LED_D
Text GLabel 8850 4000 0    50   Input ~ 0
LED_GND
$EndSCHEMATC
