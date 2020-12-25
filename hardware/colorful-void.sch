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
P 4900 3800
F 0 "J1" H 4250 5050 50  0000 C CNN
F 1 "Raspberry_Pi_Zero" H 5500 5050 50  0000 C CNN
F 2 "" H 4900 3800 50  0001 C CNN
F 3 "https://www.raspberrypi.org/documentation/hardware/raspberrypi/schematics/rpi_SCH_3bplus_1p0_reduced.pdf" H 4900 3800 50  0001 C CNN
	1    4900 3800
	1    0    0    -1  
$EndComp
$Comp
L power:+5V #PWR02
U 1 1 5FD55063
P 7200 2700
F 0 "#PWR02" H 7200 2550 50  0001 C CNN
F 1 "+5V" H 7215 2873 50  0000 C CNN
F 2 "" H 7200 2700 50  0001 C CNN
F 3 "" H 7200 2700 50  0001 C CNN
	1    7200 2700
	1    0    0    -1  
$EndComp
$Comp
L misc_hardware:74AHCT125 U1
U 1 1 5FD63880
P 7200 3800
F 0 "U1" H 6950 4650 50  0000 C CNN
F 1 "74AHCT125" H 7450 4650 50  0000 C CNN
F 2 "Package_DIP:DIP-14_W7.62mm" H 7200 3800 50  0001 C CNN
F 3 "" H 7200 4200 50  0001 C CNN
	1    7200 3800
	1    0    0    -1  
$EndComp
Wire Wire Line
	7200 4700 7200 4800
$Comp
L power:GND #PWR05
U 1 1 5FD65A50
P 7200 4900
F 0 "#PWR05" H 7200 4650 50  0001 C CNN
F 1 "GND" H 7205 4727 50  0000 C CNN
F 2 "" H 7200 4900 50  0001 C CNN
F 3 "" H 7200 4900 50  0001 C CNN
	1    7200 4900
	1    0    0    -1  
$EndComp
Wire Wire Line
	7600 3300 7700 3300
Text GLabel 7700 3300 2    50   Input ~ 0
LED_D
Wire Wire Line
	7600 3600 7700 3600
Wire Wire Line
	7700 3900 7600 3900
Wire Wire Line
	7600 4200 7700 4200
Wire Wire Line
	6700 4200 6800 4200
Wire Wire Line
	6700 3900 6800 3900
Wire Wire Line
	6700 3600 6800 3600
NoConn ~ 7700 3600
NoConn ~ 7700 3900
NoConn ~ 7700 4200
Wire Wire Line
	7200 4800 7400 4800
Wire Wire Line
	7400 4800 7400 4900
Connection ~ 7200 4800
Text GLabel 7400 4900 3    50   Input ~ 0
LED_GND
Wire Wire Line
	7200 4900 7200 4800
Wire Wire Line
	7200 2700 7200 2800
Wire Wire Line
	7200 2800 7400 2800
Wire Wire Line
	7400 2800 7400 2700
Connection ~ 7200 2800
Wire Wire Line
	7200 2800 7200 2900
Text GLabel 7400 2700 1    50   Input ~ 0
LED_VCC
Wire Wire Line
	6600 3750 6800 3750
Wire Wire Line
	6600 4050 6800 4050
Wire Wire Line
	6600 4350 6800 4350
Wire Wire Line
	6700 3450 6700 3600
Wire Wire Line
	6700 4800 7200 4800
Wire Wire Line
	6700 3450 6800 3450
Connection ~ 6700 3600
Wire Wire Line
	6700 3600 6700 3900
Connection ~ 6700 3900
Wire Wire Line
	6700 3900 6700 4200
Connection ~ 6700 4200
Wire Wire Line
	6700 4200 6700 4800
Wire Wire Line
	6600 4350 6600 4050
Wire Wire Line
	6600 2800 7200 2800
Connection ~ 6600 3750
Wire Wire Line
	6600 3750 6600 2800
Connection ~ 6600 4050
Wire Wire Line
	6600 4050 6600 3750
Wire Wire Line
	4500 5100 4500 5200
$Comp
L power:GND #PWR06
U 1 1 5FD70DC2
P 4500 5200
F 0 "#PWR06" H 4500 4950 50  0001 C CNN
F 1 "GND" H 4505 5027 50  0000 C CNN
F 2 "" H 4500 5200 50  0001 C CNN
F 3 "" H 4500 5200 50  0001 C CNN
	1    4500 5200
	1    0    0    -1  
$EndComp
Wire Wire Line
	4700 2500 4700 2400
$Comp
L power:+5V #PWR01
U 1 1 5FD7188B
P 4800 2400
F 0 "#PWR01" H 4800 2250 50  0001 C CNN
F 1 "+5V" H 4815 2573 50  0000 C CNN
F 2 "" H 4800 2400 50  0001 C CNN
F 3 "" H 4800 2400 50  0001 C CNN
	1    4800 2400
	1    0    0    -1  
$EndComp
Wire Wire Line
	4600 5100 4600 5200
NoConn ~ 4600 5200
Wire Wire Line
	4700 5100 4700 5200
NoConn ~ 4700 5200
Wire Wire Line
	4800 5100 4800 5200
NoConn ~ 4800 5200
Wire Wire Line
	4900 5100 4900 5200
NoConn ~ 4900 5200
Wire Wire Line
	5000 5100 5000 5200
NoConn ~ 5000 5200
Wire Wire Line
	5100 5100 5100 5200
NoConn ~ 5100 5200
Wire Wire Line
	5200 5100 5200 5200
NoConn ~ 5200 5200
Wire Wire Line
	5700 4600 5800 4600
NoConn ~ 5800 4600
Wire Wire Line
	5700 4500 5800 4500
NoConn ~ 5800 4500
Wire Wire Line
	5700 4300 5800 4300
NoConn ~ 5800 4300
Wire Wire Line
	5700 4100 5800 4100
NoConn ~ 5800 4100
Wire Wire Line
	5700 4000 5800 4000
NoConn ~ 5800 4000
Wire Wire Line
	5700 3900 5800 3900
NoConn ~ 5800 3900
Wire Wire Line
	5700 3700 5800 3700
NoConn ~ 5800 3700
Wire Wire Line
	5700 3600 5800 3600
NoConn ~ 5800 3600
Wire Wire Line
	5700 3500 5800 3500
NoConn ~ 5800 3500
Wire Wire Line
	5700 3300 5800 3300
NoConn ~ 5800 3300
Wire Wire Line
	5700 3200 5800 3200
NoConn ~ 5800 3200
Wire Wire Line
	5700 3000 5800 3000
NoConn ~ 5800 3000
Wire Wire Line
	5700 2900 5800 2900
NoConn ~ 5800 2900
Wire Wire Line
	4100 2900 4000 2900
NoConn ~ 4000 2900
Wire Wire Line
	4100 3000 4000 3000
NoConn ~ 4000 3000
Wire Wire Line
	4100 3200 4000 3200
NoConn ~ 4000 3200
Wire Wire Line
	4100 3300 4000 3300
NoConn ~ 4000 3300
Wire Wire Line
	4100 3600 4000 3600
NoConn ~ 4000 3600
Wire Wire Line
	4100 3700 4000 3700
NoConn ~ 4000 3700
Wire Wire Line
	4100 3800 4000 3800
NoConn ~ 4000 3800
Wire Wire Line
	4100 4000 4000 4000
NoConn ~ 4000 4000
Wire Wire Line
	4100 4100 4000 4100
NoConn ~ 4000 4100
Wire Wire Line
	4100 4200 4000 4200
NoConn ~ 4000 4200
Wire Wire Line
	4100 4300 4000 4300
NoConn ~ 4000 4300
Wire Wire Line
	4100 4400 4000 4400
NoConn ~ 4000 4400
Wire Wire Line
	4100 4500 4000 4500
NoConn ~ 4000 4500
Wire Wire Line
	4800 2500 4800 2400
Wire Wire Line
	5100 2500 5100 2400
Wire Wire Line
	5000 2400 5000 2500
NoConn ~ 5000 2400
NoConn ~ 5100 2400
$Comp
L Connector:Barrel_Jack J2
U 1 1 5FDE4F93
P 2600 3800
F 0 "J2" H 2657 4125 50  0000 C CNN
F 1 "Barrel_Jack" H 2657 4034 50  0000 C CNN
F 2 "" H 2650 3760 50  0001 C CNN
F 3 "~" H 2650 3760 50  0001 C CNN
	1    2600 3800
	1    0    0    -1  
$EndComp
Wire Wire Line
	2900 3700 3000 3700
Wire Wire Line
	3000 3700 3000 3600
Wire Wire Line
	2900 3900 3000 3900
Wire Wire Line
	3000 3900 3000 4000
$Comp
L power:+5V #PWR03
U 1 1 5FDEBA13
P 3300 3600
F 0 "#PWR03" H 3300 3450 50  0001 C CNN
F 1 "+5V" H 3315 3773 50  0000 C CNN
F 2 "" H 3300 3600 50  0001 C CNN
F 3 "" H 3300 3600 50  0001 C CNN
	1    3300 3600
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR04
U 1 1 5FDEC1C7
P 3300 4000
F 0 "#PWR04" H 3300 3750 50  0001 C CNN
F 1 "GND" H 3305 3827 50  0000 C CNN
F 2 "" H 3300 4000 50  0001 C CNN
F 3 "" H 3300 4000 50  0001 C CNN
	1    3300 4000
	1    0    0    -1  
$EndComp
$Comp
L power:PWR_FLAG #FLG01
U 1 1 5FDF5B6E
P 3000 3600
F 0 "#FLG01" H 3000 3675 50  0001 C CNN
F 1 "PWR_FLAG" H 3000 3773 50  0000 C CNN
F 2 "" H 3000 3600 50  0001 C CNN
F 3 "~" H 3000 3600 50  0001 C CNN
	1    3000 3600
	1    0    0    -1  
$EndComp
Wire Wire Line
	3300 3600 3300 3700
Wire Wire Line
	3300 3700 3000 3700
Connection ~ 3000 3700
Wire Wire Line
	3300 4000 3300 3900
Wire Wire Line
	3300 3900 3000 3900
Connection ~ 3000 3900
$Comp
L power:PWR_FLAG #FLG02
U 1 1 5FDFB526
P 3000 4000
F 0 "#FLG02" H 3000 4075 50  0001 C CNN
F 1 "PWR_FLAG" H 3000 4173 50  0000 C CNN
F 2 "" H 3000 4000 50  0001 C CNN
F 3 "~" H 3000 4000 50  0001 C CNN
	1    3000 4000
	-1   0    0    1   
$EndComp
$Comp
L Connector_Generic:Conn_01x03 J3
U 1 1 5FE0F243
P 9000 3800
F 0 "J3" H 9080 3842 50  0000 L CNN
F 1 "Conn_01x03" H 9080 3751 50  0000 L CNN
F 2 "" H 9000 3800 50  0001 C CNN
F 3 "~" H 9000 3800 50  0001 C CNN
	1    9000 3800
	1    0    0    -1  
$EndComp
Wire Wire Line
	8800 3700 8700 3700
Wire Wire Line
	8700 3800 8800 3800
Wire Wire Line
	8800 3900 8700 3900
Text GLabel 8700 3700 0    50   Input ~ 0
LED_VCC
Text GLabel 8700 3800 0    50   Input ~ 0
LED_D
Text GLabel 8700 3900 0    50   Input ~ 0
LED_GND
Wire Wire Line
	5800 4200 5700 4200
NoConn ~ 5800 4200
Text GLabel 6500 3300 0    50   Input ~ 0
LED_CTRL
Text GLabel 4000 3400 0    50   Input ~ 0
LED_CTRL
Wire Wire Line
	4000 3400 4100 3400
Wire Wire Line
	6500 3300 6800 3300
NoConn ~ 4700 2400
$EndSCHEMATC
