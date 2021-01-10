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
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x06_P2.54mm_Vertical" H 4900 3800 50  0001 C CNN
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
	6600 3750 6600 3100
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
L Connector_Generic:Conn_01x03 J3
U 1 1 5FE0F243
P 9000 3800
F 0 "J3" H 9080 3842 50  0000 L CNN
F 1 "Conn_01x03" H 9080 3751 50  0000 L CNN
F 2 "misc_hardware:TerminalBlock_Kaweei_1x03_P2.54mm_Horizontal" H 9000 3800 50  0001 C CNN
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
Text GLabel 8700 3900 0    50   Input ~ 0
LED_D
Text GLabel 8700 3800 0    50   Input ~ 0
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
$Comp
L Device:Net-Tie_2 NT1
U 1 1 5FF0F2A7
P 6600 3000
F 0 "NT1" V 6650 3200 50  0000 R CNN
F 1 "Net-Tie_2" V 6550 3450 50  0000 R CNN
F 2 "NetTie:NetTie-2_SMD_Pad0.5mm" H 6600 3000 50  0001 C CNN
F 3 "~" H 6600 3000 50  0001 C CNN
	1    6600 3000
	0    -1   -1   0   
$EndComp
Wire Wire Line
	6600 2900 6600 2800
Wire Wire Line
	6700 4800 7200 4800
$Comp
L Mechanical:MountingHole H1
U 1 1 5FF4F0EB
P 9000 4250
F 0 "H1" H 9100 4296 50  0000 L CNN
F 1 "MountingHole" H 9100 4205 50  0000 L CNN
F 2 "misc_hardware:MountingHole_M2.5" H 9000 4250 50  0001 C CNN
F 3 "~" H 9000 4250 50  0001 C CNN
	1    9000 4250
	1    0    0    -1  
$EndComp
$Comp
L Mechanical:MountingHole H2
U 1 1 5FF5065F
P 9000 4500
F 0 "H2" H 9100 4546 50  0000 L CNN
F 1 "MountingHole" H 9100 4455 50  0000 L CNN
F 2 "misc_hardware:MountingHole_M2.5" H 9000 4500 50  0001 C CNN
F 3 "~" H 9000 4500 50  0001 C CNN
	1    9000 4500
	1    0    0    -1  
$EndComp
$Comp
L power:+5V #PWR03
U 1 1 5FDEBA13
P 3300 3650
F 0 "#PWR03" H 3300 3500 50  0001 C CNN
F 1 "+5V" H 3315 3823 50  0000 C CNN
F 2 "" H 3300 3650 50  0001 C CNN
F 3 "" H 3300 3650 50  0001 C CNN
	1    3300 3650
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR04
U 1 1 5FDEC1C7
P 3300 3950
F 0 "#PWR04" H 3300 3700 50  0001 C CNN
F 1 "GND" H 3305 3777 50  0000 C CNN
F 2 "" H 3300 3950 50  0001 C CNN
F 3 "" H 3300 3950 50  0001 C CNN
	1    3300 3950
	1    0    0    -1  
$EndComp
Wire Wire Line
	3300 3850 3000 3850
Wire Wire Line
	3300 3950 3300 3850
Wire Wire Line
	3300 3750 3000 3750
Connection ~ 3000 3750
Wire Wire Line
	3300 3650 3300 3750
$Comp
L power:PWR_FLAG #FLG01
U 1 1 5FDF5B6E
P 3000 3650
F 0 "#FLG01" H 3000 3725 50  0001 C CNN
F 1 "PWR_FLAG" H 3000 3823 50  0000 C CNN
F 2 "" H 3000 3650 50  0001 C CNN
F 3 "~" H 3000 3650 50  0001 C CNN
	1    3000 3650
	1    0    0    -1  
$EndComp
Wire Wire Line
	3000 3750 3000 3650
Connection ~ 3000 3850
Wire Wire Line
	3000 3850 3000 3950
$Comp
L power:PWR_FLAG #FLG02
U 1 1 5FDFB526
P 3000 3950
F 0 "#FLG02" H 3000 4025 50  0001 C CNN
F 1 "PWR_FLAG" H 3000 4123 50  0000 C CNN
F 2 "" H 3000 3950 50  0001 C CNN
F 3 "~" H 3000 3950 50  0001 C CNN
	1    3000 3950
	-1   0    0    1   
$EndComp
Wire Wire Line
	2900 3750 3000 3750
Wire Wire Line
	2900 3850 3000 3850
$Comp
L Connector_Generic:Conn_01x02 J2
U 1 1 5FEFCA38
P 2700 3850
F 0 "J2" H 2700 3650 50  0000 C CNN
F 1 "Conn_01x02" H 2700 3950 50  0000 C CNN
F 2 "misc_hardware:TerminalBlock_Kaweei_1x02_P2.54mm_Horizontal" H 2700 3850 50  0001 C CNN
F 3 "~" H 2700 3850 50  0001 C CNN
	1    2700 3850
	-1   0    0    1   
$EndComp
$Comp
L Device:R_Small R1
U 1 1 5FFA7B48
P 7800 3300
F 0 "R1" V 7604 3300 50  0000 C CNN
F 1 "470" V 7695 3300 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P12.70mm_Horizontal" H 7800 3300 50  0001 C CNN
F 3 "~" H 7800 3300 50  0001 C CNN
	1    7800 3300
	0    1    1    0   
$EndComp
Wire Wire Line
	7900 3300 8000 3300
Text GLabel 8000 3300 2    50   Input ~ 0
LED_D
$EndSCHEMATC
