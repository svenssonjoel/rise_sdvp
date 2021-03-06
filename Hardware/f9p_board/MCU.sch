EESchema Schematic File Version 5
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 3 4
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
Comment5 ""
Comment6 ""
Comment7 ""
Comment8 ""
Comment9 ""
$EndDescr
$Comp
L Connector:USB_B_Mini J1
U 1 1 5D88294A
P 8650 2250
F 0 "J1" H 8420 2147 50  0000 R CNN
F 1 "USB_B_Mini" H 8420 2238 50  0000 R CNN
F 2 "Connector_USB:USB_Mini-B_Lumberg_2486_01_Horizontal" H 8800 2200 50  0001 C CNN
F 3 "~" H 8800 2200 50  0001 C CNN
	1    8650 2250
	-1   0    0    1   
$EndComp
$Comp
L Connector:USB_B_Mini J2
U 1 1 5D8865AF
P 8650 3475
F 0 "J2" H 8420 3372 50  0000 R CNN
F 1 "USB_B_Mini" H 8420 3463 50  0000 R CNN
F 2 "Connector_USB:USB_Mini-B_Lumberg_2486_01_Horizontal" H 8800 3425 50  0001 C CNN
F 3 "~" H 8800 3425 50  0001 C CNN
	1    8650 3475
	-1   0    0    1   
$EndComp
$Comp
L power:GND #PWR0105
U 1 1 5D88B2F0
P 9825 2850
F 0 "#PWR0105" H 9825 2600 50  0001 C CNN
F 1 "GND" H 9830 2677 50  0000 C CNN
F 2 "" H 9825 2850 50  0001 C CNN
F 3 "" H 9825 2850 50  0001 C CNN
	1    9825 2850
	1    0    0    -1  
$EndComp
Wire Wire Line
	9825 2850 9450 2850
Wire Wire Line
	8650 2850 8650 3075
Wire Wire Line
	8750 3075 8750 2850
Connection ~ 8750 2850
Wire Wire Line
	8750 2850 8650 2850
Wire Wire Line
	9450 1850 9450 2850
Connection ~ 9450 2850
Wire Wire Line
	9450 2850 8750 2850
Wire Wire Line
	8650 1850 8750 1850
Connection ~ 8750 1850
Wire Wire Line
	8750 1850 9450 1850
NoConn ~ 8350 2050
NoConn ~ 8350 3275
Text GLabel 8350 3475 0    50   BiDi ~ 0
MCU_USB_DP
Text GLabel 8350 3375 0    50   BiDi ~ 0
MCU_USB_DM
Text GLabel 8350 2250 0    50   Input ~ 0
F9P_USB_DP
Text GLabel 8350 2150 0    50   Input ~ 0
F9P_USB_DM
Text GLabel 8350 2450 0    50   Input ~ 0
F9P_USB_VBUS
Text GLabel 8350 3675 0    50   Input ~ 0
MCU_USB_VBUS
Text GLabel 5320 2420 2    50   BiDi ~ 0
MCU_USB_DM
Text GLabel 5320 2520 2    50   BiDi ~ 0
MCU_USB_DP
$Comp
L Device:R_Small R6
U 1 1 5D85C4C3
P 5825 3020
F 0 "R6" V 5629 3020 50  0000 C CNN
F 1 "1K" V 5720 3020 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 5825 3020 50  0001 C CNN
F 3 "~" H 5825 3020 50  0001 C CNN
	1    5825 3020
	0    1    1    0   
$EndComp
$Comp
L Device:Crystal_GND24 Y1
U 1 1 5D878BA8
P 2375 2520
F 0 "Y1" H 2181 2474 50  0000 R CNN
F 1 "8Mhz" H 2181 2565 50  0000 R CNN
F 2 "Crystal:Crystal_SMD_5032-4Pin_5.0x3.2mm" H 2375 2520 50  0001 C CNN
F 3 "~" H 2375 2520 50  0001 C CNN
	1    2375 2520
	-1   0    0    1   
$EndComp
$Comp
L power:GND #PWR0108
U 1 1 5D87A909
P 2375 2195
F 0 "#PWR0108" H 2375 1945 50  0001 C CNN
F 1 "GND" H 2380 2022 50  0000 C CNN
F 2 "" H 2375 2195 50  0001 C CNN
F 3 "" H 2375 2195 50  0001 C CNN
	1    2375 2195
	-1   0    0    1   
$EndComp
Wire Wire Line
	2375 2320 2375 2270
Wire Wire Line
	2375 2720 2375 2870
Wire Wire Line
	2375 2870 2675 2870
Wire Wire Line
	2675 2870 2675 2270
Wire Wire Line
	2675 2270 2375 2270
Connection ~ 2375 2270
Wire Wire Line
	2375 2270 2375 2220
Wire Wire Line
	2225 2520 1950 2520
Wire Wire Line
	1625 2520 1625 2945
Wire Wire Line
	1625 2945 2825 2945
$Comp
L Device:C C8
U 1 1 5D87FC5D
P 1950 2370
F 0 "C8" H 2065 2416 50  0000 L CNN
F 1 "15p" H 2065 2325 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 1988 2220 50  0001 C CNN
F 3 "~" H 1950 2370 50  0001 C CNN
	1    1950 2370
	1    0    0    -1  
$EndComp
Connection ~ 1950 2520
Wire Wire Line
	1950 2520 1625 2520
Wire Wire Line
	2975 2220 2375 2220
Connection ~ 2375 2220
Wire Wire Line
	2375 2220 2375 2195
Wire Wire Line
	1950 2220 2375 2220
$Comp
L power:VCC #PWR0111
U 1 1 5D8BC73F
P 4225 1025
F 0 "#PWR0111" H 4225 875 50  0001 C CNN
F 1 "VCC" H 4242 1198 50  0000 C CNN
F 2 "" H 4225 1025 50  0001 C CNN
F 3 "" H 4225 1025 50  0001 C CNN
	1    4225 1025
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C12
U 1 1 5D8C0DB8
P 6100 1250
F 0 "C12" H 6192 1296 50  0000 L CNN
F 1 "2.2u" H 6192 1205 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 6100 1250 50  0001 C CNN
F 3 "~" H 6100 1250 50  0001 C CNN
	1    6100 1250
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C13
U 1 1 5D8C2E84
P 6425 1250
F 0 "C13" H 6517 1296 50  0000 L CNN
F 1 "2.2u" H 6517 1205 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 6425 1250 50  0001 C CNN
F 3 "~" H 6425 1250 50  0001 C CNN
	1    6425 1250
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C14
U 1 1 5D8C9388
P 6750 1250
F 0 "C14" H 6842 1296 50  0000 L CNN
F 1 "2.2u" H 6842 1205 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 6750 1250 50  0001 C CNN
F 3 "~" H 6750 1250 50  0001 C CNN
	1    6750 1250
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C17
U 1 1 5D8CC73F
P 7675 1250
F 0 "C17" H 7767 1296 50  0000 L CNN
F 1 "2.2u" H 7767 1205 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 7675 1250 50  0001 C CNN
F 3 "~" H 7675 1250 50  0001 C CNN
	1    7675 1250
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C16
U 1 1 5D8CC74D
P 7350 1250
F 0 "C16" H 7442 1296 50  0000 L CNN
F 1 "2.2u" H 7442 1205 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 7350 1250 50  0001 C CNN
F 3 "~" H 7350 1250 50  0001 C CNN
	1    7350 1250
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C15
U 1 1 5D8CC75B
P 7025 1250
F 0 "C15" H 7117 1296 50  0000 L CNN
F 1 "2.2u" H 7117 1205 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 7025 1250 50  0001 C CNN
F 3 "~" H 7025 1250 50  0001 C CNN
	1    7025 1250
	1    0    0    -1  
$EndComp
Wire Wire Line
	7675 1150 7350 1150
Wire Wire Line
	7025 1150 7350 1150
Connection ~ 7350 1150
Wire Wire Line
	7025 1150 6750 1150
Connection ~ 7025 1150
Wire Wire Line
	6750 1150 6425 1150
Connection ~ 6750 1150
Wire Wire Line
	6425 1150 6100 1150
Connection ~ 6425 1150
Wire Wire Line
	6100 1350 6425 1350
Connection ~ 6425 1350
Wire Wire Line
	6425 1350 6750 1350
Wire Wire Line
	6750 1350 7025 1350
Connection ~ 6750 1350
Wire Wire Line
	7350 1350 7025 1350
Connection ~ 7025 1350
Wire Wire Line
	7675 1350 7350 1350
Connection ~ 7350 1350
$Comp
L power:VCC #PWR0112
U 1 1 5D8D5819
P 6025 1150
F 0 "#PWR0112" H 6025 1000 50  0001 C CNN
F 1 "VCC" H 6042 1323 50  0000 C CNN
F 2 "" H 6025 1150 50  0001 C CNN
F 3 "" H 6025 1150 50  0001 C CNN
	1    6025 1150
	1    0    0    -1  
$EndComp
Wire Wire Line
	6100 1150 6025 1150
Connection ~ 6100 1150
$Comp
L power:GND #PWR0113
U 1 1 5D8DE261
P 6025 1350
F 0 "#PWR0113" H 6025 1100 50  0001 C CNN
F 1 "GND" H 6030 1177 50  0000 C CNN
F 2 "" H 6025 1350 50  0001 C CNN
F 3 "" H 6025 1350 50  0001 C CNN
	1    6025 1350
	1    0    0    -1  
$EndComp
Wire Wire Line
	6100 1350 6025 1350
Connection ~ 6100 1350
Text Notes 6150 1175 0    50   ~ 0
place close to pin 1, 19, 32, 48, 64, 13\n\n
$Comp
L Connector_Generic:Conn_01x06 P1
U 1 1 56C07D77
P 8875 5175
F 0 "P1" H 8875 5525 50  0000 C CNN
F 1 "SWD" V 8975 5175 50  0000 C CNN
F 2 "Connector_JST:JST_PH_B6B-PH-K_1x06_P2.00mm_Vertical" H 8875 5175 60  0001 C CNN
F 3 "" H 8875 5175 60  0000 C CNN
	1    8875 5175
	1    0    0    1   
$EndComp
NoConn ~ 8675 5375
$Comp
L power:GND #PWR024
U 1 1 56C1E83A
P 8325 5425
F 0 "#PWR024" H 8325 5175 50  0001 C CNN
F 1 "GND" H 8325 5275 50  0000 C CNN
F 2 "" H 8325 5425 60  0000 C CNN
F 3 "" H 8325 5425 60  0000 C CNN
	1    8325 5425
	-1   0    0    -1  
$EndComp
$Comp
L power:VCC #PWR023
U 1 1 56C275FD
P 8325 4825
F 0 "#PWR023" H 8325 4675 50  0001 C CNN
F 1 "VCC" H 8325 4975 50  0000 C CNN
F 2 "" H 8325 4825 60  0000 C CNN
F 3 "" H 8325 4825 60  0000 C CNN
	1    8325 4825
	-1   0    0    -1  
$EndComp
Wire Wire Line
	8675 5075 8325 5075
Wire Wire Line
	8325 5075 8325 5425
Wire Wire Line
	8675 4875 8325 4875
Wire Wire Line
	8325 4875 8325 4825
Text GLabel 8675 4975 0    50   Input ~ 0
SWCLK
Text GLabel 8675 5175 0    50   Input ~ 0
SWDIO
Text GLabel 8675 5275 0    50   Input ~ 0
NRST
Text GLabel 3790 1320 0    50   Input ~ 0
NRST
Text GLabel 5320 2620 2    50   Input ~ 0
SWDIO
Text GLabel 5320 2720 2    50   Input ~ 0
SWCLK
$Comp
L Sensor_Motion:BMI160 U5
U 1 1 5D9248EE
P 2475 4900
F 0 "U5" H 2425 5481 50  0000 C CNN
F 1 "BMI160" H 2425 5390 50  0000 C CNN
F 2 "Package_LGA:Bosch_LGA-14_3x2.5mm_P0.5mm" H 2475 4900 50  0001 C CNN
F 3 "https://ae-bst.resource.bosch.com/media/_tech/media/datasheets/BST-BMI160-DS000.pdf" H 1775 5750 50  0001 C CNN
	1    2475 4900
	1    0    0    -1  
$EndComp
Text GLabel 5320 3920 2    50   Output ~ 0
CAN_TX
$Comp
L power:VCC #PWR0117
U 1 1 5DA39F71
P 2700 4500
F 0 "#PWR0117" H 2700 4350 50  0001 C CNN
F 1 "VCC" H 2717 4673 50  0000 C CNN
F 2 "" H 2700 4500 50  0001 C CNN
F 3 "" H 2700 4500 50  0001 C CNN
	1    2700 4500
	1    0    0    -1  
$EndComp
Wire Wire Line
	2700 4500 2475 4500
Wire Wire Line
	2475 4500 2375 4500
Connection ~ 2475 4500
$Comp
L power:GND #PWR0118
U 1 1 5DA3A944
P 2725 5425
F 0 "#PWR0118" H 2725 5175 50  0001 C CNN
F 1 "GND" H 2730 5252 50  0000 C CNN
F 2 "" H 2725 5425 50  0001 C CNN
F 3 "" H 2725 5425 50  0001 C CNN
	1    2725 5425
	1    0    0    -1  
$EndComp
Wire Wire Line
	2725 5425 2475 5425
Wire Wire Line
	2475 5425 2475 5400
Wire Wire Line
	2475 5425 2375 5425
Wire Wire Line
	2375 5425 2375 5400
Connection ~ 2475 5425
$Comp
L power:VCC #PWR0119
U 1 1 5DA3B2E7
P 1025 5500
F 0 "#PWR0119" H 1025 5350 50  0001 C CNN
F 1 "VCC" H 1042 5673 50  0000 C CNN
F 2 "" H 1025 5500 50  0001 C CNN
F 3 "" H 1025 5500 50  0001 C CNN
	1    1025 5500
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0120
U 1 1 5DA3B82B
P 1025 5850
F 0 "#PWR0120" H 1025 5600 50  0001 C CNN
F 1 "GND" H 1030 5677 50  0000 C CNN
F 2 "" H 1025 5850 50  0001 C CNN
F 3 "" H 1025 5850 50  0001 C CNN
	1    1025 5850
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C19
U 1 1 5DA3C205
P 1275 5650
F 0 "C19" H 1367 5696 50  0000 L CNN
F 1 "2.2u" H 1367 5605 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 1275 5650 50  0001 C CNN
F 3 "~" H 1275 5650 50  0001 C CNN
	1    1275 5650
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C20
U 1 1 5DA3D634
P 1625 5650
F 0 "C20" H 1717 5696 50  0000 L CNN
F 1 "2.2u" H 1717 5605 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 1625 5650 50  0001 C CNN
F 3 "~" H 1625 5650 50  0001 C CNN
	1    1625 5650
	1    0    0    -1  
$EndComp
Wire Wire Line
	1625 5550 1275 5550
Wire Wire Line
	1025 5500 1025 5550
Wire Wire Line
	1025 5550 1275 5550
Connection ~ 1275 5550
Wire Wire Line
	1625 5750 1275 5750
Wire Wire Line
	1025 5850 1025 5750
Wire Wire Line
	1025 5750 1275 5750
Connection ~ 1275 5750
Text Notes 725  5850 0    50   ~ 0
Place near VDD/GND BMI160\n
NoConn ~ 2875 4800
NoConn ~ 2875 4900
NoConn ~ 2875 5000
NoConn ~ 2875 5100
NoConn ~ 1975 5100
Text GLabel 1975 5200 0    50   Output ~ 0
IMU_INT2
$Comp
L power:VCC #PWR0121
U 1 1 5DA4C45E
P 1450 4600
F 0 "#PWR0121" H 1450 4450 50  0001 C CNN
F 1 "VCC" H 1467 4773 50  0000 C CNN
F 2 "" H 1450 4600 50  0001 C CNN
F 3 "" H 1450 4600 50  0001 C CNN
	1    1450 4600
	1    0    0    -1  
$EndComp
Wire Wire Line
	1450 5000 1450 4600
Text GLabel 1180 4900 0    50   Output ~ 0
SCL
Text GLabel 1180 4800 0    50   Output ~ 0
SDA
$Comp
L Device:R_Small R10
U 1 1 5DA5CE83
P 1730 4800
F 0 "R10" H 1789 4846 50  0000 L CNN
F 1 "1K" H 1789 4755 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 1730 4800 50  0001 C CNN
F 3 "~" H 1730 4800 50  0001 C CNN
	1    1730 4800
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small R9
U 1 1 5DA6454D
P 1520 4700
F 0 "R9" H 1579 4746 50  0000 L CNN
F 1 "1K" H 1579 4655 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 1520 4700 50  0001 C CNN
F 3 "~" H 1520 4700 50  0001 C CNN
	1    1520 4700
	1    0    0    -1  
$EndComp
Wire Wire Line
	1450 5000 1975 5000
Wire Wire Line
	1180 4900 1730 4900
Connection ~ 1975 4900
Wire Wire Line
	1975 4900 1990 4900
Wire Wire Line
	1980 4800 1975 4800
Connection ~ 1520 4800
Wire Wire Line
	1520 4800 1180 4800
Connection ~ 1730 4900
Wire Wire Line
	1730 4900 1975 4900
Wire Wire Line
	1730 4710 1730 4700
Wire Wire Line
	1730 4600 1520 4600
Connection ~ 1730 4700
Wire Wire Line
	1730 4700 1730 4600
Wire Wire Line
	1520 4600 1450 4600
Connection ~ 1520 4600
Connection ~ 1450 4600
$Comp
L power:GND #PWR0122
U 1 1 5DA8ACAA
P 2050 4230
F 0 "#PWR0122" H 2050 3980 50  0001 C CNN
F 1 "GND" H 2055 4057 50  0000 C CNN
F 2 "" H 2050 4230 50  0001 C CNN
F 3 "" H 2050 4230 50  0001 C CNN
	1    2050 4230
	1    0    0    -1  
$EndComp
Wire Wire Line
	1970 4700 1970 4230
Wire Wire Line
	1970 4230 2050 4230
Text GLabel 5320 3820 2    50   Input ~ 0
CAN_RX
Connection ~ 1975 4700
Connection ~ 1975 4800
Wire Wire Line
	1975 4800 1520 4800
Text GLabel 5320 3620 2    50   Input ~ 0
SCL
Text GLabel 5320 3720 2    50   Input ~ 0
SDA
Text GLabel 5320 3520 2    50   Input ~ 0
IMU_INT2
Wire Wire Line
	3920 1320 3860 1320
$Comp
L Device:C_Small C21
U 1 1 5D864B0A
P 3860 1160
F 0 "C21" H 3952 1206 50  0000 L CNN
F 1 "100n" H 3952 1115 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3860 1160 50  0001 C CNN
F 3 "~" H 3860 1160 50  0001 C CNN
	1    3860 1160
	1    0    0    -1  
$EndComp
Wire Wire Line
	3860 1260 3860 1320
Connection ~ 3860 1320
Wire Wire Line
	3860 1320 3790 1320
$Comp
L power:GND #PWR0123
U 1 1 5D86877B
P 3640 870
F 0 "#PWR0123" H 3640 620 50  0001 C CNN
F 1 "GND" H 3645 697 50  0000 C CNN
F 2 "" H 3640 870 50  0001 C CNN
F 3 "" H 3640 870 50  0001 C CNN
	1    3640 870 
	1    0    0    -1  
$EndComp
Wire Wire Line
	3860 1060 3860 720 
Wire Wire Line
	3860 720  3640 720 
Wire Wire Line
	3640 720  3640 870 
Wire Wire Line
	3920 1520 3420 1520
Wire Wire Line
	3420 1520 3420 720 
Wire Wire Line
	3420 720  3640 720 
Connection ~ 3640 720 
$Comp
L STM32F4:STM32F405RGTx U2
U 1 1 5D83A137
P 4620 2920
F 0 "U2" H 4135 1160 50  0000 C CNN
F 1 "STM32F405RGTx" H 5060 1165 50  0000 C CNN
F 2 "Package_QFP:LQFP-64_10x10mm_P0.5mm" H 4020 1220 50  0001 R CNN
F 3 "http://www.st.com/st-web-ui/static/active/en/resource/technical/document/datasheet/DM00037051.pdf" H 4620 2920 50  0001 C CNN
	1    4620 2920
	1    0    0    -1  
$EndComp
Wire Wire Line
	4920 1120 4920 1030
Wire Wire Line
	4920 1030 4820 1030
Wire Wire Line
	4225 1030 4225 1025
Connection ~ 4225 1025
Wire Wire Line
	4420 1120 4420 1030
Connection ~ 4420 1030
Wire Wire Line
	4420 1030 4225 1030
Wire Wire Line
	4520 1120 4520 1030
Connection ~ 4520 1030
Wire Wire Line
	4520 1030 4420 1030
Wire Wire Line
	4620 1120 4620 1030
Connection ~ 4620 1030
Wire Wire Line
	4620 1030 4520 1030
Wire Wire Line
	4720 1120 4720 1030
Connection ~ 4720 1030
Wire Wire Line
	4720 1030 4620 1030
Wire Wire Line
	4820 1120 4820 1030
Connection ~ 4820 1030
Wire Wire Line
	4820 1030 4720 1030
Connection ~ 7375 3120
Wire Wire Line
	7375 3020 6675 3020
Wire Wire Line
	7375 3120 7375 3020
Wire Wire Line
	7375 3120 6975 3120
Wire Wire Line
	6675 3120 6275 3120
Wire Wire Line
	6375 3020 5925 3020
$Comp
L Device:LED D3
U 1 1 5D84A7A7
P 6825 3120
F 0 "D3" H 6818 2865 50  0000 C CNN
F 1 "GREEN" H 6818 2956 50  0000 C CNN
F 2 "LED_SMD:LED_0603_1608Metric" H 6825 3120 50  0001 C CNN
F 3 "~" H 6825 3120 50  0001 C CNN
	1    6825 3120
	-1   0    0    1   
$EndComp
$Comp
L Device:LED D2
U 1 1 5D849E88
P 6525 3020
F 0 "D2" H 6518 2765 50  0000 C CNN
F 1 "RED" H 6518 2856 50  0000 C CNN
F 2 "LED_SMD:LED_0603_1608Metric" H 6525 3020 50  0001 C CNN
F 3 "~" H 6525 3020 50  0001 C CNN
	1    6525 3020
	-1   0    0    1   
$EndComp
$Comp
L power:GND #PWR0107
U 1 1 5D849679
P 7375 3120
F 0 "#PWR0107" H 7375 2870 50  0001 C CNN
F 1 "GND" H 7380 2947 50  0000 C CNN
F 2 "" H 7375 3120 50  0001 C CNN
F 3 "" H 7375 3120 50  0001 C CNN
	1    7375 3120
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small R7
U 1 1 5D85D5B6
P 6175 3120
F 0 "R7" V 5979 3120 50  0000 C CNN
F 1 "1K" V 6070 3120 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 6175 3120 50  0001 C CNN
F 3 "~" H 6175 3120 50  0001 C CNN
	1    6175 3120
	0    1    1    0   
$EndComp
Wire Wire Line
	5320 3020 5725 3020
Wire Wire Line
	5320 3120 6075 3120
Connection ~ 3400 1870
Wire Wire Line
	3250 1870 3400 1870
Wire Wire Line
	3400 1870 3400 1920
Wire Wire Line
	3575 1870 3400 1870
$Comp
L Device:C_Small C10
U 1 1 5D899EC0
P 3250 1770
F 0 "C10" H 3342 1816 50  0000 L CNN
F 1 "2.2u" H 3342 1725 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3250 1770 50  0001 C CNN
F 3 "~" H 3250 1770 50  0001 C CNN
	1    3250 1770
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C11
U 1 1 5D89271D
P 3575 1770
F 0 "C11" H 3667 1816 50  0000 L CNN
F 1 "2.2u" H 3667 1725 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3575 1770 50  0001 C CNN
F 3 "~" H 3575 1770 50  0001 C CNN
	1    3575 1770
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0109
U 1 1 5D890EC6
P 3400 1920
F 0 "#PWR0109" H 3400 1670 50  0001 C CNN
F 1 "GND" H 3405 1747 50  0000 C CNN
F 2 "" H 3400 1920 50  0001 C CNN
F 3 "" H 3400 1920 50  0001 C CNN
	1    3400 1920
	1    0    0    -1  
$EndComp
Wire Wire Line
	3780 1820 3780 1635
Wire Wire Line
	3780 1635 3575 1635
Wire Wire Line
	3575 1635 3575 1670
Wire Wire Line
	2525 2520 2975 2520
$Comp
L Device:C C9
U 1 1 5D88021D
P 2975 2370
F 0 "C9" H 3090 2416 50  0000 L CNN
F 1 "15p" H 3090 2325 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3013 2220 50  0001 C CNN
F 3 "~" H 2975 2370 50  0001 C CNN
	1    2975 2370
	1    0    0    -1  
$EndComp
Connection ~ 2975 2520
Wire Wire Line
	2975 2520 3920 2520
Wire Wire Line
	3920 2620 2825 2620
Wire Wire Line
	2825 2620 2825 2945
$Comp
L Connector:Conn_01x04_Female J4
U 1 1 5DA1C984
P 7800 5860
F 0 "J4" H 7828 5836 50  0000 L CNN
F 1 "CAN" H 7828 5745 50  0000 L CNN
F 2 "Connector_JST:JST_PH_B4B-PH-K_1x04_P2.00mm_Vertical" H 7800 5860 50  0001 C CNN
F 3 "~" H 7800 5860 50  0001 C CNN
	1    7800 5860
	1    0    0    -1  
$EndComp
Text GLabel 7600 5960 0    50   Input ~ 0
CANH
Text GLabel 7600 5860 0    50   Output ~ 0
CANL
$Comp
L power:GND #PWR0124
U 1 1 5DA2B1FA
P 6950 5770
F 0 "#PWR0124" H 6950 5520 50  0001 C CNN
F 1 "GND" H 6955 5597 50  0000 C CNN
F 2 "" H 6950 5770 50  0001 C CNN
F 3 "" H 6950 5770 50  0001 C CNN
	1    6950 5770
	1    0    0    -1  
$EndComp
Wire Wire Line
	7600 5760 6950 5760
Wire Wire Line
	6950 5760 6950 5770
Text GLabel 3920 3820 0    50   Input ~ 0
TIMEPULSE
Text GLabel 3920 3620 0    50   Input ~ 0
MCU_TX_F9P_RX
Text GLabel 3920 3720 0    50   Input ~ 0
MCU_RX_F9P_TX
Text GLabel 3920 3920 0    50   Input ~ 0
F9P_RESET_N
Text GLabel 3920 4020 0    50   Input ~ 0
TX_READY
Text Notes 2800 4060 0    50   ~ 0
F9P TX Buffer full.
$Comp
L Connector_Generic:Conn_02x08_Odd_Even J3
U 1 1 5D95B7FE
P 6550 4770
F 0 "J3" H 6600 5287 50  0000 C CNN
F 1 "GPIO" H 6600 5196 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x08_P2.54mm_Vertical" H 6550 4770 50  0001 C CNN
F 3 "~" H 6550 4770 50  0001 C CNN
	1    6550 4770
	1    0    0    -1  
$EndComp
Text GLabel 5320 1320 2    50   Input ~ 0
GPIO1
Text GLabel 5320 1420 2    50   Input ~ 0
GPIO2
Text GLabel 5320 1520 2    50   Input ~ 0
GPIO3
Text GLabel 5320 1620 2    50   Input ~ 0
GPIO4
Text GLabel 5320 1720 2    50   Input ~ 0
GPIO5
Text GLabel 5320 1820 2    50   Input ~ 0
GPIO6
Text GLabel 5320 1920 2    50   Input ~ 0
GPIO7
Text GLabel 5320 2020 2    50   Input ~ 0
GPIO8
Text GLabel 6850 4770 2    50   Input ~ 0
GPIO8
Text GLabel 6850 4570 2    50   Input ~ 0
GPIO4
Text GLabel 6350 4570 0    50   Input ~ 0
GPIO3
Text GLabel 6350 4470 0    50   Input ~ 0
GPIO1
Text GLabel 6350 4670 0    50   Input ~ 0
GPIO5
Text GLabel 6350 4770 0    50   Input ~ 0
GPIO7
Text GLabel 6850 4670 2    50   Input ~ 0
GPIO6
Text GLabel 6850 4470 2    50   Input ~ 0
GPIO2
Text GLabel 3920 4220 0    50   Input ~ 0
GPIO9
Text GLabel 3920 4320 0    50   Input ~ 0
GPIO10
Text GLabel 3920 4420 0    50   Input ~ 0
GPIO11
Text GLabel 3920 4520 0    50   Input ~ 0
GPIO12
Text GLabel 5320 4220 2    50   Input ~ 0
GPIO13
Text GLabel 5320 4320 2    50   Input ~ 0
GPIO14
Text GLabel 5320 4420 2    50   Input ~ 0
GPIO15
Text GLabel 5320 4520 2    50   Input ~ 0
GPIO16
Text GLabel 6850 5170 2    50   Input ~ 0
GPIO16
Text GLabel 6350 5070 0    50   Input ~ 0
GPIO13
Text GLabel 6350 5170 0    50   Input ~ 0
GPIO15
Text GLabel 6850 5070 2    50   Input ~ 0
GPIO14
Text GLabel 6850 4870 2    50   Input ~ 0
GPIO10
Text GLabel 6850 4970 2    50   Input ~ 0
GPIO12
Text GLabel 6350 4970 0    50   Input ~ 0
GPIO11
Text GLabel 6350 4870 0    50   Input ~ 0
GPIO9
$Comp
L power:GND #PWR0110
U 1 1 5DA39A14
P 4520 4820
F 0 "#PWR0110" H 4520 4570 50  0001 C CNN
F 1 "GND" H 4525 4647 50  0000 C CNN
F 2 "" H 4520 4820 50  0001 C CNN
F 3 "" H 4520 4820 50  0001 C CNN
	1    4520 4820
	1    0    0    -1  
$EndComp
Wire Wire Line
	4520 4720 4520 4820
Wire Wire Line
	4520 4720 4620 4720
Connection ~ 4520 4720
Wire Wire Line
	4620 4720 4720 4720
Connection ~ 4620 4720
Wire Wire Line
	3920 1820 3780 1820
Wire Wire Line
	3860 1720 3860 1590
Wire Wire Line
	3860 1590 3250 1590
Wire Wire Line
	3250 1590 3250 1670
Wire Wire Line
	3920 1720 3860 1720
NoConn ~ 5320 2120
NoConn ~ 5320 2220
NoConn ~ 5320 2320
NoConn ~ 5320 2820
NoConn ~ 5320 3220
NoConn ~ 5320 3320
NoConn ~ 5320 3420
NoConn ~ 3920 4120
NoConn ~ 3920 3520
NoConn ~ 3920 3420
NoConn ~ 3920 3320
NoConn ~ 3920 3020
NoConn ~ 3920 2820
NoConn ~ 3920 3120
NoConn ~ 3920 3220
Text GLabel 7600 6060 0    50   Input ~ 0
MCU_USB_VBUS
Text GLabel 4430 5720 2    50   Input ~ 0
MCU_USB_VBUS
Connection ~ 3655 6220
Wire Wire Line
	3455 6220 3655 6220
Wire Wire Line
	3930 6220 3655 6220
Connection ~ 3930 6520
Wire Wire Line
	3655 6520 3655 6420
Wire Wire Line
	3930 6520 3655 6520
$Comp
L Device:C_Small C18
U 1 1 5D93D9F5
P 3655 6320
F 0 "C18" H 3747 6366 50  0000 L CNN
F 1 "2.2u" H 3747 6275 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3655 6320 50  0001 C CNN
F 3 "~" H 3655 6320 50  0001 C CNN
	1    3655 6320
	1    0    0    -1  
$EndComp
$Comp
L power:VCC #PWR0115
U 1 1 5D939D1D
P 3455 6220
F 0 "#PWR0115" H 3455 6070 50  0001 C CNN
F 1 "VCC" H 3472 6393 50  0000 C CNN
F 2 "" H 3455 6220 50  0001 C CNN
F 3 "" H 3455 6220 50  0001 C CNN
	1    3455 6220
	1    0    0    -1  
$EndComp
Connection ~ 4430 6520
Wire Wire Line
	3930 6520 4430 6520
Wire Wire Line
	3930 6320 3930 6520
Connection ~ 4530 6520
Wire Wire Line
	4530 6520 4430 6520
Wire Wire Line
	4530 6520 4530 6620
$Comp
L power:GND #PWR0114
U 1 1 5D938D2C
P 4530 6620
F 0 "#PWR0114" H 4530 6370 50  0001 C CNN
F 1 "GND" H 4535 6447 50  0000 C CNN
F 2 "" H 4530 6620 50  0001 C CNN
F 3 "" H 4530 6620 50  0001 C CNN
	1    4530 6620
	1    0    0    -1  
$EndComp
Wire Wire Line
	5255 6370 5530 6370
Connection ~ 5255 6370
Wire Wire Line
	5255 6195 5255 6370
Wire Wire Line
	5255 5820 4930 5820
Connection ~ 5255 5820
Wire Wire Line
	5255 5995 5255 5820
$Comp
L Device:R_Small R8
U 1 1 5D931940
P 5255 6095
F 0 "R8" H 5314 6141 50  0000 L CNN
F 1 "220R" H 5314 6050 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 5255 6095 50  0001 C CNN
F 3 "~" H 5255 6095 50  0001 C CNN
	1    5255 6095
	1    0    0    -1  
$EndComp
Wire Wire Line
	4930 6370 5255 6370
Wire Wire Line
	4930 6220 4930 6370
Wire Wire Line
	4930 5820 4930 6020
Wire Wire Line
	5530 5820 5255 5820
Text GLabel 5530 6370 2    50   Output ~ 0
CANL
Text GLabel 5530 5820 2    50   Input ~ 0
CANH
Text GLabel 3930 6020 0    50   Input ~ 0
CAN_RX
Text GLabel 3930 5920 0    50   Output ~ 0
CAN_TX
$Comp
L Interface_CAN_LIN:TJA1051TK-3 U6
U 1 1 5D92858D
P 4430 6120
F 0 "U6" H 4430 6701 50  0000 C CNN
F 1 "TJA1051TK-3" H 4430 6610 50  0000 C CNN
F 2 "Package_DFN_QFN:DFN-8-1EP_3x3mm_P0.65mm_EP1.55x2.4mm" H 4430 5620 50  0001 C CIN
F 3 "http://www.nxp.com/documents/data_sheet/TJA1051.pdf" H 4430 6120 50  0001 C CNN
	1    4430 6120
	1    0    0    -1  
$EndComp
NoConn ~ 5320 4020
NoConn ~ 5320 4120
$EndSCHEMATC
