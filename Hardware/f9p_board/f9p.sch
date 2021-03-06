EESchema Schematic File Version 5
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 2 4
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
L f9p:Ublox_ZED_F9P U1
U 1 1 5D82C8DE
P 5590 4030
F 0 "U1" H 5565 5655 50  0000 C CNN
F 1 "Ublox_ZED_F9P" H 5565 5564 50  0000 C CNN
F 2 "f9p:UBLOX_ZED_F9P" H 5590 4030 50  0001 C CNN
F 3 "" H 5590 4030 50  0001 C CNN
	1    5590 4030
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0101
U 1 1 5D82E054
P 890 3460
F 0 "#PWR0101" H 890 3210 50  0001 C CNN
F 1 "GND" H 895 3287 50  0000 C CNN
F 2 "" H 890 3460 50  0001 C CNN
F 3 "" H 890 3460 50  0001 C CNN
	1    890  3460
	1    0    0    -1  
$EndComp
Text GLabel 890  3290 1    50   Input ~ 0
GND
Text GLabel 4390 2630 0    50   Input ~ 0
GND
Text GLabel 4390 2830 0    50   Input ~ 0
GND
Text GLabel 4390 3930 0    50   Input ~ 0
GND
Text GLabel 6740 3030 2    50   Input ~ 0
GND
Text GLabel 6740 3930 2    50   Input ~ 0
GND
Text GLabel 6740 4730 2    50   Input ~ 0
GND
NoConn ~ 6740 2630
NoConn ~ 6740 2730
NoConn ~ 6740 2830
NoConn ~ 6740 2930
NoConn ~ 4390 3330
NoConn ~ 4390 3430
NoConn ~ 4390 3530
NoConn ~ 4390 3630
NoConn ~ 4390 3830
NoConn ~ 4390 4130
NoConn ~ 4390 4230
NoConn ~ 4390 4330
NoConn ~ 4390 4430
NoConn ~ 4390 4730
NoConn ~ 4390 4830
NoConn ~ 4390 4930
NoConn ~ 4390 5030
NoConn ~ 4390 5130
NoConn ~ 6740 5130
NoConn ~ 6740 5330
Text GLabel 5490 5730 3    50   Input ~ 0
GND
$Comp
L Device:R_Small R4
U 1 1 5D84F531
P 7915 3730
F 0 "R4" V 7719 3730 50  0000 C CNN
F 1 "27 Ohm" V 7810 3730 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 7915 3730 50  0001 C CNN
F 3 "~" H 7915 3730 50  0001 C CNN
	1    7915 3730
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R5
U 1 1 5D84FC6B
P 8315 3830
F 0 "R5" V 8119 3830 50  0000 C CNN
F 1 "27 Ohm" V 8210 3830 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 8315 3830 50  0001 C CNN
F 3 "~" H 8315 3830 50  0001 C CNN
	1    8315 3830
	0    1    1    0   
$EndComp
Wire Wire Line
	7815 3730 6740 3730
Wire Wire Line
	8215 3830 6740 3830
Text GLabel 8890 3830 2    50   Input ~ 0
F9P_USB_DP
Text GLabel 8890 3730 2    50   Input ~ 0
F9P_USB_DM
Wire Wire Line
	8415 3830 8890 3830
Wire Wire Line
	8015 3730 8890 3730
$Comp
L Device:R_Small R3
U 1 1 5D863978
P 7365 3505
F 0 "R3" H 7423 3459 50  0000 L CNN
F 1 "100K" H 7423 3550 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 7365 3505 50  0001 C CNN
F 3 "~" H 7365 3505 50  0001 C CNN
	1    7365 3505
	-1   0    0    1   
$EndComp
Wire Wire Line
	6740 3630 7365 3630
Wire Wire Line
	7365 3630 7365 3605
Wire Wire Line
	7365 3630 7765 3630
Wire Wire Line
	7765 3630 7765 3230
Connection ~ 7365 3630
Text GLabel 6740 3530 2    50   Input ~ 0
GND
Text GLabel 7365 3405 1    50   Input ~ 0
GND
$Comp
L Connector:Conn_01x01_Female J5
U 1 1 5DA49920
P 3870 2510
F 0 "J5" H 3898 2536 50  0000 L CNN
F 1 "Active Antenna" H 3898 2445 50  0000 L CNN
F 2 "Connector_Coaxial:U.FL_Molex_MCRF_73412-0110_Vertical" H 3870 2510 50  0001 C CNN
F 3 "~" H 3870 2510 50  0001 C CNN
	1    3870 2510
	0    -1   -1   0   
$EndComp
Text GLabel 7765 3230 2    50   Input ~ 0
F9P_3v3
NoConn ~ 6740 4930
NoConn ~ 6740 5030
NoConn ~ 6740 4630
NoConn ~ 4390 4530
$Comp
L power:VCC #PWR0116
U 1 1 5D8A54ED
P 7190 2805
F 0 "#PWR0116" H 7190 2655 50  0001 C CNN
F 1 "VCC" H 7207 2978 50  0000 C CNN
F 2 "" H 7190 2805 50  0001 C CNN
F 3 "" H 7190 2805 50  0001 C CNN
	1    7190 2805
	1    0    0    -1  
$EndComp
Wire Wire Line
	6740 3130 7190 3130
Wire Wire Line
	7190 3130 7190 2805
Wire Wire Line
	6740 3230 7190 3230
Wire Wire Line
	7190 3230 7190 3130
Connection ~ 7190 3130
$Comp
L Device:C_Small C23
U 1 1 5D8A7E75
P 7415 2955
F 0 "C23" H 7415 3055 50  0000 L CNN
F 1 "2.2u" H 7340 2880 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 7415 2955 50  0001 C CNN
F 3 "~" H 7415 2955 50  0001 C CNN
	1    7415 2955
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C25
U 1 1 5D8AA32D
P 7840 2955
F 0 "C25" H 7865 3055 50  0000 L CNN
F 1 "2.2u" H 7790 2880 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 7840 2955 50  0001 C CNN
F 3 "~" H 7840 2955 50  0001 C CNN
	1    7840 2955
	1    0    0    -1  
$EndComp
Wire Wire Line
	7190 3130 7415 3130
Wire Wire Line
	7415 3130 7415 3055
Wire Wire Line
	7840 3130 7840 3055
$Comp
L power:GND #PWR0125
U 1 1 5D8B002A
P 8240 2680
F 0 "#PWR0125" H 8240 2430 50  0001 C CNN
F 1 "GND" H 8245 2507 50  0000 C CNN
F 2 "" H 8240 2680 50  0001 C CNN
F 3 "" H 8240 2680 50  0001 C CNN
	1    8240 2680
	1    0    0    -1  
$EndComp
Wire Wire Line
	8240 2680 7840 2680
Wire Wire Line
	7840 2680 7840 2855
Wire Wire Line
	7840 2680 7690 2680
Wire Wire Line
	7415 2680 7415 2855
Connection ~ 7840 2680
Wire Wire Line
	6740 3430 7190 3430
Wire Wire Line
	7190 3430 7190 3230
Connection ~ 7190 3230
$Comp
L Device:C_Small C24
U 1 1 5D8B6460
P 7690 2955
F 0 "C24" H 7690 3080 50  0000 L CNN
F 1 "2.2u" H 7615 2880 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 7690 2955 50  0001 C CNN
F 3 "~" H 7690 2955 50  0001 C CNN
	1    7690 2955
	1    0    0    -1  
$EndComp
Wire Wire Line
	7690 3130 7690 3055
Wire Wire Line
	7690 3130 7840 3130
Wire Wire Line
	7690 2855 7690 2680
Connection ~ 7690 2680
Wire Wire Line
	7690 2680 7415 2680
Wire Wire Line
	7415 3130 7690 3130
Connection ~ 7415 3130
Connection ~ 7690 3130
Text GLabel 4390 3730 0    50   Input ~ 0
GND
$Comp
L Device:LED D1
U 1 1 5D8D128B
P 3865 4630
F 0 "D1" H 3858 4846 50  0000 C CNN
F 1 "RED" H 3858 4755 50  0000 C CNN
F 2 "LED_SMD:LED_0603_1608Metric" H 3865 4630 50  0001 C CNN
F 3 "~" H 3865 4630 50  0001 C CNN
	1    3865 4630
	1    0    0    -1  
$EndComp
Wire Wire Line
	4390 4630 4015 4630
$Comp
L Device:R_Small R11
U 1 1 5D8D2E93
P 3315 4630
F 0 "R11" V 3119 4630 50  0000 C CNN
F 1 "1k" V 3210 4630 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 3315 4630 50  0001 C CNN
F 3 "~" H 3315 4630 50  0001 C CNN
	1    3315 4630
	0    1    1    0   
$EndComp
Wire Wire Line
	3715 4630 3415 4630
$Comp
L power:GND #PWR0127
U 1 1 5D8D396B
P 3040 4705
F 0 "#PWR0127" H 3040 4455 50  0001 C CNN
F 1 "GND" H 3045 4532 50  0000 C CNN
F 2 "" H 3040 4705 50  0001 C CNN
F 3 "" H 3040 4705 50  0001 C CNN
	1    3040 4705
	1    0    0    -1  
$EndComp
Wire Wire Line
	3215 4630 3040 4630
Wire Wire Line
	3040 4630 3040 4705
Text GLabel 6740 5230 2    50   Output ~ 0
TIMEPULSE
Text GLabel 6740 4130 2    50   Input ~ 0
MCU_RX_F9P_TX
Text GLabel 6740 4230 2    50   Input ~ 0
MCU_TX_F9P_RX
Text GLabel 6740 4830 2    50   Input ~ 0
F9P_RESET_N
Text Notes 7755 5305 0    50   ~ 0
DSEL 	1 (or open)		|	0 \n42		UART TX			|	SPI_MISO\n43		UART RX			|	SPI_MOSI\n44		I2C Data			|	SPI CS \n45		I2C_CLOCK			|	SPI_CLK 
Wire Notes Line
	9420 4850 9420 5375
Wire Notes Line
	9420 5375 7645 5375
Wire Notes Line
	7645 5375 7645 4850
Wire Notes Line
	7645 4850 9420 4850
Text Notes 7430 4215 0    50   ~ 0
Host-interface UART
Text Notes 4760 5395 0    50   ~ 0
Correction UART  RX/TX\n\n
NoConn ~ 6740 3330
$Comp
L pspice:INDUCTOR L2
U 1 1 5D8D6A1F
P 3310 2710
F 0 "L2" H 3310 2529 50  0000 C CNN
F 1 "56n" H 3310 2620 50  0000 C CNN
F 2 "Inductor_SMD:L_0402_1005Metric" H 3310 2710 50  0001 C CNN
F 3 "~" H 3310 2710 50  0001 C CNN
	1    3310 2710
	-1   0    0    1   
$EndComp
$Comp
L Device:R_Small R12
U 1 1 5D8D71E7
P 3930 3230
F 0 "R12" V 3734 3230 50  0000 C CNN
F 1 "4.7R" V 3825 3230 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 3930 3230 50  0001 C CNN
F 3 "~" H 3930 3230 50  0001 C CNN
	1    3930 3230
	0    1    1    0   
$EndComp
$Comp
L Device:C_Small C22
U 1 1 5D8D796F
P 3610 3460
F 0 "C22" H 3702 3506 50  0000 L CNN
F 1 "100n" H 3702 3415 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3610 3460 50  0001 C CNN
F 3 "~" H 3610 3460 50  0001 C CNN
	1    3610 3460
	1    0    0    -1  
$EndComp
Wire Wire Line
	4390 3230 4030 3230
$Comp
L power:GND #PWR0103
U 1 1 5D8F12BF
P 3610 3650
F 0 "#PWR0103" H 3610 3400 50  0001 C CNN
F 1 "GND" H 3615 3477 50  0000 C CNN
F 2 "" H 3610 3650 50  0001 C CNN
F 3 "" H 3610 3650 50  0001 C CNN
	1    3610 3650
	1    0    0    -1  
$EndComp
Wire Wire Line
	3830 3230 3610 3230
Wire Wire Line
	3610 3230 3610 3360
Wire Wire Line
	3610 3560 3610 3650
Wire Wire Line
	4390 2730 3870 2730
Wire Wire Line
	3870 2730 3870 2710
Wire Wire Line
	3870 2730 3560 2730
Wire Wire Line
	3560 2730 3560 2710
Connection ~ 3870 2730
Wire Wire Line
	3060 2710 2670 2710
Wire Wire Line
	2670 2710 2670 3230
Wire Wire Line
	2670 3230 3610 3230
Connection ~ 3610 3230
NoConn ~ 4390 2930
NoConn ~ 4390 3030
NoConn ~ 4390 3130
Text Notes 4850 3060 0    50   ~ 0
Output\n
Text Notes 5070 3160 0    50   ~ 0
Input (Active low)\n
Text Notes 5000 2960 0    50   ~ 0
Input (Active high)
NoConn ~ 6740 4330
NoConn ~ 6740 4430
Text GLabel 6740 4530 2    50   Input ~ 0
TX_READY
NoConn ~ 4390 5230
NoConn ~ 4390 5330
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 5DA99163
P 1150 3340
F 0 "#FLG0102" H 1150 3415 50  0001 C CNN
F 1 "PWR_FLAG" H 1150 3513 50  0000 C CNN
F 2 "" H 1150 3340 50  0001 C CNN
F 3 "~" H 1150 3340 50  0001 C CNN
	1    1150 3340
	1    0    0    -1  
$EndComp
Wire Wire Line
	890  3290 890  3340
Wire Wire Line
	1150 3340 890  3340
Connection ~ 890  3340
Wire Wire Line
	890  3340 890  3460
$EndSCHEMATC
