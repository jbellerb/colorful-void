# hardware

This folder contains hardware schematics and an optional controller PCB for connecting the control Raspberry Pi Zero W to the panel's WS2812 LED strip.

## Usage

The Raspberry Pi outputs at 3.3V while the WS2812 LEDs require 5V control signals, so a level shifter is needed to connect the two. For convenience, a PCB was designed to sit over the upper half of the Raspberry Pi's GPIO pins and interface between it and the LEDs. The PCB also routes power for both the LEDs and Pi, so only a single power supply is needed. Out of an abundance of caution, a 5V 4A power supply ([Adafruit product 1466](https://www.adafruit.com/product/1466)) was used to power the prototype. While the PCB is recommended for reliability, everything can also be hand-wired. When hand-wiring, pins 4-13 (excluding 7) of the SN74AHCT125 level shifter can be left disconnected. The data signal was not reliable on a breadboard, so building with one is not recommended. All components were purchased from Adafruit and are linked below.

## Bill of Materials

| Ref | Description                     | Part No.    | Adafruit PID |
| --- | ------------------------------- | ----------- | ------------ |
| J1  | Raspberry Pi Zero W Pins 1-12   | Zero W      | [3400][P1]   |
| J2  | 2-Pin 0.1" Pitch Terminal Block | KF120-2P    | [2138][P2]   |
| J3  | 3-Pin 0.1" Pitch Terminal Block | KF120-3P    | [2136][P3]   |
| U1  | Quad 3V to 5V Level-Shifter     | SN74AHCT125 | [1787][P4]   |

[P1]: https://www.adafruit.com/product/3400
[P2]: https://www.adafruit.com/product/2138
[P3]: https://www.adafruit.com/product/2136
[P4]: https://www.adafruit.com/product/1787

<br />

#### License

<sup>
Copyright (C) Jared Beller, 2021.
</sup>
<br />
<sup>
Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.txt">GNU General Public License, Version 3</a> or later. See <a href="LICENSE">LICENSE</a> for more information.
</sup>
