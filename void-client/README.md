# void-client

This folder contains software to control the panel's LED strip over the network.

## Building

This project requires a recent version of the Rust arm-unknown-linux-gnueabihf toolchain as well as libclang. See the README of [rs_ws281x](https://github.com/rpi-ws281x/rpi-ws281x-rust) for more information about setting this up. This program assumes the LED strip in the panel has 75 lights. If this is not the case for your build, modify the const `LED_COUNT` in [src/light.rs](src/light.rs).

## Usage

This program needs root permissions to run. Logging output is configured with the `RUST_LOG` environment variable. An example systemd unit file is provided in [../infrastructure/void-client.service](../infrastructure/void-client.service).

<br />

#### License

<sup>
Copyright (C) Jared Beller, 2020.
</sup>
<br />
<sup>
Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.txt">GNU General Public License, Version 3</a> or later. See <a href="LICENSE">LICENSE</a> for more information.
</sup>
