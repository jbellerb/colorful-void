# void-server

This folder contains software to control one or many panels through the Google Smart Home platform.

## Building

This project can either be built with cabal-install under nix or with stack. Stack is recommended. Before compiling, the program must be configured by editing [src/Auth.hs](src/Auth.hs). The program comes configured for a single user with one panel. First, set the `clientSecret` string. I personally use 16 random alphanumeric characters. Next, set authorization token in `auths` for your user. I use 16 random alphanumeric characters for this as well. Finally, set the device's address field to whatever address you use to connect to the panel. Ideally this should not be localhost since the panel should not be exposed to the public internet.

## Usage

The server does not support TLS, but it is required for the authentication endpoints. As such, the server must be proxied by something that supports TLS termination, such as Nginx. An example Nginx site configuration is provided in [../infrastructure/nginx.site](../infrastructure/nginx.site). An example systemd unit file is provided in [../infrastructure/void-server.service](../infrastructure/void-server.service). This program requires no permissions to run.

<br />

#### License

<sup>
Copyright (C) Jared Beller, 2020.
</sup>
<br />
<sup>
Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.txt">GNU General Public License, Version 3</a> or later. See <a href="LICENSE">LICENSE</a> for more information.
</sup>
