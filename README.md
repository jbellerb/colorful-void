# Colorful Void

This is a monorepo for "colorful void", a work-in-progress smart accent light built from a recycled monitor's backlight assembly. See each folder for specific instructions about each component.

## Usage

At it's core, this project is an internet connected light, controllable with a simple JSON-based REST API. The lights in the panel are connected to a Raspberry Pi Zero W running [void-client](void-client/), which exposes an internal control API. While this API is designed to be platform-agnostic, the client is intended to be used with [void-server](void-server/), a relay server that allows the Google Smart Home platform to communicate with the light.

## Security

The void-client API is unsecured and should **never** be exposed to the public internet. While security was a goal of both programs, attack surface mitigation is always helpful—especially since void-client is running as root. If possible, run void-client and void-server on different machines and make sure the Raspberry Pi cannot be accessed from the internet. A VPN solution like Wireguard should be used to enable communication between the two machines.

## Google Smart Home Setup

Usage with the Google Smart Home platform is functional, but some usability aspects are incomplete, such as the lack of a proper login form and requiring a full setup every time void-server is restarted. These steps assume void-client is built and running on a Pi, void-server is built and running on a publically accessable domain with HTTPS, and there is some way for the two to talk (such as a Wireguard tunnel)

### Action Setup

First you must create an Actions on Google project. This is covered in steps 1-6 of [this Google Smart Home documentation page](https://developers.google.com/assistant/smarthome/develop/create#create-project). After that, click on "Account linking" in the side bar and fill out the OAuth Client Information. Client ID should be "google" and Client Secret should be whatever you set when building void-server. The Authorization URL should be `https://whatever-url-you-are-hosting-void-server-at/auth` and the Token URL is `https://whatever-url-you-are-hosting-void-server-at/token`. Finally, click on "Actions" in the side bar and set the Fulfillment URL to `https://whatever-url-you-are-hosting-void-server-at/fulfillment`.

### Logging In

To actually control the light, you need to log in using the Google Home app. In the app, tap Settings, Works with Google, and then whatever you named your Action (which should be prefixed by [test]). This will open a browser window which will 404 because I didn't bother to write a proper login endpoint. To login, you will need to rewrite the URL to another one. Given `https://whaterver-url-you-are-hosting-void-server-at/auth?...&state={state string}&...` visit `https://oauth-redirect.googleusercontent.com/r/{your Actions project id}?code={your authorization token from building void-server}&state={state string from earlier}`. The Google Home app should open again. Follow the remaining instructions in the app and the device should be set up.

<br />

#### License

<sup>
Copyright (C) Jared Beller, 2021.
</sup>
<br />
<sup>
Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.txt">GNU General Public License, Version 3</a> or later. See <a href="LICENSE">LICENSE</a> for more information.
</sup>
