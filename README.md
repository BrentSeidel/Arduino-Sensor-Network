# Arduino Sensor Network
This repository contains software for a RS-485 based network of Arduinos reporting sensor
values.  A BeagleBone Black is connected to the RS-485 network and collects the data to
be presented was web pages.

Right now, the repository only contains software.  It will be updated to also include
OpenSCAD models of parts for 3D printing, parts lists, and wiring diagrams.

While this project currently only monitors sensors, it could easily be updated to add some
control elements to do various things.  Use this as a starting point for your own project.

## Contents
The contents are divided up into several directories as described below.

### Analyzer
This is Arduino software that just listens to the RS-485 bus and sends the contents back
to the host PC via USB.  It is occasionally useful when debugging to see what is really
on the RS-485 bus.

### Controller
This is Arduino software that polls the other Arduinos on the RS-485 bus.  There should be
only one of these on the bus.  There are provisions for it to accept some commands from
the BeagleBone Black via discrete inputs.

### Documentation
This contains some documentation for the system.

### Monitor
This is Arduino software for a device that listens to the RS-485 bus and displays output
on an LCD.  It is currently obsolete and not being updated.  It might be useful is some
projects.

### Responder
This is Arduino software for a device that listens to the RS-485 bus and responds with data
from sensors.  Currently a few I2C based sensors are supported.  Discrete and Analog input
can also be added.

### RS-485
This is an Arduino library that handles most of the RS-485 network protocol.  It is used
by all of the other Arduino software and needs to be placed in your Arduino library folder.

### Web
This is Ada code for the web server that runs on the BeagleBone Black.  It is uses my
Ada-Web-Server repository on GitHub.

## Dependancies
Some of the items in this repository depend on other repositories.  In particular:
* Web depends on Ada-Web-Server and BBS-Embed-Ada, both of which also depend on  BBS-Ada.

## License
This software is licensed under GPL V3.0.  Should you with to use it with a different
license, please contact the author.
