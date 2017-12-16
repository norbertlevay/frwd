#!/bin/bash

# modules build command needs to be exected where driver sources are
# in cypress_m8.c and .h search for FRWD for my changes

cd /home/robi/linux-3.0.0/drivers/usb/serial

make -C /usr/src/linux-headers-`uname -r` M=`pwd` modules

cd -

