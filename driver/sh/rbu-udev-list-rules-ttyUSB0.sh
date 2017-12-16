#udevadm info -a -p /sys/bus/usb-serial/devices/ttyUSB0/
udevadm info -a -p $(udevadm info -q path -n /dev/ttyUSB0)
