
# run as sudo

mount -t debugfs none_debugs /sys/kernel/debug
#modprobe usbmon  <- is compiled into kernel?
ls /sys/kernel/debug/usbmon

# 0ut packates on all busses, or need bus number of the device:
# lsusb shows Dongle onbus 2 (when connected to left,up USB socket on EeePc)
# u t and etc designates different output formats od usbmon

# Then observe with command (as sudo):
#cat /sys/kernel/debug/usbmon/2u

