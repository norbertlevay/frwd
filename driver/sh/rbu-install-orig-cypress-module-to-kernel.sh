
# run as root

modprobe -r cypress_m8

cp /lib/modules/2.6.29-1-netbook/kernel/drivers/usb/serial/cypress_m8.ko-orig /lib/modules/2.6.29-1-netbook/kernel/drivers/usb/serial/cypress_m8.ko
   
ls -l /lib/modules/2.6.29-1-netbook/kernel/drivers/usb/serial/cypress_m8.ko*   

depmod -a


