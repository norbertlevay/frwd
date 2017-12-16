
# run as root

modprobe -r usbhid

cp linux-2.6.29.1/drivers/usb/serial/cypress_m8.ko /lib/modules/2.6.29-1-netbook/kernel/drivers/usb/serial/cypress_m8.ko

ls -l /lib/modules/2.6.29-1-netbook/kernel/drivers/usb/serial/cypress_m8.ko*
   
   

depmod -a


