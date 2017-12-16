 

sudo modprobe -r usbhid
sudo modprobe -r cypress_m8
#sudo modprobe usbserial debug=1
sudo modprobe usbserial 

#sudo insmod ../../linux-3.0.0/drivers/usb/serial/cypress_m8.ko debug=1
#sudo insmod ../../linux-3.0.0/drivers/usb/serial/cypress_m8.ko 
#sudo insmod ../../linux-3.0.0/drivers/usb/serial/cypress_m8.ko stats=1 interval=10
sudo insmod ../obj/drivers/usb/serial/cypress_m8.ko

dmesg |tail
