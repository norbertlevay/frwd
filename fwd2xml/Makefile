

# to write zip file needs libbz2-dev package installed!
# fwdheader needs libssl-dev package installed!
PROG=fwd2xml

all : fwd2xml 

# link statically, otherwise would need to bepart of some distribution
$(PROG) : $(PROG).c
	gcc $(PROG).c -lcrypto -lz -ldl -static -o $(PROG)
#	gcc $(PROG).c  -lcrypto -lz -lbz2 -o $(PROG)
#	gcc $(PROG).c  -lcrypto -o $(PROG)
#	gcc $(PROG).c -lmcrypt -lcrypt -o $(PROG)


clean :
	rm $(PROG)


install:
	sudo install fwd2xml fwdxmlunzip.sh /usr/local/bin

