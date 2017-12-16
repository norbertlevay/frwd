# FRWD has several components:
#
# driver    : frwd/driver/src
# converter : frwd/fwd2xml (decrypting)
# csv conv. : frwd/xml/ada-fwdxml2csv (includes kcal energy consumption estimate)
# math scripts: frwd/m (smoothing, plotting,...)
#

release :
	make -C fwd2xml
	make -C xml2csv/ada-fwdxml2csv
	mkdir fwd-0.9
	cp fwd2xml/fwd2xml fwd2xml/fwdxmlunzip.sh fwd-0.9
	cp xml2csv/ada-fwdxml2csv/fwdxml2csv fwd-0.9
	tar cfzv fwd-0.9.tgz fwd-0.9
	rm -fr fwd-0.9

install :
	cp fwd2xml/fwd2xml fwd2xml/fwdxmlunzip.sh ~/bin
	cp xml2csv/ada-fwdxml2csv/fwdxml2csv ~/bin
