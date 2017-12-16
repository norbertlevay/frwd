
VER="0.1.0"

name=FRWD_CmdLine
installdir="/home/robi/.wine/drive_c/Program Files/"

src/frwd-cmdline.jar:
	make -C src jar

zip: src/frwd-cmdline.jar
	cp README src/frwd-cmdline.jar src/frwd $(name)
	zip -r $(name)-$(VER).zip $(name)

clean:
	rm -f *~ $(name)-$(VER).zip

distclean:
	make -C src clean
	make clean

install:
	cp FRWD_CmdLine-$(VER).zip $(installdir)
	ls -lth $(installdir)
	make -C $(installdir) --eval="VER=$(VER)"
