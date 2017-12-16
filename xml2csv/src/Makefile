
fwdxml2csv:
	gnatmake -P fwdxml2csv.gpr



clean:
	rm -f fwdxml2csv *~
 

install:
	install fwdxml2csv /usr/local/bin


fixme:
	@grep -n FIXME *.ad?

todo:
	@grep -n TODO *.ad?

run:
	./fwdxml2csv  ~/frwd/datafromwin/*.fwd.xml

