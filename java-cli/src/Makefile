
# JAR
JAR=../FRWD_CmdLine/jdk1.5.0_05/bin/jar.exe
TARGET=frwd-cmdline.jar

# frwd                                                       
wjdk=/home/robi/.wine/drive_c/Program\ Files/Java/jdk1.5.0_05
root=/home/robi/frwd/java-frwd-cli/src
dosroot=$(shell winepath -w /home/robi/frwd/src)
jc=wine $(wjdk)/bin/javac.exe
#jx=wine $(wjdk)/jre/bin/java.exe
jx=wine $(root)/../FRWD_CmdLine/jdk1.5.0_05/jre/bin/java.exe

dllpath=$(root)/../FRWD_CmdLine/dll
#dllpath=$(wjdk)/jre/bin
#dosjdk=`winepath -w $(dllpath)`
#dosjdk=$(shell winepath -w $(dllpath))

frwd.class : frwd.java
	$(jc) -classpath $(root) frwd.java


runfrwdinfo :
	#echo "DEBUG dosjdk : "${dosjdk}
	$(jx) frwd help
	WINEPATH=$(dllpath) $(jx) frwd info --settings  "1,C:\\Program Files\\FRWD_data"
#	WINEPATH=$(dllpath) $(jx) -Djava.library.path=${dosjdk} -cp ${dosroot} frwd info

runfrwdgetlog :
	WINEPATH=$(dllpath) $(jx) -cp $(root) frwd getlog 3
#	WINEPATH=$(dllpath) $(jx) -Djava.library.path=. -cp $(root) frwd getlog 3

runfrwdclean :
	WINEPATH=$(dllpath) $(jx) -cp $(root) frwd clean


jar: frwd.class
	wine $(JAR) cfm $(TARGET) META-INF/MANIFEST.MF *.class business/device/*.class


clean:
	rm -fr *~ $(TARGET) *.class
