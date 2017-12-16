
#mknod /tmp/wine_debug_pipe p
# then whe running cat /tmp/wine_debug_pipe

cd /home/robi/.wine/drive_c/Program\ Files/FRWD\ Replayer
WINEDEBUG=+relay,+snoop wine  JRE/bin/javaw.exe  -Xms16m -Xmx256m -jar FRWD.jar &> /tmp/wine_debug_pipe



# Xms   initial Java heapsize
# Xmx   maximum Java heap size
# Xss   java thread stack size
