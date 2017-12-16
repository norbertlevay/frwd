
dd = load("frwd.csv","-ascii");

size(dd)

# indeces
ixlon=1
ixlat=2
ixsiv=9 # satellites in view
ixfov=10 # FigureOfMerit (integer, the smaller the better. Min = 1
ixHDOP=14
ixFix=15 # 1=valid
ixRR1=17 # RRs look like 16bit bitfields
ixRR2=18
ixRR3=19
ixRR4=20

dg = fwdfgps(dd);

size(dg)

