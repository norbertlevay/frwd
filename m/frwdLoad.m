# load fwrd.csv log file
function df = frwdLoad(filename)

    dpath = "/home/robi/.wine/drive_c/Program Files/FRWD_data/";

    dd = load([dpath,filename],"-ascii");
    size(dd);

# HW order:
# Pressure Temperature HeartRate LapTimeMark FigureOfMerit Latitude Longitude GPSAltitude GPSWeek TimeOfWeek HorizontalVelocity MotionDirection VerticalVelocity SatellitesInView FixInfo HDOP RR1 RR2 RR3 RR4
# indeces
#  1  4  Pressure Temperature HeartRate LapTimeMark
#  5 10  FigureOfMerit Latitude Longitude GPSAltitude GPSWeek TimeOfWeek
# 11 13  HorizontalVelocity MotionDirection VerticalVelocity
# 14 20  SatellitesInView FixInfo HDOP RR1 RR2 RR3 RR4
    isP  = 1;
    isT  = 2;
    ixHR = 3;
    ixFOM= 5; # FigureOfMerit integer, the smaller the better. Min = 1
    ixLat= 6;
    ixLon= 7;
    ixAlt= 8;
    ixWN = 9; # WeekNumber
    ixTOW=10; # TimeOfWeek
    ixVh =11; # [m/s] velocity horizontal
    ixVv =13; # velocity vertical
    ixSIV=14; # satellites in view
    ixFix=15; # 1=valid
    ixHDOP=16;
    ixRR1=17; # RRs look like 16bit bitfields
    ixRR2=18;
    ixRR3=19;
    ixRR4=20;

    df = frwdFilterValid(dd);

endfunction
