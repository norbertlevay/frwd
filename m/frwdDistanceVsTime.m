# input  : ld logdata
# output : GPS-time and distances at each meas sample
function dd = frwdDistanceVsTime(ld)

    ixLon =  7;
    ixLat =  6;
    ixTOW = 10; # TimeOfWeek


# theta and phi step
    dT = ld(2:end,ixLon) - ld(1:(end-1),ixLon);
    dF = ld(2:end,ixLat) - ld(1:(end-1),ixLat);

    dd(1,1) = 0;
    dd(1,2) = 0;

# Earth radius
# Earth radius: 6353 ... 6384 km
# lat lon is in xml file expressed with 9 digits 12.3456789
# mean radius of Earth = 6371.088km, diff in poles is about 30km
# (6.28*6371000.0/360.0) = 111km
    Re = 6371088; # [m]

    for ii = 1 : length(dT) # 1 distance between 2 points: result is one row shorter

        dd(ii+1,1) = ld(ii+1,ixTOW) - ld(1,ixTOW); # FIXME assumes WeekNumber did not change during log
        dd(ii+1,2) = (3.1415926/180.0) * Re * sum( sqrt(dT(1:ii).^2 + dF(1:ii).^2) );

    endfor;

endfunction

