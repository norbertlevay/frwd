# input  : ld logdata
# output : log distance (scalar)
function dd = frwdDistance(ld)

    ixLon = 7;
    ixLat = 6;

# theta and phi step
    dT = ld(2:end,ixLon) - ld(1:(end-1),ixLon);
    dF = ld(2:end,ixLat) - ld(1:(end-1),ixLat);

# Earth radius
# Earth radius: 6353 ... 6384 km
# lat lon is in xml file expressed with 9 digits 12.3456789
# mean radius of Earth = 6371.088km, diff in poles is about 30km
# (6.28*6371000.0/360.0) = 111km
    Re = 6371088; # [m]
    dd = (3.1415926/180.0) * Re * sum(sqrt(dT.^2 + dF.^2));

endfunction

