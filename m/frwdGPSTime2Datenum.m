#
function datenum = frwdGPSTime2Datenum(GPSWeek,TimeOfWeek)

# 1024 accounts for one 19 year rollover
# GPA time starts at: Jan 6 1980
    datenum = gps2utc(((GPSWeek+1024)*7*24*3600 + TimeOfWeek)./86400 .+ datenum("Jan 6 1980"));

endfunction
