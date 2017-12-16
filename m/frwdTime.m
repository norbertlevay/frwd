# print start end of the log - se is 2x1 vector
# Returned value is datenum (fractional number of days since Jan 1 0000)
# Use datestr(se) to convert to human readable form
function se = frwdTime(logdata)

 se = frwdGPSTime2Datenum(logdata([1,end],9),logdata([1,end],10));

endfunction
