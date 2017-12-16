# 
function se = frwdHist(logdata)

    ixVv = 11;

# 3.6: convert m/s -> km/h
    hist(3.6*logdata(:,ixVv),[0:1:30]);

endfunction
