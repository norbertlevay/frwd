# input
# ld logdata
function dhr = frwdDistPerHR(ld)

    ixHR = 3;

# integrated distance
    dist=frwdDistance(ld);

# integrated HeartRate
    iHR = sum(ld(:,ixHR)/60);

# if no HR recorded (?)
    if iHR == 0
    dhr = Inf;
    else
    dhr = dist/iHR;
    endif

endfunction

