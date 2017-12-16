# filter out valid data rows
function df = frwdFilterValid(dd)

# 1. filter by GPS fix validity
    ixFIX=15; # valid fix = 1
    ix = find(dd(:,ixFIX)==1);
    df=dd(ix,:);

endfunction
