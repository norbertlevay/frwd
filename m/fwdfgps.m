# filter by gps fix validity
function df = fwdfgps(dd)

    ixFIX=15; # valid fix = 1
    ix = find(dd(:,ixFIX)==1);
    df=dd(ix,:);

endfunction
