# in  :
# distTime : matrix 1st column time 2nd column dist upt to that time
# N        : search for best time of consecutive N-km
# out :
# time [sec] : best time found
# dist [m]   : and correspondig exact dist
function [time, dist, ifrom, ito] = frwdBestNkm(distTime,N)

#size(distTime);

    ifrom = 1;
    ii = 0;
    dd = 0;
    ts = 1000000000; # stores current shortest time
    ds = 0; # and its corresponding exact distance
    # while not end-of-meas
    while ( (ii+1) <= rows(distTime) )

        # while shorter then Nkm and not end-of-meas
        while ( (dd <= N) && ((ii+1) <= rows(distTime)) )
           ii++;
           dd = distTime(ii,2) - distTime(ifrom,2);
        endwhile;

        # if not end of meas
#        if ii <= rows(distTime)

        tt = distTime(ii,1) - distTime(ifrom,1);

         if(tt < ts)
             ts = tt;
             ds = dd;
             ixs = ii;
             ifroms = ifrom;
         endif

         ifrom++;
         dd = distTime(ii,2) - distTime(ifrom,2);
#        endif

    endwhile;

    time = ts;
    dist = ds;

    ifrom = ifroms;
    ito   = ixs;

endfunction;



