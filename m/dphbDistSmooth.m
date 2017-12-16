# in  vector : [ dist_i ]  i day since first run
# in integer : N number of days to sum (N=7 : 1 week)
# out vector : dpd distance per day where index is th day
function dpN = dphbDistSmooth( dpd, N )

    dpN = zeros( size(dpd) );

# sum N-days dist from previous N days
    for rr = N : rows(dpd)

        dpN(rr,1) = sum( dpd((rr-N+1):rr,1) );

    endfor;

endfunction

