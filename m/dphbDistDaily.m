# in matrix  : [ daynumber dist ]
#    daynumber is fractional number of days since some fixed date
#    here we will count from zero at first run := daynumber_i - daynumber_first
# out vector : dpd distance per day where index is th day
function dpd = dphbDistDaily( distVShr )

# we need days & dist only
    dhr = distVShr(:,1:2);
    dhr = dhr(:,:) - dhr(1,1);
    dpd = zeros( ceil( dhr(end,1) + 1 ), 1);

# fill days from sparse dhr matrix into non-sparse vector
    for rr = 1 : rows(dhr)

        day = 1 + floor( dhr(rr,1) );

        dpd(day,1) = dpd(day,1) + dhr(rr,2);

    endfor;

endfunction

