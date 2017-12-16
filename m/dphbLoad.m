# load xxxx file
function df = dphbLoad(filename)

    ddd=load("-ascii",filename);
#load("-ascii",filename,"ddd");

    size(ddd)

    ix=find(ddd(:,3) < 5);

    tdf=ddd(ix,:);
    size(tdf)
    df = [tdf(:,1)-tdf(1,1), tdf(:,2)/1000, tdf(:,3)];


    # remove duplicate rows (based on date/daynumber)


    plot( df(:,1),df(:,2),"--b;dist km;", df(:,1),10*df(:,3),"--r;10 x step;");
    grid on;


endfunction

