#!/usr/bin/octave -q


clear all

dd=load("xmls-all.csv");

# filter outliers
ixol=find( dd(:,7)<=30000);
ddf=dd(ixol,:);

size(dd);
size(ddf);

# select by year
for yy=2009:2015

ixyy=find( ddf(:,1)==yy);
ddfy=ddf(ixyy,:);
distkm=sum(ddfy(:,7))/1000

# select by month from the above year
for mm = 1:12
    ixmm = find( ddfy(:,2) == mm );

    if length(ixmm) == 0
#       ddmm(mm,:) = [ yy, mm, 1, 0 , 0 ];
       continue;
    endif

        ddmm(mm,:) = [ ddfy(ixmm(1),1:4) ,sum( ddfy(ixmm,7) ) ];
endfor

clear ddww
# select by week from the above year
for ww = 1:52
    ixww = find( ddfy(:,4) == ww );

    if length(ixww) == 0
# ddww(ww,:) = [ yy, 1+floor(ww/4), 1, ww , 1 ];
       continue;
    endif

        ddww(ww,:) = [ ddfy(ixww(1),1:4) ,sum( ddfy(ixww,7) ) ];

    clear ixww;
endfor


# pass to plot nonzero rows only
ixnz = find( ddww(:,1) != 0 );
ft = ddww(ixnz,:);

dax = datenum(ft(:,1),ft(:,2),ft(:,3));
ddd = ft(:,5);
plot(dax,ddd,'*');
#bar(dax,ddd);
#xlabel ('date');
ylabel ('weekly distance [m]');
datetick ('x',1,'keeplimits');
title(strcat(num2str(yy)," Year tot [km]: ",num2str(distkm)))
axis([datenum(yy,1,1),datenum(yy,12,31),0,42000]);
grid on

fn = strcat("run-",num2str(yy),".eps")
print(fn,"-deps");

endfor
