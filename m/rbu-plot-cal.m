#!/usr/bin/octave -q


#ft = load ("xmls-2009-2012.csv");
ft = load ("xmls-all.csv");
dax = datenum(ft(:,1),ft(:,2),ft(:,3));
dd = ft(:,7);

maxdata = max(dd)

plot(dax,dd,'*');
#bar(dax,dd);
#xlabel ('date');
ylabel ('distance [m]');

# add date-formated labels to axis
datetick ('x',1,'keeplimits');
#datetick ('x',1,'keepticks');
#datetick ('x',2,'keeplimits');

grid on

input("OCT>");

