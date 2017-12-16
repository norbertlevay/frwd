

dphr = dphbLoad("distVShrNew");

dpd = dphbDistDaily(dphr);

nnn = 30;
dpN = dphbDistSmooth(dpd, nnn);


clf; # clear figure ??
[ax,h1,h2] = plotyy( dphr(:,1),dphr(:,3) , 1:rows(dpN),dpN);
set([h2],"color","blue");
set([h2],"linestyle","--");
set([h1],"linestyle","-");
set([h1],"color","red");
set([h1],"marker",".");
title(["y-left/red: length in one heart-beat [m/beat]        y-right/blue: dist [km] in " ,num2str(nnn), " days"]);
grid on;



