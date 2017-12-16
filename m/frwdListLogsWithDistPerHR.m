# utility func, list data files
# set: more off for immediate printf
function  mm = frwdListLogsWithDistPerHR()

    fwdDataPath = "/home/robi/.wine/drive_c/Program\\ Files/FRWD_data/"
#[status,text] = system(["ls ",fwdDataPath])
    [status,text] = system(["ls ",fwdDataPath]);

    fns=strsplit(text);

    [rr,cc]=size(fns);
#320
    for i = 1:(cc-1)
    dd = frwdLoad(fns{i});
#size(dd)
    dphr = frwdDistPerHR(dd);
    dist = frwdDistance(dd);
    days = frwdTime(dd); #returns both start and end date, pick one
#printf("%s\t%10.1f\t%8.1f\t%8.1f\n",fns{i},days,dist/1000,dphr);
    printf("%s\t%8.1f\t%8.1f\n",datestr(days(1)),dist/1000,dphr);
    mm(i,:) = [days(1),dist,dphr];
    clear dd
    endfor

    ddd=mm;
    save distVShrNew ddd
    save distVShrFilesNew fns

endfunction
