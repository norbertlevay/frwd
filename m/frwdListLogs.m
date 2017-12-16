# utility func, list data files
function frwdListLogs()

    fwdDataPath = "/home/robi/.wine/drive_c/Program\\ Files/FRWD_data/"
#[status,text] = system(["ls ",fwdDataPath])
    system(["ls ",fwdDataPath])

endfunction
