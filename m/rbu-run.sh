#!/bin/bash


if false
then

#origfwdpath="/home/robi/.wine/drive_c/Program Files/FRWD Replayer/data/Robert"
fwdpath=fwds

echo -e "Decrypt  .fwd -> *.fwd.xml.zip ... "

# convert all .fwd -> .xml.zip
# FIXME puts decrypted file in the dir where also the fwd is BUT should put to working dir
# FIXME add -d dirname switch: 
# put decypted file(s) in the dirname if exists, otherwise exit with error
fwd2xml fwds/*.fwd

fwds=$(ls ${fwdpath}/2*.fwd.xml.zip)

echo -e "Unzip  .fwd.xml.zip -> .fwd.xml ... "

# FIXME write fwdunzip  shell script which behaves like fwd2xml
# places unziped file in current working dir unless -d switch given
for fwd in $fwds
do
#echo -e "DBG "$fwd

xmlfn=$(basename $fwd)
# strip .zip extension:
xmlfn="${xmlfn%.*}"
#echo -e "DBG "$xmlfn
unzip -p $fwd > xmls/$xmlfn

done

fi

echo -e "Convert  .fwd.xml -> .fwd.csv ... "

cd csvs
# CORRECT  puts csv file in the working dir
# FIXME add -d dirname switch: 
# put csv file(s) in the dirname if exists, otherwise exit with error
fwdxml2csv ../xmls/*.xml > ../xmls-all.csv
cd -

# and exec rbu-frwdscv-split.m with xmls-all.csv

