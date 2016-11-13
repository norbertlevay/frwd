#!/bin/bash

if test $# -eq 0
then
 cmdn=$(basename  $0 )
 echo -e "\nUsage: $cmdn file(s).fwd.xml.zip\n"
 exit
fi

# are wildcards expanded - should we do a for-cycle here, or
# pass all params to unzip ?
# unzip multiple files: unzip '*.zip' - unzip interprets widlcard otherwise
# will not work - handles only one file at a time

# all arguments except $0
for fn in $@
do
bfn=$(basename $fn)
unzip -p $1 > "${bfn%.*}"
done
