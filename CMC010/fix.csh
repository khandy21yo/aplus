#!/usr/bin/tcsh
#
# Script to help mass-fix source code to Unix format
#
while ($#argv)
   set fnm = ${argv[1]}

sed -i 's/\x0//g' $fnm
dos2unix -f $fnm
mv $fnm  ~/aplus/CMC020/002001/

   shift
end


