#!/usr/bin/env fish

set size (count $argv)
echo $size
if test "$size" -eq 0
    echo "ZERO NVR" >>/tmp/nvrlog
    exit
else if test "$size" -eq 1
     echo "ONE NVR" >>/tmp/nvrlog
     nvr -c ":edit $argv[1]"
else 
     echo "Many NVR" >>/tmp/nvrlog
     nvr -o $argv
end

