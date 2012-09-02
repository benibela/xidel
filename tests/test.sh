#!/bin/bash
mydir=`dirname "$0"`

if ($mydir/../xidel ${*:2} 2> /dev/null  | diff - $mydir/$1); 
then echo SAME: $1;
else echo DIFF: $1
fi
