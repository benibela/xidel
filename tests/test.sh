#!/bin/bash
mydir=`dirname "$0"`

if [ -f $mydir/output/$1 ]; then

  if ($mydir/../xidel "${@:2}" 2> /dev/null  | diff - $mydir/output/$1); 
  then echo OK: $1;
  else echo FAILED: $1
  fi
else
  echo Missing test case: $1
  echo Creating:
  $mydir/../xidel "${@:2}" | tee $mydir/output/$1  
fi
