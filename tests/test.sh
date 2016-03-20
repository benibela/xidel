#!/bin/bash
mydir=`dirname "$0"`
if [ -z "$XIDEL" ]; then XIDEL=$mydir/../xidel; fi


if [ -f $mydir/output/$1 ]; then

  if ($XIDEL "${@:2}" 2> /dev/null  | diff --strip-trailing-cr  $mydir/output/$1 -); 
  then echo OK: $1;
       echo $1 >> /tmp/xidel-tests-state-ok
  else echo FAILED: $1
   echo $1 >> /tmp/xidel-tests-state-failed
  
   echo "${@:2}" 
  
   echo "Old (left):"
   cat $mydir/output/$1
  
   echo "New (right):"
   $XIDEL "${@:2}" 2> /dev/null
   
   echo -----------------------------------------
  fi
else
  echo Missing test case: $1
  echo Creating:
  $mydir/../xidel "${@:2}" | tee $mydir/output/$1  
fi
