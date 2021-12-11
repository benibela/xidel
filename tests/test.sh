#!/bin/bash
mydir=`dirname "$0"`
if [ -z "$XIDEL" ]; then XIDEL=$mydir/../xidel; fi


if [ -f $mydir/output/$1 ]; then

  if ($XIDEL "${@:2}" 2> /dev/null  | diff --strip-trailing-cr  $mydir/output/$1 -); 
  then echo "[1;32mOK[0m": $1;
       echo $1 >> /tmp/xidel-tests-state-ok
  else echo '[1;31mFAILED:[0m' $1
   echo $1 >> /tmp/xidel-tests-state-failed
  
   echo "${@:2}" 
  
   echo "[1;32mOld (left):[0m"
   cat $mydir/output/$1
  
   echo '[1;31mNew (right):[0m'
   $XIDEL "${@:2}" 2> /dev/null
   
   echo -----------------------------------------
  fi
else
  echo Missing test case: $1
  echo Creating:
  $mydir/../xidel "${@:2}" | tee $mydir/output/$1  
fi
