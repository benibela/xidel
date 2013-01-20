#!/bin/bash

if [ -f $TESTDIR/$1 ]; then echo FAILED. File already exists: $@
else
  $XIDEL "${@:2}" 2> /dev/null
  if [ -f $TESTDIR/$1 ]; then rm $TESTDIR/$1; echo OK: $1
  else echo FAILED. File not created: $@
  fi
fi

