#!/bin/bash
XIDEL=../../xidel
cd tests/zorbajsoniq/
TOTAL=0
PASSED=0
FAIL=0
SKIPPED=0
NORES=0
for test in ./*.xq; do 
  (( TOTAL=$TOTAL + 1 ))
  if grep -qE "(append|rename|replace|insert|delete) +json" "$test"; then 
    echo Skip JSON Update: $test
    (( SKIPPED=$SKIPPED + 1 ))
  else if grep -qE ":flatten" "$test"; then 
    echo Skip flatten: $test
    (( SKIPPED=$SKIPPED + 1 ))
  else if grep -q "http://www.zorba-xquery.com/modules/store/dynamic/collections/" "$test"; then
    echo Skip Zorba Collection "$test"
    (( SKIPPED=$SKIPPED + 1 ))
  else if grep -q "[{][|]" "$test"; then
    echo Skip Zorba "{|" syntax "$test"
    (( SKIPPED=$SKIPPED + 1 ))
  else
    resname=$(echo "$test" | sed  "s/[.]xq/.xml.res/" - )
    resname=results/$resname
#  echo $test "=>" $resname
    if [ -f "$resname" ]; then 
      if grep -q '<?xml version="1.0" encoding="UTF-8"?>' "$resname"; then GETRES="tail -n +2 $resname"
      else GETRES="cat $resname"
      fi;
      EXTRACT=$(sed  "s/variable/declare variable/g" "$test" | sed "s/declare declare/declare/g" - )
      diff --ignore-all-space <($XIDEL --extract "$EXTRACT" --extract-kind=xquery --printed-node-format xml 2>/dev/null | tr -d '\n' ) <($GETRES); 
      if [ "$?" -eq 1 ]; then 
        (( FAILED=$FAILED + 1 ))
        echo FAILED: $test ; 
        echo GOT: $XIDEL "--extract-file=$test" --extract-kind=xquery --printed-node-format xml 2>/dev/null ; 
        $XIDEL --extract "$EXTRACT" --extract-kind=xquery --printed-node-format xml | head -20
        echo EXPECTED:
        $GETRES | head -20
        echo ------------
        echo
        echo
      else (( PASSED=$PASSED + 1 ))
      fi
     #error test, or error test result => ignore
    else
      (( NORES=$NORES + 1 ))
    fi
  fi fi fi fi
done
echo Passed: $PASSED Failed: $FAILED Skipped: $SKIPPED+$NORES / $TOTAL
