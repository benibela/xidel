#!/bin/bash
#ls /usr/lib/lazarus/default/lcl/units/* /usr/lib/lazarus/default/lcl/units/*/*
cd programs/internet/xidel #if this fails we are already in the correct directory

function addpaths() {
  paths="$paths $(find $1 -type d  | while read -r f; do 
    for t in $f/*.pas $f/*.pp $f/*.o $f/*.ppu ; do
      [ -e "$t" ] && echo -Fu$f/*; 
      break
    done
    for t in $f/*.inc; do
      [ -e "$t" ] && echo -Fi$f; 
      break
    done
  done)"
}

export paths=""
addpaths ../../..


echo $paths

fpc -FE. $paths xidel.pas 
