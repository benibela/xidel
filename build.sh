#!/bin/bash
#ls /usr/lib/lazarus/default/lcl/units/* /usr/lib/lazarus/default/lcl/units/*/*
cd programs/internet/xidel #if this fails we are already in the correct directory

function addpaths() {
  paths="$paths $(find $1 -type d  | while read -r f; do echo -Fu$f/* -Fi$f; done)"
}
#addpaths /usr/lib/lazarus/default/lcl/units/ ]]; then paths="$paths -Fu/usr/lib/lazarus/default/lcl/units/*/"; fi #debian
#addpaths /usr/lib/lazarus/lcl/units/ ]]; then paths="$paths -Fu/usr/lib/lazarus/lcl/units/*/"; fi #fedora
#addpaths /usr/lib64/lazarus/lcl/units/ ]]; then paths="$paths -Fu/usr/lib64/lazarus/lcl/units/*/"; fi #fedora

export paths="$(find ../../.. -type d | while read -r f; do echo -Fu$f -Fi$f; done)"
addpaths /usr/lib/lazarus 
addpaths /usr/lib64/lazarus 

echo $paths

fpc -FE. $paths xidel.pas 
