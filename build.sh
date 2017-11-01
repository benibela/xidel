#!/bin/bash
#Copyright (C) 2015 - 2017 Benito van der Zander 
#                     2017 Michael Klement 
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

die() { echo "${BASH_SOURCE##*/}: ERROR: ${1:-"ABORTING due to unexpected error."}" 1>&2; exit ${2:-1}; }
dieSyntax() { echo "${BASH_SOURCE##*/}: ARGUMENT ERROR: ${1:-"Invalid argument(s) specified."} Use -h for help." 1>&2; exit 2; }

[[ $1 == '-h' || $1 == '--help' ]] && { cat <<EOF; exit 0; }

${BASH_SOURCE##*/} [ [-s] [-t] | -r ]

  -s ... strip debug symbols after building
  -t ... run tests after building
  -i ... installs the binary locally
  -a ... runs all tasks above
  --compiler= path of fpc

EOF

  # Process *options*.
  # Declare getopt's 1st argument as <optChar1>[:]<optChar2>[:]...
  # An *initial* ':' means that you must print your own error messages.
  # An *internal* ':' indicates a required option *argument*
  # Example:
  #   getopts :qn:t: # declares options -q, -n, -t, where -n and -t require an argument.  
  #   # Test with: set -- -q -nOptArgN -t OptArgT  -- -notanoption more
strip=0 test=0 install=0 fpc=fpc
while getopts ':stia-:' opt; do
  [[ $opt == '?' ]] && dieSyntax "Unknown option: -$OPTARG"
  [[ $opt == ':' ]] && dieSyntax "Option -$OPTARG is missing its argument."  
  case "$opt" in
    s)
      strip=1
      ;;
    t)
      test=1
      ;;
    i)
      install=1
      ;;
    a)
      strip=1 test=1 install=1
      ;;
    -) 
      LONG_OPTARG="${OPTARG#*=}"
      case $OPTARG in
        compiler=*) fpc="$LONG_OPTARG" ;;
        * )         dieSyntax "Illegal option --$OPTARG" ;;
      esac
      ;;
    *)
      die "DESIGN ERROR: option -$opt not handled."
      ;;
  esac
done
shift $((OPTIND - 1)) # Skip the already-processed arguments (options).


# Change to the dir. this script is located in (assuming its not being
# invoked via a symlink placed in a different dir.)
cd "${BASH_SOURCE%/*}"

# Determine the platform-appropriate target dir. for use with -i
[[ $(uname) == 'Darwin' ]] && installDir='/usr/local/bin' || installDir='/usr/bin'

# Find the unit and include search paths and add them to
# the script-global ${paths[@]} array variable.
function addpaths() {
  while read -r f; do
    for t in "$f"/*.pas "$f"/*.pp "$f"/*.o "$f"/*.ppu ; do
      [ -e "$t" ] && paths+=( "-Fu$f" )
      break
    done
    for t in "$f"/*.inc; do
      [ -e "$t" ] && paths+=( "-Fi$f" )    
      break
    done
  done < <(find "$1" -type d)
}

#the svn and source tarball contains xidel in programs/internet/xidel and dependencies in components/pascal
#but someone might have checked out the git and installed dependencies elsewhere, so try to guess the local paths
sourceroot=.
sourcepath=.
if [[ -f programs/internet/xidel/xidel.pas ]]; then sourcepath=programs/internet/xidel; sourceroot=../../..;
elif [[ -f ./xidel.pas && -f ../../../programs/internet/xidel/xidel.pas ]]; then sourceroot=../../..; 
elif [[ -f ./xidel.pas ]]; then echo -n 
else
  die "failed to find xidel.pas";
fi

cd $sourcepath

paths=()
addpaths $sourceroot

touch xidelbuilddata.inc

{ echo "-- Building..." && $fpc -O3 -Cort -CX -XX -FE. "${paths[@]}" xidel.pas; } && ls -hl xidel || exit
(( strip )) && { echo "-- Stripping..." && strip xidel && ls -hl xidel || exit; }
(( test )) && { echo "-- Running tests..." && ./tests/tests.sh || exit; }
(( install )) && { echo "-- Installing..." && cp 'xidel' "$installDir/" && echo "-- xidel copied to: $installDir" || exit; }
echo "-- Done."