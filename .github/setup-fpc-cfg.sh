#!/bin/bash
FPCCFG="$1"
CURPATH="$2"
if [[ -z "$FPCCFG" ]]; then FPCCFG=~/.fpc.cfg; fi
if [[ ! -e "$FPCCFG" ]]; then echo '#INCLUDE /etc/fpc.cfg' > $FPCCFG; fi 

if [[ -z "$CURPATH" ]]; then CURPATH=$PWD; fi

echo -Fu$CURPATH/import/flre/src/             >> $FPCCFG
echo -Fu$CURPATH/import/pasdblstrutils/src/   >> $FPCCFG
echo -Fu$CURPATH/import/synapse/              >> $FPCCFG
echo -Fi$CURPATH/internettools/               >> $FPCCFG
echo -Fu$CURPATH/internettools/               >> $FPCCFG
echo -Fu$CURPATH/rcmdline/                    >> $FPCCFG


cat $FPCCFG