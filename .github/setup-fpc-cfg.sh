#!/bin/bash
FPCCFG="$1"
if [[ -z "$FPCCFG" ]]; then FPCCFG=~/.fpc.cfg; fi
if [[ ! -e "$FPCCFG" ]]; then echo '#INCLUDE /etc/fpc.cfg' > $FPCCFG; fi 
echo -Fu$PWD/import/flre/src/             >> $FPCCFG
echo -Fu$PWD/import/pasdblstrutils/src/   >> $FPCCFG
echo -Fu$PWD/import/synapse/              >> $FPCCFG
echo -Fi$PWD/internettools/               >> $FPCCFG
echo -Fu$PWD/internettools/               >> $FPCCFG
echo -Fu$PWD/rcmdline/                    >> $FPCCFG


cat $FPCCFG