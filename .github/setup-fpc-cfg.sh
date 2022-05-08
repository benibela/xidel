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


cat >> $FPCCFG <<EOF
#ifdef linux


# for cross compiling
-Fu/usr/local/lib/fpc/$fpcversion/units/$fpctarget
-Fu/usr/local/lib/fpc/$fpcversion/units/$fpctarget/*
-Fu/usr/local/lib/fpc/$fpcversion/units/$fpctarget/rtl


#ifdef cpui386
-XPi686-linux-gnu-
-Xr/usr/i686-linux-gnu/lib/
-Fl/usr/i686-linux-gnu/lib/
#endif

#ifdef cpuarm
-XParm-linux-gnueabi-
-Xr/usr/arm-linux-gnueabi/lib/
-Fl/usr/arm-linux-gnueabi/lib/
#endif


#ifdef cpuaarch64
-XPaarch64-linux-gnu-
-Xr/usr/aarch64-linux-gnu/lib
-Fl/usr/aarch64-linux-gnu/lib
-Fl/usr/lib/gcc-cross/aarch64-linux-gnu/10/
#endif



#endif
EOF

cat $FPCCFG