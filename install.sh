#!/bin/bash
PREFIX=$1
sourceprefix=
if [[ -d programs/internet/xidel/ ]]; then sourceprefix=programs/internet/xidel/; else sourceprefix=./;  fi
mkdir -p $PREFIX/usr/bin

install -v $sourceprefix/xidel $PREFIX/usr/bin
if [[ -f $sourceprefix/meta/cacert.pem ]]; then 
  mkdir -p $PREFIX/usr/share/xidel
  install -v $sourceprefix/meta/cacert.pem $PREFIX/usr/share/xidel/; 
fi
