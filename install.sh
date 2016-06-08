#!/bin/bash
PREFIX=$1
sourceprefix=
if [[ -d programs/internet/xidel/ ]]; then sourceprefix=programs/internet/xidel/; else sourceprefix=./;  fi
mkdir -p $PREFIX/usr/bin

install -v $sourceprefix/xidel $PREFIX/usr/bin
