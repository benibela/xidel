#!/bin/bash
if [[ ! -e ~/.fpc.cfg ]]; then echo '#INCLUDE /etc/fpc.cfg' > ~/.fpc.cfg; fi 
echo -Fu$PWD/import/flre/src/             >> ~/.fpc.cfg
echo -Fu$PWD/import/pasdblstrutils/src/   >> ~/.fpc.cfg
echo -Fu$PWD/import/synapse/              >> ~/.fpc.cfg
echo -Fi$PWD/internettools/               >> ~/.fpc.cfg
echo -Fu$PWD/internettools/               >> ~/.fpc.cfg
echo -Fu$PWD/rcmdline/                    >> ~/.fpc.cfg

