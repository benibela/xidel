#!/bin/bash
mkdir -p import
git clone https://github.com/benibela/flre.git import/flre
mkdir -p import/pasdblstrutils/src && curl https://raw.githubusercontent.com/BeRo1985/pasdblstrutils/master/src/PasDblStrUtils.pas > import/pasdblstrutils/src/PasDblStrUtils.pas
git clone https://github.com/benibela/ararat-synapse.git import/synapse
git clone https://github.com/benibela/rcmdline.git rcmdline
git clone https://github.com/benibela/internettools.git internettools
