#/bin/bash


source ../../../manageUtils.sh

function getVersion(){
  MINOR_VERSION=`grep minorVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  MAJOR_VERSION=`grep majorVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  VERSION=$MAJOR_VERSION.$MINOR_VERSION
}

sfProject videlibri
getVersion 

action=2

function pushhg(){
	VIDELIBRIBASE=$HGROOT/programs/internet/VideLibri
	PUBLICHG=$HGROOT/../videlibrixidelpublichg

	syncHg $VIDELIBRIBASE/_hg.filemap $HGROOT $PUBLICHG
}

case "$1" in
web)
	cd web
	webUpload xidel.html xidel.css  /
	webUpload ../readme.txt xidel_readme.txt
	exit;
	;;
	
linux64)
        lazCompileLinux64 xidel
        if [ $action -lt 2 ]; then exit; fi
        tar -vczf xidel-$VERSION.linux64.tar.gz xidel readme.txt install.sh
        fileUpload xidel-$VERSION.linux64.tar.gz "/Xidel/Xidel\ $VERSION/"
        checkinstall --install=no --pkgname=Xidel --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libc6" bash ./install.sh 
        fileUpload xidel_$VERSION-1_amd64.deb "/Xidel/Xidel\ $VERSION/"
        ;;

linux32)
        lazCompileLinux32 xidel
        if [ $action -lt 2 ]; then exit; fi
        tar -vczf xidel-$VERSION.linux32.tar.gz xidel readme.txt install.sh
        fileUpload xidel-$VERSION.linux32.tar.gz "/Xidel/Xidel\ $VERSION/"
        checkinstall --pkgarch=i386 --install=no --pkgname=Xidel --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libc6" bash ./install.sh 
        fileUpload xidel_$VERSION-1_i386.deb "/Xidel/Xidel\ $VERSION/"
        ;;

win32)
        lazCompileWin32 xidel 
        if [ $action -lt 2 ]; then exit; fi
        zip -v xidel-$VERSION.win32.zip xidel.exe readme.txt
        fileUpload xidel-$VERSION.win32.zip "/Xidel/Xidel\ $VERSION/"
        ;;

hg)     pushhg;;

src)
	pushhg
	SRCDIR=/tmp/xidel-$VERSION-src
	rm -R $SRCDIR
	cp -r $PUBLICHG $SRCDIR
	cd /tmp
	rm -Rvf $SRCDIR/programs/internet/VideLibri $SRCDIR/programs/internet/sourceforgeresponder/
      	tar -cvzf /tmp/xidel-$VERSION.src.tar.gz --exclude=.hg xidel-$VERSION-src
        fileUpload xidel-$VERSION.src.tar.gz "/Xidel/Xidel\ $VERSION/"
	;;	
		
*)
        echo "Unknown command (use web)"
        ;;

esac
