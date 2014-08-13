#/bin/bash


source ../../../manageUtils.sh

function getVersion(){
  MINOR_VERSION=`grep -i minorVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  MAJOR_VERSION=`grep -i majorVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  BUILD_VERSION=`grep -i buildVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  if [[ $BUILD_VERSION = 0 ]] ; then  VERSION=$MAJOR_VERSION.$MINOR_VERSION; 
  else VERSION=$MAJOR_VERSION.$MINOR_VERSION.$BUILD_VERSION ; fi
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
        tar -vczf xidel-$VERSION.linux64.tar.gz xidel readme.txt changelog install.sh
        fileUpload xidel-$VERSION.linux64.tar.gz "/Xidel/Xidel\ $VERSION/"
        checkinstall --install=no --pkgname=Xidel --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libc6" bash ./install.sh 
        fileUpload xidel_$VERSION-1_amd64.deb "/Xidel/Xidel\ $VERSION/"
        ;;

linux32)
        lazCompileLinux32 xidel
        if [ $action -lt 2 ]; then exit; fi
        tar -vczf xidel-$VERSION.linux32.tar.gz xidel readme.txt changelog install.sh
        fileUpload xidel-$VERSION.linux32.tar.gz "/Xidel/Xidel\ $VERSION/"
        checkinstall --pkgarch=i386 --install=no --pkgname=Xidel --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libc6" bash ./install.sh 
        fileUpload xidel_$VERSION-1_i386.deb "/Xidel/Xidel\ $VERSION/"
        ;;

win32)
        lazCompileWin32 xidel 
        if [ $action -lt 2 ]; then exit; fi
        zip -v xidel-$VERSION.win32.zip xidel.exe changelog readme.txt
        fileUpload xidel-$VERSION.win32.zip "/Xidel/Xidel\ $VERSION/"
        ;;

cgi)    lazCompileLinux64 xidelcgi
        webUpload xidelcgi  /../cgi-bin
        ;;

release)
        ./manage.sh src
        ./manage.sh linux32
        ./manage.sh linux64        
        ./manage.sh win32
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
		

downloadTable)

   xidel http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%20$VERSION/  --extract-kind=xquery  -e 'concat(x"The following Xidel downloads are available on the <a href=&quot;{$url}&quot;>sourceforge download page</a>: <br><br>")' -e 'declare function verboseName($n){ concat ( if (contains($n, "win")) then "Windows: " else if (contains($n, "linux")) then "Universal Linux: " else if (contains($n, ".deb")) then "Debian: " else if (contains($n, "src")) then "Source:" else "", if (contains($n, "32") or contains($n, "386")) then "32 Bit" else if (contains($n, "64"))then "64 Bit" else ""  )   };            
                           <table class="downloadTable">
                           <tr><th>Operating System</th><th>Filename</th><th>Size</th></tr>
                           { for $link in match(<TABLE id="files_list"><t:loop><TR class="file"><TH><A class="name">{{link := object(), link.verboseName := verboseName(.), link.a := .}}</A></TH><td/><td>{{link.size := .}}</td></TR></t:loop></TABLE>, /).link 
                             order by $link.verboseName descending 
                             return <tr><td>{$link.verboseName}</td><td><a href="{$link.a/@href}">{$link.a/text()}</a></td><td>{$link.size/text()}</td></tr>}
                           <tr><td>Mac 10.8</td><td colspan="2"><a href="https://www.evernote.com/shard/s69/sh/ff1e78f3-a369-4855-b18f-6184ce789c45/f3511927d0fb356ce883835f2eb712e0">externally prebuilt version</a> and compile instructions.</td></tr>
                           </table>'     --printed-node-format xml > /tmp/downloadTable;
  
  cat /tmp/downloadTable
                                                    
   

  ;;

*)
        echo "Unknown command (use web hg cgi win32 linux32 linux64 src)"
        ;;

esac
