#/bin/bash


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/../../../manageUtils.sh

function getVersion(){
  MINOR_VERSION=`grep -i minorVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  MAJOR_VERSION=`grep -i majorVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  BUILD_VERSION=`grep -i buildVersion xidelbase.pas | head -1 | grep -oE [0-9]+`
  if [[ $BUILD_VERSION = 0 ]] ; then  VERSION=$MAJOR_VERSION.$MINOR_VERSION; 
  else VERSION=$MAJOR_VERSION.$MINOR_VERSION.$BUILD_VERSION ; fi
  UPLOAD_PATH="/Xidel/Xidel\ $VERSION/"
  BUILDDATE=`date +%Y%m%d`.`hg log -l 1 | head -1 | sed -e 's/^[^:]*: *//' | tr : .`
  echo "writeln('($BUILDDATE)');" > xidelbuilddata.inc
  ISPRERELEASE=""
  if [[ $BUILD_VERSION = 1 ]] || [[ $BUILD_VERSION = 3 ]] || [[ $BUILD_VERSION = 5 ]] || [[ $BUILD_VERSION = 7 ]] || [[ $BUILD_VERSION = 9 ]]; then 
    UPLOAD_PATH="/Xidel/Xidel\ development/"
    VERSION=$VERSION.$BUILDDATE; 
    ISPRERELEASE=true
  fi
}

sfProject videlibri

action=2

BASE=$HGROOT/programs/internet/xidel


function pushhg(){
	VIDELIBRIBASE=$HGROOT/programs/internet/VideLibri
	PUBLICHG=$HGROOT/../videlibrixidelpublichg

	syncHg $VIDELIBRIBASE/_hg.filemap $HGROOT $PUBLICHG
}

function compile(){
  getVersion
  eval $1 xidel
  echo > xidelbuilddata.inc  
}

function xidelCompileAndroidArm(){
  rm xidel  
  #fpc -Tandroid -Parm -MObjFPC -Scghi -CX -Crt -O3 -g -gl -XX -l -vewnhibq -Filib/arm-android -Fu../../../components/pascal/import/synapse -Fu../../../components/pascal/internet -Fu../../../components/pascal/data -Fu../../../components/pascal/system -Fu../../../components/pascal/import/regexpr/source -Fu../../../components/pascal/import/utf8tools -Fu../../../components/pascal/lib/arm-android -Fu/opt/lazarus/packager/units/arm-android -Fu. -FUlib/arm-android -dUSE_SYNAPSE_WRAPPER -Cg xidel.pas
  #we cannot compile dependencies, as they default to Java based internet access instead Synapse
  /opt/lazarus/lazbuild -d --bm=androidarm xidel.lpi || (echo "FAILED!"; exit)
  #arm-linux-androideabi-strip --strip-all xidel
}

case "$1" in
web)
	cd web
	webUpload xidel.html xidel.css  /
	webUpload ../readme.txt xidel_readme.txt
	exit;
	;;
	
linux64)
        compile lazCompileLinux64 
        if [ $action -lt 2 ]; then exit; fi
        tar -vczf xidel-$VERSION.linux64.tar.gz xidel readme.txt changelog install.sh
        fileUpload xidel-$VERSION.linux64.tar.gz "$UPLOAD_PATH"
        fileUpload $(./meta/build.deb.sh | tail -n 1) "$UPLOAD_PATH"
        ;;

linux32)
        compile lazCompileLinux32 xidel
        if [ $action -lt 2 ]; then exit; fi
        tar -vczf xidel-$VERSION.linux32.tar.gz xidel readme.txt changelog install.sh
        fileUpload xidel-$VERSION.linux32.tar.gz "$UPLOAD_PATH"
        fileUpload $(./meta/build.deb.sh | tail -n 1) "$UPLOAD_PATH"
        ;;

win32)
        compile lazCompileWin32 
        if [ $action -lt 2 ]; then exit; fi
        zip -v xidel-$VERSION.win32.zip xidel.exe changelog readme.txt
        fileUpload xidel-$VERSION.win32.zip "$UPLOAD_PATH"
        ;;

androidarm)
        compile xidelCompileAndroidArm
        if [ $action -lt 2 ]; then exit; fi
        tar -vczf xidel-$VERSION.androidarm.tar.gz xidel readme.txt changelog install.sh
        fileUpload xidel-$VERSION.androidarm.tar.gz "$UPLOAD_PATH"
        #fileUpload $(./meta/build.deb.sh | tail -n 1) "$UPLOAD_PATH"
        ;;

cgi)    lazCompileLinux64 xidelcgi
        webUpload xidelcgi  /../cgi-bin
        ;;

release)
        ./manage.sh src
        ./manage.sh linux32
        ./manage.sh linux64        
        ./manage.sh win32
        ./manage.sh mirror
        ;;
        
hg)     pushhg
        ;;

mirror) 
        pushhg
        SF_PROJECT= 
        mirroredProject xidel
        syncHg $BASE/_hg.filemap
        ;;

src)
  getVersion 
        pushhg
        SRCDIR=/tmp/xidel-$VERSION-src
        rm -R $SRCDIR
        cp -r $PUBLICHG $SRCDIR
        cd /tmp
        rm -Rvf $SRCDIR/programs/internet/VideLibri $SRCDIR/programs/internet/sourceforgeresponder/
        
        mkdir $SRCDIR/components/pascal/import/flre $SRCDIR/components/pascal/import/flre/src
        cp ~/components/pascal/import/flre/README.md ~/components/pascal/import/flre/COPYING* $SRCDIR/components/pascal/import/flre
        cp ~/components/pascal/import/flre/src/*.pas $SRCDIR/components/pascal/import/flre/src
        
        tar -cvzf /tmp/xidel-$VERSION.src.tar.gz --exclude=.hg xidel-$VERSION-src
        fileUpload xidel-$VERSION.src.tar.gz "$UPLOAD_PATH"
        ;;


downloadTable)
   getVersion 
  if [[ "$ISPRERELEASE" = true ]]; then 
    ((BUILD_VERSION = $BUILD_VERSION - 1 ))
    if [[ $BUILD_VERSION = 0 ]] ; then VERSION=$MAJOR_VERSION.$MINOR_VERSION; 
    else VERSION=$MAJOR_VERSION.$MINOR_VERSION.$BUILD_VERSION ; fi
  fi;

   xidel --dot-notation=on http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%20$VERSION/  --extract-kind=xquery  -e '(x"The following Xidel downloads are available on the <a href=&quot;{$url}&quot;>sourceforge download page</a>: <br><br>")' -e 'declare function verboseName($n){ concat ( if (contains($n, "win")) then "Windows: " else if (contains($n, "linux")) then "Universal Linux: " else if (contains($n, ".deb")) then "Debian: " else if (contains($n, "src")) then "Source:" else "", if (contains($n, "32") or contains($n, "386")) then "32 Bit" else if (contains($n, "64"))then "64 Bit" else ""  )   };   
                           <table class="downloadTable">
                           <tr><th>Operating System</th><th>Filename</th><th>Size</th></tr>
                           { for <TABLE id="files_list"><t:loop><TR class="file"><TH>
                             {link := {"verboseName": verboseName(.), "url": resolve-uri(.) || "/download", "name": ./data()}}</TH><td/><td>{link.size := .}</td></TR></t:loop></TABLE> in (/) 
                             order by $link.verboseName descending 
                             return <tr><td>{$link.verboseName}</td><td><a href="{$link.url}">{$link.name}</a></td><td>{$link.size/text()}</td></tr>}
                           <tr><td>Mac 10.8</td><td colspan="2"><a href="https://www.evernote.com/shard/s69/sh/ff1e78f3-a369-4855-b18f-6184ce789c45/f3511927d0fb356ce883835f2eb712e0">externally prebuilt version</a> and compile instructions.</td></tr>
                           </table>'     --printed-node-format xml > /tmp/downloadTable.html;
  
  if [[ "$ISPRERELEASE" = true ]]; then 
    echo '<br> Prereleases for the next version are also <a href="https://sourceforge.net/projects/videlibri/files/Xidel/Xidel%20development/">available</a>.' >> /tmp/downloadTable.html
  fi
  
  cat /tmp/downloadTable.html
  
  xidel --html web/xidel.html --xquery 'transform(/, function($e) {
    if ($e/@class = "downloadSection") then <div class="downloadSection">{ doc("/tmp/downloadTable.html")//body/node() } </div> else $e
  })' > /tmp/xidel.html
  cp /tmp/xidel.html web/

  ;;

usage)
  tr -d '\r' < readme.txt | sed -e "s/'/''/g"  | awk '{print " \047"$0"\047, "}'   > printUsage.pre.inc
  echo 'const data: array[0..' $(wc -l printUsage.pre.inc | grep -oE '[0-9]+' ) '] of string = (' > printUsage.inc
  cat printUsage.pre.inc >>  printUsage.inc
  echo "'');" >> printUsage.inc
  rm printUsage.pre.inc
  ;;

*)
        echo "Unknown command (use web hg cgi win32 linux32 linux64 src)"
        ;;

esac
