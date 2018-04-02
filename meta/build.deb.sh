#!/bin/sh
builddir=/tmp/xideldebbuild
rm -rf $builddir/
mkdir -p $builddir/DEBIAN $builddir/usr/bin $builddir/usr/share/doc/xidel $builddir/usr/share/man/man1/
./install.sh $builddir
cp -r meta/debian/control meta/debian/conffiles $builddir/DEBIAN/
gzip -9 -n -c meta/debian/changelog > $builddir/usr/share/doc/xidel/changelog.gz
gzip -9 -n -c meta/xidel.1 > $builddir/usr/share/man/man1/xidel.1.gz
cp meta/debian/copyright $builddir/usr/share/doc/xidel/copyright

version=$(./xidel --version | head -1 | grep -oE "[0-9.]+")
sed -Ee "s/Version:.*/Version: $version/" -i $builddir/DEBIAN/control
if file xidel | grep x86-64; then arch=amd64; 
else if file xidel | grep ARM; then arch=arm; else arch=i386; 
fi; fi
sed -Ee "s/Architecture:.*/Architecture: $arch/" -i $builddir/DEBIAN/control

pkg=xidel_$version-1_$arch.deb
fakeroot dpkg-deb -b $builddir/ $pkg

echo $pkg