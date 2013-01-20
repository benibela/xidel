#!/bin/bash

OWNDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export XIDEL=$OWNDIR/../xidel
export TESTDIR=/tmp/xideldownloadtests
TEST=$OWNDIR/downloadTest.sh
ADOMAIN=http://example.org


rm -Rf $TESTDIR
mkdir $TESTDIR
cd $TESTDIR

#Testing if the created file has the correct name

$TEST index.html $ADOMAIN --download .
$TEST foobar $ADOMAIN/foobar --download .
$TEST nest $ADOMAIN/foo/bar/nest --download .
$TEST index.html $ADOMAIN/foo/bar/ --download .
$TEST nest2 $ADOMAIN/'foo/bar/nest2#anchor' --download .
$TEST nest3 $ADOMAIN/'foo/bar/nest3?data' --download .
$TEST nest4 $ADOMAIN/'foo/bar/nest4?data#anchor' --download .

$TEST index.html $ADOMAIN --download ./
$TEST foobar $ADOMAIN/foobar --download ./
$TEST foo/bar/nest $ADOMAIN/foo/bar/nest --download ./
$TEST foo/bar/index.html $ADOMAIN/foo/bar/ --download ./
$TEST foo/bar/nest2 $ADOMAIN/'foo/bar/nest2#anchor' --download ./
$TEST foo/bar/nest3 $ADOMAIN/'foo/bar/nest3?data' --download ./
$TEST foo/bar/nest4 $ADOMAIN/'foo/bar/nest4?data#anchor' --download ./

$TEST index.html $ADOMAIN --download ./.
$TEST foobar $ADOMAIN/foobar --download ./.
$TEST nest $ADOMAIN/foo/bar/nest --download ./.
$TEST index.html $ADOMAIN/foo/bar/ --download ./.
$TEST nest2 $ADOMAIN/'foo/bar/nest2#anchor' --download ./.
$TEST nest3 $ADOMAIN/'foo/bar/nest3?data' --download ./.
$TEST nest4 $ADOMAIN/'foo/bar/nest4?data#anchor' --download ./.

$TEST abc $ADOMAIN --download abc
$TEST abc $ADOMAIN/foobar --download abc
$TEST abc $ADOMAIN/foo/bar/nest --download abc
$TEST abc $ADOMAIN/foo/bar/ --download abc

$TEST abc/index.html $ADOMAIN --download abc/
$TEST abc/foobar $ADOMAIN/foobar --download abc/
$TEST abc/foo/bar/nest $ADOMAIN/foo/bar/nest --download abc/
$TEST abc/foo/bar/index.html $ADOMAIN/foo/bar/ --download abc/

$TEST abc/index.html $ADOMAIN --download abc/.
$TEST abc/foobar $ADOMAIN/foobar --download abc/.
$TEST abc/nest $ADOMAIN/foo/bar/nest --download abc/.
$TEST abc/index.html $ADOMAIN/foo/bar/ --download abc/.

$TEST abc/def $ADOMAIN --download abc/def
$TEST abc/def $ADOMAIN/foobar --download abc/def
$TEST abc/def $ADOMAIN/foo/bar/nest --download abc/def
$TEST abc/def $ADOMAIN/foo/bar/ --download abc/def

$TEST abc/def/index.html         $ADOMAIN --download abc/def/
$TEST abc/def/foobar             $ADOMAIN/foobar --download abc/def/
$TEST abc/def/foo/bar/nest       $ADOMAIN/foo/bar/nest --download abc/def/
$TEST abc/def/foo/bar/index.html $ADOMAIN/foo/bar/ --download abc/def/

$TEST abc/def/index.html $ADOMAIN --download abc/def/.
$TEST abc/def/foobar     $ADOMAIN/foobar --download abc/def/.
$TEST abc/def/nest       $ADOMAIN/foo/bar/nest --download abc/def/.
$TEST abc/def/index.html $ADOMAIN/foo/bar/ --download abc/def/.

$TEST downloadTest.sh         $TEST --download .
$TEST $OWNDIR/downloadTest.sh $TEST --download ./

$TEST abcdef      '<test/>' --download abcdef
$TEST abcdef/ghi  '<test/>' --download abcdef/ghi

$TEST shouldFailTest $ADOMAIN/foo/bar/ --download abc/.


