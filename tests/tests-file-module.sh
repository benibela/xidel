#!/bin/sh

dir=/tmp/xidel/file/test
mkdir -p tests/output/file
ARGS=--strict-type-checking

tests/test.sh file/init $ARGS --xquery "(if (file:exists('$dir')) then file:delete('$dir', true()) else (), file:create-dir('$dir'), file:write-text('$dir/utf8', 'aäöü'), file:write-text('$dir/latin1', 'aäöü', 'latin1'))"
tests/test.sh file/exists $ARGS --xquery "file:exists('$dir'), file:exists('$dir/utf8')"
tests/test.sh file/is-dir $ARGS --xquery "file:is-dir('$dir'), file:is-dir('$dir/utf8')"
tests/test.sh file/is-file $ARGS --xquery "file:is-file('$dir'), file:is-file('$dir/utf8')"
tests/test.sh file/last-modified $ARGS --xquery "let \$delta := current-dateTime() - file:last-modified('$dir/utf8') return \$delta < xs:dayTimeDuration('PT5M')"
tests/test.sh file/size $ARGS --xquery "file:size('$dir'), file:size('$dir/utf8')"
tests/test.sh file/append $ARGS --xquery "( file:write-text('$dir/append', ''), file:append('$dir/append', (1,2,3) ), file:append('$dir/append', (4,5,6) ), 'reread:', file:read-text('$dir/append' ) )"
tests/test.sh file/append-binary $ARGS --xquery "( file:write-text('$dir/append', ''), file:append-binary('$dir/append', base64Binary('IGZvbwo=') ), file:append-binary('$dir/append', base64Binary('IGJhcgo=') ), 'reread:', file:read-text('$dir/append' ) )"
tests/test.sh file/append-text $ARGS --xquery "( file:write-text('$dir/append', ''), file:append-text('$dir/append', 'xyzÄ' ), file:append-text('$dir/append', 'abcÖ', 'latin1' ), 'reread:', file:read-binary('$dir/append' ) )"
tests/test.sh file/append-text-lines $ARGS --xquery "( file:write-text('$dir/append', ''), file:append-text-lines('$dir/append', ('a','b','ä') ), file:append-text-lines('$dir/append', ('c','d','ä'), 'latin1' ), 'reread:', file:read-binary('$dir/append' ) )"
tests/test.sh file/copy $ARGS --xquery "file:copy('$dir/utf8','$dir/utf8copied'), file:read-text('$dir/utf8copied'), file:copy('$dir', '${dir}copy'), file:read-text('${dir}copy/latin1')"
tests/test.sh file/create-temp-dir $ARGS --xquery "let \$temp := file:create-temp-dir('pre', 'suff', '$dir') return (file:is-dir(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-temp-dir $ARGS --xquery "let \$temp := file:create-temp-dir('pre', 'suff') return (file:is-dir(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-temp-file $ARGS --xquery "let \$temp := file:create-temp-file('pre', 'suff', '$dir') return (file:is-dir(\$temp), file:is-file(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-temp-file $ARGS --xquery "let \$temp := file:create-temp-file('pre', 'suff') return (file:is-dir(\$temp), file:is-file(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-dir $ARGS --xquery "file:create-dir('$dir/foo/bar'), file:write-text('$dir/foo/bar/uvw', 'hallo'), file:read-text('$dir/foo/bar/uvw')"  #this creates files for list and move
tests/test.sh file/list-self $ARGS --xquery "for \$x in file:list('$dir') order by \$x return \$x, '---', file:list('$dir', false(), 'app*') " 
tests/test.sh file/list-recurse $ARGS --xquery "for \$x in file:list('$dir', true()) order by \$x return \$x"
tests/test.sh file/children $ARGS --xquery "for \$x in file:children('$dir') order by \$x return \$x"
tests/test.sh file/move $ARGS --xquery "file:move('$dir/append', '$dir/appendmoved'), file:exists('$dir/append'), file:exists('$dir/appendmoved'),'--',file:exists('$dir/foo/bar/uvw'), file:move('$dir/foo', '$dir/foo2'), file:exists('$dir/foo/bar/uvw'), file:exists('$dir/foo2/bar/uvw'), file:read-text('$dir/foo2/bar/uvw')"
tests/test.sh file/delete $ARGS --xquery "(file:create-dir('$dir/foo2/bar'), file:write-text('$dir/foo2/bar/uvw', 'hallo'), file:read-text('$dir/foo2/bar/uvw'), file:delete('$dir/foo2', true()), file:exists('$dir/foo2/bar/uvw'))" 
tests/test.sh file/read-binary $ARGS --xquery "file:read-binary('$dir/utf8'), file:read-binary('$dir/utf8',0,7), file:read-binary('$dir/utf8',1,6), try { file:read-binary('$dir/utf8',1,7) } catch file:out-of-range { 'out-of-range' }, file:read-binary('$dir/utf8',5,2)" 
tests/test.sh file/read-text $ARGS --xquery "file:read-text('$dir/utf8'), file:read-text('$dir/latin1'), file:read-text('$dir/latin1', 'latin1')"
tests/test.sh file/read-text-lines $ARGS --xquery "file:read-text-lines('$dir/appendmoved'), count(file:read-text-lines('$dir/appendmoved'))"
tests/test.sh file/write $ARGS --xquery "file:write('$dir/write', (1,2,3)), file:read-text-lines('$dir/write'), file:write('$dir/write', (7,8,9), <output:serialization-parameters xmlns:output='http://www.w3.org/2010/xslt-xquery-serialization'/>), file:read-text-lines('$dir/write')"
tests/test.sh file/write-binary1 $ARGS --xquery "file:write-binary('$dir/utf8', xs:base64Binary('b3ZlcnJpZGU=')), file:read-text('$dir/utf8'), file:write-binary('$dir/utf8', xs:base64Binary('WA==')), file:read-text('$dir/utf8')"
tests/test.sh file/write-binary2 $ARGS --xquery "file:write-binary('$dir/utf8', xs:base64Binary('b3ZlcnJpZGU='), 0), file:read-text('$dir/utf8'), file:write-binary('$dir/utf8', xs:base64Binary('WA=='), 0), file:read-text('$dir/utf8'), file:write-binary('$dir/utf8', xs:base64Binary('Zm9vYmFy'), 5), file:read-text('$dir/utf8')"
#write-text tested in init
tests/test.sh file/write-text-lines $ARGS --xquery  "file:write-text-lines('$dir/utf8', ('abc', 'äöü') ), file:read-text('$dir/utf8'), file:write-text-lines('$dir/latin1', ('abc', 'äöü'), 'latin1' ), file:read-text('$dir/latin1')"



tests/test.sh file/name $ARGS --xquery  "file:name('$dir/utf8')"
tests/test.sh file/parent $ARGS --xquery  "file:parent('$dir/utf8')"
#children tested above
#tests/test.sh file/parent1 $ARGS --xquery  "file:parent('$dir')"
tests/test.sh file/path-to-native $ARGS  --xquery "('$dir', ('$dir', '$dir/././latin1', '$dir/../test/../test/utf8' ) ! file:path-to-native(.))"
tests/test.sh file/path-to-uri -e "('$dir', ('$dir', '$dir/////xyt', '$dir/./.././../foo') ! file:path-to-uri(.))"
tests/test.sh file/separators $ARGS --xquery 'file:dir-separator(), file:line-separator(), file:path-separator()'
export TEMP=/tmp
tests/test.sh file/temp-dir $ARGS --xquery  "file:temp-dir()"
echo The next test will fail unless $(pwd) is /home/benito/hg/programs/internet/xidel
tests/test.sh file/current-dir-tests $ARGS --xquery 'file:base-dir(), file:current-dir(), file:resolve-path("abc/xyz")'

