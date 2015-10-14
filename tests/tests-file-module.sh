#!/bin/sh

dir=/tmp/xidel/file/test
mkdir -p tests/output/file


tests/test.sh file/init --xquery "(file:delete('$dir', true()), file:create-dir('$dir'), file:write-text('$dir/utf8', 'aäöü'), file:write-text('$dir/latin1', 'aäöü', 'latin1'))"
tests/test.sh file/exists --xquery "file:exists('$dir'), file:exists('$dir/utf8')"
tests/test.sh file/is-dir --xquery "file:is-dir('$dir'), file:is-dir('$dir/utf8')"
tests/test.sh file/is-file --xquery "file:is-file('$dir'), file:is-file('$dir/utf8')"
tests/test.sh file/last-modified --xquery "let \$delta := current-dateTime() - file:last-modified('$dir/utf8') return \$delta < xs:dayTimeDuration('PT5M')"
tests/test.sh file/size --xquery "file:size('$dir'), file:size('$dir/utf8')"
tests/test.sh file/append --xquery "( file:write-text('$dir/append', ''), file:append('$dir/append', (1,2,3) ), file:append('$dir/append', (4,5,6) ), 'reread:', file:read-text('$dir/append' ) )"
tests/test.sh file/append-binary --xquery "( file:write-text('$dir/append', ''), file:append-binary('$dir/append', base64Binary('IGZvbwo=') ), file:append-binary('$dir/append', base64Binary('IGJhcgo=') ), 'reread:', file:read-text('$dir/append' ) )"
tests/test.sh file/append-text --xquery "( file:write-text('$dir/append', ''), file:append-text('$dir/append', 'xyzÄ' ), file:append-text('$dir/append', 'abcÖ', 'latin1' ), 'reread:', file:read-binary('$dir/append' ) )"
tests/test.sh file/append-text-lines --xquery "( file:write-text('$dir/append', ''), file:append-text-lines('$dir/append', ('a','b','ä') ), file:append-text-lines('$dir/append', ('c','d','ä'), 'latin1' ), 'reread:', file:read-binary('$dir/append' ) )"
tests/test.sh file/copy --xquery "file:copy('$dir/utf8','$dir/utf8copied'), file:read-text('$dir/utf8copied'), file:copy('$dir', '${dir}copy'), file:read-text('${dir}copy/latin1')"
tests/test.sh file/create-temp-dir --xquery "let \$temp := file:create-temp-dir('pre', 'suff', '$dir') return (file:is-dir(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-temp-dir --xquery "let \$temp := file:create-temp-dir('pre', 'suff') return (file:is-dir(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-temp-file --xquery "let \$temp := file:create-temp-file('pre', 'suff', '$dir') return (file:is-dir(\$temp), file:is-file(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-temp-file --xquery "let \$temp := file:create-temp-file('pre', 'suff') return (file:is-dir(\$temp), file:is-file(\$temp), matches(\$temp, 'pre.*suff'), file:delete(\$temp), file:exists(\$temp)) "
tests/test.sh file/create-dir --xquery "file:create-dir('$dir/foo/bar'), file:write-text('$dir/foo/bar/uvw', 'hallo'), file:read-text('$dir/foo/bar/uvw')"  #this creates files for list and move
tests/test.sh file/list-self --xquery "for \$x in file:list('$dir') order by \$x return \$x, '---', file:list('$dir', false(), 'app*') " 
tests/test.sh file/list-recurse --xquery "for \$x in file:list('$dir', true()) order by \$x return \$x"
tests/test.sh file/children --xquery "for \$x in file:children('$dir') order by \$x return \$x"
tests/test.sh file/move --xquery "file:move('$dir/append', '$dir/appendmoved'), file:exists('$dir/append'), file:exists('$dir/appendmoved'),'--',file:exists('$dir/foo/bar/uvw'), file:move('$dir/foo', '$dir/foo2'), file:exists('$dir/foo/bar/uvw'), file:exists('$dir/foo2/bar/uvw'), file:read-text('$dir/foo2/bar/uvw')"
tests/test.sh file/delete --xquery "(file:create-dir('$dir/foo2/bar'), file:write-text('$dir/foo2/bar/uvw', 'hallo'), file:read-text('$dir/foo2/bar/uvw'), file:delete('$dir/foo2', true()), file:exists('$dir/foo2/bar/uvw'))" 
tests/test.sh file/read-binary --xquery "file:read-binary('$dir/utf8'), file:read-binary('$dir/utf8',0,7), file:read-binary('$dir/utf8',1,6), try { file:read-binary('$dir/utf8',1,7) } catch file:out-of-range { 'out-of-range' }, file:read-binary('$dir/utf8',5,2)" 
tests/test.sh file/read-text --xquery "file:read-text('$dir/utf8'), file:read-text('$dir/latin1'), file:read-text('$dir/latin1', 'latin1')"
tests/test.sh file/read-text-lines --xquery "file:read-text-lines('$dir/appendmoved'), count(file:read-text-lines('$dir/appendmoved'))"
tests/test.sh file/write --xquery "file:write('$dir/write', (1,2,3)), file:read-text-lines('$dir/write'), file:write('$dir/write', (7,8,9), ()), file:read-text-lines('$dir/write')"
tests/test.sh file/write-binary1 --xquery "file:write-binary('$dir/utf8', xs:base64Binary('b3ZlcnJpZGU=')), file:read-text('$dir/utf8'), file:write-binary('$dir/utf8', xs:base64Binary('WA==')), file:read-text('$dir/utf8')"
tests/test.sh file/write-binary2 --xquery "file:write-binary('$dir/utf8', xs:base64Binary('b3ZlcnJpZGU='), 0), file:read-text('$dir/utf8'), file:write-binary('$dir/utf8', xs:base64Binary('WA=='), 0), file:read-text('$dir/utf8'), file:write-binary('$dir/utf8', xs:base64Binary('Zm9vYmFy'), 5), file:read-text('$dir/utf8')"
#write-text tested in init
tests/test.sh file/write-text-lines --xquery  "file:write-text-lines('$dir/utf8', ('abc', 'äöü') ), file:read-text('$dir/utf8'), file:write-text-lines('$dir/latin1', ('abc', 'äöü'), 'latin1' ), file:read-text('$dir/latin1')"



tests/test.sh file/name --xquery  "file:name('$dir/utf8')"
tests/test.sh file/parent --xquery  "file:parent('$dir/utf8')"
#children tested above
#tests/test.sh file/parent1 --xquery  "file:parent('$dir')"
tests/test.sh file/path-to-native --xquery "('$dir', ('$dir', '$dir/////xyt', '$dir/./.././../foo') ! file:path-to-native(.))"
tests/test.sh file/path-to-uri -e "('$dir', ('$dir', '$dir/////xyt', '$dir/./.././../foo') ! file:path-to-uri(.))"
tests/test.sh file/separators --xquery 'file:dir-separator(), file:line-separator(), file:path-separator()'
export TEMP=/tmp
tests/test.sh file/temp-dir --xquery  "file:temp-dir()"
echo The next test will fail unless $(pwd) is /home/benito/hg/programs/internet/xidel
tests/test.sh file/current-dir-tests --xquery 'file:base-dir(), file:current-dir(), file:resolve-path("abc/xyz")'

