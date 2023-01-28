(: declare default element namespace "http://www.w3.org/2001/XMLSchema"; :)
xquery version "3.1";

declare function local:row($table as element()){
  $table / tbody / tr 
};
declare function local:td($tr as element()){
  $tr / td[1] / text()
};
declare function local:td2($tr as element()){
  $tr /(if ( count(*) > 1 ) then td[2] / text() else "")
};
declare function local:comment($c as xs:string){
  parse-xml(concat("<!-- ", $c, " -->"))
};

parse-xml('<html><body>
<table id="t1"><tbody>
<tr><td>Hello</td></tr>
</tbody></table>
<table id="t2"><tbody>
<tr><td>123</td></tr>
<tr><td>456</td><td>other</td></tr>
<tr><td>foo</td><td>columns</td></tr>
<tr><td>bar</td><td>are</td></tr>
<tr><td>xyz</td><td>ignored</td><td>zomg</td></tr>
</tbody></table>
</body></html>')/<table>
<tbody>{for $row in local:row(id('t2')), $td in local:td($row), $comment in local:comment(local:td2($row)) return ('
',<tr class="{$row/*/concat('C_',text())} {$row/(for $i in 1 to count($row/*) return map:get(map{'1': 'hascol1', '2': 'hascomment', '3': 'zomg'}, xs:string($i)))}" id="{$td}">
  {<td>{if (count($row/*) > 2) then attribute name{$row/td[3]/text()} else ()}{$td}</td>}{$comment}
</tr>
)}
</tbody>
</table>
