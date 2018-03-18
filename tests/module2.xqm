module namespace foobar2 = "pseudo://test-module2";
import module namespace foo =  "pseudo://test-module" at "module.xq";

declare variable $foobar2:def := $foo:abc * 1000 ;
declare function foobar2:setglobal (){
  $newglobal := "GLOABL"
};