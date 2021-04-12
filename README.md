Xidel [![Build Status](https://api.travis-ci.com/benibela/xidel.svg?branch=master)](https://travis-ci.com/benibela/xidel)
=============
Xidel is a command line tool to download and extract data from HTML/XML pages using CSS selectors, XPath/XQuery 3.0, as well as querying JSON files or APIs (e.g. REST) using JSONiq. 

There are dependency-free [binaries for Windows, Linux and Mac](http://www.videlibri.de/xidel.html). 

It is a wrapper around my Pascal Internet Tools (see repository internettools), so it supports XPath 2.0, XPath 3.0, XQuery 1.0, XQuery 3.0, JSONiq, CSS selectors and my own extensions/languages (e.g. pattern matching) and if you can compile that project, you can compile Xidel. 

A simple example to return the titles of all pages linked by some starting page:

     xidel http://example.org --follow //a --extract //title
     
or simpler

     xidel http://example.org -f //a -e //title
     

The language can be explicitly chosen. For example

     xidel input.html --css 'a'
     xidel input.html --xpath '//a/@href'
     xidel input.html --xquery 'for $var in //a order by $var return $var'

returns all links, the target URI of each link or the text of all links alphabetically.

There are more examples on the above page with binaries, the github wiki and in the directory examples.


Screenshots
-----------

<p align="center">

<img src="http://www.videlibri.de/img/xidel-linux.png" alt="Xidel on Linux">

<img src="http://www.videlibri.de/img/xidel-windows-blue.png" alt="Xidel on Windows">

</p>

Compilation and Installation
-----------

You can compile it by calling `build.sh` and install it by calling `build.sh -t`. Alternatively you can compile it with the Lazarus IDE.

You can call the commands from the [.travis.yml](.travis.yml) script to download dependencies.



