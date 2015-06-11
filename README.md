Xidel
=============
Xidel is a command line tool to download and extract data from HTML/XML pages.

There are [binaries](http://www.videlibri.de/xidel.html) for Windows, Linux and Mac. 

It is a wrapper around my Pascal [Internet Tools](../internettools), so it supports XPath, XQuery, JSONiq, CSS selectors and my own extensions/languages (e.g. pattern matching).

A simple example is:

     xidel http://example.org --follow //a --extract //title
     
or simpler

     xidel http://example.org -f //a -e //title

There are more examples on the above page with binaries.