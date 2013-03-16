#!/bin/sh
../../xidel http://bazaar.launchpad.net/~zorba-coders/zorba/trunk/files/head:/test/rbkt/Queries/zorba/jsoniq -f '<img alt="Download File">{..}</img>*' --download .
../../xidel http://bazaar.launchpad.net/~zorba-coders/zorba/trunk/files/head:/test/rbkt/ExpQueryResults/zorba/jsoniq/ -f '<img alt="Download File">{..}</img>*' --download results/. 
 
