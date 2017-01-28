#!/bin/sh

#This example shows how to access the Bitbucket API with Xidel on Linux.
# 
#It creates an repository for an user after reading the following parameters from stdin. 

read -r USER  # Username
read -r BITBUCKET_AUTH_PASS    #App password. Obtained on the Bitbucket access management config.
read -r name
read -r description


xidel --method PUT --verbose --post "{serialize-json({'description': '$description', 'scm': 'hg', 'has_issues': true()})}" -H "Content-Type: application/json" "https://$USER:$BITBUCKET_AUTH_PASS@api.bitbucket.org/2.0/repositories/$USER/$name" -e '.'