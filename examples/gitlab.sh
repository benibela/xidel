#!/bin/sh

#This example shows how to access the Bitbucket API with Xidel on Linux.
# 
#It creates an repository for the current user after reading the following parameters from stdin. 

read -r GITLAB_AUTH    #Personal Access Token. Obtained on the Gitlab access tokens config.
read -r name
read -r description


xidel --verbose --post "{serialize-json({'name': '$name', 'description': '$description', 'public': true()})}" -H "Content-Type: application/json" -H "PRIVATE-TOKEN: $GITLAB_AUTH" "https://gitlab.com/api/v3/projects" -e '$raw'