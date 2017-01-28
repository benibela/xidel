#!/bin/sh

#This example shows how to access the GitHub API with Xidel on Linux.
# 
#It creates an repository for the current user after reading the following parameters from stdin. 

read -r GITHUB_AUTH   # Personal access token. Can be obtained on the GitHub account config
read -r name
read -r description

useragent=test #GitHub recommends putting your (app) name here


xidel --user-agent "$useragent" --verbose  --post "{serialize-json({'name': '$name', 'description': '$description'})}" -H  "Authorization: token $GITHUB_AUTH" -H "Accept: application/vnd.github.v3+json" "https://api.github.com/user/repos" -e . --error-handling=4xx=accept