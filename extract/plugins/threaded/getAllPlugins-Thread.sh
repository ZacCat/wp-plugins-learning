#!/bin/bash

minV=4
minY=2016

output=`curl https://wordpress.org/plugins/$1/`
e1=$(echo "$output" | grep "dateModified" | sed 's/.*"dateModified": "\([^-]*\).*/\1/')
e2=$(echo "$output" | grep "Tested up to" | sed 's/.*<li>Tested up to: <strong>\([^.]*\).*/\1/')

if [[ $e1 -ge $minY && $e2 -ge $minV ]]; then
	mkdir $1
	svn co http://plugins.svn.wordpress.org/$1/trunk $1
else
	echo "Skipping $1 Modified : $e1 Tested : $e2"
fi