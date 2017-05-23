#!/bin/bash

minV=4
minY=2015

while read p; do

  output=`curl https://wordpress.org/plugins/$p/`
  e1=$(echo "$output" | grep "dateModified" | sed 's/.*"dateModified": "\([^-]*\).*/\1/')
  e2=$(echo "$output" | grep "Tested up to" | sed 's/.*<li>Tested up to: <strong>\([^.]*\).*/\1/')

  if [[ $e1 -ge $minY && $e2 -ge $minV ]]; then
    mkdir $p
    svn co http://plugins.svn.wordpress.org/$p/trunk $p
  else
  	echo "Skipping $p"
  fi
done<plugins.txt
