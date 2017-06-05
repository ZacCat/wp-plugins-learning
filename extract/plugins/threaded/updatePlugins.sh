#!/bin/bash

curl http://plugins.svn.wordpress.org | grep '  <li><a href=' | sed 's;  <li><a href=".*">\([^<]*\)<.*;\1;' | sed 's;/;;' > plugins.txt