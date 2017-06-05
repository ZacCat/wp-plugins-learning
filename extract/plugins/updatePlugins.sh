#!/bin/bash

svn ls http://plugins.svn.wordpress.org | sed 's/\/$//' >> plugins.txt