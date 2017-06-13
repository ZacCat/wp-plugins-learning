#! /usr/bin/python3.5

# Training method eg. Train by class, or Train by Plugin
trainMethod = 'Class'

# Desired WP Version
version = '4.3'

baseLoc = '/home/zac/corpus/training/Unsupervised'

# Generate the file location, given Features, fMatrix, Labels, lMatrix 
def genLoc( str ):
	return baseLoc + '/TrainBy' + trainMethod + "-" + str + '-' + version + '.txt'

# Read in either array of either:
# Feature names ( str = Features )
# Label names ( str = Labels )
def genList( str ):
	s = open(genLoc( str )).readline()
	s = s[1 : len(s) - 1].replace('\"', '')

	return s.split(',')

# Read in either:
# Feature Matrix ( str = fMatrix )
# Label Matrix ( str = lMatrix )
def genMatrix( str ):
	s = open(genLoc( str )).readline()
	s = s[2 : len(s) - 2].replace('"', '')
	lst = s.split('],[')

	m = []

	for item in lst:
		m.append(item.split(','))

	return m
