#!/bin/bash

if [ -d "wordpress" ]
then
	cd wordpress
else
	mkdir wordpress
	cd wordpress
fi

while read p; do

	wget https://wordpress.org/wordpress-$p.zip
	unzip wordpress-$p.zip -d wordpress-$p
	rm wordpress-$p.zip
	mv wordpress-$p/wordpress/* wordpress-$p/
	rmdir wordpress-$p/wordpress

done < wpVersions.txt
