#!/bin/bash

I=0

while read p; do

	I=$(($I+1))
	if [ $(($I%10)) -eq 0 ]; then 
		urxvt -e ./getAllPlugins-Thread.sh $p
	else
		urxvt -e ./getAllPlugins-Thread.sh $p &
	fi

done<plugins.txt
