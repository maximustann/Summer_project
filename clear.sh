#!/bin/bash 

if [[ -z $3 ]]; then
	if [[ -z $1 ]] || [[ -z $2 ]]; then
		rm [[:digit:]]*
	else
		cp $1'.csv' ./experiment_result/$1*/$2*/
		cp $1'_time.csv' ./experiment_result/$1*/$2*/
		rm [[:digit:]]*
	fi
else
	cp $1'_initialisation.csv' ./experiment_result/$1*/$2*/
	cp $1'_initialisation_time.csv' ./experiment_result/$1*/$2*/
	rm [[:digit:]]*
fi


