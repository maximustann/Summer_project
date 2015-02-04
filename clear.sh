#!/bin/bash 

cp $1'.csv' ./experiment_result/$1*/$2*/
cp $1'_time.csv' ./experiment_result/$1*/$2*/
rm [[:digit:]]*
