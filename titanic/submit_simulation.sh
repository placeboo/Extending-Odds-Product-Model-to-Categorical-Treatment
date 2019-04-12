#!/bin/sh
#
#
cd ~/Linbo/odds/titanic 
for seed in $(seq 5)
do
	qsub -cwd ./codes/call_mono.sh $seed
done

