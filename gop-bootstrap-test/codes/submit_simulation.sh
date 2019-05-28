#!/bin/sh
#
#
cd ~/Linbo/odds/gop-bootstrap-test
for seed in $(seq 50)
do
for sample_size in 500 1000
do
qsub -cwd ./codes/call_simulation.sh $sample_size $seed
done
done
