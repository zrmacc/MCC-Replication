#!/bin/bash
fin=Configs/VarConfig.txt

sed 1d ${fin} | while read line
do
	# Read parameter configuration.
	n=$(echo ${line} | awk "{print \$1}")
	t=$(echo ${line} | awk "{print \$2}")
	c=$(echo ${line} | awk "{print \$3}")
	d=$(echo ${line} | awk "{print \$4}")

	# Run simulation.
	Rscript Rscripts/BootSim.R --n ${n} --time ${t} --censor ${c} --death ${d} --reps 2 --boot 2 --out "Simulations/Boot/";
done