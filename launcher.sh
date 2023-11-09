#!/bin/bash

# this file is just in case for launching from terminal
# at first just put the files into the files dir

# NOT NESCESSARY, DELETE LATER

usage() {
cat << EOF
Usage: compute_metrics [-h] HEALTHY_DIR PAT_DIR OUTPUT_DIR [-r] [-j]
Process MRI in DATA_DIR using DL+DiReCT output in DL_DIR and put results i.
Compare healthy controls from HEALTHY_DIR with patients in PAT_DIR and put results into OUTPUT_DIR

optional arguments:
	-h|--help		show this usage
	-r|--read_files		read the single-patient files and create dataframe
	-j|--jokes		version with build in jokes
	
EOF
	exit 0
}

invalid() {
	echo "ERROR: Invalid argument $1"
	usage 1
}

die() {
	echo "ERROR: $1"
	exit 1
}

# generate plots
#Rscript --vanilla ${SCRIPT_DIR}/create_ggseg.R ${DST} `cat ${DST}/create_ggseg.txt`
# for now it is enough to give the input and ouptput dir
Rscript --vanilla ${SCRIPT_DIR}/create_ggseg.R ${INPUT} ${OUTPUT}  `cat ${OUTPUT}/create_ggseg.txt`
