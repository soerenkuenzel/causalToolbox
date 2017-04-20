#!/bin/bash
R CMD BATCH --no-save "--args setup_i=$1 nthread=$2 ntrain=$3 dim=$4" RF_autotune_pipeline.R RF_autotune_pipeline_$1_$2_$3_$4.out