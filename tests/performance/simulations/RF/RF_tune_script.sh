#!/bin/bash
R CMD BATCH --no-save "--args setup_i=$1 nthread=$2 ntrain=$3 dim=$4" RF_tune_pipeline.R RF_tune_pipeline_$1_$2_$3_$4.out
