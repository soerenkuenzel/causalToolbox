#!/bin/bash
R CMD BATCH --no-save "--args setup_i=$1 nthread=$2" RF_honesty_pipeline.R RF_honesty_pipeline_$1.out
