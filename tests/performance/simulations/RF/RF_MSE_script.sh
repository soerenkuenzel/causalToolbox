#!/bin/bash
R CMD BATCH --no-save "--args setup_i=$1 nthread=$2" RF_MSE_pipeline.R RF_MSE_pipeline_$1.out
