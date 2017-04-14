#!/bin/bash
#SBATCH --cpus-per-task 2
R CMD BATCH --no-save --args -$1 CATE_MSE_evaluate_pipeline.R CATE_MSE_evaluate_pipeline_$1.out

