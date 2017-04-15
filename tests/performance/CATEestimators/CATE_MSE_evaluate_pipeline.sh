#!/bin/bash
#SBATCH --cpus-per-task 8
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
R CMD BATCH --no-save --args -$1 CATE_MSE_evaluate_pipeline.R CATE_MSE_evaluate_pipeline_$1.out

