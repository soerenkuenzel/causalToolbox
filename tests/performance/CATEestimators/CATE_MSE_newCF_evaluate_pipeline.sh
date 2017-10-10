#!/bin/bash
#SBATCH --cpus-per-task 12
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
R CMD BATCH --no-save --args -$1 CATE_MSE_newCF_evaluate_pipeline.R CATE_MSE_newCF_evaluate_setup_$1.Rout


