#!/bin/bash
#SBATCH --cpus-per-task 32
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASKR
R CMD BATCH --no-save --args -"$1 $2" RF_basic_eval.R RF_basic_eval_$1_$2.Rout
