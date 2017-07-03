#!/bin/bash
#SBATCH --cpus-per-task 24
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
R CMD BATCH --no-save summarize_datasets.R summarize_datasets.Rout

