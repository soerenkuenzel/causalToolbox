#!/bin/bash
#SBATCH --cpus-per-task 8
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
R CMD BATCH --no-save "--args $1" selector_evaluate_pipeline.R selector_evaluate_pipeline_$1.Rout

