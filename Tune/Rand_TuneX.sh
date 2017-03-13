#!/bin/bash
#SBATCH --cpus-per-task 2
R CMD BATCH --no-save '--args seed=1412' Rand_TuneX.R Rand_TuneX.Rout

