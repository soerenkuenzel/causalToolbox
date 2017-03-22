#!/bin/bash
#SBATCH --cpus-per-task 24
R CMD BATCH --no-save CI_X_hRF.R CI_X_hRF.Rout
