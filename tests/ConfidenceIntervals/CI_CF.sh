#!/bin/bash
#SBATCH --cpus-per-task 24
R CMD BATCH --no-save CI_CF.R CI_CF.Rout
