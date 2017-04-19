for i in 1 2 3; do sbatch --cpus-per-task 8 -p high RF_MSE_script.sh $i 8; done
