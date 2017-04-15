for i in {1..3}; do sbatch -p high RF_MSE_script.sh $i 8; done
