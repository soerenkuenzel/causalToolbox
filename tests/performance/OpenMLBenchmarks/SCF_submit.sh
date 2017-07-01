sbatch -p high RF_basic_eval.sh
while true; do squeue --user=srk; sleep 1; done
