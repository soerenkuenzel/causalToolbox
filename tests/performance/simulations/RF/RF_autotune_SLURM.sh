for i in 1 2 3; do for j in 1000 3162 10000 31622; do for k in 10 20; do sbatch --cpus-per-task 8 RF_autotune_script.sh $i 8 $j $k; done; done; done
