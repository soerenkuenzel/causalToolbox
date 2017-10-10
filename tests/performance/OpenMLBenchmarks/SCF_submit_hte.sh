for i in {1..2}; do
    for learner in {hte_adaptive_nomsp,hte_adaptive_wmsp,hte_honest_wmsp};do
	sbatch RF_basic_eval.sh $i $learner;
    done
done
