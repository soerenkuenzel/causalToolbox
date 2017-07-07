for i in {1..3}; do
    for learner in {ranger,randomForest,hte_adaptive_nomsp,hte_adaptive_wmsp,hte_honest_wmsp};do
	sbatch -p high RF_basic_eval.sh $i $learner;
    done
done
