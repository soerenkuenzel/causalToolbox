for i in {1..20}; do
    echo $i;
    sbatch -p high selector_evaluate_pipeline.sh $i;
done
