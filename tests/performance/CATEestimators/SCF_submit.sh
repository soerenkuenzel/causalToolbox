for i in {1..11}; do
    echo $i;
    sbatch -p high SCF_CATE_MSE_evaluate_pipeline.sh $i;
done
