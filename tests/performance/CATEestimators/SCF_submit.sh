for i in {3..11}; do
    echo $i;
    sbatch -p low CATE_MSE_newCF_evaluate_pipeline.sh $i;
done
