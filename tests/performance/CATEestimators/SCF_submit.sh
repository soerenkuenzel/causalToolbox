for i in {1..2}; do
    echo $i;
    sbatch -p high CATE_MSE_newCF_evaluate_pipeline.sh $i;
done
