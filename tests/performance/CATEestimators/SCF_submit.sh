for i in {3..11}; do
    echo $i;
    sbatch -p low SCF_CATE_MSE_evaluate_pipeline $i;
done
