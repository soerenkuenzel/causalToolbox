module load r
module load gcc/4.8.5


for i in {5..5}; do sbatch BRC_CATE_MSE_evaluate_pipeline.sh $i; done