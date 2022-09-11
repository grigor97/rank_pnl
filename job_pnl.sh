#!/bin/bash
#SBATCH -o /dss/dsshome1/lxc02/ge79mok2/new1_rank_pnl/res/myjob_pnl1.out
#SBATCH -D /dss/dsshome1/lxc02/ge79mok2/new1_rank_pnl/
#SBATCH -J job_pnl1
#SBATCH --get-user-env
#SBATCH --clusters=cm2_tiny
#SBATCH --partition=cm2_tiny
#SBATCH --nodes=1
#SBATCH --mail-type=end
#SBATCH --mail-user=keropyan@in.tum.de
#SBATCH --export=NONE
#SBATCH --time=00:02:00


module load slurm_setup


module unload intel-mpi
module unload intel
module load gcc/11.2.0
module load r/4.0.5-gcc11-mkl
module load gsl/2.7-gcc11

cd src/

# SAMPLE_SIZE=("10" "20")
# NUM_DATASETS=("20", "30")

# run jobs
# for n in "${SAMPLE_SIZE[@]}"; do
#	for num_d in "${NUM_DATASETS[@]}"; do
#		while true; do
#			SUBJOBS= `jobs -r | wc -l` # detect how manv subiobs are alreadv runnina
#			if test $SUBJOBS -It $SLURM_JOB_NUM_NODES; then
#				sleep 1
#				srun -N 1 -n $SLURM_NTASKS_PER_NODE -c $SLURM_CPUS_PER_TASK Rscript ../run_ltm.R n dum_d gaussian cube smoothed
#			fi
#		done
#	done
#done


Rscript ../run_pnl.R 10 3 7 gaussian cube prlg

# query status for my job, approx. time for pending job, past jobs
# squeue -M cm2 -u ge79mok2
# squeue -M cm2 -u ge79mok2 --start
# sacct -X -M cm2 -u ge79mok2

# abort a job
# scancel -M mpp2 <jobid>

# see the busy cluster segments
# sinfo

# see available clusters
# sacctmgr list clusters

# slow cluster (serial, serial_std)
