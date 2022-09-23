#!/bin/bash
#SBATCH -o /dss/dsshome1/lxc02/ge79mok2/new1_rank_pnl/res/myjob_pnl.out
#SBATCH -D /dss/dsshome1/lxc02/ge79mok2/new1_rank_pnl/
#SBATCH -J jobpnl
#SBATCH --get-user-env
#SBATCH --clusters=cm2
#SBATCH --partition=cm2_large
#SBATCH --qos=cm2_large
#SBATCH --nodes=20
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=56
#SBATCH --mail-type=all
#SBATCH --mail-user=keropyan@in.tum.de
#SBATCH --export=NONE
#SBATCH --time=48:00:00


module load slurm_setup


module unload intel-mpi
module unload intel
module load gcc/11.2.0
module load r/4.0.5-gcc11-mkl
module load gsl/2.7-gcc11

cd src/

NUM_DATASETS=("100")
N=("100" "500" "1000" "1500" "2000")
D=("4" "7")
NOISE=("gaussian" "evd")
METHOD=("prlg")

# Run jobs
for d in "${D[@]}"; do
    for n in "${N[@]}"; do
        for noise in "${NOISE[@]}"; do
            for num_datasets in "${NUM_DATASETS[@]}"; do
                for method in "${METHOD[@]}"; do
                    while true; do
                        SUBJOBS=`jobs -r | wc -l` # detect how many subjobs are already running
                        if test $SUBJOBS -lt $SLURM_JOB_NUM_NODES; then
                            sleep 1
                            srun -N 1 -n $SLURM_NTASKS_PER_NODE -c $SLURM_CPUS_PER_TASK \
                            --export=ALL --exclusive Rscript ../run_pnl.R $n $d $num_datasets $noise "cube" $method &
                            break
                        fi
                    done
                done
            done
        done
    done
done

wait
 
exit


# Rscript ../run_pnl.R 10 3 7 gaussian cube prlg

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
