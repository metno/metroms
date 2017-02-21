#PBS -N norkyst800
#PBS -A mifa01hi
#PBS -l walltime=240:00:00
#PBS -l select=32:ncpus=32:mpiprocs=16:ompthreads=16:mem=29gb
#PBS -j oe
###PBS -V
set -x
datstamp=`date +%Y_%m_%d_%H_%M`
exec 1>/work/$USER/tmproms/run/norkyst-800m/run.log_${datstamp} 2>&1
ln -sf /work/$USER/tmproms/run/norkyst-800m/run.log_${datstamp} /work/$USER/tmproms/run/norkyst-800m/run.log
source ~/metroms/apps/myenv.bash
# Load modules needed
source /etc/profile.d/modules.sh
source ~/metroms/apps/modules.sh
#module load intelcomp netcdf mpt python
module list
#
cd ~/metroms/apps/norkyst-800m
export MPI_BUFS_PER_PROC=256
export MPI_BUFS_PER_HOST=1024
#export PYTHONPATH=$PYTHONPATH:~/metroms/apps/common/python/
python my_norkyst800m.py
set +x
