#PBS -N arctic4
###PBS -A nn9239k
#PBS -A mifa01hi
#PBS -l walltime=240:00:00
#PBS -l select=18:ncpus=32:mpiprocs=12:ompthreads=12:mem=29gb
####PBS -l select=5:ncpus=32:mpiprocs=12:ompthreads=12:mem=30gb
#PBS -j oe
###PBS -V
datstamp=`date +%Y_%m_%d_%H_%M`
exec 1>/work/$USER/tmproms/run/arctic-4km/run.log_${datstamp} 2>&1
source ~/metroms/apps/myenv.bash
# Load modules needed
source /etc/profile.d/modules.sh
source ~/metroms/apps/modules.sh
#module load intelcomp netcdf mpt python
#
cd ~/metroms/apps/arctic-4km
export MPI_BUFS_PER_PROC=256
export MPI_BUFS_PER_HOST=1024
#export PYTHONPATH=$PYTHONPATH:~/metroms/apps/common/python/
python my_arctic4km.py
