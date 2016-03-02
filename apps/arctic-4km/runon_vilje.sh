#PBS -N arctic-4km
###PBS -A nn9239k
#PBS -A mifa01hi
#PBS -l walltime=24:00:00
#PBS -l select=28:ncpus=32:mpiprocs=8:ompthreads=8:mem=30gb
####PBS -l select=3:ncpus=32:mpiprocs=16:ompthreads=16:mem=30gb
#PBS -j oe
###PBS -V
datstamp=`date +%Y_%m_%d_%H_%M`
exec 1>/work/$USER/tmproms/run/arctic-4km/run.log_${datstamp} 2>&1
source ~/metroms/apps/myenv.bash
# Load modules needed
source /etc/profile.d/modules.sh
#source ~/metroms/apps/modules.sh
module load intelcomp netcdf mpt python
#
cd ~/metroms/apps/arctic-4km
export MPI_BUFS_PER_PROC=256
export MPI_BUFS_PER_HOST=1024
#export PYTHONPATH=$PYTHONPATH:~/metroms/apps/common/python/
python arctic4km.py
