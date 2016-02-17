#PBS -N arctic-4km
#PBS -A mifa01hi
#PBS -l walltime=1:00:00
#PBS -l select=6:ncpus=32:mpiprocs=8:ompthreads=8:mem=30gb
####PBS -l select=3:ncpus=32:mpiprocs=16:ompthreads=16:mem=30gb
#PBS -j oe
#PBS -V
datstamp=`date +%Y_%m_%d_%H_%M`
exec 1>/work/$USER/tmproms/run/arctic-4km/run.log_${datstamp} 2>&1
# Load modules needed
source /etc/profile.d/modules.sh
module load python
#
cd ~/metroms/apps/arctic-4km
export MPI_BUFS_PER_PROC=256
export MPI_BUFS_PER_HOST=1024
python arctic4km.py
