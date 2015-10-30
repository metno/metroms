#PBS -N arctic-20km
#PBS -A mifa01hi
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=32:mpiprocs=16:ompthreads=16:mem=29gb
#PBS -o /work/$USER/tmproms/run/arctic-20km/prep_o.log
#PBS -j oe
#PBS -V

module load python
cd ~/metroms/apps/arctic-20km
python arctic20km_local.py
