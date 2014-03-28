      PROGRAM mct_roms
!=======================================================================
!                                                                      !
!  Master program to couple ROMS/TOMS to other models using the Model  !
!  Coupling Toolkit (MCT) library.                                     !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_iounits
      USE mod_scalars

      USE m_MCTWorld, ONLY : MCTWorld_clean => clean

      USE ocean_control_mod, ONLY : ROMS_initialize
      USE ocean_control_mod, ONLY : ROMS_run
      USE ocean_control_mod, ONLY : ROMS_finalize
!
      implicit none
!
!  Local variable declarations.
!
      logical, save :: first

      integer :: MyColor, MyCOMM, MyError, MyKey, Nnodes
      integer :: ng

      real(r4) :: CouplingTime             ! single precision
!
!-----------------------------------------------------------------------
!  Initialize distributed-memory (MPI) configuration
!-----------------------------------------------------------------------
!
!  Initialize MPI execution environment.
!
      CALL mpi_init (MyError)
!
!  Get rank of the local process in the group associated with the
!  comminicator.
!
      CALL mpi_comm_size (MPI_COMM_WORLD, Nnodes, MyError)
      CALL mpi_comm_rank (MPI_COMM_WORLD, MyRank, MyError)
!
!  Set temporarily the ocean communicator to current handle before
!  splitting so the input coupling script name can be broadcasted to
!  all the nodes.
!
      OCN_COMM_WORLD=MPI_COMM_WORLD
!
!  Read in coupled model parameters from standard input.
!
      CALL read_CouplePar (iNLM)
!
!  Allocate several coupling variables.
!
      CALL allocate_coupler (Nnodes)
!
!  Split the communicator into coupled models sub-groups based
!  on color and key.
!
      MyKey=0
      IF ((pets(Iocean)%val(1).le.MyRank).and.                          &
     &    (MyRank.le.pets(Iocean)%val(Nthreads(Iocean)))) THEN
        MyColor=OCNid
      END IF
!ifdef AIR_OCEAN
!      IF ((pets(Iatmos)%val(1).le.MyRank).and.                          &
!     &    (MyRank.le.pets(Iatmos)%val(Nthreads(Iatmos)))) THEN
!        MyColor=ATMid
!      END IF
!endif

      CALL mpi_comm_split (MPI_COMM_WORLD, MyColor, MyKey, MyCOMM,      &
     &                     MyError)
!
!-----------------------------------------------------------------------
!  Run coupled models according to the processor rank.
!-----------------------------------------------------------------------
!
!#ifdef WRF_COUPLING
!      IF (MyColor.eq.ATMid) THEN
!        CouplingTime=REAL(TimeInterval(Iocean,Iatmos))
!!      CALL module_wrf_top_mp_wrf_init (MyCOMM)
!!      CALL module_wrf_top_mp_wrf_run (TimeInterval(Iocean,Iwaves))
!!      CALL module_wrf_top_mp_wrf_finalize
!        CALL module_wrf_top_wrf_init (MyCOMM)
!        CALL module_wrf_top_wrf_run (CouplingTime)
!        CALL module_wrf_top_wrf_finalize
!      END IF
!#endif
      IF (MyColor.eq.OCNid) THEN
        first=.TRUE.
        IF (exit_flag.eq.NoError) THEN
          CALL ROMS_initialize (first, mpiCOMM=MyCOMM)
          run_time=0.0_r8
          DO ng=1,Ngrids
            run_time=MAX(run_time, dt(ng)*ntimes(ng))
          END DO
        END IF
        IF (exit_flag.eq.NoError) THEN
          CALL ROMS_run (run_time)
        END IF
        CALL ROMS_finalize
!#ifdef WRF_COUPLING
!        CALL finalize_ocn2atm_coupling
!#endif
      END IF
!
!-----------------------------------------------------------------------
!  Terminates all the mpi-processing and coupling.
!-----------------------------------------------------------------------
!
      CALL mpi_barrier (MPI_COMM_WORLD)
      CALL MCTWorld_clean ()
      CALL mpi_finalize (MyError)

      STOP

      END PROGRAM mct_roms
