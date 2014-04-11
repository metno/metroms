      PROGRAM mct_roms

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

      integer :: color, COMM, error, key, nodes, rank
      integer :: ng

      real(r4) :: CouplingTime             ! single precision
!
!-----------------------------------------------------------------------
!  Initialize distributed-memory (MPI) configuration
!-----------------------------------------------------------------------
!
!  Initialize MPI execution environment.
!
      CALL mpi_init(error)
!
!  Get rank of the local process in the group associated with the
!  comminicator.
!
      CALL mpi_comm_size(MPI_COMM_WORLD, nodes, error)
      CALL mpi_comm_rank(MPI_COMM_WORLD, rank, error)
!
!  Set temporarily the ocean communicator to current handle before
!  splitting so the input coupling script name can be broadcasted to
!  all the nodes.
!
      OCN_COMM_WORLD=MPI_COMM_WORLD
!
!  Read in coupled model parameters from standard input.
!
      CALL read_CouplePar(iNLM)
!
!  Allocate coupling variables.
!
      CALL allocate_coupler(nodes)
!
!  Split the communicator into coupled models sub-groups based
!  on color and key.
!
      key=0
      IF ((pets(Iocean)%val(1) .le. rank) .and. (rank .le. pets(Iocean)%val(Nthreads(Iocean)))) THEN
        color = OCNid
      END IF
      IF ((pets(Icice)%val(1) .le. rank) .and. (rank .le. pets(Iice)%val(Nthreads(Iice)))) THEN
        MyColor = CICEid
      END IF

      CALL mpi_comm_split (MPI_COMM_WORLD, color, key, COMM, error)
!
!-----------------------------------------------------------------------
!  Run coupled models according to the processor rank.
!-----------------------------------------------------------------------
!
      IF (MyColor .eq. CICEid) THEN
        CouplingTime=REAL(TimeInterval(Iocean,Icice))
        CALL module_cice_init(COMM)
        CALL module_cice_run(CouplingTime)
        CALL module_cice_wrf_finalize()
      END IF

      IF (MyColor.eq.OCNid) THEN
        first=.TRUE.
        IF (exit_flag.eq.NoError) THEN
          CALL ROMS_initialize(first, mpiCOMM=MyCOMM)
          run_time=0.0_r8
          DO ng=1,Ngrids
            run_time=MAX(run_time, dt(ng)*ntimes(ng))
          END DO
        END IF
        IF (exit_flag.eq.NoError) THEN
          CALL ROMS_run(run_time)
        END IF
        CALL ROMS_finalize()
        CALL finalize_ocn2cice_coupling()
      END IF

      CALL mpi_barrier(MPI_COMM_WORLD)
      CALL MCTWorld_clean()
      CALL mpi_finalize(error)

      STOP

      END PROGRAM mct_roms
