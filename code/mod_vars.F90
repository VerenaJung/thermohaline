!!---------------------------------------------------------------------------
!!
!!       MODULE mod_vars:
!!
!!          Collection of different modules that define variables
!!
!!---------------------------------------------------------------------------

! Grid variables
MODULE mod_grid

  IMPLICIT NONE

  ! Regular size
  INTEGER                                   :: imt   ! Number of zonal grid points
  INTEGER                                   :: jmt   ! Number of meridional grid points
  INTEGER                                   :: kmt   ! Number of vertical grid points

  ! dx/dy
  REAL(4), DIMENSION(:,:), ALLOCATABLE    :: dxt, dxv
  REAL(4), DIMENSION(:,:), ALLOCATABLE    :: dyt, dyu

  ! Gridcell area
  REAL(4), DIMENSION(:,:), ALLOCATABLE    :: dxdy

  ! Gridcell mass
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: masscell

  ! Vertical thickness - dzt/dzu/dzv
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: dzt     ! T point
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: dzu     ! U point
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: dzv     ! V point

  ! Height scale
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: zstot
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: zstou
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: zstov

  ! Bathimetry
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: kzt

  ! Land/Sea mask
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: mask
  INTEGER, DIMENSION(:,:), ALLOCATABLE    :: mask_atl

  ! Sea surface height
  REAL(4), DIMENSION(:,:,:), ALLOCATABLE  :: zos

ENDMODULE mod_grid


! Input/Output information
MODULE mod_inout

  IMPLICIT NONE

  CHARACTER(LEN=200)                        :: inDataDir    ! Path to the input files
  CHARACTER(LEN=200)                        :: u_name       ! Path to the u input files
  CHARACTER(LEN=200)                        :: v_name       ! Path to the u input files
  CHARACTER(LEN=200)                        :: ueul_name    ! Path to the u input files
  CHARACTER(LEN=200)                        :: veul_name    ! Path to the v input files
  CHARACTER(LEN=200)                        :: usgs_name    ! Path to the v input files
  CHARACTER(LEN=200)                        :: vsgs_name    ! Path to the v input files
  CHARACTER(LEN=200)                        :: zos_name     ! Path to the SSH input files
  CHARACTER(LEN=200)                        :: mass_name    ! Path to the mass of grid cell input files

  CHARACTER(LEN=200)                        :: topoDataDir   ! Path to the topography directory
  CHARACTER(LEN=200)                        :: topoSuffix

  CHARACTER(LEN=200)                        :: dxv_name      ! Name of variable Dx in V points
  CHARACTER(LEN=200)                        :: dyu_name      ! Name of variable Dy in U points
  CHARACTER(LEN=200)                        :: dxt_name      ! Name of variable Dx in T points
  CHARACTER(LEN=200)                        :: dyt_name      ! Name of variable Dy in T points

  CHARACTER(LEN=200)                        :: dzt_name      ! Name of variable dz in T points
  CHARACTER(LEN=200)                        :: dzv_name      !                     in V points
  CHARACTER(LEN=200)                        :: dzu_name      !                     in U points

  CHARACTER(LEN=200)                        :: kzt_name     ! Name of variable area (dx*dy)
  CHARACTER(LEN=200)                        :: area_name     ! Name of variable area (dx*dy)

  CHARACTER(LEN=200), DIMENSION(2)          :: otracer_name  ! Ocean tracer name
  CHARACTER(LEN=200), DIMENSION(2)          :: atracer_name  ! Atmosphere tracer name

  CHARACTER(LEN=200)                        :: DataSuffix
  CHARACTER(LEN=200)                        :: fileSuffix
  CHARACTER(LEN=200)                        :: runcase

  CHARACTER(LEN=200)                        :: outDataDir  ! Path to the ouput files
  CHARACTER(LEN=200)                        :: outDataFile ! Prefix of the output data

  LOGICAL                                   :: l_rerun
  LOGICAL                                   :: l_read_wflux
  LOGICAL                                   :: l_eddy

ENDMODULE mod_inout

! Fluxes
MODULE mod_flux

  IMPLICIT NONE

  ! Fluxes
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:)  :: uflux             ! Zonal
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:)  :: vflux             ! Meridional
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:)  :: wflux2            ! Vertical if read
  REAL(4), ALLOCATABLE, DIMENSION(:)      :: wflux             ! Vertical if computed

  ! Meridional stream function / fluxes
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:)    :: fluxyz
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:)    :: psiyz
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:,:)  :: fluxyr
  REAL(4), ALLOCATABLE, DIMENSION(:,:,:,:)  :: psiyr

  ! Thermohaline/hidrothermal fluxes
  INTEGER, PARAMETER                      :: MR = 501          ! Number of grid points in the THT space

  REAL(4), DIMENSION(MR,MR,2)             :: fluxrr
  REAL(4), DIMENSION(MR,MR)               :: psirr,chirr


ENDMODULE mod_flux

! Tracers
MODULE mod_tracervars

  ! Number of tracers
  REAL(4), DIMENSION(:,:,:,:), ALLOCATABLE   :: tracers_data       ! Data tracer
  REAL(4), DIMENSION(:,:,:,:), ALLOCATABLE   :: tracers_data_p     ! Data tracer past time step

  REAL(4), DIMENSION(2)                      :: otracer_maximum, otracer_minimum, dtracer

END MODULE mod_tracervars

! Thermodynamics
MODULE mod_thermo

  USE mod_flux

  ! Evaporation and heat Fluxes
  LOGICAL                        :: l_thermo

  REAL(4), DIMENSION(MR, MR)     :: massrr
  INTEGER, DIMENSION(MR, MR)     :: maskrr

  REAL(4), DIMENSION(:,:), ALLOCATABLE  :: eva, hfl
  REAL(4), DIMENSION(MR, MR, 2)         :: evarr, hflrr

  CHARACTER(LEN=200)             :: eva_name
  CHARACTER(LEN=200)             :: hfl_name


END MODULE mod_thermo

! Calendar variables
MODULE mod_calendar

  INTEGER               :: iyear, imonth

  INTEGER               :: dtime
  INTEGER               :: dtimea
  INTEGER               :: dtimeo
  INTEGER               :: nfreq

  INTEGER               :: startYear      ! start year of the simulation
  INTEGER               :: endYear        ! end year of the simulation

END MODULE mod_calendar

! Netcdf var files
MODULE netcdfvars

  INTEGER  :: ncidu, ncidv, ncidw
  INTEGER  :: ncidm
  INTEGER  :: ncidep, ncidhf

  INTEGER, DIMENSION(2) :: ncidt

END MODULE netcdfvars
