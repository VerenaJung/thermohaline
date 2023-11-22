!!------------------------------------------------------------------------------
!!
!!       MODULE: mod_init
!!
!!          Defines and allocates variables and matrices necessary.
!!
!!          Subroutines included:
!!               - init_read_namelist    
!!               - init_alloc_var
!!
!!------------------------------------------------------------------------------

MODULE mod_init

    USE mod_grid
    USE mod_flux
    USE mod_inout
    USE mod_tracervars
    USE mod_calendar
    USE mod_thermo

    IMPLICIT NONE

    CONTAINS

      SUBROUTINE init_read_namelist()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Reads the variables from the namelist
      !
      ! --------------------------------------------------

          LOGICAL :: l_exist

          ! Setup namelists
          namelist /INIT_INFILES/ inDataDir,DataSuffix, fileSuffix, runcase, u_name, v_name, &
                                  ueul_name, veul_name, usgs_name, vsgs_name, zos_name, l_eddy,  &
                                  mass_name,otracer_name, otracer_minimum, otracer_maximum, &
                                  topoDataDir, topoSuffix, area_name
          namelist /INIT_GRID_DESCRIPTION/ imt, jmt, kmt, dxv_name, dyu_name, dxt_name, dyt_name, &
                                           dzt_name, dzu_name, dzv_name, kzt_name
          namelist /INIT_THERMO/ l_thermo, eva_name, hfl_name
          namelist /INIT_CALENDAR/startYear, endYear, nfreq, dtimeo
          namelist /INIT_OUTFILES/outDataDir, outDataFile, l_rerun


          INQUIRE(file='namelist.in', EXIST = l_exist)

          IF (l_exist .EQV. .FALSE.) THEN
              PRINT*, '-----------------------------------------'
              PRINT*, 'ERROR : The namelist file cannot be found'
              PRINT*, '-----------------------------------------'
              STOP
          END IF

          ! Read namelist
          OPEN (8,file='namelist.in',    &
               & status='OLD', delim='APOSTROPHE')
          READ (8,nml=INIT_INFILES)
          READ (8,nml=INIT_GRID_DESCRIPTION)
          READ (8,nml=INIT_THERMO)
          READ (8,nml=INIT_CALENDAR)
          READ (8,nml=INIT_OUTFILES)
          CLOSE(8)

      END SUBROUTINE init_read_namelist

      SUBROUTINE init_alloc_var()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Allocate all arrays
      !
      ! --------------------------------------------------

          INTEGER           :: itrac

          ! Allocate fluxes
          ALLOCATE(uflux(imt, jmt, kmt), vflux(imt, 0:jmt, kmt))
          uflux(:,:,:) = 0.d0
          vflux(:,:,:) = 0.d0

          ! Vertical fluxes
          ALLOCATE(wflux(0:kmt), wflux2(imt, jmt, 0:kmt))
          wflux2 = 0.d0

          ! Sea surface height
          ALLOCATE(zos(imt, jmt, -1:1))
          zos(:,:,:)   = 0.d0

          ! Height scale
          ALLOCATE(zstot(imt, jmt, -1:1), zstou(imt, jmt, -1:1), zstov(imt, jmt, -1:1))
          zstot(:,:,:) = 1.d0
          zstov(:,:,:) = 1.d0; zstou(:,:,:) = 1.d0

          ! Allocate grid Variables
          ALLOCATE(dxt(imt, jmt), dxv(imt, jmt), dyt(imt, jmt), dyu(imt, jmt))
          dxt(:,:) = 0.d0; dxv(:,:) = 0.d0
          dyt(:,:) = 0.d0; dyu(:,:) = 0.d0

          ALLOCATE(dxdy(imt, jmt))
          dxdy(:,:) = 0.d0

          ALLOCATE(dzt(imt, jmt, kmt), dzu(imt, jmt, kmt), dzv(imt, jmt, kmt))
          dzt(:,:,:) = 0.d0
          dzv(:,:,:) = 0.d0; dzu(:,:,:) = 0.d0

          ! Grid  cell mass
          ALLOCATE(masscell(imt, jmt, kmt))
          masscell(:,:,:) = 0.d0

          ! Bathymetry
          ALLOCATE(kzt(imt, jmt))
          kzt(:,:)   = 0.d0

          ! Land/sea Mask
          ALLOCATE(mask(imt, jmt), mask_atl(imt,jmt))
          mask(:,:)    = 1.d0
          mask_atl(:,:) = 0.d0

          ! Tracer array
          ALLOCATE(tracers_data(imt, jmt, kmt, 2), tracers_data_p(imt, jmt, kmt,2))

          DO itrac = 1, 2
            dtracer(itrac) =  (otracer_maximum(itrac) - otracer_minimum(itrac))/FLOAT(MR-1)
          END DO

          ! eva/hfl
          ALLOCATE(eva(imt, jmt), hfl(imt, jmt))
          eva(:,:) = 0.d0
          hfl(:,:) = 0.d0


          ! Fluxes
          ALLOCATE(fluxyz(jmt, kmt, 3), psiyz(jmt, kmt, 3))
          fluxyz(:,:,:) = 0.d0;
          psiyz(:,:,:) = 0.d0
          ALLOCATE(fluxyr(jmt, mr, 3, 3), psiyr(jmt, mr, 3, 3))    !3rd dimension: [Salinity, Temperature, Density]
                                                                   !4th dimension: [everything, empty, empty]
          fluxyr(:,:,:,:) = 0.d0;
          psiyr(:,:,:,:) = 0.d0


          IF (l_thermo) THEN
              massrr(:,:)   = 0.d0
              evarr(:,:,:)  = 0.d0
              hflrr(:,:,:)  = 0.d0
              maskrr(:,:)   = 0.d0
          END IF

      END SUBROUTINE init_alloc_var

END MODULE mod_init
