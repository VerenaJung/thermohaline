!! --------------------------------------------------------
!!
!!     Main program: THT
!!
!!     Purpose: main program that computes thermohaline
!!        /hidrothermal streamfunctions.
!!
!! --------------------------------------------------------

PROGRAM THT

    USE mod_init
    USE mod_calendar
    USE mod_fluxes
    USE mod_getdata
    USE mod_write
    USE mod_inout

    IMPLICIT NONE

    INTEGER :: num


    !******************************************************************************************
    CALL SYSTEM('date')
    PRINT *,'_____________________________________________________________________________'
    PRINT *,'                             Thermohaline calculation                        '
    PRINT *,'_____________________________________________________________________________'
    !******************************************************************************************

    ! Read namelist
    CALL init_read_namelist()

    IF (l_rerun .EQV. .FALSE.) THEN

      ! Allocate variables
      CALL init_alloc_var()

      ! Thermohaline streamfunction
      CALL init_hth_fluxes('ocean')
      CALL read_ncgrid('ocean')

      num=0
      DO iyear = startYear, endYear

       DO imonth = 1, 12

          num=num+1
          PRINT*, 'Year :', iyear, 'Month :', imonth

          ! Read fluxes
          CALL read_ncfluxes('ocean',iyear,imonth)

          ! Compute thermohaline fluxes
          CALL compute_hth_fluxes('ocean')

        END DO

      END DO

      CALL save_hth_flux('ocean')
      CALL save_meridional_flux('ocean')

      IF (l_thermo) CALL save_hth_thermo('ocean')

    ELSE

      ! Read fluxes
      CALL read_hth_flux('ocean')

    END IF

    PRINT *,'_____________________________________________________________________________'
    PRINT *,'                                  Postprocessing                             '
    PRINT *,'_____________________________________________________________________________'

    ! Compute streamfunction
    PRINT*, '   * - Computing hydro-thermo-haline stream function'
    CALL compute_hth_stream(num)

    PRINT*, '   * - Computing meridional stream function'
    CALL compute_meridional_stream(num)

    ! Save output
    CALL save_hth_stream('ocean')
    CALL save_meridional_stream('ocean')

  
    

    PRINT *,'_____________________________________________________________________________'
    PRINT *,'                        Thermohaline calculation (END)                       '
    PRINT *,'_____________________________________________________________________________'
    CALL SYSTEM('date')


END PROGRAM THT
