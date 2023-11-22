!!------------------------------------------------------------------------------
!!
!!       MODULE: mod_write
!!
!!          Saves the output THT streamfunction
!!
!!          Subroutines included:
!!               - save_hth_stream()         -> save HTH stream function
!!               - save_meridional_stream()  -> save meridional stream function
!!               - save_hth_flux             -> save HTH fluxes
!!               - save_hth_meridional       -> save meridional fluxes
!!               - save_hth_thermo           -> save thermodynamic fields
!!               - read_hth_flux             -> read HTH fluxes
!!               - read_atlmask              -> read atlantic mask
!!
!!------------------------------------------------------------------------------


MODULE mod_write

  USE mod_flux
  USE mod_grid
  USE mod_inout
  USE mod_tracervars
  USE mod_calendar
  USE mod_thermo

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE save_hth_stream(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220)           :: FileName
    CHARACTER(LEN=50)            :: wformat

    INTEGER                      :: m1

    IF (TRIM(aocase) == 'ocean') THEN
        FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psirr.dat'
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')


    wformat = "(XXXXF20.5)"
    WRITE(wformat(2:5),'(I4)') MR

    DO m1 = 1, MR
      WRITE(20,FMT=wformat) psirr(:,m1)
    END DO

    CLOSE(20)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!! CHANGES IN LINES BELOW !!!!!
    IF (TRIM(aocase) == 'ocean') THEN
      FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_chirr.dat'
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

    wformat = "(XXXXF20.5)"
    WRITE(wformat(2:5),'(I4)') MR

    DO m1 = 1, MR
       WRITE(20,FMT=wformat) chirr(:,m1)
    END DO
 
    CLOSE(20)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  END SUBROUTINE save_hth_stream

  SUBROUTINE save_meridional_stream(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220)           :: FileName
    CHARACTER(LEN=50)            :: wformat

    INTEGER                      :: m1, m2, m3

    IF (TRIM(aocase) == 'ocean') THEN
        FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyz.dat'
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')


    wformat = "(XXXXF20.5)"
    WRITE(wformat(2:5),'(I4)') JMT

    DO m1 = 1, 3
      DO m2 = 1, kmt
        WRITE(20,FMT=wformat) psiyz(:,m2,m1)
      END DO
    END DO

    CLOSE(20)


IF (TRIM(aocase) == 'ocean') THEN
    FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr11.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

!DO m3 = 1, 3
! DO m2 = 1, 3
  DO m1 = 1, MR
    WRITE(20,FMT=wformat) psiyr(:,m1,1,1)
  END DO
! END DO
!END DO

CLOSE(20)


IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr12.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

!DO m3 = 1, 3
! DO m2 = 1, 3
DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,1,2)
END DO
! END DO
!END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr13.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,1,3)
END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr21.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,2,1)
END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr22.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,2,2)
END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr23.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,2,3)
END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr31.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,3,1)
END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr32.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,3,2)
END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_psiyr33.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

DO m1 = 1, MR
  WRITE(20,FMT=wformat) psiyr(:,m1,3,3)
END DO

CLOSE(20)



  END SUBROUTINE save_meridional_stream

  SUBROUTINE save_hth_flux(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220)           :: FileName
    CHARACTER(LEN=50)            :: wformat

    INTEGER                      :: m1, m2

    IF (TRIM(aocase) == 'ocean') THEN
        FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_fluxrr.dat'
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')


    wformat = "(XXXXF20.5)"
    WRITE(wformat(2:5),'(I4)') MR

    DO m1 = 1, 2
      DO m2 = 1, MR
        WRITE(20,FMT=wformat) fluxrr(:,m2,m1)
      END DO
    END DO

    CLOSE(20)

  END SUBROUTINE save_hth_flux

  SUBROUTINE save_meridional_flux(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220)           :: FileName
    CHARACTER(LEN=50)            :: wformat

    INTEGER                      :: m1, m2

    IF (TRIM(aocase) == 'ocean') THEN
        FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_fluxyz.dat'
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

    wformat = "(XXXXF20.5)"
    WRITE(wformat(2:5),'(I4)') JMT

    DO m1 = 1, 2
      DO m2 = 1, kmt
        WRITE(20,FMT=wformat) fluxyz(:,m2,m1)
      END DO
    END DO

    CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
    FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_fluxyr1.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

!!!!! CHANGED THESE LINES !!!!!!!!!!
!DO m1 = 1, 2
DO m2 = 1, MR
   WRITE(20,FMT=wformat) fluxyr(:, m2,1,1)
END DO
!END DO

CLOSE(20)

IF (TRIM(aocase) == 'ocean') THEN
  FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_fluxyr2.dat'
END IF

OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='REPLACE', ACTION='WRITE')

wformat = "(XXXXF20.5)"
WRITE(wformat(2:5),'(I4)') JMT

!DO m1 = 1, 2
DO m2 = 1, MR
WRITE(20,FMT=wformat) fluxyr(:,m2,2,1)
END DO
!END DO

CLOSE(20)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
  END SUBROUTINE save_meridional_flux

  SUBROUTINE save_hth_thermo(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220), DIMENSION(4)   :: FileName

    INTEGER                      :: m1, m2

    IF (TRIM(aocase) == 'ocean') THEN
        FileName(1) = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_massrr.dat'
        FileName(2) = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_maskrr.dat'
        FileName(3) = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_eprr.dat'
        FileName(4) = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_hfrr.dat'
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName(1)), STATUS='REPLACE', ACTION='WRITE')
    OPEN(UNIT=21, FILE=TRIM(FileName(2)), STATUS='REPLACE', ACTION='WRITE')
    OPEN(UNIT=22, FILE=TRIM(FileName(3)), STATUS='REPLACE', ACTION='WRITE')
    OPEN(UNIT=23, FILE=TRIM(FileName(3)), STATUS='REPLACE', ACTION='WRITE')

    DO m1 = 1, 2
      DO m2 = 1, MR
        IF (m1 == 1) WRITE(20,FMT=*) massrr(:,m2)/(12.*FLOAT(endYear-startYear+1))
        IF (m1 == 1) WRITE(21,FMT=*) maskrr(:,m2)
        WRITE(22,FMT=*) evarr(:,m2,m1)/(12.*FLOAT(endYear-startYear+1))
        WRITE(23,FMT=*) hflrr(:,m2,m1)/(12.*FLOAT(endYear-startYear+1))
      END DO
    END DO

    CLOSE(20); CLOSE(21); CLOSE(22)

  END SUBROUTINE save_hth_thermo

  SUBROUTINE read_hth_flux(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220)           :: FileName

    INTEGER                      :: m1, m2

    LOGICAL                      :: l_exist

    IF (TRIM(aocase) == 'ocean') THEN
        FileName = TRIM(outDataDir)//TRIM(outDataFile)//'_ocean_fluxrr.dat'
    END IF

    INQUIRE(FILE=TRIM(FileName), EXIST = l_exist)

    IF (l_exist .EQV. .FALSE.) THEN
        PRINT*, '------------------------------------------'
        PRINT*, 'ERROR : The flux data file cannot be found'
        PRINT*, '------------------------------------------'
        STOP
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='OLD', ACTION='READ')

    DO m1 = 1, 2
      DO m2 = 1, MR
        READ(20,FMT=*) fluxrr(:,m2,m1)
      END DO
    END DO

    CLOSE(20)

  END SUBROUTINE read_hth_flux

  SUBROUTINE read_atlmask()


    CHARACTER(LEN=220)           :: FileName

    INTEGER                      :: m1

    LOGICAL                      :: l_exist

    FileName = 'mask_data/mask_atl.dat'

    INQUIRE(FILE=TRIM(FileName), EXIST = l_exist)

    IF (l_exist .EQV. .FALSE.) THEN
        PRINT*, '----------------------------------------------'
        PRINT*, 'ERROR : The atlantic mask file cannot be found'
        PRINT*, '----------------------------------------------'
        STOP
    END IF

    OPEN(UNIT=20, FILE=TRIM(FileName), STATUS='OLD', ACTION='READ')

    DO m1 = 1, JMT
        READ(20,FMT=*) mask_atl(:,m1)
    END DO

    CLOSE(20)

  END SUBROUTINE read_atlmask



END MODULE mod_write
