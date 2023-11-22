!!------------------------------------------------------------------------------
!!
!!       MODULE: mod_getdata
!!
!!          Computes thermohaline/hidrothermal fluxes and
!!          compute streamfunctions
!!
!!          Subroutines included:
!!          - read_ncgrid()     -> Read area
!!          - read_ncfluxes()   -> Read fluxes and thermodynamic variables
!! 
!!------------------------------------------------------------------------------


MODULE mod_getdata

  USE netcdf

  USE mod_flux
  USE mod_grid
  USE mod_inout
  USE mod_tracervars
  USE mod_calendar
  USE netcdfvars
  USE mod_thermo
  USE mod_write

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE read_ncgrid(aocase)

    CHARACTER(LEN=*), INTENT(IN) :: aocase

    CHARACTER(LEN=220)           :: FileName

    INTEGER :: ncid, varid, ierr

    PRINT*, 'Reading grid files'
    PRINT*, '----------------------------------------'

FileName = TRIM(topoDataDir)//TRIM(topoSuffix)
print *,'FileName: ',FileName

ierr = NF90_OPEN(TRIM(FileName), NF90_NOWRITE, ncid)
IF(ierr.ne.0) STOP 10

!IF (TRIM(aocase) == 'ocean') THEN
IF (TRIM(runcase) == 'ORCA1L42') THEN



ierr=NF90_INQ_VARID(ncid,TRIM(dxt_name),varid)
IF(ierr.ne.0) STOP 11
ierr=NF90_GET_VAR(ncid,varid, dxt,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 12
ierr=NF90_INQ_VARID(ncid,TRIM(dyt_name),varid)
IF(ierr.ne.0) STOP 13
ierr=NF90_GET_VAR(ncid,varid, dyt,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 14
ierr=NF90_INQ_VARID(ncid,TRIM(dxv_name),varid)
IF(ierr.ne.0) STOP 15
ierr=NF90_GET_VAR(ncid,varid, dxv,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 16
ierr=NF90_INQ_VARID(ncid,TRIM(dyu_name),varid)
IF(ierr.ne.0) STOP 17
ierr=NF90_GET_VAR(ncid,varid, dyu,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 18

ierr=NF90_INQ_VARID(ncid,TRIM(dzt_name),varid)
IF(ierr.ne.0) STOP 19
ierr=NF90_GET_VAR(ncid,varid, dzt,[1,1,1,1],[IMT, JMT, KMT, 1])
IF(ierr.ne.0) STOP 20
ierr=NF90_INQ_VARID(ncid,TRIM(dzu_name),varid)
IF(ierr.ne.0) STOP 21
ierr=NF90_GET_VAR(ncid,varid, dzu,[1,1,1,1],[IMT, JMT, KMT, 1])
IF(ierr.ne.0) STOP 22
ierr=NF90_INQ_VARID(ncid,TRIM(dzv_name),varid)
IF(ierr.ne.0) STOP 23
ierr=NF90_GET_VAR(ncid,varid, dzv,[1,1,1,1],[IMT, JMT, KMT, 1])
IF(ierr.ne.0) STOP 24
ierr=NF90_INQ_VARID(ncid,TRIM(kzt_name),varid)
IF(ierr.ne.0) STOP 25
ierr=NF90_GET_VAR(ncid,varid, kzt,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 26

ierr=NF90_CLOSE(ncid)

! Grid area
dxdy(1:imt,1:jmt) = dxt(1:imt,1:jmt) * dyt(1:imt,1:jmt)


kzt(1,:)=kzt(IMT-1,:)
kzt(IMT,:)=kzt(2,:)

WHERE(kzt == 0) mask(:,:) = 0


ELSEIF (TRIM(runcase) == 'ORCA1L75') THEN


ierr=NF90_INQ_VARID(ncid,TRIM(dxt_name),varid)
IF(ierr.ne.0) STOP 11
ierr=NF90_GET_VAR(ncid,varid, dxt,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 12
ierr=NF90_INQ_VARID(ncid,TRIM(dyt_name),varid)
IF(ierr.ne.0) STOP 13
ierr=NF90_GET_VAR(ncid,varid, dyt,[1,1,1,1],[IMT, JMT, 1, 1])
IF(ierr.ne.0) STOP 14




END IF



 !   IF (TRIM(aocase)=='ocean') CALL read_atlmask()

  END SUBROUTINE read_ncgrid

  SUBROUTINE read_ncfluxes(aocase, year, month)

    CHARACTER(LEN=*), INTENT(IN) :: aocase
    INTEGER, INTENT(IN)          :: year, month

    CHARACTER(LEN=220)           :: FileNameU,FileNameV,FileNameT
    CHARACTER(LEN=30)            :: date

    INTEGER :: ncid, varid, ierr
    INTEGER :: itrac, k

    REAL(4), ALLOCATABLE, DIMENSION(:,:,:)  :: tmp3d
    REAL(4), ALLOCATABLE, DIMENSION(:,:)  :: hs,zstou,zstov,zstot

    ALLOCATE(tmp3d(imt,jmt,kmt),hs(imt,jmt),zstou(imt,jmt),zstov(imt,jmt),zstot(imt,jmt))

    ! Setting date
    date = 'YYYY'

      WRITE(date(1:4),'(I4)')  year
!      WRITE(date(8:11),'(I4)') year

! Read u
IF (TRIM(runcase) == 'ORCA1L42') THEN
 date = 'YYYY'
 WRITE(date(1:4),'(I4)')  year
ELSEIF (TRIM(runcase) == 'ORCA1L75') THEN
 date = 'YYYY0101_YYYY1231_opa_grid_'
 WRITE(date(1:4),'(I4)')  year
 WRITE(date(10:13),'(I4)') year
END IF
FileNameU = TRIM(inDataDir)//TRIM(DataSuffix)//trim(date)//'U'//trim(fileSuffix)



print *,'ufile: ',FileNameU

IF (month == 1) THEN
 ierr = NF90_OPEN(TRIM(FileNameU), NF90_NOWRITE, ncidu)
 IF(ierr.ne.0) STOP 30
END IF

ierr=NF90_INQ_VARID(ncidu,TRIM(ueul_name),varid)
IF(ierr.ne.0) STOP 31
ierr=NF90_GET_VAR(ncidu,varid, uflux,[1,1,1,month],[IMT, JMT, KMT, 1])
IF(ierr.ne.0) STOP 32

if(l_eddy) then
 ierr=NF90_INQ_VARID(ncidu,TRIM(usgs_name),varid)
 IF(ierr.ne.0) STOP 31
 ierr=NF90_GET_VAR(ncidu,varid, tmp3d,[1,1,1,month],[IMT, JMT, KMT, 1])
 IF(ierr.ne.0) STOP 32
 uflux=uflux+tmp3d
endif


! Read v
FileNameV = TRIM(inDataDir)//TRIM(DataSuffix)//trim(date)//'V'//trim(fileSuffix)

print *,'vfile: ',FileNameV
print *,'month: ',month

IF (month == 1) THEN
 ierr = NF90_OPEN(TRIM(FileNameV), NF90_NOWRITE, ncidv)
 IF(ierr.ne.0) STOP 30
END IF

ierr=NF90_INQ_VARID(ncidv,TRIM(veul_name),varid)
IF(ierr.ne.0) STOP 31
ierr=NF90_GET_VAR(ncidv,varid, tmp3d,[1,1,1,month],[IMT, JMT, KMT, 1])
IF(ierr.ne.0) STOP 32

vflux(1:imt,1:jmt,1:kmt) = tmp3d(1:imt,1:jmt,1:kmt)

!do i=1,IMT
! print *,i,tmp3d(i,120:127,50)
!enddo
!
!stop 45968

if(l_eddy) then
 ierr=NF90_INQ_VARID(ncidv,TRIM(vsgs_name),varid)
 IF(ierr.ne.0) STOP 33
 ierr=NF90_GET_VAR(ncidv,varid, tmp3d,[1,1,1,month],[IMT, JMT, KMT, 1])
 IF(ierr.ne.0) STOP 34
 vflux(1:imt,1:jmt,1:kmt) = vflux(1:imt,1:jmt,1:kmt) + tmp3d(1:imt,1:jmt,1:kmt)
endif

! Read T-file
FileNameT = TRIM(inDataDir)//TRIM(DataSuffix)//trim(date)//'T'//trim(fileSuffix)

print *,'Tfile: ',FileNameT
print *,'month: ',month

!IF (month == 1) THEN
 ierr = NF90_OPEN(TRIM(FileNameT), NF90_NOWRITE, ncid)
 IF(ierr.ne.0) STOP 40
!END IF

IF (TRIM(runcase) == 'ORCA1L42') THEN

ierr=NF90_INQ_VARID(ncid,TRIM(zos_name),varid)
IF(ierr.ne.0) STOP 41
ierr=NF90_GET_VAR(ncid,varid, hs,[1,1,month],[IMT, JMT, 1])
IF(ierr.ne.0) STOP 42

! Calculate SSH/depth
WHERE (SUM(dzt(:,:,:),3) /= 0)
     zstot(1:imt,1:jmt)  = hs(1:imt,1:jmt)/SUM(dzt(:,:,:),3) + 1
ELSEWHERE
     zstot = 0.d0
END WHERE

WHERE (SUM(dzu(:,:,:),3) /= 0)
     zstou(1:imt-1,1:jmt) = 0.5*(hs(1:imt-1,1:jmt)+hs(2:imt,1:jmt))/SUM(dzu(1:imt-1,1:jmt,:),3) + 1
ELSEWHERE
     zstou = 0.d0
END WHERE


WHERE (SUM(dzv(:,1:jmt-1,:),3) /= 0)
     zstov(1:imt,1:jmt-1) = 0.5*(hs(1:imt,1:jmt-1)+hs(1:imt,2:jmt))/SUM(dzv(:,1:jmt-1,:),3) + 1
ELSEWHERE
     zstov = 0.d0
END WHERE

FORALL (k = 1:kmt) uflux(:,:    ,k) = uflux(:,:    ,k)*dyu(:,:    )*dzu(:,:    ,k)*zstou(:,:)
FORALL (k = 1:kmt) vflux(:,1:jmt,k) = vflux(:,1:jmt,k)*dxv(:,1:jmt)*dzv(:,1:jmt,k)*zstov(:,:)


FORALL (k = 1:kmt) masscell(1:imt,1:jmt,k) = &
                   dxt(1:imt,1:jmt) * dyt(1:imt,1:jmt) * dzt(1:imt,1:jmt,k) * zstot(1:imt,1:jmt)

ELSEIF (TRIM(runcase) == 'ORCA1L75') THEN


ierr=NF90_INQ_VARID(ncid,'masscello',varid)
IF(ierr.ne.0) STOP 31
ierr=NF90_GET_VAR(ncid,varid, masscell,[1,1,1,month],[IMT, JMT, KMT, 1])
IF(ierr.ne.0) STOP 32

FORALL (k = 1:kmt) masscell(1:imt,1:jmt,k) = &
                  masscell(1:imt,1:jmt,k) *dxt(1:imt,1:jmt) * dyt(1:imt,1:jmt)


END IF

!do i=1,IMT
! print *,i,vflux(i,123:124,50)
!enddo

      ! Thermo studies
!      IF (l_thermo) THEN
!
!          ! Read wfo
!          FileName = TRIM(inDataDir)//TRIM(eva_name)//'/gn/latest/'//TRIM(eva_name)// &
!              TRIM(DataSuffix)//date//'.nc'
!
!          IF (month == 1) THEN
!              ierr = NF90_OPEN(TRIM(FileName), NF90_NOWRITE, ncidep)
!              IF(ierr.ne.0) STOP 15
!          END IF
!
!          ierr=NF90_INQ_VARID(ncidep,TRIM(eva_name),varid)
!          IF(ierr.ne.0) STOP 2
!
!          ierr=NF90_GET_VAR(ncidep,varid, eva,[1,1,1,month],[IMT, JMT, 1, 1])
!          IF(ierr.ne.0) STOP 3
!
!          ! Read hfl
!          FileName = TRIM(inDataDir)//TRIM(hfl_name)//'/gn/latest/'//TRIM(hfl_name)// &
!              TRIM(DataSuffix)//date//'.nc'
!
!          IF (month == 1) THEN
!              ierr = NF90_OPEN(TRIM(FileName), NF90_NOWRITE, ncidhf)
!              IF(ierr.ne.0) STOP 16
!          END IF
!
!          ierr=NF90_INQ_VARID(ncidhf,TRIM(hfl_name),varid)
!          IF(ierr.ne.0) STOP 2
!
!          ierr=NF90_GET_VAR(ncidhf,varid, hfl,[1,1,1,month],[IMT, JMT, 1, 1])
!          IF(ierr.ne.0) STOP 3
!
!      END IF


      ! Read salinity and temperature
      DO itrac = 1, 2
 !         FileNameT = TRIM(inDataDir)//TRIM(otracer_name(itrac))//'/gn/latest/'//TRIM(otracer_name(itrac))// &
 !             TRIM(DataSuffix)//date//'.nc'
FileNameT = TRIM(inDataDir)//TRIM(DataSuffix)//trim(date)//'T'//trim(fileSuffix)

          IF (month == 1) THEN
              ierr = NF90_OPEN(TRIM(FileNameT), NF90_NOWRITE, ncidt(itrac))
              IF(ierr.ne.0) STOP 51
          END IF

          ierr=NF90_INQ_VARID(ncidt(itrac),TRIM(otracer_name(itrac)),varid)
          IF(ierr.ne.0) STOP 52

          ierr=NF90_GET_VAR(ncidt(itrac),varid, tracers_data(:,:,:,itrac),[1,1,1,month],[IMT, JMT, KMT, 1])
          IF(ierr.ne.0) STOP 53

      END DO

      ! Define past tracer value
      IF (year == startYear .AND. month == 1) THEN


IF (TRIM(runcase) == 'ORCA1L42') THEN
 date = 'YYYY'
 WRITE(date(1:4),'(I4)')  endYear
ELSEIF (TRIM(runcase) == 'ORCA1L75') THEN
 date = 'YYYY0101_YYYY1231_opa_grid_'
 WRITE(date(1:4),'(I4)')  endYear
 WRITE(date(10:13),'(I4)') endYear
END IF


        DO itrac = 1, 2

            FileNameT = TRIM(inDataDir)//TRIM(DataSuffix)//trim(date)//'T'//trim(fileSuffix)

            ierr = NF90_OPEN(TRIM(FileNameT), NF90_NOWRITE, ncid)
            IF(ierr.ne.0) STOP 61

            ierr=NF90_INQ_VARID(ncid,TRIM(otracer_name(itrac)),varid)
            IF(ierr.ne.0) STOP 62

            ierr=NF90_GET_VAR(ncid,varid, tracers_data_p(:,:,:,itrac),[1,1,1,12],[IMT, JMT, KMT, 1])
            IF(ierr.ne.0) STOP 63

            ierr=NF90_CLOSE(ncid)

        END DO

      END IF

      IF (month == 12) THEN
          ierr=NF90_CLOSE(ncidu)
          ierr=NF90_CLOSE(ncidv)
          ierr=NF90_CLOSE(ncidm)

          DO itrac = 1, 2
            ierr=NF90_CLOSE(ncidt(itrac))
          END DO

      END IF

      ! Flux correction to avoid large numbers
      WHERE(ABS(uflux)>1e10)               uflux = 0.d0
      WHERE(ABS(vflux)>1e10)               vflux = 0.d0
      WHERE(ABS(wflux2)>1e10)              wflux2 = 0.d0

 !     WHERE(ABS(masscell)>1e10)            masscell = 0.d0
      WHERE(ABS(eva)>1e10)                 eva = 0.d0
      WHERE(ABS(hfl)>1e10)                 hfl = 0.d0
      WHERE(ABS(tracers_data_p)>1e10)      tracers_data_p = 0.d0
      WHERE(ABS(tracers_data)>1e10)        tracers_data = 0.d0

!    END IF

  END SUBROUTINE read_ncfluxes


END MODULE mod_getdata
