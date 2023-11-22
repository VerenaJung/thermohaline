!!------------------------------------------------------------------------------
!!
!!       MODULE: mod_fluxes
!!
!!          Computes thermohaline/hidrothermal fluxes and
!!          compute streamfunctions
!!
!!          Subroutines included:
!!               - init_hth_fluxes()            -> set the time step
!!               - compute_hth_fluxes()         -> computes THT fluxes
!!               - compute_hth_stream()         -> computes THT stream functions
!!               - compute_meridional_stream()  -> computes meridional stream function
!!
!!------------------------------------------------------------------------------


MODULE mod_fluxes

  USE mod_flux
  USE mod_grid
  USE mod_inout
  USE mod_tracervars
  USE mod_calendar
  USE mod_thermo
  USE mod_write

  IMPLICIT NONE

  CONTAINS

    SUBROUTINE init_hth_fluxes(aocase)

      CHARACTER(LEN=*), INTENT(IN) :: aocase

      fluxrr(:,:,:) = 0.d0

      IF (TRIM(aocase) == 'atmosphere') THEN
          dtime    = dtimea

      ELSE IF (TRIM(aocase) == 'ocean') THEN
       dtime    = dtimeo
       if(nfreq==1) then
        dtime = int(365.25*24.*3600./12.)
       elseif(nfreq==2) then
        dtime = 5*24*3600
       elseif(nfreq==3) then
        dtime = 24*3600
       else
        stop 38678
       END IF
     END IF

    END SUBROUTINE init_hth_fluxes

    SUBROUTINE compute_hth_fluxes(aocase)

      CHARACTER(LEN=*), INTENT(IN) :: aocase

      INTEGER :: ii, ip, im
      INTEGER :: jj, jp, jm
      INTEGER :: kk, kp, km

      INTEGER :: itrac

      INTEGER, DIMENSION(0:7,2)               :: mm

      INTEGER                                 :: m1, m2
      INTEGER                                 :: nn, n2

      REAL(8)                                :: slope


      iloop: DO ii = 2, IMT-1

        ip = ii + 1
        im = ii - 1

        jloop: DO jj = 1, JMT-1

          jp = jj + 1
          jm = jj - 1

!          IF (mask(ii,jj)==0) CYCLE jloop

          wflux(:) = 0.d0

kloop: DO kk = kmt,1,-1
!kloop: DO kk = kzt(ii,jj),1,-1

 !           IF (l_read_wflux) THEN
 !               wflux(kk-1) = -wflux2(ii,jj,kk-1)
 !           ELSE
                wflux(kk-1) = wflux(kk) + uflux(ii,jj,kk) - uflux(im,jj,kk) &
                                        + vflux(ii,jj,kk) - vflux(ii,jm,kk)
 !           END IF
            
            kp = kk + 1
            km = kk - 1

            IF (km == 0) km = 1
            IF (kp == kmt + 1) kp = kmt
            IF (kp == kzt(ii,jj) + 1) kp = kzt(ii,jj) ! detta måste testas

            fluxyz(jj,kk,1) = fluxyz(jj,kk,1) + vflux(ii, jj, kk)

!            IF (TRIM(aocase)=='ocean') THEN
!              IF (mask_atl(ii, jj) == 1) THEN
!                fluxyz(jj,kk,2) = fluxyz(jj,kk,2) + vflux(ii, jj, kk)
!              ELSE IF (mask(ii, jj) - mask_atl(ii, jj) == 1) THEN
!                fluxyz(jj,kk,3) = fluxyz(jj,kk,3) + vflux(ii, jj, kk)
!              END IF
!
!            END IF

            DO itrac = 1, 2
                ! find correct 'position' on tracer axis for each position in physical space
                mm(0, 1) = NINT( ( tracers_data(ii, jj, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1  !only this is used for yr.
                mm(1, 1) = NINT( ( tracers_data(ip, jj, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                mm(2, 1) = NINT( ( tracers_data(im, jj, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                mm(3, 1) = NINT( ( tracers_data(ii, jp, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                IF (jm == 0) THEN
                  mm(4, 1) = NINT( ( tracers_data(ii,  1, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                ELSE
                  mm(4, 1) = NINT( ( tracers_data(ii, jm, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                END IF
                IF (km == 0) THEN
                  mm(5, 1) = NINT( ( tracers_data(ii, jj,  1, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                ELSE
                  mm(5, 1) = NINT( ( tracers_data(ii, jj, km, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                END IF
                IF (kp == kmt + 1) THEN
                  mm(6, 1) = NINT( ( tracers_data(ii, jj, kmt, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                ELSE
                  mm(6, 1) = NINT( ( tracers_data(ii, jj, kp, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1
                END IF


                ! Past time tracer
                mm(7, 1) = NINT( ( tracers_data_p(ii, jj, kk, itrac) - otracer_minimum(itrac))/dtracer(itrac) ) + 1

                DO nn = 0, 7                            !Add flux to correct value on tracer axis. 
                    mm(nn, 1) = MAX( 1, mm(nn, 1))      !Either 1 or correct 'box'
                    mm(nn, 1) = MIN(MR, mm(nn, 1))      !If tracer would be larger than MR take MR. 
                END DO

                fluxyr(jj,mm(0, 1),itrac,1) = fluxyr(jj,mm(0, 1),itrac,1) + vflux(ii, jj, kk)


                SELECT CASE(itrac)
                  CASE(1)
                    n2 = 2
                  CASE(2)
                    n2 = 1
                END SELECT

                mm(0, 2) = NINT( ( tracers_data(ii, jj, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                mm(1, 2) = NINT( ( tracers_data(ip, jj, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                mm(2, 2) = NINT( ( tracers_data(im, jj, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                mm(3, 2) = NINT( ( tracers_data(ii, jp, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                IF (jm == 0) THEN
                  mm(4, 2) = NINT( ( tracers_data(ii,  1, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                ELSE
                  mm(4, 2) = NINT( ( tracers_data(ii, jm, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                END IF
                IF (km == 0) THEN
                  mm(5, 2) = NINT( ( tracers_data(ii, jj,  1, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                ELSE
                  mm(5, 2) = NINT( ( tracers_data(ii, jj, km, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                END IF
                IF (kp == kmt + 1) THEN
                  mm(6, 2) = NINT( ( tracers_data(ii, jj, kmt, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                ELSE
                  mm(6, 2) = NINT( ( tracers_data(ii, jj, kp, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1
                END IF

                ! Past time tracer
                mm(7, 2) = NINT( ( tracers_data_p(ii, jj, kk, n2) - otracer_minimum(n2))/dtracer(n2) ) + 1

                DO nn = 0, 7
                    mm(nn, 2) = MAX( 1, mm(nn, 2))
                    mm(nn, 2) = MIN(MR, mm(nn, 2))
                END DO

                ! Thermo properties
                IF (l_thermo .AND. itrac==1) THEN

                    ! Mass distribution in r-r space
                    massrr(mm(0,1), mm(0,2)) = massrr(mm(0,1), mm(0,2)) + masscell(ii, jj, kk)

                    ! Mask distribution
                    maskrr(mm(0,1), mm(0,2)) = 1

                    IF (kk == 1) THEN

                        ! WF in r-r space
                        IF (eva(ii, jj) > 0.d0) THEN
                            evarr(mm(0,1), mm(0,2), 1) = evarr(mm(0,1), mm(0,2), 1) + dxdy(ii, jj)*eva(ii, jj)
                        ELSE IF (eva(ii, jj) < 0.d0) THEN
                            evarr(mm(0,1), mm(0,2), 2) = evarr(mm(0,1), mm(0,2), 2) + dxdy(ii, jj)*eva(ii, jj)
                        END IF

                        ! HF in r-r space
                        IF (hfl(ii, jj) > 0.d0) THEN
                            hflrr(mm(0,1), mm(0,2), 1) = hflrr(mm(0,1), mm(0,2), 1) + dxdy(ii, jj)*hfl(ii, jj)
                        ELSE IF (hfl(ii, jj) < 0.d0) THEN
                            hflrr(mm(0,1), mm(0,2), 2) = hflrr(mm(0,1), mm(0,2), 2) + dxdy(ii, jj)*hfl(ii, jj)
                        END IF

                    END IF

                END IF

                DO nn = 1, 6

                  IF ( mm(nn, 2) > mm(0, 2) ) THEN

                      ! Calculate slope
                      slope = (DBLE(mm(nn, 2)) - DBLE(mm(0, 2))) / (DBLE(mm(nn, 1)) - DBLE(mm(0, 1)))

                      IF (slope == 0.d0) THEN
                          PRINT*, 'ERROR : Zero slope'
                          STOP
                      END IF

                      DO m2 = mm(0, 2), mm(nn, 2)-1

                        m1 = NINT( ( DBLE(m2) - DBLE(mm(0, 2)) )/slope + DBLE(mm(0, 1)))

                        SELECT CASE(nn)

                        CASE(1)
                          fluxrr(m1,m2, itrac) = fluxrr(m1,m2, itrac)  + uflux(ii, jj, kk)
                        CASE(2)
                          fluxrr(m1,m2, itrac) = fluxrr(m1,m2, itrac)  - uflux(im, jj, kk)
                        CASE(3)
                          fluxrr(m1,m2, itrac) = fluxrr(m1,m2, itrac)  + vflux(ii, jj, kk)
                        CASE(4)
                          fluxrr(m1,m2, itrac) = fluxrr(m1,m2, itrac)  - vflux(ii, jm, kk)
                        CASE(5)
                          fluxrr(m1,m2, itrac) = fluxrr(m1,m2, itrac)  - wflux(km) !ii, jj, kk)
                        CASE(6)
                          fluxrr(m1,m2, itrac) = fluxrr(m1,m2, itrac)  + wflux(kk) !ii, jj, km)
                        END SELECT

                      END DO
                  END IF

                END DO

                ! Stationary stream function
                ! Calculate slope
                slope = (DBLE(mm(0, 2))-DBLE(mm(7, 2))) / (DBLE(mm(0, 1))-DBLE(mm(7, 1)))
                IF (mm(0, 2) > mm(7, 2)) THEN

                    DO m2 = mm(7, 2), mm(0, 2)-1
                        m1 = INT( (DBLE(m2)-DBLE(mm(7, 2)))/slope + DBLE(mm(7, 1)) )
                        m1 = MAX(m1,1) ; m1 = MIN(m1,MR)
                        fluxrr(m1, m2, itrac) = fluxrr(m1,m2, itrac) + masscell(ii,jj,kk)/dtime
                    ENDDO

                ELSE IF (mm(0, 2) < mm(7, 2)) THEN
                    DO m2 = mm(0, 2), mm(7, 2)-1
                        m1 = INT( (DBLE(m2)-DBLE(mm(7, 2)))/slope + DBLE(mm(7, 1)) )
                        m1 = MAX(m1, 1) ; m1 = MIN(m1, MR)
                        fluxrr(m1, m2, itrac) = fluxrr(m1, m2, itrac) - masscell(ii,jj,kk)/dtime
                    END DO
                END IF

            END DO

          END DO kloop

        END DO jloop

      END DO iloop

      tracers_data_p(:,:,:,:) = tracers_data(:,:,:,:)

    END SUBROUTINE compute_hth_fluxes

    SUBROUTINE compute_hth_stream(num)

      INTEGER :: iter, m1, m2, num

      REAL(4), DIMENSION(MR,MR)  :: utot, vtot
      REAL(4), DIMENSION(MR,MR)  :: vort,divv

      REAL(4), PARAMETER         :: omega  = 1.9
      REAL(4), PARAMETER         :: omega1 = 1.d0 - omega
      REAL(4), PARAMETER         :: omega2 = 0.25*omega

!      INTEGER, DIMENSION(MR,MR) :: mask


      ! Normalise fluxes
!fluxrr(:,:,:) = fluxrr(:,:,:)/(12.*1.E6*FLOAT(endYear-startYear+1))
fluxrr(:,:,:) = fluxrr(:,:,:)/(1026.*1.E6*FLOAT(num))

      ! Compute vorticity
      utot(:,:) = 0.d0; vtot(:,:) = 0.d0

!utot(:,:) = TRANSPOSE(fluxrr(:,:,1))
!vtot(:,:) = TRANSPOSE(fluxrr(:,:,2))
!vtot(:,:) = fluxrr(:,:,2)/(1026*1.E6)
!utot(:,:) = TRANSPOSE(fluxrr(:,:,1))/(1.E6)
utot(:,:) = transpose(fluxrr(:,:,2))
vtot(:,:) = fluxrr(:,:,1)

!OPEN (unit=34,file='../data/fluxrr.bin',form='unformatted')
!write(34) fluxrr
!CLOSE (34)
!
!stop 45896

vort=0. ; divv=0.
vort(1:MR-1, 1:MR-1) = vtot(2:MR, 1:MR-1) - vtot(1:MR-1, 1:MR-1) - utot(1:MR-1, 2:MR) + utot(1:MR-1, 1:MR-1)
divv(1:MR-1, 1:MR-1) = utot(2:MR, 1:MR-1) - utot(1:MR-1, 1:MR-1) + vtot(1:MR-1, 2:MR) - vtot(1:MR-1, 1:MR-1)

!mask=0
!mask(2:MR-1,2:MR-1)=1

!psirr=0.
!DO m1=2,MR-1
! psirr(m1,:) = psirr(m1-1,:) + vtot(m1,:)
!END DO
!WHERE(psirr==0.) mask = 0
psirr=0.
DO m1=MR-1,2,-1
 psirr(m1,:) = psirr(m1+1,:) - vtot(m1,:)
END DO
!WHERE(psirr==0.) mask = 0
!psirr=0.
!DO m2=2,MR-1
! psirr(:,m2) = psirr(:,m2-1) - utot(:,m2)
!END DO
!WHERE(psirr==0.) mask = 0
!psirr=0.
!DO m2=MR-1,2,-1
! psirr(:,m2) = psirr(:,m2+1) + utot(:,m2)
!END DO
!WHERE(psirr==0.) mask = 0

!do m2=MR,1,-1
! write (*,"(200i1)") (mask(m1,m2),m1=1,200)
!enddo
!
!do m2=MR/2,MR/2
! write (*,"(500f7.4)") (psirr(m1,m2),m1=1,MR)
!enddo


psirr=0. ; chirr=0.
!  Integrate first fluxes to get a first estimate
!DO m1=2,MR-10
! psirr(m1,:) = psirr(m1-1,:) + vtot(m1,:)
! chirr(m1,:) = chirr(m1-1,:) + utot(m1,:)
!END DO
!DO m2=2,MR-10
! psirr(:,m2) = psirr(:,m2-1) - utot(:,m2)
! chirr(:,m2) = chirr(:,m2-1) + vtot(:,m2)
!END DO

      ! Solving poisson equation
DO iter = 1, 100000
 DO m1=2,MR-1
  DO m2=2,MR-1
!   if(mask(m1,m2)==1) then
    psirr(m1,m2) = omega1*psirr(m1,m2)+omega2*(psirr(m1-1,m2)+psirr(m1+1,m2)+ &
                                             & psirr(m1,m2-1)+psirr(m1,m2+1)-vort(m1,m2))
    chirr(m1,m2) = omega1*chirr(m1,m2)+omega2*(chirr(m1-1,m2)+chirr(m1+1,m2)+ &
                                             & chirr(m1,m2-1)+chirr(m1,m2+1)-divv(m1,m2))
!   endif
  END DO
 END DO
END DO


    END SUBROUTINE compute_hth_stream

    SUBROUTINE compute_meridional_stream(num)

      INTEGER :: kk,num

      DO kk = kmt-1, 1, -1
!psiyz(:,kk,:) = psiyz(:,kk+1,:) + fluxyz(:,kk,:)/(1.E6*12.*FLOAT(endYear-startYear+1))
psiyz(:,kk,:) = psiyz(:,kk+1,:) + fluxyz(:,kk,:)/(1.E6*FLOAT(num))
      END DO

      DO kk = MR-1, 1, -1
!psiyr(:,kk,:,:) = psiyr(:,kk+1,:,:) + fluxyr(:,kk,:,:)/(1.E6*12.*FLOAT(endYear-startYear+1))
psiyr(:,kk,:,:) = psiyr(:,kk+1,:,:) + fluxyr(:,kk,:,:)/(1.E6*FLOAT(num))
      END DO


!fluxyz=psiyz



    END SUBROUTINE compute_meridional_stream



END MODULE mod_fluxes
