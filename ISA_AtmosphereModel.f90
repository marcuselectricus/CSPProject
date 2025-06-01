MODULE InternationalStandardAtmosphere
!
!  Module:  InternationalStandardAtmosphere
!
!  Purpose: Standard atmosphere to support refraction calculations
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!
! Public TYPES               Name                            Description / (Units)
! ----------------------------+---------------------------------+-------------------------------------------------------

!
! Public Variables               Name                            Description / (Units)
! ----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4)                    :: UseExponentialModel = .true.   ! Atmospheric attenuation using extinction model
LOGICAL(4)                    :: UsePolynomialModel  = .false.  ! Atmospheric attenuation using 4th degree polynomial
REAL(8)  ,PARAMETER           :: RATM        = 84.852d0         ! Earth atmosphere height (km)
REAL(8)  ,PARAMETER           :: REARTH      = 6371.0d0         ! Mean radius of the Earth (km)
REAL(8)  ,PARAMETER           :: GMR         = 34.163195d0      ! hydrostatic constant
REAL(8)  ,PARAMETER           :: TREFISA     = 288.0d0          ! Base Reference Temp (K)
REAL(8)  ,PARAMETER           :: PREFISA     =   1.0d0          ! Base Reference Pressure (atm)
REAL(8)  ,PARAMETER           :: AirMassHorizonJPL = 37.9206d0  ! JPL Horizons Air Refraction Model
REAL(8)  ,PARAMETER           :: Atm_n(5) =                  &  ! Barstow correlation
                                                                 (/0.6789d0, 10.46d0, -1.7d0, 0.2854d0, 0.0d0/)
!
! Private Variables              Name                            Description / (Units)
! ----------------------------+---------------------------------+-----------------------------------
REAL(8)  ,PARAMETER, PRIVATE  :: dAlt = 0.0025d0                ! Altitude integration step size (km)

CONTAINS
!
!+
FUNCTION AirMassAtZenith(AngZen, Altitude) RESULT(AirMass)
!+
! Description: Returns the Airmass for a given altitude and zenith angle.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: AngZen    !Zenith angle (degrees)
REAL(8),INTENT(IN)            :: Altitude  !Altitude (km)
REAL(8)                       :: AirMass   !Airmass returned
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: alt                  !Altitude restricted to  [0 ... RATM]                         (km)
REAL(8)                       :: azen                 !Zenith angle restricted to [0 ... 90]                   (degrees)
REAL(8)                       :: dx
REAL(8)                       :: y
REAL(8)                       :: yMax
REAL(8)                       :: N_n(2)
REAL(8)                       :: dR_n(2)
REAL(8)                       :: P_n(2)
REAL(8)                       :: altAtm
REAL(8)                       :: sumRho               !Sum of per unit densities
REAL(8)                       :: sigma                !density/sea-level standard density
REAL(8)                       :: delta                !pressure/sea-level standard pressure
REAL(8)                       :: theta                !temperature/sea-level standard temperature

!----------------------------------------------------------------------------
  alt  = MIN(RATM, MAX(0.0d0, Altitude))
  azen = MIN(90.0d0, MAX(0.0d0, AngZen))
  dx = RATM-alt
  IF(azen >= 90.0d0) THEN
    yMax = SQRT( (REARTH+RATM)**2 - (REARTH+alt)**2)
    N_n  = (/0,1/)
  ELSE
    y    = TAND(azen)*dx
    dR_n = (/REARTH + RATM,y/) - (/REARTH + alt,0d0/)
    N_n  = dR_n/SQRT(DOT_PRODUCT(dR_n,dR_n))
  END IF
  P_n = (/REARTH+alt,0.0d0/)
  altAtm = alt
  sumRHO = 0
  DO WHILE(altAtm <= RATM)
    CALL Atmosphere(altAtm, sigma, delta, theta)
    sumRHO = sumRHO + sigma
    P_n = P_n + dAlt*N_n
    altAtm = DSQRT(DOT_PRODUCT(P_n,P_n)) - REARTH
  END DO
  AirMass = sumRHO*dAlt *(AirMassHorizonJPL/35.1737867d0)**( MAX(0.0d0,MIN(1.0d0,azen/90.0d0))**3)
RETURN
END FUNCTION AirMassAtZenith
!
FUNCTION AirmassCalculationISA(AngleZenith, Elevation) RESULT(Airmass)
!+
! Description: Returns the Airmass for a given elevation and zenith angle.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
USE UnitConversion

IMPLICIT NONE
!                               Passed                           Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)            :: AngleZenith                    ! Solar Zenith Angle (degrees)
REAL(8),INTENT(IN)            :: Elevation                      ! Elevation (km)
REAL(8)                       :: Airmass                        ! Airmass (AM1)
!
!Local Variables
!----------------------------------------
REAL(8)                       :: Angle                          ! Local copy of angle (degrees)
REAL(8)                       :: RatioRho
REAL(8)                       :: RatioPressure
REAL(8)                       :: RatioTemperature
INTEGER(4)                    :: ang
LOGICAL(4) ,SAVE              :: AtmosphereModelSetup = .false. ! Atmospheric model has been initializeed
LOGICAL(4) ,SAVE              :: AtmosphereModelLoaded = .false.! Set true if airmass model loaded
REAL(8)    ,SAVE              :: AirMass_ang(0:90)              ! AirMass at the elevation of the location (per-unit)
REAL(8)    ,SAVE              :: AirMassSeaLevel_ang(0:90)      ! AirMass at sea level for location (per-unit)
REAL(8)    ,SAVE              :: ElevationBase                  ! Reference elevation for airmass model (km)
REAL(8)    ,SAVE              :: PsiBase                        ! Reference pressure for airmass model (psia)
REAL(8)    ,SAVE              :: TempBase                       ! Reference temperature for airmass model (degF)
REAL(8)    ,SAVE              :: AirMassOne                     ! Airmass at sea level when sun is overhead

!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(AtmosphereModelLoaded) THEN
    angle = MAX(0.0d0, MIN(ABS(angleZenith), 90.0d0))
    ang   = INT(angle)
    Airmass = AirMass_ang(ang)
    IF(angle > DBLE(ang)) THEN
      Airmass = Airmass + (Angle-DBLE(ang))*(AirMass_ang(ang+1) - AirMass)
    END IF
    RETURN
  END IF

  IF(.not.AtmosphereModelSetup) THEN
    AtmosphereModelSetup = TRUE
    AirMassOne = AirMassAtZenith(0d0, 0d0)
    DO ang = 0,90
      AirmassSeaLevel_ang(ang) = AirMassAtZenith(DBLE(ang),0.0d0)/AirMassOne
    END DO
    AirMassSeaLevel_ang = AirMassSeaLevel_ang/AirMassOne
    AirMass_ang         = AirMassSeaLevel_ang
    ElevationBase       = 0d0
    PsiBase             = PrefISA * PsiPerAtm
    TempBase            = DegFFromDegK(TrefISA)
  END IF

  IF(Elevation.ne.ElevationBase) THEN
    ElevationBase = Elevation
    CALL Atmosphere(Elevation, RatioRho, RatioPressure, RatioTemperature)
    PsiBase  = PrefISA  * RatioPressure * PsiPerAtm
    TempBase = DegFFromDegK(TrefISA *  RatioTemperature)
    DO ang = 0,90
      Airmass_ang(ang) = AirMassAtZenith(DBLE(ang),Elevation)/AirMassOne
    END DO
  END IF
  angle = MAX(0.0d0, MIN(ABS(angleZenith), 90.0d0))
  ang   = INT(angle)
  Airmass = AirMass_ang(ang)
  IF(angle > DBLE(ang)) THEN
    Airmass = Airmass + (Angle-DBLE(ang))*(AirMass_ang(ang+1) - AirMass)
  END IF
  RETURN
END FUNCTION AirmassCalculationISA
!
SUBROUTINE Atmosphere(alt, sigma, delta, theta)
!+
! Description:  Compute the properties of the 1976 standard atmosphere to 86 km.
! AUTHOR - Ralph Carmichael, Public Domain Aeronautical Software
! NOTE - If alt > 86, the values returned will not be correct, but they will
!   not be too far removed from the correct values for density.
!   The reference document does not use the terms pressure and temperature
!   above 86 km.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: alt    ! geometric altitude, km.
REAL(8),INTENT(OUT)           :: sigma  ! density/sea-level standard density
REAL(8),INTENT(OUT)           :: delta  ! pressure/sea-level standard pressure
REAL(8),INTENT(OUT)           :: theta  ! temperature/sea-level standard temperature
!=======================================================================================================================
!=======================================================================================================================
INTEGER(4),PARAMETER          :: NTAB=8 ! number of entries in the defining tables
!=======================================================================================================================
!=======================================================================================================================
INTEGER(4)                    :: i,j,k           ! counters
REAL(8)                       :: h               ! geopotential altitude (km)
REAL(8)                       :: tgrad, tbase    ! temperature gradient & base temp of this layer
REAL(8)                       :: tlocal          ! local temperature
REAL(8)                       :: deltah          ! height above base of this layer
!=======================================================================================================================
!=======================================================================================================================
REAL(8),DIMENSION(NTAB),PARAMETER:: htab= &
                        (/0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852/)
REAL(8),DIMENSION(NTAB),PARAMETER:: ttab= &
        (/288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946/)
REAL(8),DIMENSION(NTAB),PARAMETER:: ptab= &
             (/1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3, 1.0945601E-3, &
                                   6.6063531E-4, 3.9046834E-5, 3.68501E-6/)
REAL(8),DIMENSION(NTAB),PARAMETER:: gtab= &
                              (/-6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0/)
!----------------------------------------------------------------------------
  h=alt*REARTH/(alt+REARTH)       ! convert geometric to geopotential altitude

  i=1
  j=NTAB                                ! setting up for binary search
  DO
    k=(i+j)/2                           ! integer division
    IF (h < htab(k)) THEN
      j=k
    ELSE
      i=k
    END IF
    IF (j <= i+1) EXIT
  END DO

  tgrad=gtab(i)                         ! i will be in 1...NTAB-1
  tbase=ttab(i)
  deltah=h-htab(i)
  tlocal=tbase+tgrad*deltah
  theta=tlocal/ttab(1)                  ! temperature ratio

  IF (tgrad == 0.0) THEN                ! pressure ratio
    delta=ptab(i)*EXP(-GMR*deltah/tbase)
  ELSE
    delta=ptab(i)*(tbase/tlocal)**(GMR/tgrad)
  END IF

  sigma=delta/theta                     ! density ratio
  RETURN
END SUBROUTINE Atmosphere
!
! -----------------------------------------------
FUNCTION DNIMeinelModel(DNIbase, AngleZenith, ElevKm) RESULT(DNI)
!+
! Description: Returns the DNI adjusted under the Meinel model.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: DNIBase        ! Base Level Of DNI  (kw/m^2)
REAL(8),INTENT(IN)            :: AngleZenith    ! Solar Zenith angle (degrees)
REAL(8),INTENT(IN)            :: ElevKm         ! Elevation above reference ellipsoid (km)
REAL(8)                       :: DNI            ! DNI result (kw/m^2)
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: AsunZen

  AsunZen = AngleZenith
  IF(AsunZen > 89.999d0) THEN
    DNI = 0.0d0
  ELSE
    DNI = MIN( 1.0d0, (1.0d0-0.14d0*ElevKm) * DEXP(-0.357d0 * (1.0d0/DCOSD(AsunZen))**0.678d0) + 0.14d0*ElevKm) &
        /   ( (1.0d0-0.14d0*ElevKm)*DEXP(-0.357d0) + 0.14d0*ElevKm ) * DNIBase


    !PRINT *, (1.0d0/DCOSD(AsunZen))
  END IF
  RETURN
END FUNCTION DNIMeinelModel
!
FUNCTION DNILatest(AngleZenith, ElevKm, Ds, SolarFraction) RESULT(DNI)
!+
! Description: Returns the modeled DNI.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE PhysicalConstants
USE NumericalConstants
USE UnitConversion

IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: AngleZenith    ! Solar Zenith angle (degrees)
REAL(8),INTENT(IN)            :: ElevKm         ! Elevation above reference ellipsoid (km)
REAL(8),INTENT(IN)            :: Ds
REAL(8),INTENT(IN)            :: SolarFraction  !
REAL(8)                       :: DNI            ! DNI result (kw/m^2)

!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: AsunZen
REAL(8)                       :: PressureRatioAtElev
REAL(8)                       :: TempRatioAtElev
REAL(8)                       :: DensityRatioAtElev
REAL(8)                       :: AM

AsunZen = AngleZenith
IF(AsunZen > 89.999d0) THEN
  DNI = 0.0d0
ELSE

  CALL Atmosphere(ElevKm, DensityRatioAtElev, PressureRatioAtElev, TempRatioAtElev)


  !  Correct for airmass at temperature


  !AM = AirmassCalculationISA(AsunZen, 0d0)!*PressureRatioAtElev
  AM = AirmassCalculationISA(AsunZen, ElevKm)!*PressureRatioAtElev
  DNI  = SolarFluxAtOneAU * (((1d0 - (1d0/7.1d0)*ElevKm) * EXP(-0.357d0 * (AM)**0.678d0) + (1d0/7.1d0)*ElevKm )*SolarFraction) * (KmPerAU/Ds)**2 / 1000.0d0


  !DNI  = SolarFluxAtOneAU * (((1d0 - (1d0/7.1d0)*ElevKm) * EXP(-0.357d0 * (AM*PressureRatioAtElev)**0.678d0) + (1d0/7.1d0)*ElevKm )*SolarFraction) * (KmPerAU/Ds)**2 / 1000.0d0

END IF
RETURN
END FUNCTION DNILatest
!!
FUNCTION DNIMeinelModelNewAMatElev(DNIbase, AngleZenith, ElevKm, Ds) RESULT(DNI)
!+
! Description: Returns the DNI adjusted under the Meinel model.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE UnitConversion
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: DNIBase        ! Base Level Of DNI  (kw/m^2)
REAL(8),INTENT(IN)            :: AngleZenith    ! Solar Zenith angle (degrees)
REAL(8),INTENT(IN)            :: ElevKm         ! Elevation above reference ellipsoid (km)
REAL(8),INTENT(IN)            :: Ds
REAL(8)                       :: DNI            ! DNI result (kw/m^2)
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: AsunZen
REAL(8)                       :: AM
REAL(8)                       :: PressureRatioAtElev
REAL(8)                       :: TempRatioAtElev
REAL(8)                       :: DensityRatioAtElev
LOGICAL(4),PARAMETER          :: EvaluateModel = .false.

  AsunZen = AngleZenith
  IF(AsunZen > 89.999d0) THEN
    DNI = 0.0d0
  ELSE

    CALL Atmosphere(ElevKm, DensityRatioAtElev, PressureRatioAtElev, TempRatioAtElev)
    AM = AirmassCalculationISA(AsunZen, ElevKm)
    DNI = MIN( 1.0d0, (1.0d0-(1d0/7.1d0)*ElevKm) * DEXP(-0.357d0 * (AM)**0.678d0) + (1d0/7.1d0)*ElevKm) &
        /   ( (1.0d0-(1d0/7.1d0)*ElevKm)*DEXP(-0.357d0) + (1d0/7.1d0)*ElevKm ) * (KmPerAU/Ds)**2
    IF(EvaluateModel) THEN
      WRITE(*,FMT='(F8.4,",",F8.4,",",F10.6)') DNIBase, DNI, DNIBase-DNI
    END IF
  END IF
RETURN
END FUNCTION DNIMeinelModelNewAMatElev
!!
FUNCTION DNI_MODEL_0219(DNIbase, AngleZenith, ElevKm, Ds) RESULT(DNI)
!+
! Description: Returns the modeled DNI.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE PhysicalConstants
USE NumericalConstants
USE UnitConversion
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: DNIBase        ! Base Level Of DNI  (kw/m^2)
REAL(8),INTENT(IN)            :: AngleZenith    ! Solar Zenith angle (degrees)
REAL(8),INTENT(IN)            :: ElevKm         ! Elevation above reference ellipsoid (km)
REAL(8),INTENT(IN)            :: Ds
REAL(8)                       :: DNI            ! DNI result (kw/m^2)
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: AsunZen
REAL(8)                       :: AM
REAL(8)                       :: PressureRatioAtElev
REAL(8)                       :: TempRatioAtElev
REAL(8)                       :: DensityRatioAtElev
LOGICAL(4),PARAMETER          :: EvaluateModel = .false.

AsunZen = AngleZenith
IF(AsunZen > 89.999d0) THEN
  DNI = 0.0d0
ELSE

  CALL Atmosphere(ElevKm, DensityRatioAtElev, PressureRatioAtElev, TempRatioAtElev)
  AM = AirmassCalculationISA(AsunZen, ElevKm)

  DNI = (1d0/1000d0) * 1.1d0 * SolarFluxAtOneAU * ((0.7d0)**((AM*PressureRatioAtElev)**0.678d0)) * (KmPerAU/Ds)**2

  IF(EvaluateModel) THEN
    WRITE(*,FMT='(F8.4,",",F8.4,",",F10.6)') DNIBase, DNI, DNIBase-DNI
  END IF

END IF
RETURN
END FUNCTION DNI_MODEL_0219
!
FUNCTION DNIMeinelModelNew(DNIbase, AngleZenith, ElevKm, Ds) RESULT(DNI)
!+
!  Description: This routine calculates DNI for a site based on the solar zenith angle, the elevation of the site and
!               the distance from the earth to the sun.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
USE UnitConversion


IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: DNIBase                        ! Base Level Of DNI  (kw/m^2)
REAL(8),INTENT(IN)            :: AngleZenith                    ! Solar Zenith angle (degrees)
REAL(8),INTENT(IN)            :: ElevKm                         ! Elevation above reference ellipsoid (km)
REAL(8),INTENT(IN)            :: Ds                             ! Distance to sun
REAL(8)                       :: DNI                            ! DNI result (kw/m^2)
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: AsunZen
REAL(8)                       :: PressureRatioAtElev
REAL(8)                       :: TempRatioAtElev
REAL(8)                       :: DensityRatioAtElev
REAL(8)                       :: AM
REAL(8)                       :: AM_n(5)
LOGICAL(4), PARAMETER         :: EvaluateModels = .false.

REAL(8)   , PARAMETER         :: Yatm                 = 9d0
REAL(8)                       :: CorrectionTerm

  AsunZen = AngleZenith
  IF(AsunZen > 89.999d0) THEN
    DNI = 0.0d0
  ELSE

    CALL Atmosphere(ElevKm, DensityRatioAtElev, PressureRatioAtElev, TempRatioAtElev)
    CorrectionTerm = 1d0/PressureRatioAtElev**0.5d0  ! DensityRatioAtElev*TempRatioAtElev/PressureRatioAtElev

    !  Airmass compare with JPL @ horizon = 37.909
    AM = AirmassCalculationISA(AsunZen, ElevKm)
    IF(EvaluateModels) THEN
      AM_n(1)  = 1.0d0/(DCOSD(AsunZen))
      AM_n(2) = (1d0/(DCOSD(AsunZen) + 0.50572d0*((96.07995d0-AsunZen)**(-1.6364)))) ! Kasten and Young
      AM_n(3) = 1d0/(SIND(90d0 - AngleZenith + 244d0/(165d0 + 47d0*((90d0-AngleZenith)**1.1))))    ! Pickering (2002)
      AM_n(4) = ( COSD(AsunZen)*COSD(AsunZen) * ((REARTH)/Yatm)**2 + ((2*REARTH)/(Yatm*Yatm))*(Yatm) + 1d0)**0.5d0 - ((REARTH)/Yatm)*COSD(AsunZen)
      AM_n(5) =  (( COSD(AsunZen)*COSD(AsunZen) * ((REARTH + ElevKm)/Yatm)**2 + ((2d0*REARTH)/(Yatm*Yatm))*(Yatm-ElevKm) - (ElevKm/Yatm)**2 + 1d0)**0.5d0 - ((REARTH+ElevKm)/Yatm)*COSD(AsunZen))
    END IF

    !  Separate out AirMass definition
    DNI =  MIN( 1.0d0, CorrectionTerm * (1.0d0-(1d0/7.1d0)*ElevKm) * DEXP(-0.357d0 * (AM)**0.678d0) + (1d0/7.1d0)*ElevKm) &
        /   ( (1.0d0-(1d0/7.1d0)*ElevKm)*DEXP(-0.357d0) + (1d0/7.1d0)*ElevKm ) * DNIBase * (KmPerAU/Ds)**2


   IF(EvaluateModels) THEN
      WRITE(*,'(F8.4, 8(",",F12.4))')  AsunZen, AM, AM_n(1), AM_n(2), AM_n(3), AM_n(4), AM_n(5) , DNI, DNIBase
   END IF

  END IF

RETURN
END FUNCTION DNIMeinelModelNew
!
FUNCTION DNIMeinelModelModified(AngleZenith, ElevKm, dayOfYear) RESULT(DNI)
!+
! Description: Returns the DNI adjusted under the Meinel model.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE PhysicalConstants

IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8)   , INTENT(IN)        :: AngleZenith    ! Solar Zenith angle (degrees)
REAL(8)   , INTENT(IN)        :: ElevKm         ! Elevation above reference ellipsoid (km)
INTEGER(4), INTENT(IN)        :: dayOfYear      ! Current day of year
REAL(8)                       :: DNI            ! DNI result (kw/m^2)
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: AsunZen
REAL(8)                       :: AM
REAL(8)                       :: Gamma
REAL(8)                       :: Eo
REAL(8)                       :: PressureRatioAtElev
REAL(8)                       :: TempRatioAtElev
REAL(8)                       :: DensityRatioAtElev

AsunZen = AngleZenith
IF(AsunZen > 89.999d0) THEN
  DNI = 0.0d0
ELSE

  Gamma = 360d0 * (DBLE(dayOfYear) - 1d0)/365d0
  Eo = 1.00011 + 0.034221*DCOSD(Gamma) + 0.00128*DSIND(Gamma) + 0.000719*DCOSD(2*Gamma) + 0.000077*DSIND(2*Gamma)

  CALL Atmosphere(ElevKm, DensityRatioAtElev, PressureRatioAtElev, TempRatioAtElev)
  AM = AirMassCalculationISA(AsunZen, ElevKm)

  DNI = SolarFluxAtOneAU *  Eo * (0.7**(AM**0.678)) !*DCOSD(AsunZen)


END IF
RETURN
END FUNCTION DNIMeinelModelModified
!
FUNCTION RatioFluxAirmass(Airmass) RESULT(RatioFlux)
!+
! Description: Returns the ratio of flux seen at location vs outside atmosphere.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: Airmass        ! Relative Airmass (Ratio)
REAL(8)                       :: RatioFlux      ! Ratio wrt Flux Outside Atmosphere
!=======================================================================================================================
!=======================================================================================================================
REAL(8),PARAMETER             :: c0 = DLOG(950.0d0/1367.0d0)
REAL(8),PARAMETER             :: k0 = DLOG(DLOG(827.0d0/950.0d0)/c0)/DLOG(1.5d0)

RatioFlux = DEXP(c0*DABS(Airmass)**(-k0))
RatioFlux = (0.7d0)**(AirMass**0.678d0)

RETURN
END FUNCTION RatioFluxAirmass

FUNCTION AttenuationAtmosphere(Visibility,Distance) RESULT(Attenuation)
!+
! Description: Returns the flux attenuation ratio in atmosphere.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: Visibility     ! Atmospheric Visibility (km)
REAL(8),INTENT(IN)            :: Distance       ! Distance traveled in atmosphere (km)
REAL(8)                       :: Attenuation    ! Ratio Flux remaining (fraction)
!=======================================================================================================================
!=======================================================================================================================
REAL(8),SAVE                  :: VisibilityLocal = 23d0
REAL(8),SAVE                  :: Vo              = 0.1017305d0

IF(UseExponentialModel) THEN
  Attenuation = 0.05d0**(Distance/Visibility)
ELSE IF(UsePolynomialModel) THEN
  Attenuation = DOT_PRODUCT((/1d0, Distance , Distance**2, Distance**3, Distance**4/), Atm_n)

ELSE
  IF(Visibility /= VisibilityLocal) THEN
    VisibilityLocal = MAX(0.5d0,MIN(Visibility,23d0))
    Vo              = 0.1017305d0 * (23d0/VisibilityLocal)**0.13170557d0
  END IF
  Attenuation = MAX(0.001d0,MIN(Vo**(Distance/Visibility), 1d0))
END IF
END FUNCTION AttenuationAtmosphere

PURE FUNCTION AttenuationOfAtmosphere(Visibility,Distance) RESULT(Attenuation)
!+
! Description: Returns the flux attenuation ratio in atmosphere.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!=======================================================================================================================
!=======================================================================================================================
REAL(8),INTENT(IN)            :: Visibility     ! Atmospheric Visibility (km)
REAL(8),INTENT(IN)            :: Distance       ! Distance traveled in atmosphere (km)
REAL(8)                       :: Attenuation    ! Ratio Flux remaining (fraction)
!=======================================================================================================================
!=======================================================================================================================
REAL(8)                       :: VisibilityLocal
REAL(8)                       :: Vo

IF(UseExponentialModel) THEN
  Attenuation = 0.05d0**(Distance/Visibility)
ELSE IF(UsePolynomialModel) THEN
  Attenuation = DOT_PRODUCT((/1d0, Distance , Distance**2, Distance**3, Distance**4/), Atm_n)

ELSE

  VisibilityLocal = MAX(0.5d0,MIN(Visibility,23d0))
  Vo              = 0.1017305d0 * (23d0/VisibilityLocal)**0.13170557d0
  Attenuation = MAX(0.001d0,MIN(Vo**(Distance/Visibility), 1d0))
END IF
END FUNCTION AttenuationOfAtmosphere
!
! -----------------------------------------------

END MODULE InternationalStandardAtmosphere
