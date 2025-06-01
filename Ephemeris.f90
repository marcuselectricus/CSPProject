! Source: Ephemeris.f90
!
! Module: Ephemeris
!
! Purpose: Provide a library of routines to interface to public domain codes from NASA JPL and the Naval Observatory
!          to calculate the positions of bodies in the solar system.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
MODULE Ephemeris
!+
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE TimeDatePackage
USE EphemerisTypes
USE ISO_C_BINDING
!
! Public Variables
!                               Name                              Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),PUBLIC,PARAMETER   :: EPH_S_NORMAL  =  1
INTEGER(4),PUBLIC,PARAMETER   :: EPH_E_FAILURE = -1
!
! Private Variables
!                               Name                             Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4), PRIVATE           :: Initialized = .false.          ! Initialization Flag
LOGICAL(4), PRIVATE           :: ReadJPLEphemeris = .false.     ! Set true after JPL Ephemeris is read in
LOGICAL(4), PRIVATE           :: ModelLightTravelTime = .true.  ! Consider light travel time in position differences
!
! Interfaces Provided by this module
!
! Location and Transformations for the Celestial Reference System
!
INTERFACE PositionCRS
  MODULE PROCEDURE PositionTime, PositionJdt, PositionJdt2, PositionTimeLightTravel
END INTERFACE
INTERFACE T_PCS_CRS_Body
  MODULE PROCEDURE T_PCS_CRS_BodyAtJDT, T_PCS_CRS_BodyAtTime
END INTERFACE
!
! Sun position For Tiesol
TYPE, BIND(C)                 :: AzElTiesol
  REAL(C_DOUBLE)              :: Az                             ! Azimuthal Angle (radians)
  REAL(C_DOUBLE)              :: El                             ! Elevation Angle (radians)
END TYPE AzElTiesol

!
!Interfaces to the ISA Standard Atmosphere Module
!
! Private Values                 Name                             Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4)       , PRIVATE    :: AtmosphereModelLoaded = .false.! Set true if airmass model loaded
LOGICAL(4)       , PRIVATE    :: AtmosphereModelSetup  = .false.! Set true if initialization complete
REAL(8)          , PRIVATE    :: AirMass_ang(0:90)              ! AirMass at the elevation of the location    (per-unit)
REAL(8)          , PRIVATE    :: AirMassSeaLevel_ang(0:90)      ! AirMass at sea level for location           (per-unit)
REAL(8)          , PRIVATE    :: ElevationBase                  ! Reference elevation for airmass model             (km)
REAL(8)          , PRIVATE    :: PsiBase                        ! Reference pressure for airmass model            (psia)
REAL(8)          , PRIVATE    :: TempBase                       ! Reference temperature for airmass model         (degF)
REAL(8)          , PRIVATE    :: AirMassOne                     ! Absolute Sea level Airmass Zenith angle 90  (per-unit)
TYPE(SolarTimes) , PRIVATE    :: TimesSolarToday              & ! Solar Times for the current day            (10^-7 sec)
                                                                  = SolarTimes(0,0,0,0)
TYPE(TimeDateISO), PRIVATE    :: DateAndTimeToday             & ! Data and Time for the current day
                                                                  = TimeDateISO(0,0,0,0,0,0,0,0)
!
! Transverse Mercator Data for Surveying Coordinates
TYPE Transverse
  SEQUENCE
  CHARACTER(8)               :: ID
  REAL(8)                    :: Ko
  REAL(8)                    :: LatitudeOrigin
  REAL(8)                    :: LongitudeOrigin
END TYPE
!
!IERS International Reference Meridian
!
!                               Name                             Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),PARAMETER             :: dLongitudeIERS = 5.31d0/3.6d3  ! IERS/WGS84 Reference Meridian (deg)
REAL(8)                       :: dUT1           =  0.0d0        ! Delta between UT1 and UTC from IERS Bulletin-A (sec)
REAL(8)                       :: dTimeModel     =  0.0d0        ! Delta between JPL Horizons and CSP Model (sec)
!
! High precision Earth Rotation Angle
TYPE EarthRotationAngle
REAL(16)                      :: Angle                          ! Earth Rotation Angle                         (degrees)
REAL(16)                      :: Tu                             ! Universal Time reference to J2000 epoch         (days)
REAL(16)                      :: a = 0.7790572732640            ! Constant term in calculating ERA              (cycles)
REAL(16)                      :: b = 1.00273781191135448        ! Linear term calculating ERA               (cycles/day)
END TYPE EarthRotationAngle
!
!Interfaces to the U.S. Naval Observatory Vector Astronometric (NOVAS) Fortran Routines
!
!                               Name                             Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),PARAMETER          :: GeoCenter      = 0             ! Observer at Geocenter
INTEGER(4),PARAMETER          :: EarthSurface   = 1             ! Observer at Earth's surface
INTEGER(4),PARAMETER          :: NearEarth      = 2             ! Observer in near earth spacecraft
INTEGER(4),PARAMETER          :: PlanetoCentric = 0             ! Planetocentric coordinates
INTEGER(4),PARAMETER          :: EquinoxOfDate  = 1             ! True Equator and Equinox of Date
INTEGER(4),PARAMETER          :: CIOOfDate      = 2             ! True Equator and CIO of date
INTEGER(4),PARAMETER          :: Astrometric    = 3             ! Astrometric coordinates
INTERFACE NOVAS
  SUBROUTINE SETDT(deltaT)
    REAL(8)                   :: deltaT                         ! Time delta, TT - UT1 (seconds)
  END SUBROUTINE SETDT
  SUBROUTINE GEOPOS(JulianDate, Observer, Location_Vec, R_PCS, V_PCS)
    REAL(8)                   :: JulianDate                     ! Julian Date (days)
    INTEGER(4)                :: Observer                       ! Observer Type [Geocenter,NearEarth]
    REAL(8)                   :: Location_Vec(6)                ! Location of observer
    REAL(8)                   :: R_PCS(3)                       ! Position of observer (AU)
    REAL(8)                   :: V_PCS(3)                       ! Velocity of observer (AU/Day)
  END SUBROUTINE GEOPOS
  SUBROUTINE NOVAS_PLACE(JulianDate, IDBody, Observer, Coordinates, R_Star, Location_Vec, P_Sky)
    REAL(8)                   :: JulianDate                     ! Julian Date (days)
    CHARACTER(*)              :: IDBody                         ! Name of Solar System body
    INTEGER(4)                :: Observer                       ! Observer Type [Geocenter,NearEarth]
    INTEGER(4)                :: Coordinates                    ! Coordinates for output [Planetocentric ... Astrometric]
    REAL(8)                   :: R_Star(3)                      ! Location of star (not used)
    REAL(8)                   :: Location_Vec(6)                ! Location of observer
    REAL(8)                   :: P_Sky(7)                       ! Position and velocity of body wrt the observer
  END SUBROUTINE NOVAS_PLACE
  SUBROUTINE ZDAZ(UT1Date, Xpolar, Ypolar, Longitude, Latitude, Height, Ra, Dec, Refract, Zenith, Azimuth, RaR, DecR)
    REAL(8)                   :: UT1Date                        ! UT1 Date (days)
    REAL(8)                   :: Xpolar                         ! X coordinate of polar motion (arcseconds)
    REAL(8)                   :: Ypolar                         ! Y coordinate of polar motion (arcseconds)
    REAL(8)                   :: Longitude                      ! Longitude WGS84 (degrees)
    REAL(8)                   :: Latitude                       ! Latitude WGS84 (degrees)
    REAL(8)                   :: Height                         ! Height above reference ellipsoid (m)
    REAL(8)                   :: RA                             ! Right ascension (hours)
    REAL(8)                   :: Dec                            ! Declination (degrees)
    INTEGER(4)                :: Refract                        ! = 1 consider refraction, =0 ignore refraction
    REAL(8)                   :: Zenith                         ! Zenith angle (degrees)
    REAL(8)                   :: Azimuth                        ! Azimuth angle (degrees)
    REAL(8)                   :: RaR                            ! Right Ascension adjusted for refraction (hours)
    REAL(8)                   :: DecR                           ! Declination adjusted for refraction (degrees)
  END SUBROUTINE ZDAZ
END INTERFACE NOVAS
!
! Interfaces to Jet Propulsion Laboratories DE405/406/431 Fortran Routines
!
! JPL Parameters
!                              Name                              Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), PARAMETER         :: Mercury               =  1     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Venus                 =  2     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Earth                 =  3     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Mars                  =  4     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Jupiter               =  5     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Saturn                =  6     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Uranus                =  7     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Neptune               =  8     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Pluto                 =  9     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Moon                  = 10     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Sun                   = 11     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Solarsystem           = 12     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: SolarsystemBaryCenter = 12     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: EarthMoonBaryCenter   = 13     ! Body index for Pleph position calculations
INTEGER(4), PARAMETER         :: Nutations             = 14     ! Body index for Pleph Nutation calculations
INTEGER(4), PARAMETER         :: Librations            = 15     ! Body index for Pleph Libration calculations

TYPE Ellipsoid
  REAL(8)                     :: Ra      = 6378.1366d0          ! semi-major axis              (km)
  REAL(8)                     :: f       = 1d0/298.257223563d0  ! flattening             (fraction)
  REAL(8)                     :: Rb      = 6356.7519d0          ! semi-minor axis              (km)
  REAL(8)                     :: e       = 0.081819084d0        ! first eccentricity     (per-unit)
  REAL(8)                     :: e2      = 0.006694380d0        ! eccentricity squared   (per-unit)
  REAL(8)                     :: e2prime = 0.00673950168d0      ! eccentricity primed    (per-unit)
END TYPE Ellipsoid

TYPE   ,PUBLIC                :: AtmPerturbations               ! Atmospheric Effects
  SEQUENCE
  REAL(8)                     :: ElevKm                         ! Elevation                   (km)
  REAL(8)                     :: AngZenith                      ! True Zenith Angle       (degrees)
  REAL(8)                     :: Airmass                        ! Airmass                (per-unit)
  REAL(8)                     :: dAngZenith                     ! Zenith Delta Refraction (degrees)
  REAL(8)                     :: SolarFlux                      ! Solar Flux                (w/m^2)
END TYPE AtmPerturbations

TYPE WeatherData
  SEQUENCE
  REAL(8)                     :: Psia                  = 14.5d0 ! Site Pressure (psia)
  REAL(8)                     :: Pkpa                  = 10.0d2 ! Site Pressure (psia)
  REAL(8)                     :: TempF                 = 40.0d0 ! Site Temperature (F)
  REAL(8)                     :: TempC                 = 40.0d0 ! Site Temperature (C)
  REAL(8)                     :: SpeedWind             =  0.0d0 ! Windspeed (mph)
  REAL(8)                     :: AngleWind             =  0.0d0 ! Wind angle, clockwise from north, [0,360). (degrees)
  REAL(8)                     :: RelativeHumidity      = 20.0d0 ! Relative Humidity (%)
  REAL(8)                     :: Visibility            = 25.0d0 ! Visibility (km)
  REAL(8)                     :: FluxDNI               =  0.0d0 ! Solar Direct Normal Incident Radiation (w/m^2)
  REAL(8)                     :: SpeedWindPeak         =  0.0d0 ! Peak Windspeed (mph)
  REAL(8)                     :: Precipitation         =  0.0d0 ! Precipitation (inch)
  REAL(8)                     :: IntensityRainfall     =  0.0d0 ! Precipitation Rate (inch/hour)
END TYPE WeatherData

TYPE WeatherQuality
  SEQUENCE
  LOGICAL(4)                  :: Pressure              = .false.! Error in measurement of Site Pressure
  LOGICAL(4)                  :: Temperature           = .false.! Error in measurement of Site Temperature
  LOGICAL(4)                  :: SpeedWind             = .false.! Error in measurement of Windspeed
  LOGICAL(4)                  :: AngleWind             = .false.! Error in measurement of Wind angle
  LOGICAL(4)                  :: RelativeHumidity      = .false.! Error in measurement of Relative Humidity
  LOGICAL(4)                  :: Visibility            = .false.! Error in measurement of Visibility
  LOGICAL(4)                  :: FluxDNI               = .false.! Error in measurement of Solar Direct Normal Incident Radiation
  LOGICAL(4)                  :: SpeedWindPeak         = .false.! Error in measurement of Peak Windspeed
  LOGICAL(4)                  :: Precipitation         = .false.! Error in measurement of Precipitation
  LOGICAL(4)                  :: IntensityRainfall     = .false.! Error in measurement of Precipitation Rate
END TYPE WeatherQuality

TYPE LocationENU
  REAL(8)                     :: Longitude             = 0d0    ! Geodetic Longitude (degrees)
  REAL(8)                     :: Latitude              = 0d0    ! Geodetic Longitude (degrees)
  REAL(8)                     :: Elevation             = 0d0    ! Surface elevation  (km)
  REAL(8)                     :: T_ENU_PCS(3,3)                 ! PCS->ENU Coordinate Transformation
  REAL(8)                     :: T_PCS_ENU(3,3)                 ! ENU->PCS Coordinate Transformation
  REAL(8)                     :: Nx_PCS(3)                      ! East Basis Vector in PCS Frame
  REAL(8)                     :: Ny_PCS(3)                      ! North Basis Vector in PCS Frame
  REAL(8)                     :: Nz_PCS(3)                      ! Up Basis Vector in PCS Frame
  REAL(8)                     :: R_PCS (3)                      ! PCS Coordinates of Tower (AU)
  INTEGER(4)                  :: body                  = Earth  ! Solar System body
END TYPE LocationENU

TYPE, PUBLIC :: SolarData
  SEQUENCE
  INTEGER(MJD)                :: Time                           ! Modified Julian Date(10^-7 seconds)
  CHARACTER(24)               :: TimeStampLocal                 ! Timestamp in local timezone
  CHARACTER(24)               :: TimeStampUTC                   ! Timestamp in UTC
  CHARACTER(4)                :: FormatLocal                    ! TimeZone and Daylight saving
  CHARACTER(12)               :: Weekday                        ! Day of the week
  INTEGER(4)                  :: Year                           ! Year in local time
  INTEGER(4)                  :: Month                          ! Month in local time
  INTEGER(4)                  :: Day                            ! Day of the month in local time
  INTEGER(4)                  :: Hour                           ! Hour of the day local time
  INTEGER(4)                  :: Minute                         ! Hour of the day local time
  INTEGER(4)                  :: Second                         ! Second of the day local time
  REAL(8)                     :: jdt                            ! Julian Date (days)
  REAL(8)                     :: AngleSolarDisk                 ! Solar disk solid angle (degrees)
  REAL(8)                     :: DistanceSun                    ! Distance to the sun (km)
  REAL(8)                     :: AngleAzimuthal                 ! Sun azimuthal angle (degrees)
  REAL(8)                     :: AngleElevation                 ! Sun apparent elevation angle (degrees)
  REAL(8)                     :: AngleElevation0                ! Sun elevation angle (degrees)
  REAL(8)                     :: AngleZenith                    ! Sun apparent zenith angle (degrees)
  REAL(8)                     :: AngleZenith0                   ! Sun zenith angle (degrees)
  REAL(8)                     :: Attenuation                    ! Attenuation of solar flux (ratio)
  REAL(8)                     :: SolarFlux                      ! Solar flux (w/m^2)
  REAL(8)                     :: SolarFraction                  ! Fraction of sun above horizon
  REAL(8)                     :: Rts_ENU(3)                     ! ENU Vector Tower->Sun (km)
  REAL(8)                     :: Nts_ENU(3)                     ! ENU Normal Vector Tower->Sun
  REAL(8)                     :: Rtas_ENU(3)                    ! ENU Vector Tower->ApparentSun (km)
  REAL(8)                     :: Ntas_ENU(3)                    ! ENU Normal Vector Tower->ApparentSun
  REAL(8)                     :: Rts_PCS(3)                     ! PCS Vector Tower to Sun (km)
  REAL(8)                     :: Nts_PCS(3)                     ! PCS Normalized Vector Sun to Tower
  REAL(8)                     :: T_PCS_CRS(3,3)                 ! Tranform CRS->PCS at time
END TYPE SolarData
!
!----------------------------+----------------------------------+-------------------------------------------------------
TYPE, PUBLIC :: JPL_Vector
  SEQUENCE                                                      ! Celestial Reference System (CRS)
  REAL(8)                     :: R_CRS(3)                       ! Position (AU)
  REAL(8)                     :: V_CRS(3)                       ! Velocity (AU/Day)
END TYPE JPL_Vector

TYPE, PUBLIC :: JPL_Nutation
  SEQUENCE
  REAL(8)                     :: Values(6)                       ! Longitude Nutation (Radians)
END TYPE JPL_Nutation

INTERFACE JPL
  SUBROUTINE CONST(Name_JPL, Val_JPL, VAl_SSS, JPLEnd)
    CHARACTER(6)              :: NAME_JPL(1000)
    REAL(8)                   :: VAL_JPL (1000)
    REAL(8)                   :: VAL_SSS (3)
    INTEGER(4)                :: JPLEnd
  END SUBROUTINE CONST
  SUBROUTINE PLEPH(jdt, BodyTarget, BodyOrigin, JPL_Vec)
    REAL(8)                   :: jdt
    INTEGER(4)                :: BodyTarget
    INTEGER(4)                :: BodyOrigin
    REAL(8)                   :: JPL_Vec(6)
  END SUBROUTINE PLEPH
  SUBROUTINE DPLEPH(jdt, BodyTarget, BodyOrigin, JPL_Vec)
    REAL(8)                   :: jdt(2)
    INTEGER(4)                :: BodyTarget
    INTEGER(4)                :: BodyOrigin
    REAL(8)                   :: JPL_Vec(6)
  END SUBROUTINE DPLEPH
END INTERFACE JPL
TYPE, PUBLIC :: JPLRange
  SEQUENCE
  REAL(8)                     :: jdtStart
  REAL(8)                     :: jdtEnd
  REAL(8)                     :: jdtDelta
END TYPE JPLRange
!
! Public  Values               Name                              Description / (units)
!-----------------------------+----------------------------------+------------------------------------------------------
TYPE(JPLRange) ,SAVE          :: EPHRange
REAL(8)                       :: VAL_SSS(3)
EQUIVALENCE                     (VAL_SSS,EPHRange)
INTEGER(4)   , PARAMETER      :: BodyEnd  = Sun                  ! The Sun is the last true body in the JPL Ephemeris
CHARACTER(12), PARAMETER      :: ID_Body(SolarSystem)   = &
                              (/'MERCURY    '             &
                               ,'VENUS      '             &
                               ,'EARTH      '             &
                               ,'MARS       '             &
                               ,'JUPITER    '             &
                               ,'SATURN     '             &
                               ,'URANUS     '             &
                               ,'NEPTUNE    '             &
                               ,'PLUTO      '             &
                               ,'MOON       '             &
                               ,'SUN        '             &
                               ,'SOLARSYSTEM'             &
                              /)
TYPE(Ellipsoid)               :: Ellipsoid_Body(BodyEnd)
CHARACTER(6)                  :: NAME_JPL(1000)

TYPE, PUBLIC :: JPLValues
  SEQUENCE
  REAL(8)                     :: DENUM       !   431.00000000000000
  REAL(8)                     :: LENUM       !   431.00000000000000
  REAL(8)                     :: TDATEF      !   20130526104458.000
  REAL(8)                     :: TDATEB      !   20130524193151.000
  REAL(8)                     :: JDEPOC      !   2440400.5000000000
  REAL(8)                     :: CENTER      !   0.0000000000000000
  REAL(8)                     :: CLIGHT      !   299792.45799999998
  REAL(8)                     :: BETA        !   1.0000000000000000
  REAL(8)                     :: GAMMA       !   1.0000000000000000
  REAL(8)                     :: AU          !   149597870.69999999
  REAL(8)                     :: EMRAT       !   81.300569074190619
  REAL(8)                     :: GM1         !  0.49124804503647600E-10
  REAL(8)                     :: GM2         !  0.72434523326441197E-09
  REAL(8)                     :: GMB         !  0.89970113901998714E-09
  REAL(8)                     :: GM4         !  0.95495486955507705E-10
  REAL(8)                     :: GM5         !  0.28253458408338699E-06
  REAL(8)                     :: GM6         !  0.84597060732450306E-07
  REAL(8)                     :: GM7         !  0.12920248257829600E-07
  REAL(8)                     :: GM8         !  0.15243573478851101E-07
  REAL(8)                     :: GM9         !  0.21784410519741800E-11
  REAL(8)                     :: GMS         !  0.29591220828559109E-03
  REAL(8)                     :: XS          !  0.45025087846405536E-02
  REAL(8)                     :: YS          !  0.76707642709100709E-03
  REAL(8)                     :: ZS          !  0.26605791776697769E-03
  REAL(8)                     :: XDS         ! -0.35174953607552312E-06
  REAL(8)                     :: YDS         !  0.51776264098334048E-05
  REAL(8)                     :: ZDS         !  0.22291021789120292E-05
  REAL(8)                     :: X1          !  0.36176271656028203
  REAL(8)                     :: Y1          ! -0.90781972156766005E-01
  REAL(8)                     :: Z1          ! -0.85714972562751165E-01
  REAL(8)                     :: XD1         !  0.33674939720057589E-02
  REAL(8)                     :: YD1         !  0.24894520557683431E-01
  REAL(8)                     :: ZD1         !  0.12946300409704089E-01
  REAL(8)                     :: X2          !  0.61275194083507223
  REAL(8)                     :: Y2          ! -0.34836536903362220
  REAL(8)                     :: Z2          ! -0.19527828667594380
  REAL(8)                     :: XD2         !  0.10952068423528230E-01
  REAL(8)                     :: YD2         !  0.15617684267867679E-01
  REAL(8)                     :: ZD2         !  0.63311057029778644E-02
  REAL(8)                     :: XB          !  0.12051741410138470
  REAL(8)                     :: YB          ! -0.92583847476914871
  REAL(8)                     :: ZB          ! -0.40154022645315218
  REAL(8)                     :: XDB         !  0.16811268309783790E-01
  REAL(8)                     :: YDB         !  0.17483092307343439E-02
  REAL(8)                     :: ZDB         !  0.75820289738312910E-03
  REAL(8)                     :: X4          ! -0.11018607714879820
  REAL(8)                     :: Y4          !  -1.3275994503029831
  REAL(8)                     :: Z4          ! -0.60588914048429143
  REAL(8)                     :: XD4         !  0.14481653057047570E-01
  REAL(8)                     :: YD4         !  0.24246307683646861E-03
  REAL(8)                     :: ZD4         ! -0.28152072792433881E-03
  REAL(8)                     :: X5          !  -5.3797067685539366
  REAL(8)                     :: Y5          ! -0.83048132656339779
  REAL(8)                     :: Z5          ! -0.22482887442656549
  REAL(8)                     :: XD5         !  0.10920125942373380E-02
  REAL(8)                     :: YD5         ! -0.65181166128073838E-02
  REAL(8)                     :: ZD5         ! -0.28207827622986790E-02
  REAL(8)                     :: X6          !   7.8943906829095312
  REAL(8)                     :: Y6          !   4.5964780551712732
  REAL(8)                     :: Z6          !   1.5586958428319000
  REAL(8)                     :: XD6         ! -0.32175565165009161E-02
  REAL(8)                     :: YD6         !  0.43358103417466256E-02
  REAL(8)                     :: ZD6         !  0.19286463168601551E-02
  REAL(8)                     :: X7          !  -18.265402253872359
  REAL(8)                     :: Y7          !  -1.1619554186758700
  REAL(8)                     :: Z7          ! -0.25010605772133798
  REAL(8)                     :: XD7         !  0.22119039101561470E-03
  REAL(8)                     :: YD7         ! -0.37624750081088438E-02
  REAL(8)                     :: ZD7         ! -0.16510150274299501E-02
  REAL(8)                     :: X8          !  -16.055035780233371
  REAL(8)                     :: Y8          !  -23.942191559854709
  REAL(8)                     :: Z8          !  -9.4001579688023948
  REAL(8)                     :: XD8         !  0.26427698479800552E-02
  REAL(8)                     :: YD8         ! -0.14983125505409779E-02
  REAL(8)                     :: ZD8         ! -0.67904196080291327E-03
  REAL(8)                     :: X9          !  -30.483313767183841
  REAL(8)                     :: Y9          ! -0.87240555684105014
  REAL(8)                     :: Z9          !   8.9115761724995508
  REAL(8)                     :: XD9         !  0.32220737349778080E-03
  REAL(8)                     :: YD9         ! -0.31435763936453289E-02
  REAL(8)                     :: ZD9         ! -0.10779497595973129E-02
  REAL(8)                     :: XM          ! -0.80817734612081602E-03
  REAL(8)                     :: YM          ! -0.19946299868838198E-02
  REAL(8)                     :: ZM          ! -0.10872626830592670E-02
  REAL(8)                     :: XDM         !  0.60108481618792106E-03
  REAL(8)                     :: YDM         ! -0.16744546783318669E-03
  REAL(8)                     :: ZDM         ! -0.85562141195294665E-04
  REAL(8)                     :: XC          !   0.0000000000000000
  REAL(8)                     :: YC          !   0.0000000000000000
  REAL(8)                     :: ZC          !   0.0000000000000000
  REAL(8)                     :: XDC         !   0.0000000000000000
  REAL(8)                     :: YDC         !   0.0000000000000000
  REAL(8)                     :: ZDC         !   0.0000000000000000
  REAL(8)                     :: PHI         !  0.51283729839643596E-02
  REAL(8)                     :: THT         !  0.38239283974808169
  REAL(8)                     :: PSI         !   1.2941681541841259
  REAL(8)                     :: OMEGAX      !  0.45740268715155188E-04
  REAL(8)                     :: OMEGAY      ! -0.21967910928663370E-05
  REAL(8)                     :: OMEGAZ      !  0.22994485882801841
  REAL(8)                     :: PHIC        ! -0.24199092704068411E-02
  REAL(8)                     :: THTC        !  0.41101946488652730
  REAL(8)                     :: PSIC        ! -0.46309468558363681
  REAL(8)                     :: OMGCX       ! -0.64545028083314582E-02
  REAL(8)                     :: OMGCY       ! -0.12104389145708161E-02
  REAL(8)                     :: OMGCZ       !  0.22711838612562740
  REAL(8)                     :: ASUN        !   696000.00000000000
  REAL(8)                     :: J2SUN       !  0.21106088532726840E-06
  REAL(8)                     :: J3SUN       !   0.0000000000000000
  REAL(8)                     :: J4SUN       !   0.0000000000000000
  REAL(8)                     :: RE          !   6378.1366000000001
  REAL(8)                     :: J2E         !  0.10826254500000000E-02
  REAL(8)                     :: J3E         ! -0.25324100000000000E-05
  REAL(8)                     :: J4E         ! -0.16198980000000001E-05
  REAL(8)                     :: J5E         ! -0.22773450000000001E-06
  REAL(8)                     :: J2EDOT      ! -0.26000000000000001E-10
  REAL(8)                     :: K2E0        !  0.33500000000000002
  REAL(8)                     :: K2E1        !  0.32000000000000001
  REAL(8)                     :: K2E2        !  0.32000000000000001
  REAL(8)                     :: TAUE0       !  0.64000000000000001E-01
  REAL(8)                     :: TAUE1       ! -0.43999999999999991E-01
  REAL(8)                     :: TAUE2       ! -0.10000000000000001
  REAL(8)                     :: TAUER1      !  0.73632190228041889E-02
  REAL(8)                     :: TAUER2      !  0.25881243840999318E-02
  REAL(8)                     :: ROTEX       !  0.56754203322893467E-02
  REAL(8)                     :: ROTEY       ! -0.17022656914989529E-01
  REAL(8)                     :: DROTEX      !  0.27689915574483548E-03
  REAL(8)                     :: DROTEY      ! -0.12118591216559241E-02
  REAL(8)                     :: AM          !   1738.0000000000000
  REAL(8)                     :: K2M         !  0.24059000000000001E-01
  REAL(8)                     :: TAUM        !  0.20747145283083851
  REAL(8)                     :: LBET        !  0.63102084400807018E-03
  REAL(8)                     :: LGAM        !  0.22773287841299349E-03
  REAL(8)                     :: IFAC        !  0.69999999999999999E-03
  REAL(8)                     :: COBLAT      !  0.24623904789198149E-03
  REAL(8)                     :: KVC         !   0.0000000000000000
  REAL(8)                     :: PSIDOT      !   0.0000000000000000
  REAL(8)                     :: J2M         !  0.20321568464952571E-03
  REAL(8)                     :: C22M        !  0.22382900680455860E-04
  REAL(8)                     :: J3M         !  0.84597026974594565E-05
  REAL(8)                     :: C31M        !  0.28480741195592859E-04
  REAL(8)                     :: S31M        !  0.58915551555318644E-05
  REAL(8)                     :: C32M        !  0.48449420619770601E-05
  REAL(8)                     :: S32M        !  0.16844743962783901E-05
  REAL(8)                     :: C33M        !  0.16756178134114571E-05
  REAL(8)                     :: S33M        ! -0.24742714379805761E-06
  REAL(8)                     :: J4M         ! -0.97044138365700002E-05
  REAL(8)                     :: C41M        ! -0.57048697319733211E-05
  REAL(8)                     :: S41M        !  0.15789202789245719E-05
  REAL(8)                     :: C42M        ! -0.15912271792977430E-05
  REAL(8)                     :: S42M        ! -0.15153915796731720E-05
  REAL(8)                     :: C43M        ! -0.80678881596778210E-07
  REAL(8)                     :: S43M        ! -0.80349266627431065E-06
  REAL(8)                     :: C44M        ! -0.12692158612216040E-06
  REAL(8)                     :: S44M        !  0.82964257754075217E-07
  REAL(8)                     :: J5M         !  0.74221608384052886E-06
  REAL(8)                     :: C51M        ! -0.86629769308983564E-06
  REAL(8)                     :: S51M        ! -0.35272289393243822E-05
  REAL(8)                     :: C52M        !  0.71199537967353327E-06
  REAL(8)                     :: S52M        !  0.17107886673430380E-06
  REAL(8)                     :: C53M        !  0.15399750424904521E-07
  REAL(8)                     :: S53M        !  0.28736257616334340E-06
  REAL(8)                     :: C54M        !  0.21444704319218451E-07
  REAL(8)                     :: S54M        !  0.52652110720146801E-09
  REAL(8)                     :: C55M        !  0.76596153884006136E-08
  REAL(8)                     :: S55M        ! -0.67824035473995331E-08
  REAL(8)                     :: J6M         ! -0.13767531350969900E-04
  REAL(8)                     :: C61M        !  0.12024363601545921E-05
  REAL(8)                     :: S61M        ! -0.20453507141252220E-05
  REAL(8)                     :: C62M        ! -0.54703897324156854E-06
  REAL(8)                     :: S62M        ! -0.26966834353574270E-06
  REAL(8)                     :: C63M        ! -0.68785612757292004E-07
  REAL(8)                     :: S63M        ! -0.71063745295915786E-07
  REAL(8)                     :: C64M        !  0.12915580402925161E-08
  REAL(8)                     :: S64M        ! -0.15361616966632301E-07
  REAL(8)                     :: C65M        !  0.11737698784460501E-08
  REAL(8)                     :: S65M        ! -0.83465073195142514E-08
  REAL(8)                     :: C66M        ! -0.10913395178881539E-08
  REAL(8)                     :: S66M        !  0.16844213702632920E-08
  REAL(8)                     :: J7M         !   0.0000000000000000
  REAL(8)                     :: J8M         !   0.0000000000000000
  REAL(8)                     :: J9M         !   0.0000000000000000
  REAL(8)                     :: C71M        !   0.0000000000000000
  REAL(8)                     :: S71M        !   0.0000000000000000
  REAL(8)                     :: C72M        !   0.0000000000000000
  REAL(8)                     :: S72M        !   0.0000000000000000
  REAL(8)                     :: C73M        !   0.0000000000000000
  REAL(8)                     :: S73M        !   0.0000000000000000
  REAL(8)                     :: C74M        !   0.0000000000000000
  REAL(8)                     :: S74M        !   0.0000000000000000
  REAL(8)                     :: C75M        !   0.0000000000000000
  REAL(8)                     :: S75M        !   0.0000000000000000
  REAL(8)                     :: C76M        !   0.0000000000000000
  REAL(8)                     :: S76M        !   0.0000000000000000
  REAL(8)                     :: C77M        !   0.0000000000000000
  REAL(8)                     :: S77M        !   0.0000000000000000
  REAL(8)                     :: C81M        !   0.0000000000000000
  REAL(8)                     :: S81M        !   0.0000000000000000
  REAL(8)                     :: C82M        !   0.0000000000000000
  REAL(8)                     :: S82M        !   0.0000000000000000
  REAL(8)                     :: C83M        !   0.0000000000000000
  REAL(8)                     :: S83M        !   0.0000000000000000
  REAL(8)                     :: C84M        !   0.0000000000000000
  REAL(8)                     :: S84M        !   0.0000000000000000
  REAL(8)                     :: C85M        !   0.0000000000000000
  REAL(8)                     :: S85M        !   0.0000000000000000
  REAL(8)                     :: C86M        !   0.0000000000000000
  REAL(8)                     :: S86M        !   0.0000000000000000
  REAL(8)                     :: C87M        !   0.0000000000000000
  REAL(8)                     :: S87M        !   0.0000000000000000
  REAL(8)                     :: C88M        !   0.0000000000000000
  REAL(8)                     :: S88M        !   0.0000000000000000
  REAL(8)                     :: C91M        !   0.0000000000000000
  REAL(8)                     :: S91M        !   0.0000000000000000
  REAL(8)                     :: C92M        !   0.0000000000000000
  REAL(8)                     :: S92M        !   0.0000000000000000
  REAL(8)                     :: C93M        !   0.0000000000000000
  REAL(8)                     :: S93M        !   0.0000000000000000
  REAL(8)                     :: C94M        !   0.0000000000000000
  REAL(8)                     :: S94M        !   0.0000000000000000
  REAL(8)                     :: C95M        !   0.0000000000000000
  REAL(8)                     :: S95M        !   0.0000000000000000
  REAL(8)                     :: C96M        !   0.0000000000000000
  REAL(8)                     :: S96M        !   0.0000000000000000
  REAL(8)                     :: C97M        !   0.0000000000000000
  REAL(8)                     :: S97M        !   0.0000000000000000
  REAL(8)                     :: C98M        !   0.0000000000000000
  REAL(8)                     :: S98M        !   0.0000000000000000
  REAL(8)                     :: C99M        !   0.0000000000000000
  REAL(8)                     :: S99M        !   0.0000000000000000
  REAL(8)                     :: MA0001      !  0.14004765561723440E-12
  REAL(8)                     :: MA0002      !  0.31044481989387133E-13
  REAL(8)                     :: MA0003      !  0.36175383171479368E-14
  REAL(8)                     :: MA0004      !  0.38547501878088103E-13
  REAL(8)                     :: MA0005      !  0.37487362845520318E-15
  REAL(8)                     :: MA0006      !  0.83124192126733721E-15
  REAL(8)                     :: MA0007      !  0.21364344425714072E-14
  REAL(8)                     :: MA0008      !  0.58942565297069081E-15
  REAL(8)                     :: MA0009      !  0.10778410042407301E-14
  REAL(8)                     :: MA0010      !  0.12358007872941250E-13
  REAL(8)                     :: MA0011      !  0.13315362554599750E-14
  REAL(8)                     :: MA0012      !  0.19317757851829201E-15
  REAL(8)                     :: MA0013      !  0.17970048945074460E-14
  REAL(8)                     :: MA0014      !  0.11006456795750681E-14
  REAL(8)                     :: MA0015      !  0.46783074183509050E-14
  REAL(8)                     :: MA0016      !  0.34115868261938121E-14
  REAL(8)                     :: MA0017      !  0.20815063964697380E-15
  REAL(8)                     :: MA0018      !  0.20089277366511321E-15
  REAL(8)                     :: MA0019      !  0.10356448401311940E-14
  REAL(8)                     :: MA0020      !  0.91998074776309115E-16
  REAL(8)                     :: MA0021      !  0.25294428720409989E-15
  REAL(8)                     :: MA0022      !  0.12026244434834601E-14
  REAL(8)                     :: MA0023      !  0.18953317604197831E-15
  REAL(8)                     :: MA0024      !  0.18939016675253820E-14
  REAL(8)                     :: MA0025      !  0.72398415223662114E-16
  REAL(8)                     :: MA0026      !  0.16373439522610839E-15
  REAL(8)                     :: MA0027      !  0.38880038985457882E-15
  REAL(8)                     :: MA0028      !  0.29262727442945278E-15
  REAL(8)                     :: MA0029      !  0.19758423651245199E-14
  REAL(8)                     :: MA0030      !  0.14820190164375290E-15
  REAL(8)                     :: MA0031      !  0.63432804736486023E-14
  REAL(8)                     :: MA0032      !  0.11995850162334401E-15
  REAL(8)                     :: MA0034      !  0.29445412915212858E-15
  REAL(8)                     :: MA0035      !  0.23522561732418408E-15
  REAL(8)                     :: MA0036      !  0.16970600184097089E-15
  REAL(8)                     :: MA0037      !  0.21856205771130561E-15
  REAL(8)                     :: MA0038      !  0.13232859647467681E-15
  REAL(8)                     :: MA0039      !  0.14975196825567009E-14
  REAL(8)                     :: MA0040      !  0.29524140803084219E-15
  REAL(8)                     :: MA0041      !  0.93242237621988685E-15
  REAL(8)                     :: MA0042      !  0.27653166434743811E-15
  REAL(8)                     :: MA0043      !  0.72753938533407125E-16
  REAL(8)                     :: MA0044      !  0.46886407201292200E-16
  REAL(8)                     :: MA0045      !  0.84256780185679339E-15
  REAL(8)                     :: MA0046      !  0.32728000000000001E-15
  REAL(8)                     :: MA0047      !  0.55435235615988891E-15
  REAL(8)                     :: MA0048      !  0.25310917260150679E-14
  REAL(8)                     :: MA0049      !  0.75494816293144014E-16
  REAL(8)                     :: MA0050      !  0.16333263911175179E-15
  REAL(8)                     :: MA0051      !  0.25705491133531448E-15
  REAL(8)                     :: MA0052      !  0.24767881012558671E-14
  REAL(8)                     :: MA0053      !  0.62392433107751650E-16
  REAL(8)                     :: MA0054      !  0.56241736501924590E-15
  REAL(8)                     :: MA0056      !  0.36992883127021258E-15
  REAL(8)                     :: MA0057      !  0.36806019206396510E-15
  REAL(8)                     :: MA0058      !  0.84811739114660024E-16
  REAL(8)                     :: MA0059      !  0.63394427275876513E-15
  REAL(8)                     :: MA0060      !  0.50911367830144641E-16
  REAL(8)                     :: MA0062      !  0.10890481919600570E-15
  REAL(8)                     :: MA0063      !  0.56404017439762427E-16
  REAL(8)                     :: MA0065      !  0.31806592826525409E-14
  REAL(8)                     :: MA0068      !  0.34310265912379692E-15
  REAL(8)                     :: MA0069      !  0.51446100208767355E-15
  REAL(8)                     :: MA0070      !  0.27688888401578460E-15
  REAL(8)                     :: MA0071      !  0.14244927463509561E-15
  REAL(8)                     :: MA0072      !  0.79950510449165411E-16
  REAL(8)                     :: MA0074      !  0.35073744512956140E-15
  REAL(8)                     :: MA0075      !  0.43573746250771268E-16
  REAL(8)                     :: MA0076      !  0.83122000000000012E-15
  REAL(8)                     :: MA0077      !  0.49312955095007287E-16
  REAL(8)                     :: MA0078      !  0.84019062534638874E-16
  REAL(8)                     :: MA0079      !  0.83518243314079403E-16
  REAL(8)                     :: MA0080      !  0.11614439541131079E-15
  REAL(8)                     :: MA0081      !  0.10223675545561340E-15
  REAL(8)                     :: MA0082      !  0.66012607669307705E-16
  REAL(8)                     :: MA0083      !  0.10968348906260350E-15
  REAL(8)                     :: MA0084      !  0.12573126556318860E-15
  REAL(8)                     :: MA0085      !  0.92540854530185373E-15
  REAL(8)                     :: MA0086      !  0.21523995570228910E-15
  REAL(8)                     :: MA0087      !  0.21992951735740729E-14
  REAL(8)                     :: MA0088      !  0.25771141273110471E-14
  REAL(8)                     :: MA0089      !  0.34020311574394289E-15
  REAL(8)                     :: MA0090      !  0.12351963628284910E-15
  REAL(8)                     :: MA0091      !  0.24404616777010059E-15
  REAL(8)                     :: MA0092      !  0.40369435176860728E-15
  REAL(8)                     :: MA0093      !  0.56477307179764760E-15
  REAL(8)                     :: MA0094      !  0.12792300000000001E-14
  REAL(8)                     :: MA0095      !  0.27166197083932592E-15
  REAL(8)                     :: MA0096      !  0.15465676956243251E-14
  REAL(8)                     :: MA0097      !  0.10314956358376310E-15
  REAL(8)                     :: MA0098      !  0.24428317417320688E-15
  REAL(8)                     :: MA0099      !  0.73526620138415914E-16
  REAL(8)                     :: MA0100      !  0.13659771964686970E-15
  REAL(8)                     :: MA0102      !  0.12812662256605979E-15
  REAL(8)                     :: MA0103      !  0.10011180168586460E-15
  REAL(8)                     :: MA0104      !  0.48122396678018733E-15
  REAL(8)                     :: MA0105      !  0.37154667975341450E-15
  REAL(8)                     :: MA0106      !  0.53973999999999977E-15
  REAL(8)                     :: MA0107      !  0.16717209917006440E-14
  REAL(8)                     :: MA0109      !  0.10826185861581930E-15
  REAL(8)                     :: MA0110      !  0.14076985722105040E-15
  REAL(8)                     :: MA0111      !  0.33519192811280561E-16
  REAL(8)                     :: MA0112      !  0.57960397015532352E-16
  REAL(8)                     :: MA0113      !  0.25580213924578190E-16
  REAL(8)                     :: MA0114      !  0.17050000000000001E-15
  REAL(8)                     :: MA0115      !  0.55258241903855268E-16
  REAL(8)                     :: MA0117      !  0.44713680178417893E-15
  REAL(8)                     :: MA0118      !  0.27000759625981351E-16
  REAL(8)                     :: MA0120      !  0.11889849499520080E-14
  REAL(8)                     :: MA0121      !  0.70079069220413433E-15
  REAL(8)                     :: MA0124      !  0.88772708265623384E-16
  REAL(8)                     :: MA0127      !  0.36611682430617228E-15
  REAL(8)                     :: MA0128      !  0.96501295105487519E-15
  REAL(8)                     :: MA0129      !  0.46542452473979754E-15
  REAL(8)                     :: MA0130      !  0.99366295459092486E-15
  REAL(8)                     :: MA0132      !  0.13196141221701560E-16
  REAL(8)                     :: MA0134      !  0.33620465420887158E-15
  REAL(8)                     :: MA0135      !  0.95156050418462075E-16
  REAL(8)                     :: MA0137      !  0.85612605995538922E-15
  REAL(8)                     :: MA0139      !  0.42242882143774452E-15
  REAL(8)                     :: MA0140      !  0.31316732532224061E-15
  REAL(8)                     :: MA0141      !  0.37661406016708328E-15
  REAL(8)                     :: MA0143      !  0.11260861147488829E-15
  REAL(8)                     :: MA0144      !  0.69951633549830869E-15
  REAL(8)                     :: MA0145      !  0.75582329262288917E-15
  REAL(8)                     :: MA0146      !  0.39415999999999998E-15
  REAL(8)                     :: MA0147      !  0.40681729177164312E-15
  REAL(8)                     :: MA0148      !  0.16595259516345301E-15
  REAL(8)                     :: MA0150      !  0.45336468487968124E-15
  REAL(8)                     :: MA0154      !  0.82983887671636944E-15
  REAL(8)                     :: MA0156      !  0.26336000000000000E-15
  REAL(8)                     :: MA0159      !  0.28824066183805001E-15
  REAL(8)                     :: MA0160      !  0.10248487058744050E-15
  REAL(8)                     :: MA0162      !  0.23729075379342011E-15
  REAL(8)                     :: MA0163      !  0.86580000000000006E-16
  REAL(8)                     :: MA0164      !  0.41832602031724900E-15
  REAL(8)                     :: MA0165      !  0.10684599027046931E-14
  REAL(8)                     :: MA0168      !  0.53470999999999977E-15
  REAL(8)                     :: MA0171      !  0.15109884909293550E-15
  REAL(8)                     :: MA0172      !  0.43177157644920528E-16
  REAL(8)                     :: MA0173      !  0.39124285470831010E-15
  REAL(8)                     :: MA0175      !  0.28990971962115819E-15
  REAL(8)                     :: MA0176      !  0.16713475520093611E-15
  REAL(8)                     :: MA0177      !  0.45372886294250988E-16
  REAL(8)                     :: MA0181      !  0.16796766833548251E-15
  REAL(8)                     :: MA0185      !  0.11355858944839220E-14
  REAL(8)                     :: MA0187      !  0.94193644635953110E-15
  REAL(8)                     :: MA0191      !  0.17252555236382890E-15
  REAL(8)                     :: MA0192      !  0.25119412656578328E-15
  REAL(8)                     :: MA0194      !  0.27230587289059820E-15
  REAL(8)                     :: MA0195      !  0.83548198507378076E-16
  REAL(8)                     :: MA0196      !  0.45014136561179280E-15
  REAL(8)                     :: MA0198      !  0.20538576829083310E-16
  REAL(8)                     :: MA0200      !  0.17554616782526139E-15
  REAL(8)                     :: MA0201      !  0.10147296895716561E-15
  REAL(8)                     :: MA0203      !  0.18492472080300540E-15
  REAL(8)                     :: MA0205      !  0.91514999225958938E-16
  REAL(8)                     :: MA0206      !  0.11789669849357310E-15
  REAL(8)                     :: MA0209      !  0.25931073098872930E-15
  REAL(8)                     :: MA0210      !  0.13145378148802101E-15
  REAL(8)                     :: MA0211      !  0.30465115564904100E-15
  REAL(8)                     :: MA0212      !  0.38674996577516789E-15
  REAL(8)                     :: MA0213      !  0.95223891872172389E-16
  REAL(8)                     :: MA0216      !  0.69079712474674255E-15
  REAL(8)                     :: MA0221      !  0.18168406201733031E-15
  REAL(8)                     :: MA0223      !  0.13906039685198460E-15
  REAL(8)                     :: MA0224      !  0.39520590343452718E-16
  REAL(8)                     :: MA0225      !  0.41456571725352779E-15
  REAL(8)                     :: MA0227      !  0.12048459545465019E-15
  REAL(8)                     :: MA0230      !  0.17608987155151350E-15
  REAL(8)                     :: MA0233      !  0.19715919666254549E-15
  REAL(8)                     :: MA0236      !  0.11363293901133810E-15
  REAL(8)                     :: MA0238      !  0.52966693580739110E-15
  REAL(8)                     :: MA0240      !  0.11048622528732650E-15
  REAL(8)                     :: MA0241      !  0.30054836259460052E-15
  REAL(8)                     :: MA0247      !  0.27097270724162128E-15
  REAL(8)                     :: MA0250      !  0.19688550185992420E-15
  REAL(8)                     :: MA0259      !  0.62808585539363831E-15
  REAL(8)                     :: MA0266      !  0.18413301875237819E-15
  REAL(8)                     :: MA0268      !  0.52669011092543478E-15
  REAL(8)                     :: MA0275      !  0.18654000000000000E-15
  REAL(8)                     :: MA0276      !  0.22481550488436832E-15
  REAL(8)                     :: MA0283      !  0.20528101779869579E-15
  REAL(8)                     :: MA0287      !  0.48033737856096102E-16
  REAL(8)                     :: MA0303      !  0.28848684039510109E-15
  REAL(8)                     :: MA0304      !  0.68832710966054010E-16
  REAL(8)                     :: MA0308      !  0.72405478852581394E-15
  REAL(8)                     :: MA0313      !  0.11584591337407960E-15
  REAL(8)                     :: MA0322      !  0.82763495213043584E-16
  REAL(8)                     :: MA0324      !  0.13886265898561990E-14
  REAL(8)                     :: MA0326      !  0.18915247466562089E-15
  REAL(8)                     :: MA0328      !  0.29127000000000000E-15
  REAL(8)                     :: MA0329      !  0.51143093942960568E-16
  REAL(8)                     :: MA0334      !  0.32624933794345618E-15
  REAL(8)                     :: MA0335      !  0.14756769258889111E-15
  REAL(8)                     :: MA0336      !  0.56429999999999999E-16
  REAL(8)                     :: MA0337      !  0.34515074511865959E-16
  REAL(8)                     :: MA0338      !  0.51407683428632203E-16
  REAL(8)                     :: MA0344      !  0.53689097042583348E-15
  REAL(8)                     :: MA0345      !  0.12317521169884131E-15
  REAL(8)                     :: MA0346      !  0.22099166077176559E-15
  REAL(8)                     :: MA0347      !  0.21949122177817369E-16
  REAL(8)                     :: MA0349      !  0.70078739271302920E-15
  REAL(8)                     :: MA0350      !  0.14100078405763091E-15
  REAL(8)                     :: MA0354      !  0.15850986571596891E-14
  REAL(8)                     :: MA0356      !  0.26816131961351822E-15
  REAL(8)                     :: MA0357      !  0.17619560261002570E-15
  REAL(8)                     :: MA0358      !  0.12178097995831790E-15
  REAL(8)                     :: MA0360      !  0.33677605743530068E-15
  REAL(8)                     :: MA0362      !  0.80870792881032834E-16
  REAL(8)                     :: MA0363      !  0.69841945302118664E-16
  REAL(8)                     :: MA0365      !  0.77695552096218006E-16
  REAL(8)                     :: MA0366      !  0.11563954713645831E-15
  REAL(8)                     :: MA0369      !  0.57345815663368579E-16
  REAL(8)                     :: MA0372      !  0.17459557262705000E-14
  REAL(8)                     :: MA0373      !  0.13720617596137790E-15
  REAL(8)                     :: MA0375      !  0.45585992488069247E-15
  REAL(8)                     :: MA0377      !  0.14144228920898079E-15
  REAL(8)                     :: MA0381      !  0.35934934807617610E-15
  REAL(8)                     :: MA0385      !  0.11031851822968280E-15
  REAL(8)                     :: MA0386      !  0.15079333711965190E-14
  REAL(8)                     :: MA0387      !  0.10044659839630940E-15
  REAL(8)                     :: MA0388      !  0.34333444128591692E-15
  REAL(8)                     :: MA0389      !  0.62251018387380329E-16
  REAL(8)                     :: MA0393      !  0.15586000000000001E-15
  REAL(8)                     :: MA0404      !  0.14521678954842191E-15
  REAL(8)                     :: MA0405      !  0.55764804768085391E-15
  REAL(8)                     :: MA0407      !  0.15678504140474829E-15
  REAL(8)                     :: MA0409      !  0.82144999999999987E-15
  REAL(8)                     :: MA0410      !  0.36186546970297339E-15
  REAL(8)                     :: MA0412      !  0.14655734975631911E-15
  REAL(8)                     :: MA0415      !  0.54001214087434229E-16
  REAL(8)                     :: MA0416      !  0.14869856293449610E-15
  REAL(8)                     :: MA0419      !  0.36781999999999998E-15
  REAL(8)                     :: MA0420      !  0.48352000000000002E-15
  REAL(8)                     :: MA0423      !  0.21124383605999520E-14
  REAL(8)                     :: MA0424      !  0.73799853443752290E-16
  REAL(8)                     :: MA0426      !  0.22979758127971452E-15
  REAL(8)                     :: MA0431      !  0.63407783495216967E-16
  REAL(8)                     :: MA0432      !  0.18780122958989279E-16
  REAL(8)                     :: MA0433      !  0.99000011897959024E-18
  REAL(8)                     :: MA0442      !  0.55534656287469117E-16
  REAL(8)                     :: MA0444      !  0.90708048441145045E-15
  REAL(8)                     :: MA0445      !  0.11534210849315851E-15
  REAL(8)                     :: MA0449      !  0.13558171306734880E-15
  REAL(8)                     :: MA0451      !  0.22955593906374618E-14
  REAL(8)                     :: MA0454      !  0.76808847746999987E-16
  REAL(8)                     :: MA0455      !  0.22861593339578099E-15
  REAL(8)                     :: MA0464      !  0.80635184290055785E-16
  REAL(8)                     :: MA0465      !  0.65399257724402354E-16
  REAL(8)                     :: MA0466      !  0.27071416736527809E-15
  REAL(8)                     :: MA0469      !  0.32283999999999999E-15
  REAL(8)                     :: MA0471      !  0.84594307289596827E-15
  REAL(8)                     :: MA0476      !  0.24193160564640660E-15
  REAL(8)                     :: MA0481      !  0.33407005297045191E-15
  REAL(8)                     :: MA0485      !  0.36737960799234177E-16
  REAL(8)                     :: MA0488      !  0.19151562798850779E-15
  REAL(8)                     :: MA0489      !  0.54846724291131712E-15
  REAL(8)                     :: MA0490      !  0.36414973977832911E-15
  REAL(8)                     :: MA0491      !  0.14858970838252891E-15
  REAL(8)                     :: MA0498      !  0.12987929239170220E-15
  REAL(8)                     :: MA0503      !  0.11187183314500760E-15
  REAL(8)                     :: MA0505      !  0.34176985017472392E-15
  REAL(8)                     :: MA0506      !  0.19735313911048411E-15
  REAL(8)                     :: MA0508      !  0.34106769797534871E-15
  REAL(8)                     :: MA0511      !  0.51981269794574977E-14
  REAL(8)                     :: MA0514      !  0.29376520795314819E-15
  REAL(8)                     :: MA0516      !  0.69599999999999999E-16
  REAL(8)                     :: MA0517      !  0.10886266508800200E-15
  REAL(8)                     :: MA0521      !  0.18124493964458610E-15
  REAL(8)                     :: MA0532      !  0.93159485940656196E-15
  REAL(8)                     :: MA0535      !  0.72538616434661140E-16
  REAL(8)                     :: MA0536      !  0.10975631032822509E-14
  REAL(8)                     :: MA0545      !  0.19484986115571251E-15
  REAL(8)                     :: MA0547      !  0.28123577458657678E-16
  REAL(8)                     :: MA0554      !  0.27485668803401500E-15
  REAL(8)                     :: MA0566      !  0.62674000000000000E-15
  REAL(8)                     :: MA0568      !  0.85978021881831306E-16
  REAL(8)                     :: MA0569      !  0.61123219726683135E-16
  REAL(8)                     :: MA0584      !  0.21760595614159460E-16
  REAL(8)                     :: MA0585      !  0.11716991540679400E-16
  REAL(8)                     :: MA0591      !  0.20038393509521630E-16
  REAL(8)                     :: MA0593      !  0.50188993452074918E-16
  REAL(8)                     :: MA0595      !  0.22211133615828450E-15
  REAL(8)                     :: MA0596      !  0.38350891708780020E-15
  REAL(8)                     :: MA0598      !  0.61530641540755240E-16
  REAL(8)                     :: MA0599      !  0.69118774732740101E-16
  REAL(8)                     :: MA0602      !  0.21501467433612251E-15
  REAL(8)                     :: MA0604      !  0.64260399533186429E-16
  REAL(8)                     :: MA0618      !  0.46386880160793903E-15
  REAL(8)                     :: MA0623      !  0.14497976797693291E-16
  REAL(8)                     :: MA0626      !  0.16643778725882151E-15
  REAL(8)                     :: MA0635      !  0.16995669526331389E-15
  REAL(8)                     :: MA0654      !  0.35953000000000000E-15
  REAL(8)                     :: MA0663      !  0.12049549488578180E-15
  REAL(8)                     :: MA0667      !  0.90942944157907603E-16
  REAL(8)                     :: MA0674      !  0.14383748004467891E-15
  REAL(8)                     :: MA0675      !  0.15923470884492271E-15
  REAL(8)                     :: MA0680      !  0.99581620100255186E-16
  REAL(8)                     :: MA0683      !  0.10436103612506560E-15
  REAL(8)                     :: MA0690      !  0.47637535316542469E-15
  REAL(8)                     :: MA0691      !  0.94537052258201022E-16
  REAL(8)                     :: MA0694      !  0.14938668030133560E-15
  REAL(8)                     :: MA0696      !  0.11177734423974849E-15
  REAL(8)                     :: MA0702      !  0.88950672849270447E-15
  REAL(8)                     :: MA0704      !  0.52561686784936620E-14
  REAL(8)                     :: MA0705      !  0.29175003506482499E-15
  REAL(8)                     :: MA0709      !  0.15456008538659911E-15
  REAL(8)                     :: MA0712      !  0.50204242212925650E-15
  REAL(8)                     :: MA0713      !  0.14268157848333010E-15
  REAL(8)                     :: MA0735      !  0.85976202637173235E-16
  REAL(8)                     :: MA0739      !  0.71382433975293479E-16
  REAL(8)                     :: MA0740      !  0.11892814439317689E-15
  REAL(8)                     :: MA0747      !  0.62157460662366884E-15
  REAL(8)                     :: MA0751      !  0.17989374811476499E-15
  REAL(8)                     :: MA0752      !  0.49982048125892481E-16
  REAL(8)                     :: MA0760      !  0.58167921005732779E-16
  REAL(8)                     :: MA0762      !  0.20818012830905660E-15
  REAL(8)                     :: MA0769      !  0.19725153985693760E-15
  REAL(8)                     :: MA0772      !  0.25206144925338948E-15
  REAL(8)                     :: MA0773      !  0.19576160906423331E-15
  REAL(8)                     :: MA0776      !  0.30567119446636531E-15
  REAL(8)                     :: MA0778      !  0.47743547738799828E-16
  REAL(8)                     :: MA0780      !  0.14857405347656311E-15
  REAL(8)                     :: MA0784      !  0.16308230966598089E-15
  REAL(8)                     :: MA0786      !  0.15830198804699920E-15
  REAL(8)                     :: MA0788      !  0.21581556817636010E-15
  REAL(8)                     :: MA0790      !  0.17558991833247000E-14
  REAL(8)                     :: MA0791      !  0.12196487517410749E-15
  REAL(8)                     :: MA0804      !  0.22671033414599050E-15
  REAL(8)                     :: MA0814      !  0.45559595663772374E-15
  REAL(8)                     :: MA0849      !  0.10354972501527590E-15
  REAL(8)                     :: MA0895      !  0.37864750330894842E-15
  REAL(8)                     :: MA0909      !  0.33629754927610392E-15
  REAL(8)                     :: MA0914      !  0.41172682683995418E-16
  REAL(8)                     :: MA0980      !  0.15352978885566199E-15
  REAL(8)                     :: MA1015      !  0.12583765926450050E-15
  REAL(8)                     :: MA1021      !  0.76171409872842654E-16
  REAL(8)                     :: MA1036      !  0.64677672132374607E-16
  REAL(8)                     :: MA1093      !  0.28716404826701959E-15
  REAL(8)                     :: MA1107      !  0.95491282158876468E-16
  REAL(8)                     :: MA1171      !  0.62485687084637610E-16
  REAL(8)                     :: MA1467      !  0.11152801330348170E-15
END TYPE JPLValues
TYPE(JPLValues),PUBLIC, SAVE  :: EPH
REAL(8)        ,PUBLIC, SAVE  :: VAL_JPL(1000)
EQUIVALENCE                     (EPH,VAL_JPL)
INTEGER(4)     ,PUBLIC        :: JPLEnd
!
! Viewable image of solar system body
INTEGER(4)     ,PARAMETER     :: jViewMax =64                   ! Cell Radius of viewable 2d image in 1st coordinate
INTEGER(4)     ,PARAMETER     :: kViewMax =64                   ! Cell Radius of viewable 2d image in 2nd coordinate
REAL(8)        ,PUBLIC        :: IlluminationCalculated         ! Illumination percent of the body (fraction)
REAL(8)        ,PUBLIC        :: Rsurf_CRS_j_k                & ! Surface coordinates of viewable image (km)
                                                                  (3,-jViewMax:+jViewMax,-kViewMax:+kViewMax)
REAL(8)        ,PUBLIC        :: Rview_CRS_j_k                & ! Viewable coordinates of viewable image (km)
                                                                  (3,-jViewMax:+jViewMax,-kViewMax:+kViewMax)
REAL(8)        ,PUBLIC        :: Light_CRS_j_k                & ! =1 if viewable image of body is sunlit (1/0)
                                                                  (3,-jViewMax:+jViewMax,-kViewMax:+kViewMax)
INTEGER(MJD)   ,PUBLIC        :: TimeViewCalculated = 0         ! Modified Julian Date when time was calculated (10^-7 seconds)
INTEGER(4)     ,PUBLIC        :: BodyViewCalculated = 0         ! Solar system body for which the view was calculated
!
!
!                                Module Global Variables          Description / (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(LocationENU)             :: LocationSite                   ! Location on the surface of the Earth
TYPE(WeatherData)             :: WeatherSite                    ! Weather at the location
!
CONTAINS
!
FUNCTION LocationOnSurface(Latitude, Longitude, Elevation) RESULT(Location)
!+
! Description: This function returns the ENU location for a given Latitude, Longitude, Elevation on Earth.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),    INTENT(IN)        :: Longitude                      ! Geodetic Longitude (degrees)
REAL(8),    INTENT(IN)        :: Latitude                       ! Geodetic Longitude (degrees)
REAL(8),    INTENT(IN)        :: Elevation                      ! Surface elevation  (km)
TYPE(LocationENU)             :: Location                       ! Location returned to caller
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Location = LocationOnSurfaceOfBody(Earth, Latitude, Longitude, Elevation)
RETURN
END FUNCTION LocationOnSurface
!
FUNCTION LocationOnSurfaceOfBody(Body, Latitude, Longitude, Elevation) RESULT(Location)
!+
! Description: This function returns the ENU location for a given Latitude, Longitude, Elevation on a Solar system body.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)        :: Body                           ! Solar system body
REAL(8),    INTENT(IN)        :: Latitude                       ! Geodetic Longitude WGS (degrees)
REAL(8),    INTENT(IN)        :: Longitude                      ! Geodetic Longitude WGS (degrees)
REAL(8),    INTENT(IN)        :: Elevation                      ! Surface elevation  (km)
TYPE(LocationENU)             :: Location                       ! Location returned to caller
!
!                               Local
!-----------------------------+-------
REAL(8)                       :: Ra                             ! Equatorial radius of body (km)
REAL(8)                       :: Rb                             ! Polar radius of body (km)
REAL(8)                       :: f                              ! Flattening of body (fraction)
REAL(8)                       :: cosLat                         ! Cosine of latitude
REAL(8)                       :: sinLat                         ! Sine of latitude
REAL(8)                       :: cosLon                         ! Cosine of longitude
REAL(8)                       :: sinLon                         ! Cosine of longitude
REAL(8)                       :: chi                            ! auxiliary calculation
REAL(8)                       :: e                              ! Body eccentricity
REAL(8)                       :: Px                             ! PCS X coordinate (km)
REAL(8)                       :: Py                             ! PCS Y coordinate (km)
REAL(8)                       :: Pz                             ! PCS Z coordinate (km)

LOGICAL(4), PARAMETER         :: DebugLocal = .false.          ! Diagnostic flag for testing
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
 ! Construct a local coordinate system ENU  East (x) North (y) Up (z)
 ! Reference: https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#From_ECEF_to_ENU

  Ra     = Ellipsoid_Body(Body)%Ra                                                   ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Ra            : ",F17.7)') Ra
  f      = Ellipsoid_Body(Body)%f                                                    ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody f             : ",F17.7)') f
  Rb     = Ellipsoid_Body(Body)%Rb                                                   ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Rg            : ",F17.7)') Rb
  e      = Ellipsoid_Body(Body)%e                                                    ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody e             : ",F17.7)') e


  cosLat = DCOSD(Latitude)                                                           ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody cosLat        : ",F17.7)') cosLat
  sinLat = DSIND(Latitude)                                                           ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody sinLat        : ",F17.7)') sinLat
  cosLon = DCOSD(Longitude)                                                          ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody cosLon        : ",F17.7)') cosLon
  sinLon = DSIND(Longitude)                                                          ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody sinLon        : ",F17.7)') sinLon

  chi    = DSQRT(1d0 - (e*sinLat)**2)                                                ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody chi           : ",F17.7)') chi

  !Calculate all the Location terms
  Location%Latitude       = Latitude                                                 ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Latitude      : ",3(F17.7))') Location%Latitude
  Location%Longitude      = Longitude                                                ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Longitude     : ",3(F17.7))') Location%Longitude
  Location%Elevation      = Elevation                                                ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Elevation     : ",3(F17.7))') Location%Elevation
  Location%T_ENU_PCS(1,:) = (/ -sinLon       , +cosLon       ,   0d0  /)             ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody T_ENU_PCS(1,:): ",3(F17.7))') Location%T_ENU_PCS(1,:)
  Location%T_ENU_PCS(2,:) = (/ -cosLon*sinLat, -sinLon*sinLat, cosLat /)             ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody T_ENU_PCS(2,:): ",3(F17.7))') Location%T_ENU_PCS(2,:)
  Location%T_ENU_PCS(3,:) = (/ +cosLon*cosLat, +sinLon*cosLat, sinLat /)             ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody T_ENU_PCS(3,:): ",3(F17.7))') Location%T_ENU_PCS(3,:)
  Location%T_PCS_ENU      = TRANSPOSE(Location%T_ENU_PCS)
  Px                      = (Ra/chi + Elevation)*cosLat*cosLon                       ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Px            : ",F17.7)') Px
  Py                      = (Ra/chi + Elevation)*cosLat*sinLon                       ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Py            : ",F17.7)') Py
  Pz                      = (Ra*(1d0-e**2)/chi + Elevation)*sinLat                   ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Pz            : ",F17.7)') Pz
  Location%R_PCS          = (/Px,Py,Pz/)/EPH%AU                                      ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody R_PCS EPH%AU  : ",4(F17.7))') Location%R_PCS, EPH%AU
  Location%Nx_PCS         = MATMUL(Location%T_PCS_ENU,(/1d0,0d0,0d0/))               ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Nx_PCS        : ",3(F17.7))') Location%Nx_PCS
  Location%Ny_PCS         = MATMUL(Location%T_PCS_ENU,(/0d0,1d0,0d0/))               ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Ny_PCS        : ",3(F17.7))') Location%Ny_PCS
  Location%Nz_PCS         = MATMUL(Location%T_PCS_ENU,(/0d0,0d0,1d0/))               ; IF(DebugLocal) WRITE(*,FMT='("LocationOnSurfaceOfBody Nz_PCS        : ",3(F17.7))') Location%Nz_PCS
  Location%body           = body
  !
  RETURN
END FUNCTION LocationOnSurfaceOfBody
!
FUNCTION LocationOnSurfaceOfEllipsoid(Ra, f, Body, Latitude, Longitude, Elevation) RESULT(Location)
!+
! Description: This function returns the ENU location for a given Latitude, Longitude, Elevation on an ellipsoid body.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+----------------------------------+-------------------------------------------------------
REAL(8),    INTENT(IN)        :: Ra                             ! Equatorial radius of body (km)
REAL(8),    INTENT(IN)        :: f                              ! Flattening of body (fraction)
INTEGER(4), INTENT(IN)        :: Body                           ! Solar system body
REAL(8),    INTENT(IN)        :: Longitude                      ! Geodetic Longitude WGS (degrees)
REAL(8),    INTENT(IN)        :: Latitude                       ! Geodetic Longitude WGS (degrees)
REAL(8),    INTENT(IN)        :: Elevation                      ! Surface elevation  (km)
TYPE(LocationENU)             :: Location                       ! Location returned to caller
!
!                               Local
!-----------------------------+-------
REAL(8)                       :: Rb                             ! Polar radius of body (km)
REAL(8)                       :: cosLat                         ! Cosine of latitude
REAL(8)                       :: sinLat                         ! Sine of latitude
REAL(8)                       :: cosLon                         ! Cosine of longitude
REAL(8)                       :: sinLon                         ! Cosine of longitude
REAL(8)                       :: chi                            ! auxiliary calculation
REAL(8)                       :: e                              ! Body eccentricity
REAL(8)                       :: Px                             ! PCS X coordinate (km)
REAL(8)                       :: Py                             ! PCS Y coordinate (km)
REAL(8)                       :: Pz                             ! PCS Z coordinate (km)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! Construct a local coordinate system ENU  East (x) North (y) Up (z)
  ! Reference: https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#From_ECEF_to_ENU

  Rb     = Ra*(1d0-f)                 !semi-minor axis
  e      = DSQRT(f*(2d0-f))           !first eccentricity


  cosLat = DCOSD(Latitude)
  sinLat = DSIND(Latitude)
  cosLon = DCOSD(Longitude)
  sinLon = DSIND(Longitude)

  chi    = DSQRT(1d0 - (e*sinLat)**2)

  !Calculate all the Location terms
  Location%Latitude       = Latitude
  Location%Longitude      = Longitude
  Location%Elevation      = Elevation
  Location%T_ENU_PCS(1,:) = (/ -sinLon       , +cosLon       ,   0d0  /)
  Location%T_ENU_PCS(2,:) = (/ -cosLon*sinLat, -sinLon*sinLat, cosLat /)
  Location%T_ENU_PCS(3,:) = (/ +cosLon*cosLat, +sinLon*cosLat, sinLat /)
  Location%T_PCS_ENU      = TRANSPOSE(Location%T_ENU_PCS)
  Px                      = (Ra/chi + Elevation)*cosLat*cosLon
  Py                      = (Ra/chi + Elevation)*cosLat*sinLon
  Pz                      = (Ra*(1d0-e**2)/chi + Elevation)*sinLat
  Location%R_PCS          = (/Px,Py,Pz/)/EPH%AU
  Location%Nx_PCS         = MATMUL(Location%T_PCS_ENU,(/1d0 ,0d0 ,0d0 /))
  Location%Ny_PCS         = MATMUL(Location%T_PCS_ENU,(/0d0 ,1d0 ,0d0 /))
  Location%Nz_PCS         = MATMUL(Location%T_PCS_ENU,(/0d0 ,0d0 ,1d0 /))
  Location%body           = body
  !
  RETURN
END FUNCTION LocationOnSurfaceOfEllipsoid
!
!
FUNCTION TimesSolarForLocation(Location, Weather, Time) RESULT(TimesSolar)
!+
! Description: Ths function returns the Sunrise, Sunset, Solar Transit and SolarNoon times at a given ENU location.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE TimeDatePackage
USE NumericalConstants
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Solar System body, Latitude, Longitude and Elevation
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Temperature and Pressure               (degF and Psia)
INTEGER(MJD)     ,INTENT(IN)  :: Time                           ! Modified Julian Date                        (10^-7sec)
TYPE(SolarTimes)              :: TimesSolar                     ! Sunrise,Sunset,SolarNoon,SolarTransit       (10^-7sec)
!
!                                Local
!--------------------------=--+-------------
TYPE(TimeDateISO)             :: DateAndTime                    ! ISO 8601:1988 compatible date/time structure
LOGICAL(4)                    :: AlreadyHaveResults             ! Set true if TimesSolar are available already
TYPE(GeoData)                 :: LocationGeo                    ! WGS84 Latitude, Longitude and Elevation
REAL(8)                       :: HoursOfSunLight                ! Hours of sunlight                              (hours)
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
INTEGER(MJD)                  :: TimeSunrise                    ! MJD for Sunrise                             (10^-7sec)
INTEGER(MJD)                  :: TimeSolarNoon                  ! MJD for SolarNoon                           (10^-7sec)
INTEGER(MJD)                  :: TimeSolarTransit               ! MJD for SolarTransit                        (10^-7sec)
INTEGER(MJD)                  :: TimeSunset                     ! MJD for Sunset                              (10^-7sec)
INTEGER(MJD)                  :: TimeAtSunrise
INTEGER(MJD)                  :: TimeAtSolarTransit
INTEGER(MJD)                  :: TimeAtSolarNoon
INTEGER(MJD)                  :: TimeAtSunset
REAL(8)                       :: AngleElevation                 ! Angle Target for sunrise or sunset          (degrees)
REAL(8)                       :: AngleAzimuthal                 ! Angle Target for solar transit              (degrees)
INTEGER(4)                    :: StatusTransition               ! Return from finding solar transitions
REAL(8)                       :: AngleAzimuthalAtSunrise
REAL(8)                       :: AngleAzimuthalAtSunset
REAL(8)                       :: AngleElevationAtTransit
REAL(8)                       :: AngleElevationNoon
CHARACTER(24)                 :: TimeStamp                      ! Time stamp created
CHARACTER(4)                  :: FormatTime                     ! Time Zone
LOGICAL(4)    ,PARAMETER      :: debugTimes = .false.           ! Diagnostic flag for in-house testing
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! Check to see if we already have the results
  CALL CalculateISODateFromTime(Time,DateAndTime)
  AlreadyHaveResults = DateAndTime%Year  == DateAndTimeToday%Year .and. &
                       DateAndTime%Day   == DateAndTimeToday%Day  .and. &
                       DateAndTime%Month == DateAndTimeToday%Month
  IF(AlreadyHaveResults) THEN
    TimesSolar = TimesSolarToday
    RETURN
  END IF

  LocationGeo%Latitude  = Location%Latitude
  LocationGeo%Longitude = Location%Longitude
  LocationGeo%Elevation = Location%Elevation
  !
  ! Calculate approximate solar times
  CALL ApproximateDailySolarData(Time, LocationGeo, HoursOfSunLight, TimeSolarNoon, TimeSunrise, TimeSunset)

  TimeSolarTransit  = TimeSolarNoon

  SolarInformation  = SolarInformationForLocation(TimeSunrise, Location, Weather)
  AngleElevation    = -SolarInformation%AngleSolarDisk/2d0 + 0.0000001d0
  StatusTransition  = FindWhenTheSunIsAtAngleElevation(AngleElevation, TimeSunrise, Location, Weather, TimeAtSunrise, AngleAzimuthalAtSunrise)
  TimeSunrise       = TimeAtSunrise

  SolarInformation  = SolarInformationForLocation(TimeSunset, Location, Weather)
  AngleElevation    = -SolarInformation%AngleSolarDisk/2d0 - 0.0000001d0
  StatusTransition  = FindWhenTheSunIsAtAngleElevation(AngleElevation, TimeSunset, Location, Weather, TimeAtSunset, AngleAzimuthalAtSunset)
  TimeSunset        = TimeAtSunset

  AngleAzimuthal    = 180d0
  StatusTransition  = FindWhenTheSunIsAtAngleAzimuthal(AngleAzimuthal, TimeSolarTransit, Location, Weather, TimeAtSolarTransit, AngleElevationAtTransit)
  TimeSolarTransit  = TimeAtSolarTransit

  TimeAtSolarNoon   = TimeSunrise/2 + TimeSunset/2
  StatusTransition  = FindWhenTheTimeIsSolarNoon(TimeAtSolarNoon, Location, Weather, AngleElevationNoon, TimeSolarNoon)

  TimesSolar%Rise    = TimeSunrise
  TimesSolar%Noon    = TimeSolarNoon
  TimesSolar%Transit = TimeSolarTransit
  TimesSolar%Set     = TimeSunSet

  !ToDo: SCR 150: Add a type for the Timestamps so we can fill in the Places data
  IF(DebugTimes) THEN
    CALL ConvertTimeToLocalTimeStamp(TimesSolar%Rise,    TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *R"
    CALL ConvertTimeToLocalTimeStamp(TimesSolar%Noon,    TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *N"
    CALL ConvertTimeToLocalTimeStamp(TimesSolar%Transit, TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *T"
    CALL ConvertTimeToLocalTimeStamp(TimesSolar%Set,     TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *S"
  END IF
  RETURN
END FUNCTION TimesSolarForLocation

SUBROUTINE InitializeAirmassModel(Altitude)
!+
! Description: Sets up the airmass model of airmass as a function of zenith angle
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 09-MAR-2025     | MA    | SCR 150: Update Ephemeris to be independent of Habitat Database
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-!
USE UnitConversion
USE InternationalStandardAtmosphere
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)            :: Altitude                       ! Altitude                                         (km)
!
!                                Local
!-----------------------------+---------
INTEGER(4)                    :: angZen                         ! Zenith angle used as an index                (degrees)
REAL(8)                       :: RatioRho                       ! Ratio of base to reference density
REAL(8)                       :: RatioPressure                  ! Ratio of base to reference pressure
REAL(8)                       :: RatioTemperature               ! Ratio of base to reference temperature
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! Return if the model has already been setup
  IF(AtmosphereModelLoaded .and. AtmosphereModelSetup) THEN
    RETURN
  END IF
  CALL Atmosphere(Altitude, RatioRho, RatioPressure, RatioTemperature)
  PsiBase  = PrefISA  * RatioPressure * PsiPerAtm
  TempBase = DegFFromDegK(TrefISA *  RatioTemperature)
  AirMassOne = AirMassAtZenith(0d0,0d0)
  DO angZen = 0,90
    AirmassSeaLevel_ang(angZen) = AirMassAtZenith(DBLE(angZen),0d0)/AirMassOne
    Airmass_ang        (angZen) = AirMassAtZenith(DBLE(angZen),Altitude)/AirMassOne
  END DO
  AtmosphereModelLoaded = .true.
  AtmosphereModelSetup  = .true.
  RETURN
END SUBROUTINE InitializeAirmassModel
!
FUNCTION dAngleZenithRefractionLocation(AngleZenith, Location, Weather) RESULT(dAngleZen)
!+
! Description: Ths function returns the refraction angle delta due to the atmosphere at a given location.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)            :: AngleZenith                    ! True Zenith Angle (degrees)
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Solar System body & Elevation (km)
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Temperature (F) & Pressure (psia)
REAL(8)                       :: dAngleZen                      ! Refraction angle delta (degrees)
!
!                                Local
!----------------------------------------
REAL(8)                       :: Angle                          ! Local copy of angle (degrees)

Angle = AngleZenith
IF(Angle > 180.0d0) THEN
   Angle = Angle - 360.0d0
END IF
Angle  = MIN(ABS(Angle),90.0d0)
IF(Location%Body==Earth) THEN
  dAngleZen = dAngleZenithRefraction(Angle, Location%Elevation, Weather%TempF, Weather%Psia)
ELSE
  dAngleZen = 0d0
END IF
RETURN
END FUNCTION dAngleZenithRefractionLocation

FUNCTION dAngleZenithRefraction(AngleZenith, Elevation, TempF, Psia) RESULT (dAngleZen)
!+
! Description: Ths function returns the refraction angle delta due to the atmosphere at a given elevation.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
USE UnitConversion
USE InternationalStandardAtmosphere
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),    INTENT(IN)        :: Elevation                      ! Elevation above reference (km)
REAL(8),    INTENT(IN)        :: AngleZenith                    ! Solar Zenith Angle (degrees)
REAL(8),    INTENT(IN)        :: TempF                          ! Temperature (F)
REAL(8),    INTENT(IN)        :: Psia                           ! Pressure (Psia)
REAL(8)                       :: dAngleZen                      ! Angle correction (degrees)
!
!                                Local
!-----------------------------+----------
REAL(8)                       :: Airmass                         ! Airmass (per-unit)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Airmass   = AirmassCalculation(AngleZenith, Elevation)

dAngleZen = dAngleZenithAirmass(Airmass, TempF, Psia, Elevation)

RETURN
END FUNCTION dAngleZenithRefraction

FUNCTION dAngleZenithAirmass(Airmass, TempF, Psia, ElevKm) RESULT (dAngleZen)
!+
! Description: Returns the refraction angle delta due to the atmosphere at a given elevation.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-!
USE NumericalConstants
USE UnitConversion
USE InternationalStandardAtmosphere
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),    INTENT(IN)        :: Airmass                        ! Airmass (per-unit)
REAL(8),    INTENT(IN)        :: TempF                          ! Temperature (F)
REAL(8),    INTENT(IN)        :: Psia                           ! Pressure (Psia)
REAL(8),    INTENT(IN)        :: ElevKm                         ! Elevation (km)
REAL(8)                       :: dAngleZen                      ! Angle correction (degrees)
!
!Local Variables
!----------------------------------------
!
! Reference for 2000 paper by Aueur and Standish on refraction
!https://trs.JPL%nasa.gov/bitstream/handle/2014/13945/00-0322.pdf?sequence=1					
!
!          or for a local copy
!
!file://d:\csp\documentation\RefractionAueurAndStandish2000.pdf
!
! Rate of Change wrt Temperature and Pressure
INTEGER(4),PARAMETER          :: ang_pt (12) = (/  15,30,45,60,75,80,85,86,87,88,89,90/)
REAL(8)   ,PARAMETER          :: dP_pt  (12) = (/  0.001301115,0.001308599,0.001321256,0.001317434, &
                                                  0.001318344,0.001320646,0.001334288,0.001340803, &
                                                  0.001350600,0.001369696,0.001396927,0.001452211  &
                                               /)*760.0d0/PsiPerAtm
REAL(8)   ,PARAMETER          :: dT_pt  (12)  = (/-0.00330442,-0.003307449,-0.00330730,-0.00331442, &
                                                 -0.00336208,-0.003428940,-0.00367743,-0.00379928, &
                                                 -0.00397933,-0.004251963,-0.00467447,-0.00534038  &
                                               /)/1.8d0
INTEGER(4),PARAMETER          :: ptEnd = 12
INTEGER(4)                    :: pt

INTEGER(4)                    :: ang
REAL(8)                       :: angle
REAL(8)                       :: AirMass0
REAL(8)                       :: AirMass1
REAL(8)                       :: Ratio
REAL(8)                       :: RatioAngle
REAL(8)                       :: dT
REAL(8)                       :: dP
REAL(8)                       :: dangle1
REAL(8)                       :: dangle2
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
dAngleZen = 0d0

dAngle1   =  0.0020516159d0*AirMass**3 &
           - 0.0417284258d0*AirMass**2 &
           + 61.468917614d0*Airmass    &
           - 10.5159660437d0
dAngle2   = 0.0d0
IF(AirMass < AirMassSeaLevel_ang(0)) THEN
  dangle2 = 0d0
  Angle   = 0d0
ELSE IF(AirMass >= AirMassSeaLevel_ang(90)) THEN
  dangle2 = Infinity
  Angle   = 90d0
ELSE
  Angle = (Airmass/AirMassSeaLevel_ang(0)) * 90d0
  DO ang = 1,90
    IF(AirMassSeaLevel_ang(ang-1) <= AirMass .and. AirMass <= AirMassSeaLevel_ang(ang)) THEN
      AirMass0 = AirMassSeaLevel_ang(ang-1)
      AirMass1 = AirMassSeaLevel_ang(ang)
      Angle    = DBLE(ang-1) + (AirMass - AirMass0)/(AirMass1 - AirMass0)
      dAngle2  = (60.2458545647d0*TAND(Angle) - (-0.0673439565d0)*TAND(Angle)**3)
      EXIT
    END IF
  END DO
END IF

dangleZen = MIN(dangle1,dangle2)
ang    = INT(Angle)

!Now Adjust for temperature and pressure deltas
IF(ang <= 15) THEN
  dT = dT_pt(1)
  dP = dP_pt(1)
ELSE IF(ang >= 90) THEN
  dT = dT_pt(ptEnd)
  dP = dP_pt(ptEnd)
ELSE
  dP = dP_pt(ptEnd/2)
  dT = dT_pt(ptEnd/2)
  DO pt = 2, ptEnd
    IF(ang < ang_pt(pt)) THEN
      ratio = DBLE(ang-ang_pt(pt-1))/DBLE(ang_pt(pt)-ang_pt(pt-1))
      dT = dT_pt(pt-1) + (dT_pt(pt)-dT_pt(pt-1))*ratio
      dP = dP_pt(pt-1) + (dP_pt(pt)-dP_pt(pt-1))*ratio
      EXIT
    END IF
  END DO
END IF

RatioAngle = (1d0+(TempF-TempFReference)*dT)*(1d0+(Psia-PsiReference)*dP)*(1d0+0.0133d0*ElevKm)
dAngleZen = dAngleZen*RatioAngle/3600.0d0
END FUNCTION dAngleZenithAirmass
!
FUNCTION AirmassCalculation(AngleZenith, Elevation) RESULT(Airmass)
!+
! Description: Ths function calculates and returns the air mass AM1 coefficient for a given zenith angle and elevation.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
USE UnitConversion
USE InternationalStandardAtmosphere
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
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
  DO ang = 0,90
    AirmassSeaLevel_ang(ang) = AirMassAtZenith(DBLE(ang),0.0d0)
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
END FUNCTION AirmassCalculation
!
SUBROUTINE PrepareAtmosphereModeForSetup()
!+
! Description: Ths subroutine sets parameters for the atmospheric model.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
AtmosphereModelLoaded = .false.
RETURN
END SUBROUTINE PrepareAtmosphereModeForSetup
!
SUBROUTINE LoadAirMassModel(AirMassLoad_ang, AirMassSeaLevelLoad_ang, Elevation, Psia, TempF)
!+
! Description: Ths subroutine sets parameters for air mass model.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)            :: AirMassLoad_ang(0:90)          ! AirMass at the elevation of the location (per-unit)
REAL(8),INTENT(IN)            :: AirMassSeaLevelLoad_ang(0:90)  ! AirMass at sea level for location (per-unit)
REAL(8),INTENT(IN)            :: Elevation                      ! Reference elevation for airmass model (km)
REAL(8),INTENT(IN)            :: Psia                           ! Reference pressure for airmass model (psia)
REAL(8),INTENT(IN)            :: TempF                          ! Reference temperature for airmass model (degF)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  AirMass_ang           = AirMassLoad_ang
  AirMassSeaLevel_ang   = AirMassSeaLevelLoad_ang
  ElevationBase         = Elevation
  PsiBase               = Psia
  TempBase              = TempF
  AtmosphereModelLoaded = .true.
  RETURN
END SUBROUTINE LoadAirMassModel

SUBROUTINE GetAirMassModel(AirMassGet_ang, AirMassSeaLevelGet_ang, Elevation, Psia, TempF)
!+
! Description: This parameter get the air mass model parameters for a given elevation, pressure and temperature.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),INTENT(OUT)           :: AirMassGet_ang(0:90)           ! AirMass at the elevation of the location (per-unit)
REAL(8),INTENT(OUT)           :: AirMassSeaLevelGet_ang(0:90)   ! AirMass at sea level for location (per-unit)
REAL(8),INTENT(OUT)           :: Elevation                      ! Reference elevation for airmass model (km)
REAL(8),INTENT(OUT)           :: Psia                           ! Reference pressure for airmass model (psia)
REAL(8),INTENT(OUT)           :: TempF                          ! Reference temperature for airmass model (degF)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  AirMassSeaLevelGet_ang  = AirMassSeaLevel_ang
  AirMassGet_ang          = AirMass_ang
  Elevation               = ElevationBase
  Psia                    = PsiBase
  TempF                   = TempBase
RETURN
END SUBROUTINE GetAirMassModel
!
SUBROUTINE SetLightTravelConsideration(ConsiderLightTravel)
!+
! Description: Set flag for considering light travel time in the JPL and NOVAS routines.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                               Passed                           Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
LOGICAL(4), INTENT(IN)       :: ConsiderLightTravel             ! Flag for finite light speed
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(ConsiderLightTravel) THEN
    ModelLightTravelTime = TRUE
  ELSE
    ModelLightTravelTime = FALSE
  END IF
END SUBROUTINE SetLightTravelConsideration

FUNCTION PositionTimeLightTravel(Time, BodyTarget, BodyOrigin, LightTravelCorrection) RESULT(R_CRS)
!+
! Description: This function calculates the distance between celestial bodies with finite light speed correction.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE TimeDatePackage ,ONLY: JulianDateExtendedFromTime
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD),INTENT(IN)       :: Time                           ! Modified Julian date (10^-7 sec)
INTEGER(4)  ,INTENT(IN)       :: BodyTarget                     ! Target body [MERCURY, SOLARSYSTEM]
INTEGER(4)  ,INTENT(IN)       :: BodyOrigin                     ! Center body [MERCUTY, SOLARSYSTEM]
LOGICAL(4)  ,INTENT(IN)       :: LightTravelCorrection          ! Correct for light travel (T or False
REAL(8)                       :: R_CRS(3)                       ! Distance from Body1 to Body2 (AU)
!
!                               Local
!-----------------------------+-------
REAL(8)                       :: jdt(2)                         ! Julian date, day+fraction (days)
REAL(8)                       :: jdtLight(2)                    ! Julian date when light emitted, day+fraction (days)
INTEGER(MJD)                  :: TimeLightEmitted               ! Modified Julian date when light emitted (10^-7 sec)
REAL(8)                       :: Rorigin_CRS(3)                 ! Position of origin at light emission time (AU)
REAL(8)                       :: Rtarget_CRS(3)                 ! Position of target (AU)
REAL(8)                       :: JPL_Vec(6)                     ! Position and Velocity (AU,AU/day)
TYPE(JPL_Vector)              :: JPL                            ! Position and Velocity (AU,AU/day)
EQUIVALENCE                     (JPL,JPL_Vec)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(LightTravelCorrection) THEN

    jdt = JulianDateExtendedFromTime(Time)
    CALL DPLEPH(jdt,BodyTarget,BodyOrigin,JPL_Vec)
    TimeLightEmitted = Time - INT(1d7*DSQRT(DOT_PRODUCT(JPL%R_CRS,JPL%R_CRS))*EPH%AU/EPH%CLIGHT, KIND=8)
    jdtLight         = JulianDateExtendedFromTime(TimeLightEmitted)
    CALL DPLEPH(jdt,BodyTarget,SOLARSYSTEMBARYCENTER,JPL_Vec)
    Rtarget_CRS = JPL%R_CRS
    CALL DPLEPH(jdtLight,BodyOrigin,SOLARSYSTEMBARYCENTER,JPL_VEC)
    Rorigin_CRS = JPL%R_CRS
    R_CRS = Rtarget_CRS - Rorigin_CRS
  ELSE
    jdt = JulianDateExtendedFromTime(Time)
    CALL DPLEPH(jdt,BodyTarget,BodyOrigin,JPL_Vec)
    R_CRS = JPL%R_CRS
  END IF
  RETURN
END FUNCTION PositionTimeLightTravel

FUNCTION PositionTime(Time, BodyTarget, BodyOrigin) RESULT(R_CRS)
!+
! Description: This function calculates the distance between celestial bodies at a given time.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE TimeDatePackage, ONLY: JulianDateExtendedFromTime
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) ,INTENT(IN)      :: Time                           ! Modified Julian date (10^-7 sec)
INTEGER(4)   ,INTENT(IN)      :: BodyTarget                     ! Target body [MERCURY, SOLARSYSTEM]
INTEGER(4)   ,INTENT(IN)      :: BodyOrigin                     ! Center body [MERCURY, SOLARSYSTEM]
REAL(8)                       :: R_CRS(3)                       ! Distance from Body1 to Body2 (AU)
!
!                               Local
!----------------------------+-------
REAL(8)                       :: jdt(2)                         ! Julian date, day+fraction (days)
REAL(8)                       :: JPL_Vec(6)                     ! Position and Velocity (AU,AU/day)
TYPE(JPL_Vector)              :: JPL                            ! Position and Velocity (AU,AU/day)
EQUIVALENCE                     (JPL,JPL_Vec)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  jdt = JulianDateExtendedFromTime(time)
  CALL DPLEPH(jdt,BodyTarget,BodyOrigin,JPL_Vec)
  R_CRS = JPL%R_CRS
  RETURN
END FUNCTION PositionTime
!
FUNCTION PositionJdt(jdt, BodyTarget, BodyOrigin) RESULT(R_CRS)
!+
! Description: This function calculates the distance vector in the Celestial Reference System (CRS)
!              between two celestial bodies at a given time with normal double precision.
!
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)        :: jdt                            ! Julian date (days)
INTEGER(4), INTENT(IN)        :: BodyTarget                     ! Target body [MERCURY, SOLARSYSTEM]
INTEGER(4), INTENT(IN)        :: BodyOrigin                     ! Center body [MERCUTY, SOLARSYSTEM]
REAL(8)                       :: R_CRS(3)                       ! Distance from Body1 to Body2 (AU)
!
!                               Local
!-----------------------------+-------
REAL(8)                       :: JPL_Vec(6)                     ! Position and Velocity (AU,AU/day)
TYPE(JPL_Vector)              :: JPL                            ! Position and Velocity (AU,AU/day)
EQUIVALENCE                     (JPL,JPL_Vec)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  CALL PLEPH(jdt,BodyTarget,BodyOrigin,JPL_Vec)
  R_CRS = JPL%R_CRS
  RETURN
END FUNCTION PositionJdt
!
FUNCTION PositionJdt2(jdt, BodyTarget, BodyOrigin) RESULT(R_CRS)
!+
! Description: This function calculates the distance vector in the Celestial Reference System (CRS)
!              between two celestial bodies at a given time with extended double precision.
!
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)        :: jdt(2)                         ! Julian date, day+fraction (days)
INTEGER(4), INTENT(IN)        :: BodyTarget                     ! Target body [MERCURY, SOLARSYSTEM]
INTEGER(4), INTENT(IN)        :: BodyOrigin                     ! Center body [MERCUTY, SOLARSYSTEM]
REAL(8)                       :: R_CRS(3)                       ! Distance from Body1 to Body2 (AU)
!
!                                Local
!-----------------------------+-------
REAL(8)                       :: JPL_Vec(6)                     ! Position and Velocity (AU,AU/day)
TYPE(JPL_Vector)              :: JPL                            ! Position and Velocity (AU,AU/day)
EQUIVALENCE                    (JPL,JPL_Vec)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  CALL DPLEPH(jdt,BodyTarget,BodyOrigin,JPL_Vec)
  R_CRS = JPL%R_CRS
  RETURN
END FUNCTION PositionJdt2
!
FUNCTION RadiusEquatorialBody(Body) RESULT(Radius)
!+
! Description: This function returns the equitorial radius of a given solar system body.
!
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)        :: Body                           ! Target body [MERCURY, SOLARSYSTEM]
REAL(8)                       :: Radius                         ! Equatorial radius of Body (km)
!
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------

!Return equatorial radius of body
  SELECT CASE (Body)
  CASE (Earth)  ;  Radius = EPH%RE
  CASE (Sun)    ;  Radius = EPH%ASUN
  CASE (Moon)   ;  Radius = EPH%AM
  CASE (Mercury);  Radius = 2439.700d0
  CASE (Venus)  ;  Radius = 6051.800d0
  CASE (Mars)   ;  Radius = 3396.190d0
  CASE (Jupiter);  Radius = 71492.00d0
  CASE (Saturn) ;  Radius = 60268.00d0
  CASE (Uranus) ;  Radius = 25559.00d0
  CASE (Neptune);  Radius = 24764.00d0
  CASE (Pluto)  ;  Radius = 1151.000d0
  CASE DEFAULT  ;  Radius = 6378.1366
  END SELECT
  RETURN
END FUNCTION RadiusEquatorialBody
!
FUNCTION FlatteningBody(Body) RESULT(f)
!+
! Description: This function returns ellipsoid flattening of a solar system body.
!
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)        :: Body                           ! Target body [MERCURY, SOLARSYSTEM]
REAL(8)                       :: f                              ! Bodies oblate spheroid flattness
!
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
!Return oblate spheroid flattening of body (Eventually calculate from GM, J2 & rotation rate
!for bodies other than earth.

  SELECT CASE (Body)
  CASE (Earth)  ;  f = 1d0/298.25642d0 !Match NOVAS
  CASE (Sun)    ;  f = 0d0
  CASE (Moon)   ;  f = 0.0012d0
  CASE (Mercury);  f = 0d0
  CASE (Venus)  ;  f = 0d0
  CASE (Mars)   ;  f = 0d0
  CASE (Jupiter);  f = 0d0
  CASE (Saturn) ;  f = 0d0
  CASE (Uranus) ;  f = 0d0
  CASE (Neptune);  f = 0d0
  CASE (Pluto)  ;  f = 0d0
  CASE DEFAULT  ;  f = 0d0
  END SELECT
END FUNCTION FlatteningBody
!
!
FUNCTION T_PCS_CRS_BodyAtJdt(Body, jdt) RESULT(T_PCS_CRS)
!+
! Description: This function calculates the transformation matrix from the Celestial Reference Coordinate System to the
!              Planetocentric Coordinate System for a given solar system body at as specific time.  The Celestial
!              Reference System is the native coordinate system use by the JPL Ephemeris for the Solar System.
!
USE TimeDatePackage
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),    INTENT(IN)        :: jdt                            ! Julian date (days)
INTEGER(4), INTENT(IN)        :: Body                           ! Target body [MERCURY, SOLARSYSTEM]
REAL(8)                       :: T_PCS_CRS(3,3)                 ! Transformation from CRC to PCS
!
!                               Local
!-----------------------------+--------
INTEGER(MJD)                  :: time                           ! Modified Julian date (10^-7 sec)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
time = TimeFromJulianDate(jdt)
T_PCS_CRS = T_PCS_CRS_BodyAtTime(Body, Time)
RETURN
END FUNCTION T_PCS_CRS_BodyAtJdt
!
FUNCTION T_PCS_CRS_BodyAtTime(Body, time) RESULT(T_PCS_CRS)
!+
! Description: This function calculates the transformation matrix from the Planetocentric Coordinate System  to the
!              Celestial Reference Coordinate System for a given solar system body at a specific time.  The Celestial
!              Reference System is the native coordinate system used by the JPL Ephemeris for the Solar System.
!
USE NumericalConstants
USE MatrixVectorPackage
USE TimeDatePackage
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+----------------------------------+------------------------------------------------------
INTEGER(MJD) , INTENT(IN)     :: time                            ! Modified Julian date (10^-7 sec)
INTEGER(4)   , INTENT(IN)     :: Body                            ! Target body [MERCURY, SOLARSYSTEM]
REAL(8)                       :: T_PCS_CRS(3,3)                  ! Transformation from CRS to PCS
!
!                               Local
!-----------------------------+--------
REAL(8)                       :: jdt                             ! Julian date (days)
LOGICAL(4)                    :: LeapOccurring                   ! Leap second occuring at time
REAL(8)                       :: R_CRS_PCS(3,3)                  ! PCS Basis Vectors in CRS
REAL(8)                       :: Location_Vec(6)                 ! Vector equivalence to locations
REAL(8)                       :: Latitude                        ! Location Latitude
REAL(8)                       :: Longitude                       ! Location Longitude
REAL(8)                       :: Elevation                       ! Location Elevation
REAL(8)                       :: deltaT                          ! TT - UT1 (seconds)
EQUIVALENCE                     (Location_Vec(1),Longitude)
EQUIVALENCE                     (Location_Vec(2),Latitude)
EQUIVALENCE                     (Location_Vec(3),Elevation)
EQUIVALENCE                     (Location_Vec(4),deltaT)
REAL(8)                       :: a0                              ! Right Ascension (degrees)
REAL(8)                       :: d0                              ! Declination     (degrees)
REAL(8)                       :: W                               ! Prime Meridian Rotation(degrees)
INTEGER(MJD)                  :: dTime                           ! Accumulated leap seconds before Time (10^-7 seconds)
!TYPE(EarthRotationAngle)     :: ERA                             ! Earth Rotation Angle high precision calculation
REAL(8)                       :: angERA                          ! Earth rotation angle                       (radians)
REAL(8)                       :: cosERA                          ! Earth rotation angle cosine              [-1 ... +1]
REAL(8)                       :: sinERA                          ! Earth rotation angle sine                [-1 ... +1]
REAL(8)                       :: V_PCS(3)                        ! Velocity                                    (AU/day)
LOGICAL(4) , PARAMETER        :: UseNovasForEarth = .true.       ! Use the Naval Observatory Astrometric Software (Earth)
LOGICAL(4) , PARAMETER        :: DebugLocal       = .true.       ! Local diagnostic setting for in-house testing
LOGICAL(4)                    :: dUT1Diagnostic                  ! Ensure only 1 diagnostic of dUT1
REAL(16)                      :: Tu                              ! Universal Time with Jan 2000 epoch         (seconds)
REAL(16)                      :: dTu
REAL(16)                      :: AngTu
REAL(16)   , PARAMETER        :: Ang360 = 360.0
REAL(16)   , PARAMETER        :: a      = 0.779057273264
REAL(16)   , PARAMETER        :: b      = 1.00273781191135448
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
jdt = JulianDateFromTime(Time)
IF(Body==Earth) THEN
  !
  !Calculate deltaT
  !
  ! See if we shoud print out dUT1 (UT-UT1)
  dUT1Diagnostic = .not.InitializedBulletinD
  !
  ! Get dUT1 from the appropriate IERS Bulletin D and Leap Seconds from IERS Bulletin C
  dUT1 = dUT1FromIERSBulletins(Time)
  CALL CalculateLeapSecondsBeforeTime(Time, dTime, LeapOccurring)

  deltaT = DBLE(dTime)/DBLE(TimePerSecond) - dUT1 ;  IF(DebugLocal .and. dUT1Diagnostic) WRITE(*,FMT='("For Earth deltaT, UT-UT1 : ",F17.7,1X,F17.7)') deltaT, dUT1

  IF(UseNovasForEarth) THEN
    !
    ! Use NOVAS for Earth
    Location_vec(1:3) = (/ 0,  0,  0 /)
    CALL GEOPOS(jdt,EarthSurface,Location_Vec,R_CRS_PCS(:,1),V_PCS)
    Location_vec(1:3) = (/90,  0,  0 /)
    CALL GEOPOS(jdt,EarthSurface,Location_Vec,R_CRS_PCS(:,2),V_PCS)
    Location_vec(1:3) = (/ 0, 90,  0 /)
    CALL GEOPOS(jdt,EarthSurface,Location_Vec,R_CRS_PCS(:,3),V_PCS)
    T_PCS_CRS = TRANSPOSE(Normalize(R_CRS_PCS))

  ELSE
    !
    ! Use the Earth Rotation angle
    dTu    = REAL(deltaT/86400d0, KIND=16)
    Tu     = REAL(jdt,KIND=16) - REAL(jdt2000,KIND=16) - dTu
    AngTu  = Ang360* ((a+b*Tu) - AINT(a + b*Tu))
    angERA = REAL(AngTu, KIND=8)
    !
    ! Calculate the transformation matrix
    cosERA = DCOSD(angERA)
    sinERA = DSIND(angERA)
    T_PCS_CRS(:,1) = (/ cosERA,-sinERA, 0d0/)
    T_PCS_CRS(:,2) = (/ sinERA, cosERA, 0d0/)
    T_PCS_CRS(:,3) = (/    0d0,    0d0, 1d0/)
  END IF

ELSE
  CALL PlanetoCentricParametersForBody(Body, jdt, a0, d0, w)
  T_PCS_CRS = ProductRotations(Zaxis,w,  Xaxis,Pi/2d0-d0,  Zaxis,Pi/2d0+a0)
END IF

RETURN
END FUNCTION T_PCS_CRS_BodyAtTime

FUNCTION RilluminatedCRS_Body(Body, Robserver_CRS, TimeIlluminated) RESULT(Rilluminated_CRS)
!+
!  Description: This routine calculated the coordinates, in the Celestial Reference System, of a point on a solar
!               system body that can be regarded as the point source of light from that body for targeting purposes.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
USE TimeDatePackage
USE MatrixVectorPackage
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   ,INTENT(IN)      :: Body                           ! Solar system body
REAL(8)      ,INTENT(IN)      :: Robserver_CRS(3)               ! Position of the observer in the celestial reference (km)
INTEGER(MJD) ,INTENT(IN)      :: TimeIlluminated                ! Modified Julian Data (10^-7 seconds)
REAL(8)                       :: Rilluminated_CRS(3)            ! Illuminated centroid of body as seen from earth (km)
!
!                             Local Variables
!-----------------------------+---------------
REAL(8)                       :: jdtIlluminated(2)              ! Julian date, day+fraction (days)
REAL(8)                       :: Rorigin_CRS(3)                 ! Position at viewing origin (km)
REAL(8)                       :: Rtarget_CRS(3)                 ! Position of target viewed from earth (km)
REAL(8)                       :: Rsun_CRS(3)                    ! Position of target viewed from earth (km)
REAL(8)                       :: Ni_CRS(3)                      ! Basis vector of local viewed coordinate system (unit)
REAL(8)                       :: Nj_CRS(3)                      ! Basis vector of local viewed coordinate system (unit)
REAL(8)                       :: Nk_CRS(3)                      ! Basis vector of local viewed coordinate system (unit)
REAL(8)                       :: Radius                         ! Radius of target body (km)
INTEGER(4)                    :: j                              ! Index through 1st coordinate cells in 2D viewable image
INTEGER(4)                    :: k                              ! Index through 2nd coordinate cells in 2D viewable image
REAL(8)                       :: dR                             ! Cell increment in 2D image (m)
REAL(8)                       :: dJ                             ! 1st Coordinate of 2D viewable image (km)
REAL(8)                       :: dK                             ! 2nd Coordinate of 2D viewable image (km)
REAL(8)                       :: dI                             ! Coordinate to body surface from 2D viewable image (km)
REAL(8)                       :: dI2                            ! Square of dI (km^2)
REAL(8)                       :: Rview_CRS(3)                   ! Point on the viewable 2d image of the body. (km)
REAL(8)                       :: Rsum_CRS(3)                    ! Sum of lighted points in viewable 2d image of the body. (km)
REAL(8)                       :: Nsurf_CRS(3)                   ! Normalized vector to surface from viewable 2d image of body
INTEGER(4)                    :: NumLighted                     ! No. of lighted points in viewable 2d image of the body.
INTEGER(4)                    :: NumVisible                     ! No. of visible points in viewable 2d image of the body.
REAL(8)                       :: Nsun_CRS(3)                    ! Normal vector to the sun from the body
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Rilluminated_CRS = (/0,0,0/)
  jdtIlluminated = JulianDateExtendedFromTime(TimeIlluminated)
  Rorigin_CRS    = Robserver_CRS
  Rtarget_CRS    = RcrsBodyAtJdt(jdtIlluminated,Body)
  Rsun_CRS       = RcrsBodyAtJdt(jdtIlluminated,Sun)
  Nsun_CRS       = Normalize(Rsun_CRS-Rtarget_CRS)
  !
  ! Calculate the basis vectors for the viewed body coordinate system
  Ni_CRS = Normalize((Rorigin_CRS - Rtarget_CRS))
  Nj_CRS = OrthogonalVector(Ni_CRS)
  Nk_CRS = CrossProduct(Ni_CRS,Nj_CRS)

  Radius = RadiusEquatorialBody(Body)

  Rsurf_CRS_j_k = 0d0
  Rview_CRS_j_k = 0d0
  Light_CRS_j_k = 0d0
  Rsum_CRS      = 0d0
  NumVisible    = 0
  NumLighted    = 0

  !Generate an (i,j) grid over the viewable circle of the body
  dR     = Radius/DBLE(jViewMax)
  DO j = -jViewMax, +jViewMax
    dJ  = dR*DBLE(j)
    DO k = -kViewMax, +kViewMax
      dK  = DBLE(k)*dR
      dI2 = Radius**2 - dK**2 - dJ**2
      IF(dI2 < 0d0) CYCLE
      NumVisible = NumVisible + 1
      dI         = SQRT(dI2)
      Rview_CRS = Rtarget_CRS + Nk_CRS * dK + Nj_CRS * dJ                 !Point on the viewable projection of the body
      Rsurf_CRS_j_k(:,j,k) = Rview_CRS + dI*Ni_CRS                        !Projection of that Point onto the surface
      Rview_CRS_j_k(:,j,k) = Rview_Crs
      Nsurf_CRS            = Normalize(Rsurf_CRS_j_k(:,j,k)-Rtarget_CRS)  !Normal vector from the body center to the surface point
      IF(DOT_PRODUCT(Nsurf_CRS,Nsun_CRS) > 0d0) THEN                      !Test for illumination.
        Light_CRS_j_k(:,j,k) = 1d0
        Rsum_CRS = Rsum_CRS + Rview_CRS_j_k(:,j,k)
        NumLighted = NumLighted + 1
      END IF
    END DO
  END DO
  TimeViewCalculated     = TimeIlluminated
  BodyViewCalculated     = Body
  Rilluminated_CRS       = Rsum_CRS / (DBLE(NumLighted) + 1d-8)
  IlluminationCalculated = DBLE(NumLighted)/DBLE(NumVisible)
END FUNCTION RilluminatedCRS_Body

FUNCTION RcrsBodyAtJdt(JdtAt, Body) RESULT(R_CRS)
!+
!  Description: This routine calculates the coordinates, in the Celestial Reference System, of a solar system body at
!               a point in time.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
USE TimeDatePackage
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: Body                           ! Solar system body
REAL(8)   ,INTENT(IN)         :: jdtAt(2)                       ! Julian date, day+fraction (days)
REAL(8)                       :: R_CRS(3)                       ! Position of body (km)
!
!                              Local Variables
!-----------------------------+------------------
REAL(8)                       :: JPL_Vec(6)                     ! Position and Velocity (AU,AU/day)
TYPE(JPL_Vector)              :: JPL                            ! Position and Velocity (AU,AU/day)
EQUIVALENCE                     (JPL,JPL_Vec)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  CALL DPLEPH(jdtAt, Body, SolarSystemBarycenter, JPL_VEC)
  R_CRS = JPL%R_CRS * EPH%AU
END FUNCTION RcrsBodyAtJdt

FUNCTION IlluminationViewedShapeBody(Body, TimeIlluminated) RESULT(Illumination_i_i)
!+
!  Description: This routine calculated the coordinates, in the Celestial Reference System, of a point on a solar
!               system body that can be regarded as the point source of light from that body for targeting applications.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
USE TImeDatePackage
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN)      :: Body                           ! Solar system body
INTEGER(MJD), INTENT(IN)      :: TimeIlluminated                ! Modified Julian Data (10^-7 seconds)
REAL(8)                       :: Illumination_i_i(-64:64,-64:64)! Illuminated shape of body as seen from earth (kw/m^2)
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                       :: jdt                             !Julian data of TimeIlluminated                (days)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(Body > 0) THEN
    jdt = JulianDateFromTime(TimeIlluminated)
  END IF
  Illumination_i_i = 0
END FUNCTION IlluminationViewedShapeBody
!
SUBROUTINE PlanetoCentricParametersForBody(Body, jdt, a0, d0, w)
!+
!  Description: This routine retrieves the parameters for the Planetocentric Coordinate System for a given solar syistem
!               body at the give Julian Date.
!
USE NumericalConstants
!USE EphemerisTypes
!
!   References:
!
!           REPORT OF THE IAU/IAG WORKING GROUP ON
!   CARTOGRAPHIC COORDINATES AND ROTATIONAL ELEMENTS OF
!                  THE PLANETS AND SATELLITES
!
!          2001 Report
!          http://www.hnsky.org/iau-iag.htm
!
!          2006 Report
!          http://www2.keck.hawaii.edu/realpublic/inst/people/conrad/research/pub/WGCCRE2006Feb9.pdf
!
!
!
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: Body                           ! Solar system body
REAL(8),   INTENT(IN)         :: jdt                            ! Julian date
REAL(8),   INTENT(OUT)        :: a0                             ! Right Ascension (degrees)
REAL(8),   INTENT(OUT)        :: d0                             ! Declination     (degrees)
REAL(8),   INTENT(OUT)        :: W                              ! Prime Meridian Rotation(degrees)

!                               Local
!-----------------------------+--------
REAL(8)                       :: T                              ! Centuries since Julian Epoch
REAL(8)                       :: D                              ! Days since Julian Epoch
REAL(8)                       :: N                              ! Neptune Correction (Degrees)
REAL(8)                       :: SINN                           ! Sine of Neptune Correction
REAL(8)                       :: COSN                           ! Cosine of Neptune Correction
REAL(8)                       :: E(13)                          ! Moon Corrections (Degrees)
REAL(8)                       :: SINE(13)                       ! Sine of Moon Corrections
REAL(8)                       :: COSE(13)                       ! Cosine of Moon Corrections
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
!
d = jdt - jdt2000
T = d/36525.0d0

SELECT CASE (Body)

CASE (Mercury)
  a0 = 281.01D0  - 0.033D0*T
  d0 =  61.45D0  - 0.005D0*T
  W  = 329.548D0 + 6.1385025D0*d

CASE (Venus)
  a0 = 272.76D0
  d0 =  67.16D0
  W  = 160.20D0 - 1.4813688D0*d

CASE (Earth)
  a0 =   0.00D0 - 0.641D0*T
  d0 =  90.00D0 - 0.557D0*T
  W  = 190.147D0 + 360.9856235D0*d    !Note: 190.147 was updated in the 2006 Report

CASE (Mars)
  a0 = 317.68143D0 - 0.1061D0*T
  d0 =  52.88650D0 - 0.0609D0*T
  W  = 176.753D0   + 350.89198226D0*d

CASE (Jupiter)
  a0 = 268.05D0 - 0.009D0*T
  d0 =  64.49D0 + 0.003D0*T
  W  = 284.95D0 + 870.5366420D0*d

CASE (Saturn)
  a0 = 40.589D0 - 0.036D0*T
  d0 = 83.537D0 - 0.004D0*T
  W  = 38.900D0 + 810.7939024D0*d

CASE (Uranus)
  a0 = 257.311D0
  d0 = -15.175D0
  W  = 203.810D0 - 501.1600928D0*D

CASE (Neptune)
  N    = 357.85D0 + 52.316D0*d
  sinN = DSIND(N)
  cosN = DCOSD(N)
  a0   = 299.36D0 + 0.70D0*sinN
  d0   =  43.46D0 - 0.51D0*cosN
  W    = 253.18D0 + 536.3128492D0*D - 0.48D0*sinN

CASE (Pluto)
  a0 = 313.02D0
  d0 =   9.09D0
  W  = 236.77D0 - 56.3623195D0*d

CASE (Moon)
  E = (/125.045D0 -  0.0529921D0*d, 250.089D0 - 0.1059842D0*d, 260.008D0 + 13.0120009D0*d &
       ,176.625D0 + 13.3407154D0*d, 357.529D0 + 0.9856003D0*d, 311.589D0 + 26.4057084D0*d &
       ,134.963D0 + 13.0649930D0*d, 276.617D0 + 0.3287146D0*d,  34.226D0 +  1.7484877D0*d &
       , 15.134D0 -  0.1589763D0*d, 119.743D0 + 0.0036096D0*d, 239.961D0 +  0.1643573D0*d &
       , 25.053D0 + 12.9590088D0*d &
      /)

  sinE = DSIND(E)
  cosE = DCOSD(E)

  a0 = 269.9949D0 + 0.0031D0*T                                                     &
     - 3.8787D0*sinE(1) - 0.1204D0*sinE(2)  + 0.0700D0*sinE(3)  - 0.0172d0*sinE(4) &
     + 0.0072D0*sinE(6) - 0.0052D0*sinE(10) + 0.0043D0*sinE(13)


  d0 = 66.5392D0        + 0.0130D0*T                                               &
     + 1.5419D0*cosE(1) + 0.0239D0*cosE(2) - 0.0278D0*cosE(3)   + 0.0068D0*cosE(4) &
     - 0.0029D0*cosE(6) + 0.0009D0*cosE(7) + 0.0008D0*cosE(10)  - 0.0009D0*cosE(13)

  W  = 38.3213D0         + 13.17635815D0*d   - 1.4D-12*d**2      &
     + 3.5610D0*sinE(1)  + 0.1208D0*sinE(2)  - 0.0642D0*sinE(3)  &
     + 0.0158D0*sinE(4)  + 0.0252D0*sinE(5)  - 0.0066D0*sinE(6)  &
     - 0.0047D0*sinE(7)  - 0.0046D0*sinE(8)  + 0.0028D0*sinE(9)  &
     + 0.0052D0*sinE(10) + 0.0040D0*sinE(11) + 0.0019D0*sinE(12) &
     - 0.0044D0*sinE(13)

CASE (Sun)
  a0 = 286.13D0
  d0 =  63.87D0
  W  =  84.18D0 + 14.1844000D0*d     !Note 84.18 was corrected in the 2006 Report

CASE DEFAULT
  a0 = 0.0d0
  d0 = 0.0d0
  W  = 0.0d0

END SELECT
a0 = MOD(a0,360.0D0)*PI/180.0d0
d0 = MOD(d0,360.0d0)*PI/180.0d0
W  = MOD(W ,360.0d0)*PI/180.0d0

RETURN
END SUBROUTINE PlanetoCentricParametersForBody

FUNCTION EphemerisDailyInitialization() RESULT(StatusDaily)
!+
!  Description: This routine updates Ephemeris daily data.  It should be run during the task startup and just after
!                                                           midnight every subsequent day
!
USE TimeDatePackage
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: statusDaily                    ! Return status (1 = success)
!
!                               Local Variables
!-----------------------------+---------------------------------
INTEGER(MJD)                  :: TimeNow                        ! Modified Julian Date currently           (10^-7sec)
TYPE(TimeDateISO)             :: DateAndTime                    ! ISO 8601:1988 compatible date/time structure
LOGICAL(4)                    :: AlreadyInitialized             ! Set true if initialization has already been done
REAL(8)                       :: Altitude                       ! Height above mean sea level                    (km)
REAL(8)                       :: Longitude                      ! Geodetic Longitude WGS84                  (degrees)
REAL(8)                       :: Latitude                       ! Geodetic Longitude WGS84                  (degrees)
REAL(8)                       :: Elevation                      ! Surface elevation                              (km)
INTEGER(4)                    :: StatusJPL                      ! Return status from InitializeJPLInterface
LOGICAL(4)                    :: UpdateLocation                 ! If true then update the ENU location
CHARACTER(24)                 :: TimeStamp                      ! Local time stamp
CHARACTER(4)                  :: FormatTime                     ! Local time zone
REAL(8)                       :: HoursOfSunLight                ! Hours of sunlight                           (hours)
LOGICAL(4), PARAMETER         :: DebugLocal = .false.           ! Diagnostic flag for in-house testing
LOGICAL(4), PARAMETER         :: DebugTimes = .true.            ! Diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Get the current time
  CALL GetTimeNow(TimeNow)

  ! Check to see if we already initialized
  CALL CalculateISODateFromTime(TimeNow, DateAndTime)
  AlreadyInitialized = DateAndTime%Year  == DateAndTimeToday%Year .and. &
                       DateAndTime%Day   == DateAndTimeToday%Day  .and. &
                       DateAndTime%Month == DateAndTimeToday%Month
  IF(AlreadyInitialized) THEN
    StatusDaily = EPH_S_NORMAL
    RETURN
  END IF

  Initialized = .false.

  ! Ensure the JPL Ephemeris is read in
  statusJPL = InitializeJPLInterface()

  UpdateLocation = LocationSite%Latitude  .ne. LocationForTime%Latitude  .or. &
                   LocationSite%Longitude .ne. LocationForTime%Longitude .or. &
                   LocationSite%Elevation .ne. LocationForTime%Elevation
  IF(UpdateLocation) THEN
    Latitude  = LocationForTime%Latitude
    Longitude = LocationForTime%Longitude
    Elevation = LocationForTime%Elevation
    LocationSite = LocationOnSurface(Latitude, Longitude, Elevation)
    IF(DebugLocal) THEN
      WRITE(*,FMT='(" LocationSite Updated: Latitude  ",F17.7)') LocationSite%Latitude
      WRITE(*,FMT='(" LocationSite Updated: Longitude ",F17.7)') LocationSite%Longitude
      WRITE(*,FMT='(" LocationSite Updated: Elevation ",F17.7)') LocationSite%Elevation
    END IF
  END IF

  ! Initialize the Airmass Model
  Altitude = LocationSite%Elevation
  CALL InitializeAirmassModel(Altitude)

  TimesSolarToday = TimesSolarForLocation(LocationSite, WeatherSite, TimeNow)
  DateAndTimeToday   = DateAndTime

  IF(DebugTimes) THEN
    CALL ConvertTimeToLocalTimeStamp(TimesSolarToday%Rise,    TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *R"
    CALL ConvertTimeToLocalTimeStamp(TimesSolarToday%Noon,    TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *N"
    CALL ConvertTimeToLocalTimeStamp(TimesSolarToday%Transit, TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *T"
    CALL ConvertTimeToLocalTimeStamp(TimesSolarToday%Set,     TimeStamp, FormatTime) ; WRITE(*,FMT='(A)')Timestamp//" "//FormatTime//" *S"
    HoursOfSunLight = DBLE(TimesSolarToday%Set - TimesSolarToday%Rise)/DBLE(TimePerHour)
    WRITE(*,FMT='("Hours of Sunlight: ",F6.3)') HoursOfSunLight
  END IF

  IF(DebugLocal) THEN
    WRITE(*,FMT='("AirmassOne  : ",F17.7," (kg) ")')AirMassOne
    WRITE(*,FMT='("Altitude    : ",F17.7," (km) ")')Altitude
    WRITE(*,FMT='(" Sea Level  : AirmassZen0: ",F17.7," AirmassZen90: ",F17.7)') AirmassSeaLevel_ang(0),AirmassSeaLevel_ang(90)
    WRITE(*,FMT='(" At altitude: AirmassZen0: ",F17.7," AirmassZen90: ",F17.7)') Airmass_ang(0),Airmass_ang(90)
  END IF
  Initialized = .true.
  StatusDaily = 1
  RETURN

END FUNCTION EphemerisDailyInitialization

FUNCTION FindWhenTheSunIsAtAngleElevation(AngleElevation, TimeApproximate, Location, Weather, TimeAtElevation, AngleAzimuthal) RESULT(StatusFound)
!+
! Description: Find the time where the sun is at a target elevation during a day.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 19-MAR-2025     | MA    | SCR 150: Update the Ephemeris for GNU Fortran
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!
USE Utilities
USE TimeDatePackage
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)          ,INTENT(IN)  :: AngleElevation                 ! Target Solar Elevation Angle                  (degrees)
INTEGER(MJD)     ,INTENT(IN)  :: TimeApproximate                ! MJD Approximate time to start search    (10^-7 seconds)
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Location for solar information
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Weather Data for the site
INTEGER(MJD)     ,INTENT(OUT) :: TimeAtElevation                ! MJD Time when the sun is at the target  (10^-7 seconds)
REAL(8)          ,INTENT(OUT) :: AngleAzimuthal                 ! Solar Azimuthal Angle                         (degrees)
INTEGER(4)                    :: StatusFound                    ! EPH_S_NORMAL if successful
!
!                                Local
!-----------------------------+---------
INTEGER(MJD)                  :: TimeBefore                     ! MJD Time Before time limit in search    (10^-7 seconds)
INTEGER(MJD)                  :: TimeAfter                      ! MJD Time After time limit in search     (10^-7 seconds)
REAL(8)                       :: AngleElevationBefore           ! Sun elevation at TimeBefore                   (degrees)
REAL(8)                       :: AngleElevationAfter            ! Sun elevation at TimeAfter                    (degrees)
INTEGER(MJD)                  :: dTimeStep                      ! MJD Time delta in search                (10^-7 seconds)
LOGICAL(4)                    :: Found                          ! Set true when a search is successful
INTEGER(4)                    :: step                           ! Iteration index in search for target angle
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
REAL(8)                       :: dTimedAngle                    ! Slope of linear Angle vs Time function (seconds/degree)
INTEGER(4), PARAMETER         :: stepMax         = 20           ! Maximum iterations in search
INTEGER(4), PARAMETER         :: stepMin         = 3            ! Maximum iterations in search
REAL(8)   , PARAMETER         :: dAngleConverged = 0.000001d0   ! Convergence tolerance for search              (degrees)
LOGICAL(4), PARAMETER         :: DebugLocal      = .false.      ! Diagnostic flag for in-house testing
LOGICAL(4), PARAMETER         :: CheckAccuracy   = .false.      ! Check if the determination is accurate to 1 millisecond
INTEGER(MJD)                  :: TimeAt                         ! MJD Time during the accuracy check       (10^-7 seconds)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! Initialize Found
  Found = .false.

  ! Find TimeBefore and TimeAfter where the sun elevation target angle lies
  dTimeStep  = INT(30, KIND=8)*TimePerSecond
  TimeBefore = TimeApproximate
  TimeAfter  = TimeApproximate
  DO step = 1, stepMax
    TimeBefore = TimeBefore - dTimeStep
    TimeAfter  = TimeAfter  + dTimeStep
    SolarInformation     = SolarInformationForLocation(TimeBefore, Location, Weather)
    AngleElevationBefore = SolarInformation%AngleElevation
    SolarInformation     = SolarInformationForLocation(TimeAfter , Location, Weather)
    AngleElevationAfter  = SolarInformation%AngleElevation
    IF(AngleElevationBefore <= AngleElevationAfter) THEN
      Found = WithinClosedSet(AngleElevationBefore, AngleElevation, AngleElevationAfter )
    ELSE
      Found = WithinClosedSet(AngleElevationAfter , AngleElevation, AngleElevationBefore)
    END IF
    IF(DebugLocal) THEN
      WRITE(*,FMT='(" Find..Elev 1st Loop: Step ",I4," Angles [",3(1X,F10.6),"]")') Step                 &
                                                                                  , AngleElevationBefore &
                                                                                  , AngleElevation &
                                                                                  , AngleElevationAfter
    END IF
    IF(Found) EXIT
  END DO

  IF(.not.Found) THEN
    StatusFound = EPH_E_FAILURE
    TimeAtElevation = TimeBefore
    RETURN
  END IF

  ! Successively linearize and find solution for target angle
  DO step = 1, stepMax
    dTimedAngle = (AngleElevation - AngleElevationBefore)/(AngleElevationAfter - AngleElevationBefore)
    dTimeStep   = INT( dTimedAngle * DBLE(TimeAfter - TimeBefore), KIND=8)
    TimeBefore  = TimeBefore + dTimeStep
    SolarInformation     = SolarInformationForLocation(TimeBefore, Location, Weather)
    AngleElevationBefore = SolarInformation%AngleElevation
    Found = ABS(AngleElevationBefore - AngleElevation) < dAngleConverged .and. Step >= StepMin
    IF(DebugLocal) THEN
      WRITE(*,FMT='(" Find..Elev 2nd Loop: Step ",I4," Angles [",3(1X,F10.6),"]")') Step                 &
                                                                                  , AngleElevationBefore &
                                                                                  , AngleElevation &
                                                                                  , AngleElevationAfter
      IF(Found) WRITE(*,FMT='("   Converged dAngle: ",F10.6)') ABS(AngleElevationBefore - AngleElevation)
    END IF
    IF(Found) EXIT
  END DO
  TimeAtElevation = TimeBefore
  AngleAzimuthal  = SolarInformation%AngleAzimuthal

  IF(CheckAccuracy) THEN
    WRITE(*,FMT='(A)')"             "
    IF(AngleAzimuthal < 180d0) THEN
      WRITE(*,FMT='(A)')"Sunrise ----------------------------------------------------"
    ELSE
      WRITE(*,FMT='(A)')"Sunset -----------------------------------------------------"
    END IF
    WRITE(*,FMT='("Steps : ",I4)')Step
    dTimeStep = TimePerMsec
    DO TimeAt = TimeAtElevation - dTimeStep, TimeAtElevation + dTimeStep, dTimeStep
        SolarInformation = SolarInformationForLocation(TimeAt, Location, Weather)
        WRITE(*,FMT='(" AccuracyCheck: ",A24,1x,A4," Fraction : ",F14.11," Azimuthal ",F16.7)') SolarInformation%TimeStampLocal &
                                                                                               ,SolarInformation%FormatLocal    &
                                                                                               ,SolarInformation%SolarFraction  &
                                                                                               ,SolarInformation%AngleAzimuthal
    END DO
  END IF

  IF(.not.Found) THEN
    StatusFound   = EPH_E_FAILURE
  ELSE
    StatusFound   = EPH_S_NORMAL
  END IF
  RETURN
END FUNCTION FindWhenTheSunIsAtAngleElevation

FUNCTION FindWhenTheSunIsAtAngleAzimuthal(AngleAzimuthal, TimeApproximate, Location, Weather, TimeAtAzimuthal, AngleElevation) RESULT(StatusFound)
!+
! Description: Find the time where the sun is at a target azimuthal angle during a day.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 19-MAR-2025     | MA    | SCR 150: Update the Ephemeris for GNU Fortran
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!
USE Utilities
USE TimeDatePackage
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)          ,INTENT(IN)  :: AngleAzimuthal                 ! Target Solar Azimuthal Angle                  (degrees)
INTEGER(MJD)     ,INTENT(IN)  :: TimeApproximate                ! MJD Approximate time to start search    (10^-7 seconds)
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Location for solar information
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Weather Data for the site
INTEGER(MJD)     ,INTENT(OUT) :: TimeAtAzimuthal                ! MJD Time when the sun is at the target  (10^-7 seconds)
REAL(8)          ,INTENT(OUT) :: AngleElevation                 ! Solar Elevation Angle                         (degrees)
INTEGER(4)                    :: StatusFound                    ! EPH_S_NORMAL if successful
!
!                                Local
!-----------------------------+---------
INTEGER(MJD)                  :: TimeBefore                     ! MJD Time Before time limit in search    (10^-7 seconds)
INTEGER(MJD)                  :: TimeAfter                      ! MJD Time After time limit in search     (10^-7 seconds)
REAL(8)                       :: AngleAzimuthalBefore           ! Sun azimuthal at TimeBefore                   (degrees)
REAL(8)                       :: AngleAzimuthalAfter            ! Sun azimuthal at TimeAfter                    (degrees)
INTEGER(MJD)                  :: dTimeStep                      ! MJD Time delta in search                (10^-7 seconds)
LOGICAL(4)                    :: Found                          ! Set true when a search is successful
INTEGER(4)                    :: step                           ! Iteration index in search for target angle
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
REAL(8)                       :: dTimedAngle                    ! Slope of linear Angle vs Time function (seconds/degree)
INTEGER(4), PARAMETER         :: stepMax         = 20           ! Maximum iterations in search
INTEGER(4), PARAMETER         :: stepMin         = 3            ! Maximum iterations in search
REAL(8)   , PARAMETER         :: dAngleConverged = 0.000001d0   ! Convergence tolerance for search              (degrees)
LOGICAL(4), PARAMETER         :: DebugLocal      = .false.      ! Diagnostic flag for in-house testing
LOGICAL(4), PARAMETER         :: CheckAccuracy   = .false.      ! Check if the determination is accurate to 1 millisecond
INTEGER(MJD)                  :: TimeAt                         ! MJD Time during the accuracy check       (10^-7 seconds)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! Initialize Found
  Found = .false.

  ! Find TimeBefore and TimeAfter where the sun azimuthal target angle lies
  dTimeStep  = INT(30, KIND=8)*TimePerSecond
  TimeBefore = TimeApproximate
  TimeAfter  = TimeApproximate
  DO step = 1, stepMax
    TimeBefore = TimeBefore - dTimeStep
    TimeAfter  = TimeAfter  + dTimeStep
    SolarInformation     = SolarInformationForLocation(TimeBefore, Location, Weather)
    AngleAzimuthalBefore = SolarInformation%AngleAzimuthal
    SolarInformation     = SolarInformationForLocation(TimeAfter , Location, Weather)
    AngleAzimuthalAfter  = SolarInformation%AngleAzimuthal
    IF(AngleAzimuthalBefore <= AngleAzimuthalAfter) THEN
      Found = WithinClosedSet(AngleAzimuthalBefore, AngleAzimuthal, AngleAzimuthalAfter )
    ELSE
      Found = WithinClosedSet(AngleAzimuthalAfter , AngleAzimuthal, AngleAzimuthalBefore)
    END IF
    IF(DebugLocal) THEN
      WRITE(*,FMT='(" Find..Azim 1st Loop: Step ",I4," Angles [",3(1X,F10.6),"]")') Step                 &
                                                                                  , AngleAzimuthalBefore &
                                                                                  , AngleAzimuthal &
                                                                                  , AngleAzimuthalAfter
    END IF
    IF(Found) EXIT
  END DO

  IF(.not.Found) THEN
    StatusFound = EPH_E_FAILURE
    TimeAtAzimuthal = TimeBefore
    RETURN
  END IF

  ! Successively linearize and find solution for target angle
  DO step = 1, stepMax
    dTimedAngle = (AngleAzimuthal - AngleAzimuthalBefore)/(AngleAzimuthalAfter - AngleAzimuthalBefore)
    dTimeStep   = INT( dTimedAngle * DBLE(TimeAfter - TimeBefore), KIND=8)
    TimeBefore  = TimeBefore + dTimeStep
    SolarInformation     = SolarInformationForLocation(TimeBefore, Location, Weather)
    AngleAzimuthalBefore = SolarInformation%AngleAzimuthal
    Found = ABS(AngleAzimuthalBefore - AngleAzimuthal) < dAngleConverged .and. Step >= StepMin
    IF(DebugLocal) THEN
      WRITE(*,FMT='(" Find..Azim 2nd Loop: Step ",I4," Angles [",3(1X,F10.6),"]")') Step                 &
                                                                                  , AngleAzimuthalBefore &
                                                                                  , AngleAzimuthal &
                                                                                  , AngleAzimuthalAfter
      IF(Found) WRITE(*,FMT='("   Converged dAngle: ",F10.6)') ABS(AngleAzimuthalBefore - AngleAzimuthal)
    END IF
    IF(Found) EXIT
  END DO
  TimeAtAzimuthal = TimeBefore
  AngleElevation  = SolarInformation%AngleElevation

  IF(CheckAccuracy) THEN
    WRITE(*,FMT='(A)')"             "
    WRITE(*,FMT='(A)')"SolarTransit -----------------------------------------------"
    WRITE(*,FMT='("Steps : ",I4)')Step
    dTimeStep = TimePerMsec
    DO TimeAt = TimeAtAzimuthal - dTimeStep, TimeAtAzimuthal + dTimeStep, dTimeStep
        SolarInformation = SolarInformationForLocation(TimeAt, Location, Weather)
        WRITE(*,FMT='(" AccuracyCheck: ",A24,1x,A4," Fraction : ",F14.11," Azimuthal ",F16.7)') SolarInformation%TimeStampLocal &
                                                                                               ,SolarInformation%FormatLocal    &
                                                                                               ,SolarInformation%SolarFraction  &
                                                                                               ,SolarInformation%AngleAzimuthal
    END DO
  END IF
  IF(.not.Found) THEN
    StatusFound   = EPH_E_FAILURE
  ELSE
    StatusFound   = EPH_S_NORMAL
  END IF
  RETURN
END FUNCTION FindWhenTheSunIsAtAngleAzimuthal
!
FUNCTION FindWhenTheTimeIsSolarNoon(TimeApproximate, Location, Weather, AngleElevationNoon, TimeSolarNoon)  RESULT(StatusFound)
!+
! Description: Find Solar Noon, the time where the sun is at its maximum elevation during a day.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 19-MAR-2025     | MA    | SCR 150: Update the Ephemeris for GNU Fortran
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE TimeDatePackage
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD)     ,INTENT(IN)  :: TimeApproximate                ! MJD Approximate time to start search    (10^-7 seconds)
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Location for solar information
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Weather Data for the site
REAL(8)          ,INTENT(OUT) :: AngleElevationNoon             ! Sun Elevation Angle at Solar Noon             (degrees)
INTEGER(MJD)     ,INTENT(OUT) :: TimeSolarNoon                  ! MJD Time when the sun is highest        (10^-7 seconds)
INTEGER(4)                    :: StatusFound                    ! EPH_S_NORMAL if successful
!
!                                Local
!-----------------------------+---------
INTEGER(MJD)                  :: TimeAt                         ! MJD Time during the search              (10^-7 seconds)
INTEGER(MJD)                  :: dTimeStep                      ! MJD Time delta in search                (10^-7 seconds)
LOGICAL(4)                    :: FoundSolarNoon                 ! Set true when a search is successful
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
INTEGER(4)                    :: step                           ! Iteration index in search for target angle
INTEGER(4), PARAMETER         :: stepMax       = 60             ! Maximum iterations in search
LOGICAL(4), PARAMETER         :: DebugLocal    = .false.        ! Diagnostic flag for in-house testing
LOGICAL(4), PARAMETER         :: CheckAccuracy = .false.
INTEGER(4), PARAMETER         :: refineMax     = 12
INTEGER(4)                    :: refine
INTEGER(MJD)                  :: TimeBefore
INTEGER(MJD)                  :: TimeAfter
INTEGER(MJD)                  :: Time1st
INTEGER(MJD)                  :: Time2nd
REAL(8)                       :: Angle1st
REAL(8)                       :: Angle2nd
REAL(8)                       :: dAngle
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  FoundSolarNoon     = .false.
  SolarInformation   = SolarInformationForLocation(TimeApproximate, Location, Weather)
  TimeSolarNoon      = TimeApproximate
  AngleElevationNoon = SolarInformation%AngleElevation
  TimeAt             = TimeApproximate + TimePerSecond
  SolarInformation   = SolarInformationForLocation(TimeAt, Location, Weather)
  IF(SolarInformation%AngleElevation > AngleElevationNoon) THEN
    AngleElevationNoon = SolarInformation%AngleElevation
    TimeSolarNoon      = TimeAt
    dTimeStep          = TimePerSecond
  ELSE
    TimeAt = TimeApproximate - TimePerSecond
    SolarInformation   = SolarInformationForLocation(TimeAt, Location, Weather)
    IF(SolarInformation%AngleElevation > AngleElevationNoon) THEN
      AngleElevationNoon = SolarInformation%AngleElevation
      TimeSolarNoon      = TimeAt
      dtimeStep          = -TimePerSecond
    ELSE
      dTimeStep      = TimePerSecond
      FoundSolarNoon = .true.
    END IF
  END IF
  Step = 0;
  DO WHILE(.not.FoundSolarNoon)
    Step = Step + 1
    IF(Step > StepMax) EXIT
    TimeAt = TimeAt + dtimeStep
    SolarInformation   = SolarInformationForLocation(TimeAt, Location, Weather)
    IF(DebugLocal) THEN
      WRITE(*,FMT='(" SolarFraction: ",A24,1x,A4," Fraction : ",F11.7," Elevation ",F11.7)') SolarInformation%TimeStampLocal &
                                                                                            ,SolarInformation%FormatLocal    &
                                                                                            ,SolarInformation%SolarFraction  &
                                                                                            ,SolarInformation%AngleElevation
    END IF
    IF(SolarInformation%AngleElevation < AngleElevationNoon) THEN
      FoundSolarNoon = .true.
    ELSE
      TimeSolarNoon      = TimeAt
      AngleElevationNoon = SolarInformation%AngleElevation
    END IF
  END DO
  IF(FoundSolarNoon) THEN
    !
    ! Refine the solution
    TimeBefore = TimeSolarNoon - dTimeStep
    TimeAfter  = TimeSolarNoon + dTimeStep
    DO refine = 1, refineMax
      Time1st          = (TimeBefore + TimeSolarNoon)/2
      SolarInformation = SolarInformationForLocation(Time1st, Location, Weather)
      Angle1st         = SolarInformation%AngleElevation
      Time2nd          = (TimeSolarNoon + TimeAfter)/2
      SolarInformation = SolarInformationForLocation(Time2nd, Location, Weather)
      Angle2nd         = SolarInformation%AngleElevation
      IF(Angle1st > Angle2nd) THEN
        IF(Angle1st > AngleElevationNoon) THEN
          TimeAfter      = TimeSolarNoon
          TimeSolarNoon  = Time1st
          dAngle         = Angle1st - AngleElevationNoon
          AngleElevationNoon = Angle1st
        ELSE
          TimeBefore     = Time1st
          TimeAfter      = Time2nd
          dAngle         = AngleElevationNoon - Angle1st
        END IF
      ELSE
        IF(Angle2nd > AngleElevationNoon) THEN
          TimeBefore     = TimeSolarNoon
          TimeSolarNoon  = Time2nd
          dAngle         = Angle2nd - AngleElevationNoon
          AngleElevationNoon = Angle2nd
        ELSE
          TimeBefore     = Time1st
          TimeAfter      = Time2nd
          dAngle         = AngleElevationNoon - Angle1st
        END IF
      END IF
      IF(DebugLocal) WRITE(*,FMT='("Find Noon : Refine ",I4," dAngle: ",F12.8)') refine, ABS(dAngle)
    END DO
    IF(CheckAccuracy) THEN
      WRITE(*,FMT='(A)')"             "
      WRITE(*,FMT='(A)')"SolarNoon --------------------------------------------------"
      WRITE(*,FMT='("Steps : ",I4)')Step
      dTimeStep = TimePerMsec
      DO TimeAt = TimeSolarNoon - dTimeStep, TimeSolarNoon + dTimeStep, dTimeStep
        SolarInformation = SolarInformationForLocation(TimeAt, Location, Weather)
        WRITE(*,FMT='(" AccuracyCheck: ",A24,1x,A4," Fraction : ",F14.11," Elevation ",F16.11)') SolarInformation%TimeStampLocal &
                                                                                                ,SolarInformation%FormatLocal    &
                                                                                                ,SolarInformation%SolarFraction  &
                                                                                                ,SolarInformation%AngleElevation
      END DO
    END IF
    StatusFound   = EPH_S_NORMAL
  ELSE
    StatusFound   = EPH_E_FAILURE
  END IF
  RETURN
END FUNCTION FindWhenTheTimeIsSolarNoon

!
FUNCTION InitializeJPLInterface() RESULT(statusInit)
!+
!  Description: This routine initialize the interface to the JPL Ephemeris.
!
USE NumericalConstants
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: statusInit                     ! Return status (1 = success)
!
!                               Local
!-----------------------------+--------
INTEGER(4)                    :: body                           ! Index through Solar Syatem bodies
REAL(8)                       :: Ra                             ! Equatorial radius of body (km)
REAL(8)                       :: Rb                             ! Polar radius of body (km)
REAL(8)                       :: f                              ! Flattening of body (fraction)
REAL(8)                       :: e                              ! Body eccentricity
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  statusInit = 1
  !
  IF(ReadJPLEphemeris) RETURN

  !Retrieve the JPL Constants
  CALL CONST(Name_JPL, Val_JPL, VAl_SSS, JPLEnd)

  !From IERS Conventions (2006)
  EPH%RE   = 6378.1366d0
  !
  !IAU 2015
  EPH%ASUN = 695700.00d0

  !Set up the oblate spheriod paramters for each solar system body
  DO Body = 1, BodyEnd
    Ra  = RadiusEquatorialBody(Body)
    f   = FlatteningBody(Body)
    Rb  = Ra*(1d0-f)
    e   = DSQRT(f*(2d0-f))
    Ellipsoid_Body(Body)%Ra      = Ra
    Ellipsoid_Body(Body)%f       = f
    Ellipsoid_Body(Body)%Rb      = Rb
    Ellipsoid_Body(Body)%e       = e
    Ellipsoid_Body(Body)%e2      = e*e
    Ellipsoid_Body(Body)%e2prime = (Ra**2-Rb**2)/(Rb**2)
  END DO

  ReadJPLEphemeris = .true.
  RETURN
END FUNCTION InitializeJPLInterface

SUBROUTINE CheckPointEphemeris(Task, Text)
!+
!  Description: This routine provides a checkpoint message for a task.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
SAVE
!                               Passed Variables                  Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(LEN=*) ,INTENT(IN)  :: Task                           ! Name of the task checkpointing the message
CHARACTER(LEN=*) ,INTENT(IN)  :: Text                           ! Text of the message

!                                Local Variables
!-----------------------------+-----------------
INTEGER(4)                    :: values_rcv(8)                   ! Return value from DATE_AND_TIME
INTEGER(4)                    :: year,month,day                  ! Date components from DATE_AND_TIME
INTEGER(4)                    :: hour,minute,second,msec,zone    ! Time components from DATE_AND_TIME
EQUIVALENCE                     (values_rcv(1),year  )
EQUIVALENCE                     (values_rcv(2),month )
EQUIVALENCE                     (values_rcv(3),day   )
EQUIVALENCE                     (values_rcv(4),zone  )
EQUIVALENCE                     (values_rcv(5),hour  )
EQUIVALENCE                     (values_rcv(6),minute)
EQUIVALENCE                     (values_rcv(7),second)
EQUIVALENCE                     (values_rcv(8),msec  )
CHARACTER(12)                 :: AsciiTime                       ! Time formatted as hh:mm:ss.sss

!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL DATE_AND_TIME(VALUES = VALUES_RCV(1:8))
  WRITE(AsciiTime,FMT='(I2,":",I2,":",I2,".",I3)') Hour,Minute,Second,Msec
  IF(LEN_TRIM(Task) > 0) THEN
    WRITE(*,FMT='(A)')TRIM(Task)//" "//AsciiTime//" "//TRIM(Text)
  ELSE
    WRITE(*,FMT='(A)')"Ephemeris "//AsciiTime//" "//TRIM(Text)
  END IF
  RETURN
END SUBROUTINE CheckPointEphemeris

FUNCTION SolarInformationForLocation(Time, Location, Weather) RESULT(SolarInformation)
!+
!  Description: This routine returns the SolarData for a given location on the earth for specified weather conditions.
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE PhysicalConstants
USE UnitConversion
USE MatrixVectorPackage
USE TimeDatePackage
USE Utilities
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD)     ,INTENT(IN)  :: Time                           ! modified julian date (10^-7 sec)
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Location for solar information
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Weather Data for the site
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
!
!                               Local
!-----------------------------+---------------
REAL(8)                       :: T_PCS_CRS    (3,3)             ! Transform Celestial to Planet coordinates at Time
REAL(8)                       :: Tnext_PCS_CRS(3,3)             ! Transform Celestial to Planet coordinates at Time + 1 second
REAL(8)                       :: dR_CRS(3)                      ! Vector Tower to Earth center (AU)
REAL(8)                       :: V_CRS(3)                       ! Velocity of Tower in Celestial Coordinates           (AU/s)
REAL(8)                       :: R_CRS(3)                       ! Vector Earth to Sun, Celestial Coordinates at Time     (AU)
REAL(8)                       :: Rnext_CRS(3)                   ! Vector Earth to Sun, Celestial Coordinates at TimeNext (AU)
REAL(8)                       :: Rst_CRS(3)                     ! Vector Sun to Tower, Celestial Coordinates             (AU)
REAL(8)                       :: Rts_CRS(3)                     ! Vector Tower to Sun, Celestial Coordinates     (km)
REAL(8)                       :: Ds                             ! Distance Tower to Sun                          (km)
REAL(8)                       :: Rs                             ! Equatorial radius of the Sun                   (km)
REAL(8)                       :: dAs                            ! Solar disk solid angle (degrees)
REAL(8)                       :: Rts_PCS(3)                     ! Vector Tower->Sun Planetocentric Coordinates   (km)
REAL(8)                       :: Nts_PCS(3)                     ! Normal Tower->Sun Planetocentric Coordinates
REAL(8)                       :: Rts_ENU(3)                     ! Vector Tower->Sun ENU Coordinates              (km)
REAL(8)                       :: Nts_ENU(3)                     ! Normal Tower->Sun ENU Coordinates
REAL(8)                       :: Rtas_ENU(3)                    ! Vector Tower->ApparentSun ENU Coordinates      (km)
REAL(8)                       :: Ntas_ENU(3)                    ! Normal Tower->ApparentSun ENU Coordinates
REAL(8)                       :: AngSunEl0                      ! Solar elevation angle                      (degrees)
REAL(8)                       :: AngSunEl                       ! Solar Apparent elevation angle             (degrees)
REAL(8)                       :: AngSunAz                       ! Solar Azimuthal angle, CW From North       (degrees)
TYPE(AtmPerturbations)        :: AtmEffects                     ! Atmospheric Perturbations
REAL(8)                       :: AsunZen                        ! Solar apparent zenith angle                (degrees)
REAL(8)                       :: dAsunZen                       ! Delta solar zenith angle due to refraction (degrees)
REAL(8)                       :: AsunZen0                       ! Solar zenith angle                         (degrees)
REAL(8)                       :: SolarFraction                  ! Fraction of the sun above the horizon      (degrees)
REAL(8)                       :: SolarFlux                      ! Solar Flux                                   (w/m^2)
REAL(8)                       :: ElevKm                         ! Elevation above mean sea level                  (km)
REAL(8)                       :: Azen                           ! Solar Zenith angle to use in Meinel Model  (degrees)
REAL(8)                       :: Attenuation                    ! Atmospheric attenuation                   (fraction)
CHARACTER(24)                 :: TimeStampUTC                   ! Time stamp DD-MMM-YYYY hh:mm:ss.sss
CHARACTER(4)                  :: FormatTime                     ! Time zone abbreviation
REAL(8)                       :: c                              ! Speed of light                                (AU/s)
TYPE(TimeAndDate)             :: DateAndTimeLocal               ! Data and Time structure
LOGICAL(1),PARAMETER          :: UseMeinelModel = .true.        ! Force use of the Meinel Model
INTEGER(MJD)                  :: TimeNext                       ! Time for the next time step            (10^-7 seconds)
REAL(8)                       :: Gamma                          ! Lorentz factor                       [ 1 ... infinty )
REAL(8)                       :: dT                             ! Time for light to travel from sun to earth   (seconds)

LOGICAL(4), PARAMETER         :: DebugLocal = .false.           ! Diagnostic flag for testing
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  !Get The Transformations from Celestial Reference System to the Planetocentric Refererence System
  TimeNext  = Time + TimePerSecond
  T_PCS_CRS = T_PCS_CRS_BODY(Location%body, Time); Tnext_PCS_CRS = T_PCS_CRS_BODY(Location%body, TimeNext)

  !Get the vector from the Earth to the Tower in the Celestial Reference System
  dR_CRS    = MATMUL(TRANSPOSE(T_PCS_CRS),Location%R_PCS)

  !Get Sun to Earth Vector in Celestial Reference System
  R_CRS = PositionCRS(Time, EARTH, SUN, ModelLightTravelTime) ; Rnext_CRS = PositionCRS(TimeNext, EARTH, SUN, ModelLightTravelTime)

  !Get the velocity of the Tower in the Celestial Reference System
  V_CRS = Rnext_CRS + MATMUL(TRANSPOSE(Tnext_PCS_CRS),Location%R_PCS) - (R_CRS + dR_CRS)

  !Calculate the distance to the sun
  Rst_CRS  = R_CRS + dR_CRS
  Ds       = Magnitude(Rst_CRS) * EPH%AU
  IF(DebugLocal) THEN
    WRITE(*,FMT='("SolarInformationForLocation: R_PCS     = ",3(F17.3))')Location%R_PCS
    WRITE(*,FMT='("SolarInformationForLocation: dR_CRS    = ",3(F17.3))')dR_CRS
    WRITE(*,FMT='("SolarInformationForLocation: R_CRS     = ",3(F17.3))')R_CRS
    WRITE(*,FMT='("SolarInformationForLocation: Rnext_CRS = ",3(F17.3))')Rnext_CRS
    WRITE(*,FMT='("SolarInformationForLocation: V_CRS     = ",3(F17.3))')V_CRS
    WRITE(*,FMT='("SolarInformationForLocation: Rst_CRS   = ",3(F17.3))')Rst_CRS
  END IF

  !Apply Lorentz Transformation and calculate the vector from the Tower to the Sun
  dT      = Ds/EPH%CLIGHT
  c       = EPH%CLIGHT/KmPerAU
  Gamma   = 1d0 / DSQRT(1d0 - DOT_PRODUCT(V_CRS,V_CRS)/c**2)
  Rts_CRS = -(Rst_CRS + V_CRS*( (Gamma-1d0)*DOT_PRODUCT(R_CRS,V_CRS)/DOT_PRODUCT(V_CRS,V_CRS) - dT*Gamma ) ) * EPH%AU

  !Calculate the Solar Disk Angle
  Rs  = RadiusEquatorialBody(SUN)
  dAs = DATAN2D(Rs, Ds) * 2d0

  !Normalize the vector from the Tower to the Sun
  Rts_PCS  = MATMUL(T_PCS_CRS,Rts_CRS)
  Nts_PCS  = Normalize(Rts_PCS)
  Nts_ENU  = MATMUL(Location%T_ENU_PCS, Nts_PCS)
  Rts_ENU  = Ds * Nts_ENU

  !Calculate Solar Zenith Angle including Atmospheric Refraction Correction
  AsunZen0   = DACOSD(DOT_PRODUCT(Nts_PCS,Location%Nz_PCS))
  AtmEffects = AtmosphereEffects(AsunZen0, Location, Weather)
  dAsunZen   = AtmEffects%dAngZenith
  AsunZen    = AsunZen0 - dAsunZen

  !Calculate Solar Fraction
  SolarFraction = MAX(0d0, MIN ( (90.0d0 + dAs/2d0 - AsunZen)/dAs , 1d0))

  !Include attenuation through the atmosphere
  ElevKm      = Location%Elevation
  Azen        = MIN(ABS(Asunzen),0.999d0*90.0d0)
  IF(UseMeinelModel) THEN
    Attenuation = MIN( 1d0, (1d0-0.14D0*ElevKm)*EXP(-0.357d0*(1d0/DCOSD(Azen))**0.678d0) + 0.14d0*ElevKm)
    Attenuation = MAX(Attenuation, AtmEffects%SolarFlux/SolarFluxAtOneAU)
  ELSE
    Attenuation = AtmEffects%SolarFlux/SolarFluxAtOneAU
  END IF

  !Calculate Sun Azimuth and Elevation
  AngSunEl0 = DATAN2D(Nts_ENU(3), DSQRT(Nts_ENU(1)**2 + Nts_ENU(2)**2))
  AngSunEl  = AngSunEl0 + dAsunZen
  AngSunAz  = DATAN2D(Nts_ENU(1), Nts_ENU(2))
  IF (AngSunAz < 0d0 ) AngSunAz = AngSunAz + 360d0

  !Calculate vectors to the "Apparent Sun"
  Ntas_ENU  = (/DCOSD(AngSunEl)*DSIND(AngSunAz),DCOSD(AngSunEl)*DCOSD(AngSunAz), DSIND(AngSunEl)/)
  Rtas_ENU  = Ntas_ENU*Ds

  !Calculate Solar Intensity
  SolarFlux = SolarFluxAtOneAu * (EPH%AU/Ds)**2 * SolarFraction * Attenuation

  !Update times
  CALL Convert_Time_To_UTC_Timestamp  (Time, TimeStampUTC, FormatTime)
  DateAndTimeLocal = DateAndTimeLocalFormat(Time)

  !Prepare Return information
  SolarInformation%Time            = Time
  SolarInformation%TimeStampUTC    = TimeStampUTC
  SolarInformation%TimeStampLocal  = DateAndTimeLocal%TimeStamp
  SolarInformation%FormatLocal     = DateAndTimeLocal%FormatTime
  SolarInformation%Weekday         = Ascii_DayOfWeek(DateAndTimeLocal%DayOfWeek)
  SolarInformation%Year            = DateAndTimeLocal%Year
  SolarInformation%Month           = DateAndTimeLocal%Month
  SolarInformation%Day             = DateAndTimeLocal%Day
  SolarInformation%Hour            = DateAndTimeLocal%Hour
  SolarInformation%Minute          = DateAndTimeLocal%Minute
  SolarInformation%Second          = DateAndTimeLocal%Second
  SolarInformation%jdt             = JulianDateFromTime(Time)
  SolarInformation%AngleSolarDisk  = dAs
  SolarInformation%DistanceSun     = Ds
  SolarInformation%AngleAzimuthal  = AngSunAz
  SolarInformation%AngleElevation  = AngSunEl
  SolarInformation%AngleElevation0 = AngSunEl0
  SolarInformation%AngleZenith     = ASunZen
  SolarInformation%AngleZenith0    = ASunZen0
  SolarInformation%Attenuation     = Attenuation
  SolarInformation%SolarFlux       = SolarFlux
  SolarInformation%SolarFraction   = SolarFraction
  SolarInformation%Nts_ENU         = Nts_ENU
  SolarInformation%Rts_ENU         = Rts_ENU
  SolarInformation%Ntas_ENU        = Ntas_ENU
  SolarInformation%Rtas_ENU        = Rtas_ENU
  SolarInformation%Rts_PCS         = Rts_PCS
  SolarInformation%Nts_PCS         = Nts_PCS
  SolarInformation%T_PCS_CRS       = T_PCS_CRS
  RETURN
END FUNCTION SolarInformationForLocation

FUNCTION SunPositionTiesol(DateAndTime, Latitude, Longitude, Elevation, PressMilliBar, TempC) RESULT(Angles)  &
  BIND(C, Name="SUN_POSITION")

!+
!  Description: This routine returns the sun azimuthal and apparent elevation angles given the time, Location
!               and
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 15-MAY-2025     | MA    | STR 199: Provide a DLL for sun position calculation
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE UnitConversion
USE MatrixVectorPackage
USE TimeDatePackage
USE Utilities
USE, INTRINSIC               :: ISO_C_Binding, ONLY: C_DOUBLE
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+--------------------------------+--------------------------------------------------------
TYPE(TimeDateTiesol),VALUE    :: DateAndTime                   ! Tiesol Time/Date structure
REAL(C_DOUBLE)      ,VALUE    :: Latitude                      ! Latitude                              (degrees)
REAL(C_DOUBLE)      ,VALUE    :: Longitude                     ! Longitude                             (degrees)
REAL(C_DOUBLE)      ,VALUE    :: Elevation                     ! Elevation above mean sea level        (km)
REAL(C_DOUBLE)      ,VALUE    :: PressMilliBar                 ! Atmospheric Pressure                  (millibars)
REAL(C_DOUBLE)      ,VALUE    :: TempC                         ! Temperature                           (degC)
TYPE(AzElTiesol)              :: Angles                        ! Sun Az and apparent Elevation         (degrees)
!
!                                Local
!-----------------------------+---------------
LOGICAL(4),SAVE               :: InitializedForTiesol = .false. ! Keeps track of initialization
INTEGER(4)                    :: StatusTime                     ! Return from initializing time
INTEGER(4)                    :: StatusJPL                      ! Return from reading JPL ephemeris
REAL(8),SAVE                  :: Altitude             = 0d0     ! Altitude above sea leve              (km)
LOGICAL(4)                    :: UpdateLocation                 ! If true then update the ENU location
INTEGER(MJD)                  :: Time                           ! Modified julian date                 (10^-7 sec)
TYPE(WeatherData)             :: Weather                        ! Weather Data for the site
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
LOGICAL(4)                    :: ErrorGeo                       ! Error in initializing the locations
LOGICAL(4),PARAMETER          :: DebugLocal = .false.            ! Diagnostic flag for testing
CHARACTER(24)                 :: TimeStamp                      ! Timestamp for testing
CHARACTER(4)                  :: Zone                           ! Timezone for testing
!INTEGER(4)                    :: StatusDaily                    ! Initialize daily data
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(.not.InitializedForTiesol) THEN
    CALL SET_SYSTEM_LOCALE(Name                  =  "America/Los_Angeles" & !"Europe/London" & !"America/Denver" &
                          ,StandardTimeString    =  "PST"                 & !"GMT"           & !"MST"            &
                          ,dHoursSTD             =  -8.0                  & ! 0.0            & !-7.0             &
                          ,DayLightSavingsString =  "PDT"                 & !"GMT"           & !"MDT"            &
                          ,dHoursDST             =  -7.0)                   ! 0.0)             !-6.0)
    CALL SET_TIME_LOCALE  (Name                  =  "America/Los_Angeles" & !"Europe/London" & !"America/Denver" &
                          ,StandardTimeString    =  "PST"                 & !"GMT"           & !"MST"            &
                          ,dHoursSTD             =  -8.0                  & ! 0.0            & !-7.0             &
                          ,DayLightSavingsString =  "PDT"                 & !"GMT"           & !"MDT"            &
                          ,dHoursDST             =  -7.0)                   ! 0.0)             !-6.0)
    StatusTime = InitializeTimeAndDateCalculations()

    ! Ensure the JPL Ephemeris is read in
    statusJPL = InitializeJPLInterface()

    CALL SetGeoLocationForTime(Latitude, Longitude, Elevation, ErrorGeo)
    LocationSite = LocationOnSurface(Latitude, Longitude, Elevation)

    ! Initialize the Airmass Model
    Altitude = LocationSite%Elevation
    CALL InitializeAirmassModel(Altitude)
    IF(DebugLocal) THEN
      WRITE(*,FMT='("SunPositionTiesol: StatusJPL      : ",     I5 )') StatusJPL
      WRITE(*,FMT='("SunPositionTiesol: EPH%AU.1       : ",  F17.3 )') EPH%AU
      WRITE(*,FMT='("SunPositionTiesol: Location%Body  : ",     I5 )') LocationSite%body
      WRITE(*,FMT='("SunPositionTiesol: T_ENU_PCS(:,1) : ",3(F17.3))') LocationSite%T_ENU_PCS(:,1)
      WRITE(*,FMT='("SunPositionTiesol: T_ENU_PCS(:,2) : ",3(F17.3))') LocationSite%T_ENU_PCS(:,2)
      WRITE(*,FMT='("SunPositionTiesol: T_ENU_PCS(:,3) : ",3(F17.3))') LocationSite%T_ENU_PCS(:,3)
      WRITE(*,FMT='("SunPositionTiesol: Nx_PCS         : ",3(F17.3))') LocationSite%Nx_PCS
      WRITE(*,FMT='("SunPositionTiesol: Ny_PCS         : ",3(F17.3))') LocationSite%Ny_PCS
      WRITE(*,FMT='("SunPositionTiesol: Nz_PCS         : ",3(F17.3))') LocationSite%Nz_PCS
      WRITE(*,FMT='("SunPositionTiesol: R_PCS          : ",3(F17.3))') LocationSite%R_PCS
    END IF

    WRITE(*,FMT='("SunPositionTiesol: EPH%AU.2       : ",  F17.3 )') EPH%AU
    InitializedForTiesol = .true.

  END IF
  !StatusDaily = EphemerisDailyInitialization()
  !WRITE(*,FMT='(A)')" After EphemerisDailyInitialization         ---------------------------"

  UpdateLocation = Latitude  .ne. LocationForTime%Latitude  .or. &
                   Longitude .ne. LocationForTime%Longitude .or. &
                   Elevation .ne. LocationForTime%Elevation
  IF(UpdateLocation) THEN
    !
    ! Set the Site Location
    CALL SetGeoLocationForTime(Latitude, Longitude, Elevation, ErrorGeo)      ; IF(DebugLocal) WRITE(*,FMT='("SunPositionTiesol: EPH%AU.3       : ",  F17.3 )') EPH%AU
    LocationSite = LocationOnSurface(Latitude, Longitude, Elevation)          ; IF(DebugLocal) WRITE(*,FMT='("SunPositionTiesol: EPH%AU.4       : ",  F17.3 )') EPH%AU

    ! Update the Airmass Model
    IF(LocationSite%Elevation .ne. Altitude) THEN
      Altitude = LocationSite%Elevation                                       ; IF(DebugLocal) WRITE(*,FMT='("SunPositionTiesol: EPH%AU.5       : ",  F17.3 )') EPH%AU
      CALL InitializeAirmassModel(Altitude)                                   ; IF(DebugLocal) WRITE(*,FMT='("SunPositionTiesol: EPH%AU.6       : ",  F17.3 )') EPH%AU
    END IF

  END IF

  Time = TimeFromTiesolDateAndTime(DateAndTime)                               ; IF(DebugLocal) WRITE(*,FMT='("SunPositionTiesol: EPH%AU.7       : ",  F17.3 )') EPH%AU
  Weather%Psia  = PsiaPerBar*PressMilliBar/1d3
  Weather%TempF = DegFFromDegC(TempC)

  SolarInformation = SolarInformationForLocation(Time, LocationSite, Weather) ; IF(DebugLocal) WRITE(*,FMT='("SunPositionTiesol: EPH%AU.8       : ",  F17.3 )') EPH%AU

  IF(DebugLocal) THEN
    CALL ConvertTimeToLocalTimeStamp(Time, TimeStamp, Zone)
    WRITE(*,FMT='(A)')"SunPositionTiesol: TimeLocal      : "//TimeStamp//" "//Zone
    WRITE(*,FMT='("SunPositionTiesol: Latitude       : ", F10.3)') LocationSite%Latitude
    WRITE(*,FMT='("SunPositionTiesol: Longitude      : ", F10.3)') LocationSite%Longitude
    WRITE(*,FMT='("SunPositionTiesol: Elevation      : ", F10.3)') LocationSite%Elevation
    WRITE(*,FMT='("SunPositionTiesol: Psia           : ", F10.3)') Weather%Psia
    WRITE(*,FMT='("SunPositionTiesol: TempF          : ", F10.3)') Weather%TempF
    WRITE(*,FMT='("SunPositionTiesol: TimeMJD        : ",Z16.16)')SolarInformation%Time
    WRITE(*,FMT='(A)')"SolarInformation : TimeUTC        : "//SolarInformation%TimeStampUTC//" "//"UTC"
    WRITE(*,FMT='(A)')"SolarInformation : TimeLocal      : "//SolarInformation%TimeStampLocal//" "//SolarInformation%FormatLocal
    WRITE(*,FMT='(A)')"SolarInformation : Weekday        : "//SolarInformation%Weekday
    WRITE(*,FMT='("SolarInfomation  : Year           : ",    I5)')SolarInformation%Year
    WRITE(*,FMT='("SolarInfomation  : Month          : ",    I5)')SolarInformation%Month
    WRITE(*,FMT='("SolarInfomation  : Day            : ",    I5)')SolarInformation%Day
    WRITE(*,FMT='("SolarInfomation  : Hour           : ",    I5)')SolarInformation%Hour
    WRITE(*,FMT='("SolarInfomation  : Minute         : ",    I5)')SolarInformation%Minute
    WRITE(*,FMT='("SolarInfomation  : Second         : ",    I5)')SolarInformation%Second
    WRITE(*,FMT='("SolarInfomation  : jdt            : ", F17.7)')SolarInformation%jdt
    WRITE(*,FMT='("SolarInfomation  : AngleSolarDisk : ", F10.3)')SolarInformation%AngleSolarDisk
    WRITE(*,FMT='("SolarInfomation  : DistanceSun    : ", F17.7)')SolarInformation%DistanceSun
    WRITE(*,FMT='("SolarInfomation  : AngleAzimuthal : ", F10.3)')SolarInformation%AngleAzimuthal
    WRITE(*,FMT='("SolarInfomation  : AngleElevation : ", F10.3)')SolarInformation%AngleElevation
    WRITE(*,FMT='("SolarInfomation  : AngleElevation0: ", F10.3)')SolarInformation%AngleElevation0
    WRITE(*,FMT='("SolarInfomation  : AngleZenith    : ", F10.3)')SolarInformation%AngleZenith
    WRITE(*,FMT='("SolarInfomation  : AngleZenith0   : ", F10.3)')SolarInformation%AngleZenith0
    WRITE(*,FMT='("SolarInfomation  : Attenuation    : ", F10.3)')SolarInformation%Attenuation
    WRITE(*,FMT='("SolarInfomation  : SolarFlux      : ", F10.3)')SolarInformation%SolarFlux
    WRITE(*,FMT='("SolarInfomation  : SolarFraction  : ", F10.3)')SolarInformation%SolarFraction
    WRITE(*,FMT='("SolarInformation : T_PCS_CRS(:,1) : ",3(F17.3))')SolarInformation%T_PCS_CRS(:,1)
    WRITE(*,FMT='("SolarInformation : T_PCS_CRS(:,2) : ",3(F17.3))')SolarInformation%T_PCS_CRS(:,2)
    WRITE(*,FMT='("SolarInformation : T_PCS_CRS(:,3) : ",3(F17.3))')SolarInformation%T_PCS_CRS(:,3)
  END IF



  Angles%Az = SolarInformation%AngleAzimuthal
  Angles%El = SolarInformation%AngleElevation

  RETURN
END FUNCTION SunPositionTiesol

SUBROUTINE SpecifySolarPositionInformation(SolarInformation, AngSunAz, AngSunEl)
!+
! Description:  This routine is used to modify the SolarInformation so the user can place the sun at a desired
!               position for study purposes.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(SolarData)               :: SolarInformation               ! Calculated solar information
REAL(8),INTENT(IN)            :: AngSunAz                       ! Solar Azimuthal Position (deg)
REAL(8),INTENT(IN)            :: AngSunEl                       ! Solar Elevation Position (deg)
!
!                                Local
!-----------------------------+---------------
REAL(8)                       :: Rtas_ENU(3)                    ! Vector Tower->ApparentSun ENU Coordinates (km)
REAL(8)                       :: Ntas_ENU(3)                    ! Normal Tower->ApparentSun ENU Coordinates
REAL(8)                       :: dSunZen                        !
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Ntas_ENU = (/DSIND(AngSunAz)*DCOSD(AngSunEl), DCOSD(AngSunAz)*DCOSD(AngSunEl), DSIND(AngSunEl)/)
  Rtas_ENU = Ntas_ENU * SolarInformation%DistanceSun
  dSunZen  = SolarInformation%AngleZenith0 - SolarInformation%AngleZenith
  SolarInformation%AngleAzimuthal = AngSunAz
  SolarInformation%AngleElevation = AngSunEl
  SolarInformation%Ntas_ENU       = Ntas_ENU
  SolarInformation%Rtas_ENU       = Rtas_ENU
  SolarInformation%Nts_ENU        = Ntas_ENU
  SolarInformation%Rts_ENU        = Rtas_ENU
  SolarInformation%AngleZenith    = 90d0 - AngSunEl
  SolarInformation%AngleZenith0   = 90d0 - AngSunEl
  RETURN
END SUBROUTINE SpecifySolarPositionInformation

FUNCTION AtmosphereEffects(AngleZenith, Location, Weather) RESULT(AtmEffects)
!+
!  Description: This routine returns the Atmospheric Effects for a given location on the earth for specified weather
!               conditions.
!
! Audit Trail
! ---------------------------+-------+----------------------------------------------------------------------------------
! ---------------------------+-------+----------------------------------------------------------------------------------
!-
USE PhysicalConstants
USE InternationalStandardAtmosphere
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)            :: AngleZenith                    ! Solar True Zenith angle (deg)
TYPE(LocationENU),INTENT(IN)  :: Location                       ! Solar System body & Elevation (km)
TYPE(WeatherData),INTENT(IN)  :: Weather                        ! Temperature (F) & Pressure (psia)
TYPE(AtmPerturbations)        :: AtmEffects                     ! Atmospheric Effects
!
!                               Local
!-----------------------------+--------
REAL(8)                       :: Angle                          ! Local copy of zenith angle (deg)
REAL(8)                       :: ElevKm                         ! Elevation (km)
REAL(8)                       :: Airmass                        ! Airmass (per-unit)
REAL(8)                       :: SolarFlux                      ! Nominal Solar Flux (w/m^2)
REAL(8)                       :: dAngleZen                      ! Zenith Angle refraction (deg)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Angle     = MAX(0d0, MIN(ABS(AngleZenith),90d0))
ElevKm    = Location%Elevation

!Calculate effective airmass
Airmass   = AirmassCalculation(Angle,ElevKm)

!Calculate Nominal Value Of Solar Flux for the Airmass
SolarFlux = SolarFluxAtOneAU * RatioFluxAirMass(AirMass)

!Calculate zenith angle refraction
dAngleZen = dAngleZenithAirmass(Airmass, Weather%TempF, Weather%Psia, ElevKm)

!Return results
AtmEffects%ElevKm     = ElevKm
AtmEffects%AngZenith  = Angle
AtmEffects%Airmass    = Airmass
AtmEffects%dAngZenith = dAngleZen
AtmEffects%SolarFlux  = SolarFlux
RETURN
END FUNCTION AtmosphereEffects
!
FUNCTION GeoDataFromPCS(R_PCS) RESULT(Geo)
!+
!  Description: This routine calculates and returns the latitude, longitude and elevation given the Planetocentric
!               Coordinates on the earth.
!-
! Reference:
! http://www.ceri.memphis.edu/people/smalley/ESCI7355/coordcvt.pdf
!
! A local copy is saved as:
! file://d:/CSP/Documentation/coordcvt.pdf
!
! Audit Trail
! ---------------------------+-------+--------------------------------------------------------------------------------
! ---------------------------+-------+--------------------------------------------------------------------------------
!-
USE NumericalConstants
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)            :: R_PCS(3)                       ! Planetocentric Position (km)
TYPE(Geodata)                 :: Geo                            ! GeoData lat,long,elev (deg,deg,km)
!
!                               Local
!-----------------------------+--------
REAL(8)                       :: Ra                             ! Equatorial radius of body (km)
REAL(8)                       :: Rb                             ! Polar radius of body (km)
REAL(8)                       :: f                              ! Flattening of body (fraction)
REAL(8)                       :: e                              ! Body eccentricity
REAL(8)                       :: e2                             ! Body eccentricity squared
REAL(8)                       :: e2Prime                        ! Auxiliary data
REAL(8)                       :: Longitude                      ! Geodetic Longitude (degrees)
REAL(8)                       :: Latitude                       ! Geodetic Latitude  (degrees)
REAL(8)                       :: Elevation                      ! Surface elevation  (km)
REAL(8)                       :: theta                          ! Geographic Latitude (degrees)
REAL(8)                       :: cosTheta                       ! Cosine of geographic latitude
REAL(8)                       :: sinTheta                       ! Sine of geographic latitude
REAL(8)                       :: Pxy                            ! Magnitude of PCS xy component (km)
REAL(8)                       :: R                              ! Magnitude of PCS xyz component (km)
REAL(8)                       :: Px                             ! PCS X coordinate (km)
REAL(8)                       :: Py                             ! PCS Y coordinate (km)
REAL(8)                       :: Pz                             ! PCS Z coordinate (km)
REAL(8)                       :: Rlocal_PCS(3)                  ! Local copy of R_PCS (km)
EQUIVALENCE                    (Px,Rlocal_PCS(1)),(Py,Rlocal_PCS(2)),(Pz,Rlocal_PCS(3))
REAL(8)                       :: Zp                             ! Ratio of Pz and Pxy
REAL(8)                       :: Rn                             ! Local fro Rn(Latitude)
REAL(8)                       :: dLat                           ! Latitude iterative correction (degrees)
INTEGER(4),PARAMETER          :: iterMax = 12                    ! Maximum number of iterations
INTEGER(4)                    :: iter                           ! Iteration index
REAL(8)   ,PARAMETER          :: Tol = 1d-12                    ! Convergence tolerance
LOGICAL   ,PARAMETER          :: UseClynchMethod = .true.       ! Use James R. Clynch February 2006 method
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Ra          = Ellipsoid_Body(EARTH)%Ra
  f           = Ellipsoid_Body(EARTH)%f     !Not actually needed here
  Rb          = Ellipsoid_Body(EARTH)%Rb
  e           = Ellipsoid_Body(EARTH)%e
  e2          = Ellipsoid_Body(EARTH)%e2
  e2Prime     = Ellipsoid_Body(EARTH)%e2Prime
  Rlocal_PCS = R_PCS
  Longitude  = DATAN2D(Py,Px)
  Pxy        = DSQRT(Px**2 + Py**2)
  R          = DSQRT(Px**2 + Py**2 + Pz**2)
  Zp         = Pz/Pxy
  Latitude   = DATAN2D(Pz,Pxy)
  DO iter    = 1, iterMax
    Rn       = Ra/DSQRT(1d0 - e2 * DSIND(Latitude)**2) ; IF(iter > 1 .and. ABS(dLat) < Tol) EXIT
    !h        = Pxy/DCOSD(Latitude) - Rn
    dLat     = DATAND(Zp * (1d0 - e2 * (Rn/(Pxy/DCOSD(Latitude))))**(-1)) - Latitude
    Latitude = Latitude + dLat
  END DO
  IF(DABS(Latitude) < 80d0) THEN
    Elevation = Pxy/DCOSD(Latitude) - Rn
  ELSE
    Elevation =  Pz/DSIND(Latitude) - (1d0-e2)*Rn
  END IF
  Geo%Latitude  = Latitude
  Geo%Longitude = Longitude
  Geo%Elevation = Elevation
  IF(UseClynchMethod) RETURN

  Rlocal_PCS = R_PCS
  Pxy        = DSQRT(Px**2 + Py**2)
  theta      = DATAN2D(Pz*Ra,Pxy*Rb)
  cosTheta   = DCOSD(theta)
  sinTheta   = DSIND(theta)
  Latitude   = DATAN2D(Pz+e2prime*Rb*sinTheta**3,Pxy-e2*Ra*cosTheta**3)
  Longitude  = DATAN2D(Py,Px)
  IF(DABS(Latitude)<80.0d0) THEN
    Elevation = Pz/DSIND (Latitude) - Ra*(1d0-e2)/DSQRT(1d0-e2*DSIND(Latitude)**2)
  ELSE
    Elevation = Pxy/DCOSD(Latitude) - Ra/DSQRT(1d0-e2*DSIND(Latitude)**2)
  END IF
  Geo%Latitude  = Latitude
  Geo%Longitude = Longitude
  Geo%Elevation = Elevation
  RETURN
END FUNCTION GeoDataFromPCS

FUNCTION GeoDataFromTransverseMercator(R_XY) RESULT(Geo)
!+
!  Description: This routine calculates and returns the latitude, longitude and elevation given the Transverse
!               Mercator Coordinates on the earth.
!
! Audit Trail
! ---------------------------+-------+-------------------------------------------------------------
! ---------------------------+-------+-------------------------------------------------------------
!-
USE NumericalConstants
USE UnitConversion
IMPLICIT NONE
!                                Passed                            Description / (units)
!-----------------------------+----------------------------------+------------------------------------------------------
REAL(8),INTENT(IN)            :: R_XY(2)                         ! Easting and Northing (m)
TYPE(Geodata)                 :: Geo                             ! GeoData lat,long,elev (deg,deg,km)
!
!                               Local
!-----------------------------+--------
REAL(8),SAVE                  :: Ra                              ! Equatorial radius of body (km)
REAL(8),SAVE                  :: Rb                              ! Polar radius of body (km)
REAL(8),SAVE                  :: f                               ! Flattening of body (fraction)
REAL(8),SAVE                  :: n                               ! Ratio of radii difference and sum
REAL(8),SAVE                  :: e                               ! Body eccentricity
REAL(8),SAVE                  :: e2                              ! Body eccentricity squared
REAL(8),SAVE                  :: e4                              ! Body eccentricity to 4th power
REAL(8),SAVE                  :: e6                              ! Body eccentricity to 6th power
REAL(8),SAVE                  :: e2Prime                         ! Auxiliary data
REAL(8),SAVE                  :: A_even(4)                       ! Meridian distance calculation terms
REAL(8),SAVE                  :: theta0                          ! Reference Geographic Latitude (radians)
REAL(8),SAVE                  :: m0                              ! Meridian distance of reference latitude (km)
REAL(8)                       :: Longitude                       ! Geodetic Longitude (degrees)
REAL(8)                       :: Latitude                        ! Geodetic Latitude  (degrees)
REAL(8)                       :: Elevation                       ! Surface elevation  (km)
REAL(8)                       :: theta                           ! Geographic Latitude (degrees)
REAL(8)                       :: cosTheta                        ! Cosine of geographic latitude
REAL(8)                       :: sinTheta                        ! Sine of geographic latitude
REAL(8)                       :: Pxy                             ! Magnitude of PCS xy component (km)
REAL(8)                       :: Px                              ! PCS X coordinate (km)
REAL(8)                       :: Py                              ! PCS Y coordinate (km)
REAL(8)                       :: Pz                              ! PCS Z coordinate (km)
REAL(8)                       :: Rlocal_PCS(3)                   ! Local copy of R_PCS (km)
EQUIVALENCE                    (Px,Rlocal_PCS(1)),(Py,Rlocal_PCS(2)),(Pz,Rlocal_PCS(3))
TYPE(Transverse),PARAMETER    :: TransverseMercator = Transverse("WG23", 1d0, 0d0, 23d0)
LOGICAL(4)      ,PARAMETER    :: DebugLocal = .false.            !Local Debug for in-house testing
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
! ToDo SCR 150: R_XY is not used in the calculations below.  This is a serious problem.  We will print them out to get
!               pass the GNU fortran compiler
  IF(DebugLocal) WRITE(*,FMT='(" R_XY: ",F17.7,1x,F17.7)') R_XY
!
  Ra      = Ellipsoid_Body(EARTH)%Ra
  f       = Ellipsoid_Body(EARTH)%f
  Rb      = Ellipsoid_Body(EARTH)%Rb
  e       = Ellipsoid_Body(EARTH)%e
  e2prime = Ellipsoid_Body(EARTH)%e2prime

  n       = (Ra - Rb)/(Ra + Rb)         ! ratio of radii difference and sum
  e2      = e**2
  e4      = e**4
  e6      = e**6
  A_even  = (/(1d0 - (1d0/4d0)*e2 - (3d0/64d0) * e4 - ( 5d0/256d0) * e6)  &
             ,(    3d0/8d0)*(  e2 + (1d0/4d0)  * e4 + (15d0/128d0) * e6)  &
             , (15d0/256d0)*(                    e4 + ( 3d0/128d0) * e6)  &
             ,(35d0/3072d0)*(                                        e6)  &
            /)
  Theta0  = TransverseMercator%LatitudeOrigin * RadDeg
  m0      = Ra * DOT_PRODUCT(A_even, (/Theta0, -SIN(2d0*Theta0), SIN(4d0*Theta0), -SIN(6d0*Theta0)/))

  Rlocal_PCS = (/0,0,0/)
  Pxy        = DSQRT(Px**2 + Py**2)
  theta      = DATAN2D(Pz*Ra,Pxy*Rb)
  cosTheta   = DCOSD(theta)
  sinTheta   = DSIND(theta)
  Latitude   = DATAN2D(Pz+e2prime*Rb*sinTheta**3,Pxy-e2*Ra*cosTheta**3)
  Longitude  = DATAN2D(Py,Px)
  IF(DABS(Latitude)<80.0d0) THEN
    Elevation = Pz/DSIND (Latitude) - Ra*(1d0-e2)/DSQRT(1d0-e2*DSIND(Latitude)**2)
  ELSE
    Elevation = Pxy/DCOSD(Latitude) - Ra/DSQRT(1d0-e2*DSIND(Latitude)**2)
  END IF
  Geo%Latitude  = Latitude
  Geo%Longitude = Longitude
  Geo%Elevation = Elevation
  RETURN
END FUNCTION GeoDataFromTransverseMercator

FUNCTION GaussConformalFromGeo(Geo)  RESULT(R_XY)
!
! Description: This routine returns the Gauss Conformal Coordinate System parameters (Westing and Southing) from the
!              GEO Coordinates (latitude, longitude and elevation).  See SA Coordinate Reference System Part1+1.pdf
!
! Audit Trail
! ---------------------------+-------+----------------------------------------------------------------------------------
! ---------------------------+-------+----------------------------------------------------------------------------------
!-
USE NumericalConstants
USE UnitConversion
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(Geodata) ,INTENT(IN)     :: Geo                            ! GeoData lat,long,elev (deg,deg,km)
REAL(8)                       :: R_XY(2)                        ! Westing and Southing (m)
!
!                               Local
!-----------------------------+--------
REAL(8)                       :: Ra                             ! Equatorial radius of body (km)
REAL(8)                       :: Rb                             ! Polar radius of body (km)
REAL(8)                       :: f                              ! Flattening of body (fraction)
REAL(8)                       :: n                              ! Ratio of radii difference and sum
REAL(8)                       :: e                              ! Body eccentricity
REAL(8)                       :: e2                             ! Body eccentricity squared
REAL(8)                       :: e4                             ! Body eccentricity to  4th power
REAL(8)                       :: e6                             ! Body eccentricity to  6th power
REAL(8)                       :: e8                             ! Body eccentricity to  8th power
REAL(8)                       :: e10                            ! Body eccentricity to 10th power
REAL(8)                       :: e2Prime                        ! Auxiliary data
REAL(8)                       :: A_term(6)                      ! Meridian distance calculation terms
REAL(8)                       :: Lambda0                        ! Reference Geodetic Longitude (radians)
REAL(8)                       :: Lambda                         ! Geodetic Longitude (radians)
REAL(8)                       :: dLambda                        ! Geodetic delta Longitude from Reference (radians)
REAL(8)                       :: Phi                            ! Absolute value of Geodetic Latitude  (radians)
REAL(8)                       :: eta2                           !
REAL(8)                       :: eta4                           !
REAL(8)                       :: tau                            !
REAL(8)                       :: tau2                           !
REAL(8)                       :: tau4                           !
REAL(8)                       :: B                              !
REAL(8)                       :: Southing
REAL(8)                       :: Westing
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Ra          = Ellipsoid_Body(EARTH)%Ra
  f           = Ellipsoid_Body(EARTH)%f
  Rb          = Ellipsoid_Body(EARTH)%Rb
  e           = Ellipsoid_Body(EARTH)%e
  e2          = Ellipsoid_Body(EARTH)%e2
  e2Prime     = Ellipsoid_Body(EARTH)%e2Prime

  e2      = (Ra**2 - Rb**2)/(Ra**2)     ! ToDo  SCR 150: Recheck these calculations e2 and e2Prim end up being the same
  e2prime = (Ra**2 - Rb**2)/(Rb**2)     ! ToDo  SCR 150: Recheck these calculations e2 and e2Prim end up being the same
  e4      = e2**2
  e6      = e2**3
  e8      = e2**4
  e10     = e2**5
  A_term  = (/ DOT_PRODUCT( (/1d0,3d0/4d0,45d0/64d0,175d0/256d0,11025d0/16384d0,43659d0/65536d0 /),(/1d0,e2,e4,e6,e8,e10/)) &
             , DOT_PRODUCT( (/0d0,3d0/4d0,15d0/16d0,525d0/512d0, 2205d0/2048d0 ,72765d0/65536d0 /),(/1d0,e2,e4,e6,e8,e10/)) &
             , DOT_PRODUCT( (/0d0,    0d0,15d0/64d0,105d0/256d0, 2205d0/4096d0 ,10395d0/16384d0 /),(/1d0,e2,e4,e6,e8,e10/)) &
             , DOT_PRODUCT( (/0d0,    0d0,      0d0, 35d0/512d0,  315d0/2048d0 ,31185d0/131072d0/),(/1d0,e2,e4,e6,e8,e10/)) &
             , DOT_PRODUCT( (/0d0,    0d0,      0d0,        0d0,  315d0/16384d0, 3465d0/65536d0 /),(/1d0,e2,e4,e6,e8,e10/)) &
             , DOT_PRODUCT( (/0d0,    0d0,      0d0,        0d0,            0d0,  693d0/131072d0/),(/1d0,e2,e4,e6,e8,e10/)) &
            /)


  Lambda0  = RadDeg * ((INT(ABS(Geo%Longitude))/2)*2 + 1)
  Lambda   = RadDeg * ABS(Geo%Longitude)
  Phi      = RadDeg * ABS(Geo%Latitude)
  eta2     = e2Prime * COS(Phi)**2
  eta4     = eta2**2
  tau      = TAN(Phi)
  tau2     = tau**2
  tau4     = tau**4
  B        = Ra*1d3*(1d0-e2) * DOT_PRODUCT( A_Term, (/Phi,-SIN(2d0*Phi)/2d0,SIN(4d0*Phi)/4d0,-SIN(6d0*Phi)/6d0,SIN(8d0*Phi)/8d0,-SIN(10d0*Phi)/10d0/) )
  !B        = Ra*1d3*(1d0-e2) * DOT_PRODUCT( A_Term, (/Phi,-SIN(2d0*Phi)/2d0,SIN(4d0*Phi)/4d0,-SIN(    Phi)/6d0,SIN(8d0*Phi)/8d0,-SIN(10d0*Phi)/10d0/) )
  dLambda  = Lambda0 - Lambda
  N        = Ra*1d3/SQRT((1d0 - e2*SIN(Phi)**2))
  Southing = B + (dLambda**2/2d0)   * N*SIN(Phi)*COS(Phi)                                          &
               + (dLambda**4/24d0)  * N*SIN(Phi)*(COS(Phi)**3)*(5d0 + 9d0*eta2 + 4d0*eta4 - tau2)  &
               + (dLambda**6/720d0) * N*SIN(Phi)*(COS(Phi)**5)*(61d0 -58d0*tau2 + tau4 + 270d0*eta2 - 330d0*eta2*tau2) &
               + 0d0*(-2.4536d0)  !(-.0017d0)
  Westing  =     (dLambda)          * N*COS(Phi)                                                   &
               + (dLambda**3/6d0  ) * N*COS(Phi)**3           *(1d0 + eta2 - tau2)                 &
               + (dLambda**5/120d0) * N*COS(Phi)**5           *(5d0 - 18d0*tau2 + tau4 + 14d0*eta2 - 58d0*eta2*tau2) &
               + 0d0*((-.9786d0) + (856d0)) !(-62.59d0)
  R_xy = (/Southing,Westing/)
  RETURN
END FUNCTION GaussConformalFromGeo

FUNCTION GeoFromGaussConformal(R_XY, Long0, Elevation)  RESULT(Geo)
!
! Description: This routine returns the GEO Coordinate parameters (latitude, longitude and elevation) from the
!              Gauss Conformal Coordinate System parameters (Westing and Southing).
!              See SA Coordinate Reference System Part1+1.pdf
!
! Audit Trail
! ---------------------------+-------+----------------------------------------------------------------------------------
! ---------------------------+-------+----------------------------------------------------------------------------------
!-
USE NumericalConstants
USE UnitConversion
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)           :: R_XY(2)                        ! Westing and Southing (m)
REAL(8), INTENT(IN)           :: Long0                          ! Reference Geodetic Longitude (degrees)
REAL(8), INTENT(IN)           :: Elevation                      ! Reference Geodetic Elevation (km)
TYPE(Geodata)                 :: Geo                            ! GeoData lat,long,elev (deg,deg,km)
!
!                               Local
!----------------------------+--------
REAL(8)                       :: Ra                             ! Equatorial radius of body (km)
REAL(8)                       :: Rb                             ! Polar radius of body (km)
REAL(8)                       :: f                              ! Flattening of body (fraction)
REAL(8)                       :: n                              ! Ratio of radii difference and sum
REAL(8)                       :: e                              ! Body eccentricity
REAL(8)                       :: e2                             ! Body eccentricity squared
REAL(8)                       :: e2Prime                        ! Auxiliary data
REAL(8)                       :: Phi                            ! Absolute value of Geodetic Latitude  (radians)
REAL(8)                       :: PhiF                           ! Approximate value of Geodetic Latitude  (radians)
REAL(8)                       :: dLong                          ! Longitude delta from Central Meridian (radians)
REAL(8)                       :: Sigma                          !
REAL(8)                       :: etaF                           !
REAL(8)                       :: tauF                           !
REAL(8)                       :: Rx
REAL(8)                       :: Ry
REAL(8)                       :: Mf
REAL(8)                       :: Nf
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Ra            = Ellipsoid_Body(EARTH)%Ra
  f             = Ellipsoid_Body(EARTH)%f
  Rb            = Ellipsoid_Body(EARTH)%Rb
  e             = Ellipsoid_Body(EARTH)%e
  e2            = Ellipsoid_Body(EARTH)%e2
  e2Prime       = Ellipsoid_Body(EARTH)%e2Prime
  e2            = (Ra**2 - Rb**2)/(Ra**2)     ! ToDo  SCR 150: Recheck these calculations e2 and e2Prim end up being the same
  e2prime       = (Ra**2 - Rb**2)/(Rb**2)     ! ToDo  SCR 150: Recheck these calculations e2 and e2Prim end up being the same
  n             = (Ra-Rb)/(Ra+Rb)

  Rx            = R_XY(1)
  Ry            = R_XY(2)
  Sigma         = Rx* (1d0 + n)/(Ra*1d3*(1d0 + n**2/4d0 + n**4/64d0))
  PhiF          = Sigma + (3d0/2d0)*(n - (9d0/16d0)*n**3)*SIN(2d0*Sigma) + (21d0/16d0)*SIN(4d0*Sigma)*n**2 + (151d0/96d0)*SIN(6d0*Sigma)*n**3
  TauF          = TAN(PhiF)
  EtaF          = SQRT(e2Prime*COS(PhiF)**2)
  Mf            = Ra*1d3*(1d0-e2)/((1d0-e2*SIN(PhiF)**2)**(3d0/2d0))
  Nf            = Ra*1d3/SQRT((1d0 - e2*SIN(PhiF)**2))
  Phi           = PhiF - ((Ry**2)/2d0)*(TauF/(Mf*Nf)) + ((Ry**4)/24d0)*(TauF/(Mf*Nf**3))*(5d0 + EtaF**2 + 3d0*TauF**2 - 4d0*EtaF**4 - 9d0*(TauF*EtaF)**2)
  dLong         = Ry/(Nf*COS(PhiF)) - ((Ry**3)/6d0)*(1d0/(COS(PhiF)*Nf**3))*(1d0 + EtaF**2 + 2d0*TauF**2) &
                  + ((Ry**5)/120d0)*(1d0/(COS(PhiF)*Nf**5))*(5d0+6d0*EtaF**2+28d0*TauF**2+8d0*(TauF*EtaF)**2+24d0*TauF**4-3d0*EtaF**4)
  Geo%Latitude  = (-Phi)*DegRad
  Geo%Longitude = Long0 - dLong*DegRad
  Geo%Elevation = Elevation

END FUNCTION GeoFromGaussConformal

FUNCTION CalculateAimingAnglesAtLocation(Rlocation_ENU,SolarTimeInformation,Raim_ENU,AngleAz,AngleEl) RESULT(status)
!+
! Description:  This routine calculates azimuthal and elevation angles positions (AngleAz and AngleEl for a heliostat
!               at a specified location to focus on a target at a specified set of solar conditions (time, temperature).
!
USE MatrixVectorPackage
USE UnitConversion
!+
! Description:
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                              Passed Variables                  Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)        , INTENT(IN)   :: Rlocation_ENU(3)               ! ENU coordinates of the center of motion (m)
TYPE(SolarData), INTENT(IN)   :: SolarTimeInformation           ! SolarTimeInformation record
REAL(8)        , INTENT(IN)   :: Raim_ENU(3)                    ! Aim Point to focus sunlight on (m)
REAL(8)        , INTENT(OUT)  :: AngleAz                        ! Azimuthal Angle   (deg)
REAL(8)        , INTENT(OUT)  :: AngleEl                        ! Elevation Angle   (deg)
INTEGER(4)                    :: status                         ! Error Return Code
!
!                              Local Variables
!-----------------------------+---------------
REAL(8)                       :: R_ENU(3)                       ! Heliostat Pivot To Tower (km)
REAL(8)                       :: Rht_ENU(3)                     ! Heliostat To Tower (km)
REAL(8)                       :: Nht_ENU(3)                     ! Heliostat To Tower normalized
REAL(8)                       :: Rhs_ENU(3)                     ! Heliostat To Sun Vector (km)
REAL(8)                       :: Nhs_ENU(3)                     ! Heliostat To Sun Vector  normalized
REAL(8)                       :: Nsum_ENU(3)                    ! Sum of Nhs and Nht unnormalized
REAL(8)                       :: N_ENU(3)                       ! Sum of Nhs and Nht normalized
REAL(8)                       :: Rho_HEL(3)                     ! Mirror center (km)
REAL(8)                       :: Taz_ENU_HEL(3,3)               ! Transform for Azimuthal Movement
REAL(8)                       :: Tel_ENU_ENU(3,3)               ! Transform for Elevation Movement
REAL(8)                       :: T_ENU_HEL(3,3)                 ! Transform for Combined Az and El
REAL(8)                       :: Naxis_ENU(3)                   ! Vector Rotation Axis for Elevation
INTEGER(4)                    :: iter                           ! Mirror offset correction iteration
INTEGER(4),PARAMETER          :: iterMax = 3                    ! Number of mirror offset corrections
REAL(8)   ,PARAMETER          :: dYmirror = 0.5588d0            ! Offset of mirror from the center of motion (m)
REAL(8)   ,PARAMETER          :: dZBase   = 0.378d0             ! Height of the base for location (m)
REAL(8)   ,PARAMETER          :: dZPost   = 5.272d0             ! Height of the post (m)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  !
  !  Get the mirror offset
  Rho_HEL   = (/0d0, dYMirror, 0d0/)/1000.0d0

  Rht_ENU   = (/Raim_ENU(1) - RLocation_ENU(1) &
               ,Raim_ENU(2) - RLocation_ENU(2) &
               ,Raim_ENU(3) - RLocation_ENU(3) &
                            - dZBase           &
                            - dZPost           &
              /)/1000.0d0

  R_ENU     = Rht_ENU


  DO iter = 1, iterMax

    !Calculate Normal to heliostat
    Nht_ENU   = Rht_ENU/SQRT(DOT_PRODUCT(Rht_ENU,Rht_ENU))
    Rhs_ENU   = Rht_ENU + SolarTimeInformation%Rtas_ENU
    Nhs_ENU   = Rhs_ENU/SQRT(DOT_PRODUCT(Rhs_ENU,Rhs_ENU))
    Nsum_ENU  = Nht_ENU + Nhs_ENU
    N_ENU     = Nsum_ENU / DSQRT(DOT_PRODUCT(Nsum_ENU,Nsum_ENU ))

    !Determine Az and El rotations to align mirror
    AngleEl   = DATAN2D(N_ENU(3), DSQRT(N_ENU(1)**2 + N_ENU(2)**2))
    AngleAz   = DATAN2D(N_ENU(1), N_ENU(2))
    IF(AngleAz < 0d0) AngleAz = AngleAz + 360.0d0

    IF(iter == iterMax) EXIT

    !Correct for mirror offset from center of rotation
    Taz_ENU_HEL = Rotation(Zaxis,AngleAz*RadDeg)
    Naxis_ENU   = MATMUL(Taz_ENU_HEL,(/1,0,0/))
    Tel_ENU_ENU = Rotation(Naxis_ENU,-AngleEl*RadDeg)
    T_ENU_HEL   = MATMUL(Tel_ENU_ENU,Taz_ENU_HEL)
    Rht_ENU     = R_ENU - MATMUL(T_ENU_HEL,Rho_HEL)

  END DO
  Status = 0
  RETURN
END FUNCTION CalculateAimingAnglesAtLocation

FUNCTION dAngleCorrectionKinematicModel(aKinematicModel_m, angleAz, angleEl) RESULT(dAngleCorrection)
!+
! Description:  This routine calculates azimuthal and elevation angle corrections for a heliostat
!               position (AngleAz and AngleEl) using the BCS Kinematic Correction Model
!
!               Correction Model Element  Description
!               ------------------------+---------------------
!               aKinematicModel_m(1)    : AngleZAxisAdjustment
!               aKinematicModel_m(2)    : ElOffsetAdjustment
!               aKinematicModel_m(3)    : AzWheelAdjustment
!               aKinematicModel_m(4)    : ElWheelAdjustment
!               aKinematicModel_m(5)    : AngleXAxisAdjustment
!               aKinematicModel_m(6)    : AngleYAxisAdjustment
!               aKinematicModel_m(7)    : AxisNonOrthogonality
!               aKinematicModel_m(8)    : BoreSightMisalignment
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                               Passed Variables                  Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)           :: aKinematicModel_m(8)           ! Heliostat index
REAL(8), INTENT(IN)           :: angleAz                        ! Azimuthal angle      (degrees)
REAL(8), INTENT(IN)           :: angleEl                        ! Elevation angle      (degrees)
TYPE(RotateAzEl)              :: dAngleCorrection               ! Azimuthal and Elevation Angle Correction (degrees)
!
!                              Local Variables
!----------------------------+------------------
REAL(8)                       :: H_n_m (2,8)                    ! Gain Matrix
REAL(8)                       :: dAng_n(2)                      ! Angle Corrections    (radians)
REAL(8)                       :: dAz                            ! Azimuthal correction (radians)
REAL(8)                       :: dEl                            ! Elevation correction (radians)
EQUIVALENCE                     (dAz,dAng_n(1)),(dEl,dAng_n(2))
REAL(8)                       :: dAngAz                         ! Azimuthal correction (degrees)
REAL(8)                       :: dAngEl                         ! Elevation correction (degrees)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------

  H_n_m   = HmatrixKinematicCorrections(angleAz,angleEl)
  dAng_n  = MATMUL(H_n_m,aKinematicModel_m)
  dAngAz  = dAz * 180d0/Pi
  dAngEl  = dEl * 180d0/Pi
  dAngleCorrection%Az = dAngAz
  dAngleCorrection%El = dAngEl
  RETURN

END FUNCTION dAngleCorrectionKinematicModel

FUNCTION HmatrixKinematicCorrections(angAz, angEl) RESULT(H_n_m)
!
!---------------------------------------------------------------------------------------------------
!Description:
!
!  Calculates the H matrix for Heliostat Kinematic Corrections.  This is a duplicate of the
!  HmatrixWLSLinear function in the CSP_BCSProcessing Module.  The purpose of reuse was to be
!  able to run this routine independently of the GE-Grid infrastructure
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!
USE NumericalConstants
IMPLICIT NONE
!                               Passed Variables                  Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)           :: angAz                          !Azimuthal Angle Counter Clockwise from North (Degrees)
REAL(8), INTENT(IN)           :: angEl                          !Elevation Angle (Degrees)
REAL(8)                       :: H_n_m(2,8)                     !H matrix (n/a)
!
!                              Local Variables
!----------------------------+------------------
REAL(8)                       :: angAzRad                       !Azimuthal Angle Clockwise from North (radians)
REAL(8)                       :: angElRad                       !Elevation Angle (radians)
REAL(8)                       :: sinAz                          !Sine of azimuthal angle (n/a)
REAL(8)                       :: cosAz                          !Cosine of azimuthal angle (n/a)
REAL(8)                       :: tanEl                          !Tangent of elevation angle (n/a)
REAL(8)                       :: cosEl                          !Cosine of the elevation angle (n/a)
REAL(8),PARAMETER             :: AngElMaxRad = 0.95d0*Pi/2d0    !Maximum elevation angle (radians)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  !
  ! Convert from degrees to radians
  angAzRad = angAz * Pi / 180.0d0
  angElRad = angEl * Pi / 180.0d0
  !
  ! Calculate terms needed for the matrix
  sinAz    = SIN(angAzRad)
  cosAz    = COS(angAzRad)
  tanEl    = TAN(MAX(0d0,MIN(angElRad,angElMaxRad)))
  cosEl    = COS(MAX(0d0,MIN(angElRad,angElMaxRad)))
  !
  !  An Automated Method to Correct Heliostat Tracking Errors
  !  by SIRI SAHIB S. KHALSA, CLIFFORD K. HO, CHARLES E. ANDRAKA, SolarPACES 2011, Granada, Spain September 20-23, 2011
  !  8 term model:
  !
  !  Note that the HFCS uses a azimuthal angle that is Clockwise from the north.  The referenced paper uses an azimuthal
  !                     that is CounterClockwise from the South.
  !
  H_n_m(1,:) = (/  1d0,  0d0, angAzRad,   0d0   ,  tanEl*cosAz,  -tanEl*sinAz , tanEl, cosEl**(-1) /)
  H_n_m(2,:) = (/  0d0,  1d0,   0d0   , angElRad,    -sinAz   ,     -cosAz    , 0d0  , 0d0         /)!
  !
  RETURN
END FUNCTION HmatrixKinematicCorrections

FUNCTION qVectorKinematicCorrections(angAz, angEl, angAzMeas, angElMeas) RESULT(q_n)
!
!---------------------------------------------------------------------------------------------------
!Description:
!
!  Calculates the q vector for Heliostat Kinematic Corrections.  This is a duplicate of the
!  qVectorWLSLinear function in the CSP_BCSProcessing Module.  The purpose of reuse was to be
!  able to run this routine independently of the GE-Grid infrastructure
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!
USE NumericalConstants
IMPLICIT NONE
!                              Passed Variables                  Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)           :: angAz                          !true Azimuthal Angle (degrees)
REAL(8), INTENT(IN)           :: angEl                          !true Elevation Angle (degrees)
REAL(8), INTENT(IN)           :: angAzMeas                      !measured Azimuthal Angle (degrees)
REAL(8), INTENT(IN)           :: angElMeas                      !measured Elevation Angle (degrees)
REAL(8)                       :: q_n(2)                         !q vector (radians)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
q_n = (/MOD(angAz-angAzMeas,180d0), MOD(angEl-angElMeas,180d0)/)
q_n = q_n * Pi / 180d0

END FUNCTION qVectorKinematicCorrections


END MODULE Ephemeris
