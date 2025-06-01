! Source: EphemerisTypes.f90
!
! Module: EphemerisTypes
!
! Purpose: Provide types and constants used to interface to public domain codes from NASA JPL and the Naval Observatory
!
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! 01-MAR-2025     | MA    | SCR 150: Move Ephemeris types needed for TimeDataPackage to a common module
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
MODULE EphemerisTypes
!
!                               Name                              Description / (units)
!-----------------------------+---------------------------------+-----------------------------------------------------
!
  REAL(8)   , PARAMETER       :: JDT2000 = 2451545.0D0          ! January 1.5 2000 JDT (Julian Epoch)         (days)
!
  INTEGER(4),PUBLIC,PARAMETER :: MJD = 8                        ! Integer size for Modified Julian Date
  INTEGER(4),PUBLIC,PARAMETER :: WND = 8                        ! Integer size for Windows Time

  TYPE, PUBLIC :: SolarTimes
    SEQUENCE
    INTEGER(MJD)               :: Rise                          ! Sunrise Modified Julian Date       (10^-7 seconds)
    INTEGER(MJD)               :: Noon                          ! SolarNoon Modified Julian Date     (10^-7 seconds)
    INTEGER(MJD)               :: Transit                       ! SolarTransit Modified Julian Date  (10^-7 seconds)
    INTEGER(MJD)               :: Set                           ! Sunset Modified Julian Date        (10^-7 seconds)
  END TYPE SolarTimes

  TYPE GeoData
    SEQUENCE
    REAL(8)                    :: Latitude                      ! Geodetic Longitude                      (degrees)
    REAL(8)                    :: Longitude                     ! Geodetic Longitude                      (degrees)
    REAL(8)                    :: Elevation                     ! Surface elevation                            (km)
  END TYPE GeoData
END MODULE EphemerisTypes
