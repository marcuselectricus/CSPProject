!+
! Source:  UnitConversion.f90
!
! Module:  UnitConversion
!
! Description: This Module centralizes the definition of all unit conversion constants and routines.  Most of the unit
!              conversion constants in this routine are sourced from the following reference:
!
!                http://physics.nist.gov/Pubs/SP811/appenB8.html
!
!              Certain values, particulary those related to astronomy are sourced for the JPL Ephemeris DE431

! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!
! 1) NIST
!    http://physics.nist.gov/Pubs/SP811/appenB8.html
!
MODULE UnitConversion

REAL(8)   , PARAMETER           :: SecondsPerMinute                = 60.0d0
REAL(8)   , PARAMETER           :: MinutesFromSeconds              = 1.0d0/SecondsPerMinute
REAL(8)   , PARAMETER           :: MinutesPerSecond                = MinutesFromSeconds
REAL(8)   , PARAMETER           :: HoursFromSeconds                = 1.0d0/3600.0d0
REAL(8)   , PARAMETER           :: SecondsFromHours                = 3600.0d0
REAL(8)   , PARAMETER           :: SecondsPerHour                  = 3600.0d0
REAL(8)   , PARAMETER           :: CyclesPerMinute                 = 6
REAL(8)   , PARAMETER           :: JoulesPerBTU                    = 1055.05585262d0;
REAL(8)   , PARAMETER           :: BTUPerHrPerKW                   = 1d3 * SecondsPerHour / JoulesPerBTU
REAL(8)   , PARAMETER           :: BTUPerHrPerMW                   = 1d6 * SecondsPerHour / JoulesPerBTU
REAL(8)   , PARAMETER           :: MWFromBTUPerHr                  = 1d0/BTUPerHrPerMW
REAL(8)   , PARAMETER           :: KWFromBTUPerHr                  = 1d0/BTUPerHrPerKW
REAL(8)   , PARAMETER           :: NewtonMeterPerLbForceFt         = 1.355818d0
REAL(8)   , PARAMETER           :: BTUPerBTUth                     = 0.999331d0
REAL(8)   , PARAMETER           :: LbfFtPerBTU                     = JoulesPerBTU/NewtonMeterPerLbForceFt !778.54
REAL(8)   , PARAMETER           :: MetersPerUSSurveyFoot           = 12.0d0/39.37d0
REAL(8)   , PARAMETER           :: MetersPerFoot                   = 0.3048d0;
REAL(8)   , PARAMETER           :: MetersFromFeet                  = MetersPerFoot
REAL(8)   , PARAMETER           :: FeetPerMeter                    = 1.0d0/MetersPerFoot
REAL(8)   , PARAMETER           :: FeetFromMeters                  = FeetPerMeter
REAL(8)   , PARAMETER           :: KiloPascalsPerAtm               = 1.01325d2;
REAL(8)   , PARAMETER           :: PsiPerAtm                       = 14.6959487755134d0
REAL(8)   , PARAMETER           :: KiloPascalsPerPsi               = KiloPascalsPerAtm/PsiPerAtm !6.894757d0
REAL(8)   , PARAMETER           :: PsiaPerKiloPascal               = 1d0/KiloPascalsPerPsi
REAL(8)   , PARAMETER           :: TorrPerAtm                      = 760.0d0
REAL(8)   , PARAMETER           :: InchHgPerPsia                   = 2.0360206576d0
REAL(8)   , PARAMETER           :: PsiaPerInchHg                   = 1d0/InchHgPerPsia
REAL(8)   , PARAMETER           :: KiloPascalsPerBar               = 100d0
REAL(8)   , PARAMETER           :: PsiaPerBar                      = PsiaPerKiloPascal*KiloPascalsPerBar
REAL(8)   , PARAMETER           :: CentipoisePerPascalSec          = 1000.0d0
REAL(8)   , PARAMETER           :: PascalSecPerCentipoise          = 1.0d0/CentipoisePerPascalSec
REAL(8)   , PARAMETER           :: PascalSecFromLbmPerFtSec        = 1.488164d0
REAL(8)   , PARAMETER           :: LbmPerFtSecFromCp               = 1.0d0/PascalSecFromLbmPerFtSec/1000.0d0
REAL(8)   , PARAMETER           :: LbmPerFtHrFromCp                = LbmPerFtSecFromCp * 3600.0d0
REAL(8)   , PARAMETER           :: GramsPerOunce                   = 28.34952d0
REAL(8)   , PARAMETER           :: CubicMetersPerGallon            = 3.785412d-3
REAL(8)   , PARAMETER           :: GallonPerCubicFt                = (MetersPerFoot)**3/cubicMetersPerGallon
REAL(8)   , PARAMETER           :: KilogramsPerPound               = 0.4535924d0
REAL(8)   , PARAMETER           :: FeetPerMile                     = 5280.0d0
REAL(8)   , PARAMETER           :: MilesPerFoot                    = 1.0d0/FeetPerMile
REAL(8)   , PARAMETER           :: RadsPerSecFromRPM               = 2.0*3.14159265358979323846D0/60.0d0!
REAL(8)   , PARAMETER           :: kmPerMile                       = MetersPerFoot*FeetPerMile/1d3
REAL(8)   , PARAMETER           :: kmPerFoot                       = MetersPerFoot/1d3
REAL(8)   , PARAMETER           :: MetersPerSecondFromMPH          = MetersPerFoot*FeetPerMile/SecondsPerHour
REAL(8)   , PARAMETER           :: MPHFromMetersPerSecond          = 1d0/MetersPerSecondFromMPH
REAL(8)   , PARAMETER           :: MilePersHourFromMetersPerSecond = 1d0/MetersPerSecondFromMPH
!  Added from lc_constants_unitConverion.inc
REAL(8)   , PARAMETER           :: HoursPerSecond                  = 1.0d0/SecondsPerHour
REAL(8)   , PARAMETER           :: kmPerNauticalMile               = 1.852d0
REAL(8)   , PARAMETER           :: FeetPerInch                     = 1.0d0/12.0d0
REAL(8)   , PARAMETER           :: InchesPerFoot                   = 12.0d0
REAL(8)   , PARAMETER           :: CubicInchesPerCubicFoot         = 1728.0d0
REAL(8)   , PARAMETER           :: SlugsPerLbm                     = 32.17405d0
REAL(8)   , PARAMETER           :: AtmospheresFromPascals          = 1.0d0/101325.0d0
REAL(8)   , PARAMETER           :: PascalsFromAtmospheres          = 101325.0d0
REAL(8)   , PARAMETER           :: PsiFromPascals                  = 1.0d-3/KiloPascalsPerPsi
REAL(8)   , PARAMETER           :: DegRad                          = 180.0D0/(4.0d0*ATAN(1.0d0))
REAL(8)   , PARAMETER           :: RadDeg                          = 1.0d0/DegRad
REAL(8)   , PARAMETER           :: PsiReference                    = PsiPerAtm    !Psia
REAL(8)   , PARAMETER           :: TempFReference                  = 32.0d0       !DegF
REAL(8)   , PARAMETER           :: TempRFZERO                      = 459.67D0     !DegR
!
! 2) JPL Ephemeris DE431
!    This gives a more accurate definition of 1 Astronomical Unit than NIST
!-
REAL(8)   , PARAMETER           :: KmPerAU                         = 149597870.7D0

INTERFACE DegFFromDegC
  MODULE PROCEDURE DegFFromDegC_R4,DegFFromDegC_R8
END INTERFACE
INTERFACE DegCFromDegF
  MODULE PROCEDURE DegCFromDegF_R4,DegCFromDegF_R8,DegCFromDegF_R8Array
END INTERFACE
INTERFACE DegFFromDegK
  MODULE PROCEDURE DegFFromDegK_R4,DegFFromDegK_R8
END INTERFACE
INTERFACE DegKFromDegF
  MODULE PROCEDURE DegKFromDegF_R4,DegKFromDegF_R8
END INTERFACE
INTERFACE DegRFromDegF
  MODULE PROCEDURE DegRFromDegF_R4,DegRFromDegF_R8,DegRFromDegF_R8Array
END INTERFACE
INTERFACE DegFFromDegR
  MODULE PROCEDURE DegFFromDegR_R4,DegFFromDegR_R8,DegFFromDegR_R8Array
END INTERFACE
CONTAINS

PURE FUNCTION DegCFromDegF_R8(DegF) RESULT(DegC)
!+
! Description: This function converts a single temperature from Fahrenheit to Celsius. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegF                        ! Temperature Input  (deg-Fahrenheit)
REAL(8)                          :: DegC                        ! Temperature Output (deg-Celsius)
!
DegC = (DegF-TempFReference)/1.8d0
RETURN
END FUNCTION DegCFromDegF_R8

PURE FUNCTION DegCFromDegF_R8Array(DegF_n, nEnd) RESULT(DegC_n)
!+
! Description: This function converts an array of temperatures from Fahrenheit to Celsius. The input is a REAL(8) array.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
INTEGER(4), INTENT(IN)           :: nEnd                        ! Number of Elements
REAL(8)   , INTENT(IN)           :: DegF_n(nEnd)                ! Temperature Output (deg-Rankine)
REAL(8)                          :: DegC_n(nEnd)                ! Temperature Output (deg-Celsius)
!
DegC_n = (DegF_n - TempFReference)/1.8d0
RETURN
END FUNCTION DegCFromDegF_R8Array

!
PURE FUNCTION DegCFromDegF_R4(DegF) RESULT(DegC)
!+
! Description: This function converts a single temperature from Fahrenheit to Celsius. The input is REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegF                        ! Temperature Input  (deg-Fahrenheit)
REAL(8)                          :: DegC                        ! Temperature Output (deg-Celsius)
!
DegC = (DBLE(DegF)-TempFReference)/1.8d0
RETURN
END FUNCTION DegCFromDegF_R4

PURE FUNCTION DegFFromDegC_R8(DegC) RESULT(DegF)
!+
! Description: This function converts a single temperature from Celsius to Fahrenheit. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegC                        ! Temperature Input  (deg-Celsius)
REAL(8)                          :: DegF                        ! Temperature Output (deg-Fahrenheit)
!
DegF = DegC*1.8d0 + TempFReference
RETURN
END FUNCTION DegFFromDegC_R8
!
PURE FUNCTION DegFFromDegC_R4(DegC) RESULT(DegF)
!+
! Description: This function converts a single temperature from Celsius to Fahrenheit. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegC                        ! Temperature Input  (deg-Celsius)
REAL(4)                          :: DegF                        ! Temperature Output (deg-Fahrenheit)
!
DegF = DegC*1.8 + SNGL(TempFReference)
RETURN
END FUNCTION DegFFromDegC_R4

PURE FUNCTION DegRFromDegF_R8(DegF) RESULT(DegR)
!+
! Description: This function converts a single temperature from Fahrenheit to Rankine. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegF                        ! Temperature Input  (deg-Fahrenheit)
REAL(8)                          :: DegR                        ! Temperature Output (deg-Rankine)
!
DegR = DegF + TempRFZERO
RETURN
END FUNCTION DegRFromDegF_R8

PURE FUNCTION DegRFromDegF_R8Array(DegF_n,nEnd) RESULT(DegR_n)
!+
! Description: This function converts an array of temperatures from Fahrenheit to Rankine. The input is a REAL(8) array.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
INTEGER(4), INTENT(IN)           :: nEnd                        ! Number of Elements
REAL(8)   , INTENT(IN)           :: DegF_n(nEnd)                ! Temperature Input  (deg-Fahrenheit)
REAL(8)                          :: DegR_n(nEnd)                ! Temperature Output (deg-Rankine)
!
DegR_n = DegF_n + TempRFZERO
RETURN
END FUNCTION DegRFromDegF_R8Array
!
PURE FUNCTION DegRFromDegF_R4(DegF) RESULT(DegR)
!+
! Description: This function converts a single temperature from Fahrenheit to Rankine. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegF                        ! Temperature Input  (deg-Fahrenheit)
REAL(4)                          :: DegR                        ! Temperature Output (deg-Rankine)
!
DegR = DegF + SNGL(TempRFZERO)
RETURN
END FUNCTION DegRFromDegF_R4

PURE FUNCTION DegFFromDegR_R8Array(DegR_n, nEnd) RESULT(DegF_n)
!+
! Description: This function converts an array of temperatures from Rankine to Fahrenheit. The input is a REAL(8) array.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
INTEGER(4), INTENT(IN)           :: nEnd                        ! Number of Elements
REAL(8)   , INTENT(IN)           :: DegR_n(nEnd)                ! Temperature Output (deg-Rankine)
REAL(8)                          :: DegF_n(nEnd)                        ! Temperature Input  (deg-Fahrenheit)
!
DegF_n = DegR_n - TempRFZERO
RETURN
END FUNCTION DegFFromDegR_R8Array

PURE FUNCTION DegFFromDegR_R8(DegR) RESULT(DegF)
!+
! Description: This function converts a single temperature from Rankine to Fahrenheit. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegR                        ! Temperature Output (deg-Rankine)
REAL(8)                          :: DegF                        ! Temperature Input  (deg-Fahrenheit)
!
DegF = DegR - TempRFZERO
RETURN
END FUNCTION DegFFromDegR_R8
!
PURE FUNCTION DegFFromDegR_R4(DegR) RESULT(DegF)
!+
! Description: This function converts a single temperature from Celsius to Fahrenheit. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegR                        ! Temperature Input   (deg-Rankine)
REAL(4)                          :: DegF                        ! Temperature Output (deg-Fahrenheit)
!
DegF = DegR - SNGL(TempRFZERO)
RETURN
END FUNCTION DegFFromDegR_R4

PURE FUNCTION DegRFromDegK_R8(DegK) RESULT(DegR)
!+
! Description: This function converts a single temperature from Kelvin to Rankine. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegK                        ! Temperature Input  (deg-Kelvin)
REAL(8)                          :: DegR                        ! Temperature Output (deg-Rankine)
!
DegR = DegK*1.8d0
RETURN
END FUNCTION DegRFromDegK_R8
!
PURE FUNCTION DegRFromDegK_R4(DegK) RESULT(DegR)
!+
! Description: This function converts a single temperature from Kelvin to Rankine. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegK                        ! Temperature Input  (deg-Kelvin)
REAL(4)                          :: DegR                        ! Temperature Output (deg-Rankine)
!
DegR = DegK*1.8
RETURN
END FUNCTION DegRFromDegK_R4

PURE FUNCTION DegKFromDegR_R8(DegR) RESULT(DegK)
!+
! Description: This function converts a single temperature from Rankine to Kelvin. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegR                        ! Temperature Input  (deg-Rankine)
REAL(8)                          :: DegK                        ! Temperature Output (deg-Kelvin)
!
DegK = DegR/1.8d0
RETURN
END FUNCTION DegKFromDegR_R8
!
PURE FUNCTION DegKFromDegR_R4(DegR) RESULT(DegK)
!+
! Description: This function converts a single temperature from Rankine to Kelvin. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegR                        ! Temperature Input  (deg-Rankine)
REAL(4)                          :: DegK                        ! Temperature Output (deg-Kelvin)
!
DegK = DegR/1.8
RETURN
END FUNCTION DegKFromDegR_R4


PURE FUNCTION DegKFromDegF_R8(DegF) RESULT(DegK)
!+
! Description: This function converts a single temperature from Fahrenheit to Kelvin. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegF                        ! Temperature Input  (deg-Fahrenheit)
REAL(8)                          :: DegK                        ! Temperature Output (deg-Kelvin)
!
DegK = (DegF + TempRFZERO)/1.8d0
RETURN
END FUNCTION DegKFromDegF_R8

PURE FUNCTION DegKFromDegF_R4(DegF) RESULT(DegK)
!+
! Description: This function converts a single temperature from Fahrenheit to Kelvin. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegF                        ! Temperature Input  (deg-Fahrenheit)
REAL(4)                          :: DegK                        ! Temperature Output (deg-Kelvin)
!
DegK = (DegF + SNGL(TempRFZERO))/1.8
RETURN
END FUNCTION DegKFromDegF_R4

PURE FUNCTION DegFFromDegK_R8(DegK) RESULT(DegF)
!+
! Description: This function converts a single temperature from Kelvin to Fahrenheit. The input is a REAL(8) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(8), INTENT(IN)              :: DegK                        ! Temperature Output (deg-Kelvin)
REAL(8)                          :: DegF                        ! Temperature Output (deg-Fahrenheit)
!
DegF = DegK*1.8d0 - TempRFZERO
RETURN
END FUNCTION DegFFromDegK_R8
!
PURE FUNCTION DegFFromDegK_R4(DegK) RESULT(DegF)
!+
! Description: This function converts a single temperature from Kelvin to Fahrenheit. The input is a REAL(4) word.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                 Passed Variables                Description / (units)
!--------------------------------+------------------------------+-----------------------------------
REAL(4), INTENT(IN)              :: DegK                        ! Temperature Output (deg-Kelvin)
REAL(4)                          :: DegF                        ! Temperature Output (deg-Fahrenheit)
!
DegF = DegK*1.8 - SNGL(TempRFZERO)
RETURN
END FUNCTION DegFFromDegK_R4

END MODULE UnitConversion
