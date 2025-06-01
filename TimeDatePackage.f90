!+
! Source: TimeDataPackage.f90
!
! Module: TimeDataPackage
!
! Description: This Module provides consistent time/date information.  Support is provided for the following time
!              standards:
!
!              1) Modified Julian Date:       This time standard has a starting Epoch of Midnight November 17, 1858.
!                 (MJD)                       The normal format for this time in this module is 64 bit integer with
!                                             a resolution of 100 nanoseconds. This is the standard used in the OpenVMS
!                                             operating system.  The time scale for this standard is the
!                                             International Second defined by atomic clock measurements. Leap seconds
!                                             are included this time standard.
!
!              2) Julian Date:                This time standard has a starting Epoch of Noon January 1, 4713 B.C.
!                 (JD)                        The normal formate for the time is 64 bit double precision with units of
!                                             1 day.  There is also extended 128 bit double precision support.  This is
!                                             the time standard used in interfacing with the JPL DE431 ephemeris. The
!                                             This second duration used is also the International Second. Leap seconds
!                                             are included in this standard.
!
!              3) JPL Ephemeris Time:         This time standard is used internally in the JPL ephemesis. The starting
!                 (Teph)                      ephoch for this time is referred to as J2000 - Noon January 1, 2000. The
!                                             second duration is the International Second. This standard accounts for
!                                             relativistic time effects. Leap seconds are included in this standard.
!
!              4) Civil Time:                 This time standard is the time that we all are most familiar with in our
!                 (UTC and Local Time)        daily life.  The UTC second updates at the same rate as the second in the
!                                             standards seconds based on atomic clocks. The UTC day follows the rotation
!                                             period of the Earth. Because the rotation period of the Earth is not
!                                             constant but the period of atomic clocks is, occasionally a leap second has
!                                             to be added or subtracted. There are also time zones for Local Time in
!                                             this standard.
!
!              5) Habitat Time                This time standard is used by the GE-Grid infrastructure.  The starting
!                                             ephoch for this time is Midnight January 1, 1979. The second duration is
!                                             the International Second. Leap seconds are not treated in this time
!                                             standard.  The HFCS has to make adjustments for leap seconds when
!                                             using Habitat Time.
!
!              6) Unix Time                   This time standard is used by the Intel Fortran installation. The starting
!                                             ephoch for this time is Midnight January 1, 1970. The second duration is
!                                             the International Second. Leap seconds are not treated in this time
!                                             standard.  The HFCS has to make adjustments for leap seconds when
!                                             using Unix Time.
!
!              6) Windows Time                This time standard has a starting Epoch of Midnight January 1, 1601.
!                                             The normal format for this time in this module is 64 bit integer with
!                                             a resolution of 100 nanoseconds. This is the standard used in the Windows
!                                             operating system.  The time scale for this standard is the
!                                             International Second defined by atomic clock measurements. Leap seconds
!                                             are not treated in this time standard. The HFCS has to make adjustments
!                                             for leap seconds when using Windows Time.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
! 03-MAR-2025     | MA    | SCR 150: Added TYPE TimeDateTiesol for integrating Tietronix's Tiesol package
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
MODULE TimeDatePackage
!
USE EphemerisTypes
USE ISO_C_BINDING
IMPLICIT NONE
!                                Global Variables                 Description / (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),PUBLIC,PARAMETER   :: TIME_S_NORMAL         =  1
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_INVALID        = -1
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_NOTINITIALIZED = -2
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_FAILEDINIT     = -3
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_TZSETUP        = -4
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_TZRULES        = -5
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_TZSWITCHES     = -6
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_SETUPLOCALE    = -7
INTEGER(4),PUBLIC,PARAMETER   :: TIME_E_ZONENOTFOUND   = -8

INTEGER(MJD)                  :: TimeStarted
!
!                                Interfaces
!-----------------------------------------------------------------------------------------------------------------------
INTERFACE LIB$WAIT
  MODULE PROCEDURE LIB$WAIT_I2,LIB$WAIT_I4,LIB$WAIT_I8,LIB$WAIT_R4,LIB$WAIT_R8
END INTERFACE
!
INTERFACE LIB$CONVERT_DATE_STRING
  MODULE PROCEDURE LIB$CONVERT_DATE_STRING_I8,LIB$CONVERT_DATE_STRING_I4
END INTERFACE
!
INTERFACE SYS$GETTIM
  MODULE PROCEDURE SYS$GETTIM_I8,SYS$GETTIM_I4
END INTERFACE
!
INTERFACE SYS$ASCTIM
  MODULE PROCEDURE SYS$ASCTIM_I8,SYS$ASCTIM_I4
END INTERFACE
!
INTERFACE SYS$BINTIM
  MODULE PROCEDURE SYS$BINTIM_I8,SYS$BINTIM_I4
END INTERFACE
INTERFACE GET_TIME_NOW
  MODULE PROCEDURE GET_TIME_NOW_I8,GET_TIME_NOW_I4
END INTERFACE

INTERFACE DayFromSolarYearStart
  MODULE PROCEDURE DayFromSolarYearStartForDate, DayFromSolarYearStartForTime
END INTERFACE

INTERFACE DayInSolarYear
  MODULE PROCEDURE DayInSolarYear, DayInSolarYearForTime
END INTERFACE

INTERFACE GetTimeOfDate
  MODULE PROCEDURE GetTimeOfDateTimeArrayISO, GetTimeOfDateISO, GetTimeOfTimeDate
END INTERFACE
!
!                                Sizing Parameter                Description / (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4) ,   PARAMETER      :: MX$LocaleTz   = 512
INTEGER(4) ,   PARAMETER      :: MX$ZoneTz     = 2304
INTEGER(4) ,   PARAMETER      :: MX$RuleTz     = 256
INTEGER(4) ,   PARAMETER      :: MX$RuleInfoTz = 2304
INTEGER(4) ,   PARAMETER      :: MX$LinkTz     = 512
INTEGER(4) ,   PARAMETER      :: SwitchTz$MX   = 12
!
! Types                         Elements of Type                       Description / (Units)
!-----------------------------+-----------------------------------------------------------------------------------------
TYPE, PUBLIC :: TzLocale
  SEQUENCE
  CHARACTER(32)               :: Name                         = " "    ! Name of the locale (e.g. Europe/London)
  CHARACTER(16)               :: Continent                    = " "    ! Continent or Ocean (e.g. Europe or Pacific)
  CHARACTER(16)               :: City                         = " "    ! City
  INTEGER(4)                  :: ZoneStart                    = 0      ! Start of timezone adjustments for this locale
  INTEGER(4)                  :: ZoneEnd                      = 0      !
  INTEGER(4)                  :: YearNow                      = 0      !
  INTEGER(4)                  :: ZoneNow                      = 0      !
  INTEGER(4)                  :: SwitchEnd                    = 0      !
  INTEGER(MJD)                :: Time_Switch    (SwitchTz$MX) = 0      ! Timezone switch starting time   (10^-7 seconds)
  REAL(8)                     :: dHours_Switch  (SwitchTz$MX) = 0d0    !
  CHARACTER(8)                :: AbbrZone_Switch(SwitchTz$MX) = " "    !
  INTEGER(4)                  :: RuleInfo_Switch(SwitchTz$MX) = 0      !
END TYPE TzLocale
TYPE, PUBLIC  :: TzZone
  SEQUENCE
  INTEGER(4)                  :: Locale                       = 0      ! Locale of this time zone
  INTEGER(4)                  :: dHourUTC                     = 0      ! Nominal Hours  from UTC               (hours)
  INTEGER(4)                  :: dMinuteUTC                   = 0      ! Nominal Minutes from UTC            (minutes)
  INTEGER(4)                  :: dSecondUTC                   = 0      ! Nominal Seconds from UTC            (seconds)
  INTEGER(4)                  :: Rule                         = 0      ! Rule followed in this timezone
  LOGICAL(4)                  :: UseRule                      = .false.! Set true if a this timezone follows a rule
  INTEGER(4)                  :: dHour                        = 0      ! Shift in Nominal Hours  from UTC      (hours)
  INTEGER(4)                  :: dMinute                      = 0      ! Shift in Nominal Minutes from UTC   (minutes)
  INTEGER(4)                  :: dSecond                      = 0      ! Shift in Nominal Seconds from UTC   (seconds)
  CHARACTER(8)                :: AbbrZone                     = " "    !
  INTEGER(4)                  :: YearUntil                    = 0      !
  INTEGER(4)                  :: MonthUntil                   = 0      !
  INTEGER(4)                  :: DayUntil
  LOGICAL(4)                  :: AtUTC
  INTEGER(MJD)                :: TimeUntil
  INTEGER(4)                  :: HourUntil
  INTEGER(4)                  :: MinuteUntil
  INTEGER(4)                  :: SecondUntil
  INTEGER(4)                  :: dHourRule
  INTEGER(4)                  :: dMinuteRule
  INTEGER(4)                  :: Align
  REAL(8)                     :: dHoursUTC
END TYPE TzZone
TYPE, PUBLIC  :: TzRule
  CHARACTER(16)               :: Name
  INTEGER(4)                  :: Start$RuleInfo
END TYPE TzRule
ENUM, BIND(C)
  ENUMERATOR OnWhatDay
  ENUMERATOR OnDayOfMonth
  ENUMERATOR OnLastWeekDay
  ENUMERATOR OnWeekDayAfter
  ENUMERATOR OnWeekDayBefore
END ENUM
TYPE, PUBLIC  :: TzRuleInfo
  SEQUENCE
  INTEGER(4)                  :: YearFrom
  INTEGER(4)                  :: YearTo
  INTEGER(4)                  :: Month
  INTEGER(4)                  :: Day
  INTEGER(4)                  :: DayOfWeek
  INTEGER(4)                  :: When
  INTEGER(4)                  :: Hour
  INTEGER(4)                  :: Minute
  INTEGER(4)                  :: Second
  INTEGER(4)                  :: dHourShift
  INTEGER(4)                  :: dMinuteShift
  LOGICAL(4)                  :: AtUTC
  CHARACTER(8)                :: dAbbrZone
  INTEGER(4)                  :: Next$RuleInfo
  INTEGER(4)                  :: Align
END TYPE TzRuleInfo
TYPE, PUBLIC   :: TzLink
  SEQUENCE
  CHARACTER(64)               :: Name                    != 'Europe/London'
  CHARACTER(16)               :: Continent               != 'Europe'
  CHARACTER(16)               :: City                    != 'London'
  INTEGER(4)                  :: Locale
  INTEGER(4)                  :: Align
END TYPE TzLink
TYPE(TzLocale)                :: Tz_Locale(MX$LocaleTz)
TYPE(TzZone)                  :: Tz_Zone(MX$ZoneTz)
TYPE(TzRule)                  :: Tz_Rule(MX$RuleTz)
TYPE(TzRuleInfo)              :: Tz_RuleInfo(MX$RuleInfoTz)
TYPE(TzLink)                  :: Tz_Link(MX$LinkTz)
INTEGER(4)                    :: LV$LocaleTz     = 0
INTEGER(4)                    :: LV$ZoneTz       = 0
INTEGER(4)                    :: LV$RuleTz       = 0
INTEGER(4)                    :: LV$RuleInfoTz   = 0
INTEGER(4)                    :: LV$LinkTz       = 0
TYPE, PUBLIC :: TimeZoneRule                                  !Obsolete when Olson Tz is fully implemented
  SEQUENCE
  CHARACTER(16)               :: Name
  INTEGER(4)                  :: YearFrom
  INTEGER(4)                  :: YearTo
  INTEGER(4)                  :: Month
  INTEGER(4)                  :: Day
  INTEGER(4)                  :: DayOfWeek
  LOGICAL(1)                  :: AtDayOfMonth
  LOGICAL(1)                  :: AtDayOfWeekOnOrAfter
  LOGICAL(1)                  :: AtLastDayOfWeekInMonth
  INTEGER(4)                  :: HourAt
  INTEGER(4)                  :: MinuteAt
  INTEGER(4)                  :: HourSave
  INTEGER(4)                  :: MinuteSave
  LOGICAL(1)                  :: AtUTC
  CHARACTER(1)                :: Letter
  INTEGER(2)                  :: Align
END TYPE TimeZoneRule

TYPE,  PUBLIC :: DayLightSavingsRule                             !Obsolete when Olson Tz is fully implemented
  SEQUENCE                    !LastWednesday-September-%YR% 02:00:00 UTC
  INTEGER(MJD)                :: Time_Switch(1:2)
  INTEGER(4)                  :: Year
  CHARACTER(42)               :: TimeDateString_Switch(1:2)
  LOGICAL(4)                  :: DayLightSavings_Switch(0:2)
  CHARACTER(24)               :: TimeStampUTC_Switch(1:2)
  INTEGER(4)                  :: SecondLocalBefore_Switch(1:2)
  INTEGER(4)                  :: SecondLocalAfter_Switch (1:2)
  INTEGER(4)                  :: Align(3)
END TYPE DayLightSavingsRule

TYPE, PUBLIC :: TimeZoneLocale
  SEQUENCE
  CHARACTER(48)               :: Name                    = 'Europe/London'
  REAL(4)                     :: dHoursSTD               = 0
  REAL(4)                     :: dHoursDST               = 1
  CHARACTER(4)                :: StandardTime            = 'SDT '
  CHARACTER(4)                :: DayLightSavingsTime     = 'DST '
  CHARACTER(16)               :: Continent               = 'Europe'
  CHARACTER(16)               :: City                    = 'London'
  CHARACTER(16)               :: RuleDaylightSavings     = 'EU'
END TYPE TimeZoneLocale

TYPE, PUBLIC :: HolidayRule
  SEQUENCE
  INTEGER(MJD)                :: Time
  CHARACTER(40)               :: Description
  CHARACTER(16)               :: Substitution
  CHARACTER(24)               :: TimeStamp
  CHARACTER(48)               :: TimeDateString
END TYPE HolidayRule

! Type for interfacing with Tiesol
TYPE, BIND(C), PUBLIC :: TimeDateTiesol
  INTEGER(C_INT)              :: Year
  INTEGER(C_INT)              :: Month
  INTEGER(C_INT)              :: Day
  INTEGER(C_INT)              :: Hour
  INTEGER(C_INT)              :: Minute
  REAL(C_DOUBLE)              :: Seconds
END TYPE TimeDateTiesol
INTEGER(4)                    :: YearFromTiesol          = 0
INTEGER(MJD)                  :: dTimeTiesol_month(0:13) = 0

! Type compatible with ISO 8601:1988 (zone is minutes from UTC)
TYPE, PUBLIC :: TimeDateISO
  SEQUENCE
  INTEGER(4)                  :: Year         = 0
  INTEGER(4)                  :: Month        = 0
  INTEGER(4)                  :: Day          = 0
  INTEGER(4)                  :: dMinuteZone  = 0
  INTEGER(4)                  :: Hour         = 0
  INTEGER(4)                  :: Minute       = 0
  INTEGER(4)                  :: Second       = 0
  INTEGER(4)                  :: msec         = 0
END TYPE

TYPE, PUBLIC :: TimeDate
  SEQUENCE
  INTEGER(4)                  :: Year      = 0
  INTEGER(4)                  :: Month     = 0
  INTEGER(4)                  :: Day       = 0
  REAL(4)                     :: dHourZone = 0.0
  INTEGER(4)                  :: Hour      = 0
  INTEGER(4)                  :: Minute    = 0
  INTEGER(4)                  :: Second    = 0
  INTEGER(4)                  :: msec      = 0
  INTEGER(4)                  :: usec      = 0
  INTEGER(4)                  :: nsec      = 0
  INTEGER(4)                  :: dayOfWeek = 0
  INTEGER(4)                  :: dayOfYear = 0
END TYPE

TYPE, PUBLIC :: TimeAndDate
  SEQUENCE
  INTEGER(4)                  :: Year       = 0
  INTEGER(4)                  :: Month      = 0
  INTEGER(4)                  :: Day        = 0
  REAL(4)                     :: dHourZone  = 0.0
  INTEGER(4)                  :: Hour       = 0
  INTEGER(4)                  :: Minute     = 0
  INTEGER(4)                  :: Second     = 0
  INTEGER(4)                  :: msec       = 0
  INTEGER(4)                  :: usec       = 0
  INTEGER(4)                  :: nsec       = 0
  INTEGER(4)                  :: dayOfWeek  = 0
  INTEGER(4)                  :: dayOfYear  = 0
  CHARACTER(24)               :: TimeStamp  = "17-NOV-1858 00:00:00.000"
  CHARACTER(4)                :: FormatTime = "UTC"
END TYPE

TYPE, PUBLIC :: TimeDateLocal
  SEQUENCE
  INTEGER(2)                  :: Year       = 0
  INTEGER(2)                  :: Month      = 0
  INTEGER(2)                  :: Day        = 0
  INTEGER(2)                  :: Hour       = 0
  INTEGER(2)                  :: Minute     = 0
  INTEGER(2)                  :: Second     = 0
END TYPE

TYPE, PUBLIC :: TimeDelta
  SEQUENCE
  INTEGER(4)                  :: Days      = 0
  INTEGER(4)                  :: Hours     = 0
  INTEGER(4)                  :: Minutes   = 0
  INTEGER(4)                  :: Seconds   = 0
  INTEGER(4)                  :: msec      = 0
  INTEGER(4)                  :: usec      = 0
  INTEGER(4)                  :: nsec      = 0
END TYPE

TYPE, PUBLIC :: TimeLeap
  SEQUENCE
  INTEGER(MJD)                :: Time      = 0
  INTEGER(MJD)                :: dTime     = 0
  INTEGER(4)                  :: Year      = 0
  INTEGER(4)                  :: Month     = 0
  INTEGER(4)                  :: Day       = 0
  REAL(4)                     :: dHourZone = 0.0
  INTEGER(4)                  :: Hour      = 0
  INTEGER(4)                  :: Minute    = 0
  INTEGER(4)                  :: Second    = 0
  INTEGER(4)                  :: msec      = 0
  INTEGER(4)                  :: usec      = 0
  INTEGER(4)                  :: nsec      = 0
  INTEGER(4)                  :: DayOfWeek = 0
  INTEGER(4)                  :: DayOfYear = 0
  TYPE(TimeDate)              :: DateAndTime
END TYPE TimeLeap

TYPE, PUBLIC :: UpdateDUT1
  SEQUENCE
  INTEGER(MJD)                :: TimeStart    = 0
  REAL(8)                     :: dUT1         = 0d0
  CHARACTER(10)               :: StartDate    = " "
  CHARACTER(10)               :: AnnounceDate = " "
  INTEGER(4)                  :: Number       = 0
END TYPE UpdateDUT1

TYPE ,PUBLIC :: TimeInternal
  SEQUENCE
  CHARACTER(48)               :: TimeStamp
  INTEGER(4)                  :: Year
  INTEGER(4)                  :: Month
  INTEGER(4)                  :: Day
  REAL(4)                     :: zone
  INTEGER(4)                  :: Hour
  INTEGER(4)                  :: Minute
  INTEGER(4)                  :: Second
  INTEGER(4)                  :: msec
  INTEGER(4)                  :: usec
  INTEGER(4)                  :: nsec
  INTEGER(MJD)                :: Time
END TYPE TimeInternal

TYPE FileInfo
  SEQUENCE
  INTEGER(MJD)                :: TimeCreation                   ! Modified Julian Date of File Creation (10^-7 seconds)
  INTEGER(MJD)                :: TimeLastWrite                  ! Modified Julian Date of Last Write    (10^-7 seconds)
  INTEGER(MJD)                :: TimeLastAccess                 ! Modified Julian Date of Last Access   (10^-7 seconds)
  INTEGER(4)                  :: Length                         ! Length of the File                          (records)
  LOGICAL(4)                  :: IsDirectory                    ! True if the "file" is actually a directory
  CHARACTER(255)              :: Name                           ! Name of the file
  INTEGER(1)                  :: Align                          ! Force alignment on a 64-bit boundary
END TYPE FileInfo
!
!                                Module Global Variables          Description / (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4),PRIVATE            :: Initialized       = .false.

LOGICAL(4),PRIVATE            :: LocaleSpecified   = .false.
TYPE(TimeZoneLocale)          :: CurrentLocale
TYPE(TimeZoneLocale)          :: SystemLocale
INTEGER(4)                    :: LocaleSystem      = 0
INTEGER(4)                    :: LocaleCurrent     = 0
CHARACTER(32)                 :: NameCurrentLocale = " "
CHARACTER(32)                 :: NameSystemLocale  = " "
LOGICAL(4),PRIVATE            :: TzInfoProcessed   = .false.

LOGICAL(4)                    :: GeoLocationSpecified = .false.
TYPE(GeoData)                 :: LocationForTime

REAL(4)     ,PARAMETER        :: UTC               = 0.0;
INTEGER(MJD),PARAMETER        :: TIME_PER_USEC     = 10
INTEGER(MJD),PARAMETER        :: TIME_PER_MSEC     = 10000
INTEGER(MJD),PARAMETER        :: TIME_PER_SECOND   = 10000000
INTEGER(MJD),PARAMETER        :: TIME_PER_MINUTE   = TIME_PER_SECOND*60
INTEGER(MJD),PARAMETER        :: TIME_PER_HOUR     = TIME_PER_MINUTE*60
INTEGER(MJD),PARAMETER        :: TIME_PER_DAY      = TIME_PER_HOUR*24
INTEGER(MJD),PARAMETER        :: TIME_PER_WEEK     = TIME_PER_DAY*7
INTEGER(MJD),PARAMETER        :: TIME_PER_YEAR     = TIME_PER_DAY*365 + TIME_PER_HOUR*6
INTEGER(MJD),PARAMETER        :: TIME_PER_CENTURY  = (TIME_PER_DAY*(365*302 + 366*98))/4
INTEGER(MJD),PARAMETER        :: TIME_MAX          = INT(z'7FFFFFFFFFFFFFFF',KIND=8)
INTEGER(MJD),PARAMETER        :: TIME_MIN          = INT(z'8000000000000000',KIND=8)
INTEGER(MJD),PARAMETER        :: DAY_DELTA_MAX     = 10000
INTEGER(MJD),PARAMETER        :: TIME_DELTA_MAX    = DAY_DELTA_MAX*TIME_PER_DAY - TIME_PER_MSEC
INTEGER(4)  ,PARAMETER        :: YEAR_EPOCH        = 1858
INTEGER(4)  ,PARAMETER        :: MONTH_EPOCH       = 11
INTEGER(4)  ,PARAMETER        :: DAY_EPOCH         = 17
INTEGER(4)  ,PARAMETER        :: YEAR_START        = 1601
INTEGER(4)  ,PARAMETER        :: YEAR_END          = 2601

INTEGER(MJD),PARAMETER        :: TimePerUsec       = TIME_PER_USEC
INTEGER(MJD),PARAMETER        :: TimePerMsec       = TIME_PER_MSEC
INTEGER(MJD),PARAMETER        :: TimePerSecond     = TIME_PER_SECOND
INTEGER(MJD),PARAMETER        :: TimePerMinute     = TIME_PER_MINUTE
INTEGER(MJD),PARAMETER        :: TimePerHour       = TIME_PER_HOUR
INTEGER(MJD),PARAMETER        :: TimePerDay        = TIME_PER_DAY
INTEGER(MJD),PARAMETER        :: TimePerWeek       = TIME_PER_WEEK
INTEGER(MJD),PARAMETER        :: TimePerYear       = TIME_PER_YEAR
INTEGER(MJD),PARAMETER        :: TimePerCentury    = TIME_PER_CENTURY
INTEGER(MJD),PARAMETER        :: TimeMin           = TIME_MIN
INTEGER(MJD),PARAMETER        :: TimeMax           = TIME_MAX
INTEGER(MJD),PARAMETER        :: DayDeltaMax       = DAY_DELTA_MAX
INTEGER(MJD),PARAMETER        :: TimeDeltaMax      = TIME_DELTA_MAX
INTEGER(4)  ,PARAMETER        :: YearEpoch         = YEAR_EPOCH
INTEGER(4)  ,PARAMETER        :: MonthEpoch        = MONTH_EPOCH
INTEGER(4)  ,PARAMETER        :: DayEpoch          = DAY_EPOCH
INTEGER(4)  ,PARAMETER        :: YearStart         = YEAR_START
INTEGER(4)  ,PARAMETER        :: YearEnd           = YEAR_END
INTEGER(4)  ,PARAMETER        :: YearMax           = 2601
INTEGER(4)  ,PARAMETER        :: YearMin           = 1601
INTEGER(4)  ,PARAMETER        :: DecemberLastYear  =  0, DeciembrePasado = 0
INTEGER(4)  ,PARAMETER        :: January           =  1, Enero           =  1
INTEGER(4)  ,PARAMETER        :: February          =  2, Febrero         =  2
INTEGER(4)  ,PARAMETER        :: March             =  3, Marzo           =  3
INTEGER(4)  ,PARAMETER        :: April             =  4, Abril           =  4
INTEGER(4)  ,PARAMETER        :: May               =  5, Mayo            =  5
INTEGER(4)  ,PARAMETER        :: June              =  6, Junio           =  6
INTEGER(4)  ,PARAMETER        :: July              =  7, Julio           =  7
INTEGER(4)  ,PARAMETER        :: August            =  8, Agosto          =  8
INTEGER(4)  ,PARAMETER        :: September         =  9, Septiembre      =  9
INTEGER(4)  ,PARAMETER        :: October           = 10, Octubre         = 10
INTEGER(4)  ,PARAMETER        :: November          = 11, Noviembre       = 11
INTEGER(4)  ,PARAMETER        :: December          = 12, Diciembre       = 12
INTEGER(4)  ,PARAMETER        :: JanuaryNextYear   = 13, EneroProximo    = 13

INTEGER(4)  ,PARAMETER        :: Sunday            =  1, domingo    =  1
INTEGER(4)  ,PARAMETER        :: Monday            =  2, lunes      =  2
INTEGER(4)  ,PARAMETER        :: Tuesday           =  3, martes     =  3
INTEGER(4)  ,PARAMETER        :: Wednesday         =  4, miercoles  =  4
INTEGER(4)  ,PARAMETER        :: Thursday          =  5, jueves     =  5
INTEGER(4)  ,PARAMETER        :: Friday            =  6, viernes    =  6
INTEGER(4)  ,PARAMETER        :: Saturday          =  7, sabado     =  7

INTEGER(4)  ,PARAMETER        :: DaysNormalYear_month(0:13) = (/31,31,28,31,30,31,30 &
                                                               ,31,31,30,31,30,31,31/)
INTEGER(4)  ,PARAMETER        :: DaysLeapYear_month  (0:13) = (/31,31,29,31,30,31,30 &
                                                               ,31,31,30,31,30,31,31/)
INTEGER(4)                    :: DaysEpochYear_month (0:13)
INTEGER(MJD)                  :: dTimeNormalYear_month(0:13)
INTEGER(MJD)                  :: dTimeLeapYear_month  (0:13)
INTEGER(MJD)                  :: dTimeEpochYear_month (0:13)
INTEGER(MJD)                  :: timeStart_year(YEAR_START-1:YEAR_END+1)
INTEGER(MJD)                  :: timeEnd_year  (YEAR_START-1:YEAR_END+1)
INTEGER(4)  ,PARAMETER        :: DAY_LAST   = 31
INTEGER(4)  ,PARAMETER        :: MONTH_LAST = 12

CHARACTER(2) ,PARAMETER       :: asciiNum_DayOfWeek(7) = (/'01','02','03','04','05','06','07'/)

CHARACTER(9) ,PARAMETER       :: ascii_DayOfWeek(7)    = (/'Sunday   ','Monday   ','Tuesday  ','Wednesday', &
                                                           'Thursday ','Friday   ','Saturday '             /)

CHARACTER(3) ,PARAMETER       :: ascii_month(0:13)     = (/'DEC','JAN','FEB','MAR','APR','MAY','JUN' &
                                                          ,'JUL','AUG','SEP','OCT','NOV','DEC','JAN'/)
CHARACTER(3) ,PARAMETER       :: asciiCap_month(0:13)  = (/'Dec','Jan','Feb','Mar','Apr','May','Jun' &
                                                          ,'Jul','Aug','Sep','Oct','Nov','Dec','Jan'/)
CHARACTER(2) ,PARAMETER       :: asciiNum_month(0:13)  = (/'12','01','02','03','04','05','06','07'   &
                                                          ,'08','09','10','11','12','01'            /)
CHARACTER(10),PARAMETER       :: name_month(12)        = (/'January  ','February ','March    '          &
                                                          ,'April    ','May      ','June     '          &
                                                          ,'July     ','August   ','September'          &
                                                          ,'October  ','November ','December '         /)
CHARACTER(10),PARAMETER       :: asciiFull_month(12)   = (/'JANUARY  ','FEBRUARY ','MARCH    '          &
                                                          ,'APRIL    ','MAY      ','JUNE     '          &
                                                          ,'JULY     ','AUGUST   ','SEPTEMBER'          &
                                                          ,'OCTOBER  ','NOVEMBER ','DECEMBER '         /)
CHARACTER(4)                  :: Ascii_year   (0:9999)
CHARACTER(4)                  :: ascii_days   (0:9999)
CHARACTER(2)                  :: ascii_day    (1:31  )
CHARACTER(2)                  :: ascii_hour   (0:23  )
CHARACTER(2)                  :: ascii_minute (0:59  )
CHARACTER(2)                  :: ascii_second (0:99  )          ! Allow a leap of up to 39 seconds in a day
CHARACTER(3)                  :: ascii_msec   (0:999 )
!
INTEGER(4),PARAMETER          :: mx$bulletinD = 256             ! Maximum DUT1 notices
INTEGER(4)                    :: lv$bulletinD = 0               ! Actual number of DUT1 notices
TYPE(UpdateDUT1), PRIVATE     :: IERS_BulletinD(mx$BulletinD)   ! DUT1 distributed by IERS bulletin-D
LOGICAL(4)                    :: InitializedBulletinD = .false. ! Set true after initializeing Bulletin-D

INTEGER(4),PARAMETER          :: mx$notice    = 128             ! Maximum leap second notices
INTEGER(4)                    :: lv$notice    = 0               ! Actual leap second notices
TYPE(TimeLeap),PRIVATE        :: LeapSecond_Notice(mx$notice)   ! Leap second notices
INTEGER(4)                    :: Notice_Year(YearStart:YearEnd) ! Start of notices for a year
INTEGER(4)                    :: Order_Notice(mx$notice) = 0    ! Order of notices used to ensure they are sequential
LOGICAL(4)                    :: AddedNoticesFromIERS = .false. ! The baseline leapsecond notices have been added
CHARACTER(8)                  :: DateLeapSeconds = '00000000'   ! Date Leap Second List Last Read (CCYYMMDD)
INTEGER(8)                    :: TmNTPBulletinC  =  0           ! IERS Bulletin C NTP Time of the system         (seconds)

INTEGER(4), PARAMETER         :: MX$TimeRule     = 1024
INTEGER(4), PARAMETER         :: MX$TimeRuleInfo = 2304
INTEGER(4), PARAMETER         :: MX$TimeZone     = 512
INTEGER(4), PARAMETER         :: MX$TimeZoneInfo = 2304
INTEGER(4), PARAMETER         :: MX$TimeLinkInfo = 512
INTEGER(4)                    :: LV$TimeRule     = 0
INTEGER(4)                    :: LV$TimeRuleInfo = 0
INTEGER(4)                    :: LV$TimeZone     = 0
INTEGER(4)                    :: LV$TimeZoneInfo = 0
INTEGER(4)                    :: LV$TimeLinkInfo = 0
CHARACTER(80)                 :: Ascii_TimeRuleInfo(MX$TimeRuleInfo)
CHARACTER(80)                 :: Ascii_TimeZoneInfo(MX$TimeZoneInfo)
CHARACTER(80)                 :: Ascii_TimeLinkInfo(MX$TimeLinkInfo)

TYPE(DayLightSavingsRule)     :: USRules
TYPE(DayLightSavingsRule)     :: EURules
TYPE(DayLightSavingsRule)     :: AURules

TYPE(HolidayRule)             :: Rule_Holiday(24)
INTEGER(4)                    :: HolidayEnd  = 0
LOGICAL(4)                    :: HolidayFileRead = .false.
CHARACTER(4)                  :: YearHoliday = 'YYYY'

INTEGER(MJD)                  :: TimeJanuary1970                ! Unix EPOCH
INTEGER(MJD)                  :: dTimeLeapJanuary1970           ! Leap Seconds before TimeJanuary1970 (10^-7 sec)

INTEGER(MJD)                  :: TimeJanuary1979                ! Habitat EPOCH
INTEGER(MJD)                  :: dTimeLeapJanuary1979           ! Leap Seconds before TimeJanuary1979 (10^-7 sec)

CONTAINS
!
FUNCTION JulianDateFromTime(Time) RESULT(JulianDate)
!+
! Description: This function converts a time value from Modified Julian Date to Julian Date
!
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
!
!  References: http://tycho.usno.navy.mil/mjd.html
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN)      :: Time                           !Modified Julian Date (10^-7 sec)
REAL(8)                       :: JulianDate                     !Julian Date          (days)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
JulianDate = DBLE(Time)/DBLE(TimePerDay) + 2400000.5D0
RETURN
END FUNCTION JulianDateFromTime
!
FUNCTION TimeFromJulianDate(JulianDate) RESULT(Time)
!+
! Description: This function converts a time value from Julian Date to Modified Julian Date.
!
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)        :: JulianDate                     !Julian Date          (days)
INTEGER(MJD)                  :: Time                           !Modified Julian Date (10^-7 sec)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
Time = INT( (JulianDate - 2400000.5D0) * DBLE(TimePerDay) , KIND=8)
RETURN
END FUNCTION TimeFromJulianDate

FUNCTION JulianDateExtendedFromTime(Time) RESULT(JulianDate)
!+
! Description: This function converts a time value from Modified Julian Date to Julian Date with extended precision.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN)     :: Time                           !Modified Julian Date (10^-7 sec)
REAL(8)                       :: JulianDate(2)                  !Extended Julian Date (days)
!
!                                Local
!-------------------------------------
INTEGER(8)                    :: DayPart                        !Day part of Time     (10^-7 sec)
INTEGER(8)                    :: DayFraction                    !Day fraction of Time (10^-7 sec)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
DayFraction   = MOD(Time,TimePerDay)
DayPart       = Time - DayFraction
JulianDate(1) = DBLE(DayPart    )/DBLE(TimePerDay) + 2400000.5d0
JulianDate(2) = DBLE(DayFraction)/DBLE(TimePerDay)
RETURN
END FUNCTION JulianDateExtendedFromTime
!
FUNCTION TimeFromJulianDateExtended(JulianDate)    RESULT(Time)
!+
! Description: This function converts a time value from Julian Date with extended precision to Modified Julian Date.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)        :: JulianDate(2)                  !Julian Date          (days)
INTEGER(MJD)                  :: Time                           !Modified Julian Date (10^-7 sec)
!
!                                Local
!-------------------------------------
INTEGER(8)                    :: DayPart                        !Day part of Time     (10^-7 sec)
INTEGER(8)                    :: DayFraction                    !Day fraction of Time (10^-7 sec)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
DayPart     = INT(JulianDate(1) - 2400000.5d0,KIND=8)*TimePerDay
DayFraction = INT(JulianDate(2) - 0d0        ,KIND=8)*TimePerDay
Time        = DayPart + DayFraction
RETURN
END FUNCTION TimeFromJulianDateExtended
!
FUNCTION TimeFromTimeTag(TimeTag) RESULT(Time)
!+
! Description: This function converts a time value from Habitat Time to Modified Julian Date.

!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: TimeTag                        ! Habitat Time Tag (sec)
INTEGER(MJD)                  :: Time                           ! Modified Julian Date (10^-7 sec)
!
!                                Local
!-------------------------------------
INTEGER(MJD)                  :: dTimeLeap                      ! Leap Seconds before Time (10^-7 sec)
LOGICAL(4)                    :: LeapOccurring                  ! Leap Second is being applied at Time
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(.not.Initialized .or. TimeTag < 0) THEN
  Time = 0
  RETURN
END IF

Time = TimeJanuary1979 + TimePerSecond * INT(TimeTag, KIND=8)
CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeap, LeapOccurring)
Time = Time + dTimeLeap-dTimeLeapJanuary1979
RETURN
END FUNCTION TimeFromTimeTag
!
FUNCTION TimeTagFromTime(Time) RESULT(TimeTag)
!+
! Description: This function converts a time from Modified Julian Date to a Habitat Time.
!              ToDo  Move TimeJanuary1979 and dTimeLeapJanuary1979 to Global and calculate them during Initialization
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) ,INTENT(IN)      :: Time                           ! Modified Julian Date                 (10^-7 sec)
INTEGER(4)                    :: TimeTag                        ! Habitat Time Tag (sec)
!
!                                Local
!-------------------------------------
INTEGER(MJD)                  :: dTimeLeap                      ! Leap Seconds before Time             (10^-7 sec)
LOGICAL(4)                    :: LeapOccurring                  ! Leap Second is being applied at Time
INTEGER(8)                    :: TmSeconds                      ! Time minus leaps seconds              (seconds)
INTEGER(4)                    :: TmSeconds_w(2)                 ! Word equivalence to TmSeconds         (seconds)
EQUIVALENCE                     (TmSeconds,TmSeconds_w)

!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(.not.Initialized) THEN
  TimeTag = 0
  RETURN
END IF

IF(Time < TimeJanuary1979) THEN
  TimeTag = 0
  RETURN
END IF

CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeap, LeapOccurring)
TmSeconds = (Time - TimeJanuary1979 - (dTimeLeap - dTimeLeapJanuary1979))/TimePerSecond

IF(TmSeconds_w(2) /= 0) THEN
  TimeTag = INT(z'7FFFFFFF', KIND=4)
ELSE
  TimeTag = MAX(TmSeconds_w(1), 0)
END IF
RETURN
END FUNCTION TimeTagFromTime

FUNCTION TimeFromTime1970(TmUnix) RESULT(Time)
!+
! Description: This function converts a time value from Unix Time to Modified Julian Date.
!              ToDo  Move TimeJanuary1970 and dTimeLeapJanuary1970 to Global and calculate them during Initialization
!
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: TmUnix                         ! Unix Time (epoch 1970) (sec)
INTEGER(MJD)                  :: Time                           ! Modified Julian Date (10^-7 sec)
!
!                                Local
!-------------------------------------
INTEGER(MJD)                  :: dTimeLeap                      ! Leap Seconds before Time (10^-7 sec)
LOGICAL(4)                    :: LeapOccurring                  ! Leap Second is being applied at Time
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(.not.Initialized .or. TmUnix < 0) THEN
  Time = 0
  RETURN
END IF

Time = TimeJanuary1970 + TimePerSecond * INT(TmUnix, KIND=8)
CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeap, LeapOccurring)
Time = Time + dTimeLeap-dTimeLeapJanuary1970
RETURN
END FUNCTION TimeFromTime1970

FUNCTION Time1970FromTime(Time) RESULT(TmUnix)
!+
! Description: This function converts a time value from Modified Julian Date Time to Unix Time.
!              ToDo  Move TimeJanuary1970 and dTimeLeapJanuary1970 to Global and calculate them during Initialization
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) ,INTENT(IN)      :: Time                           ! Modified Julian Date               (10^-7 seconds)
INTEGER(4)                    :: TmUnix                         ! Unix Time  (seconds)
!
!                                Local
!-------------------------------------
INTEGER(MJD)                  :: dTimeLeap                      ! Leap Seconds before Time              (10^-7 sec)
LOGICAL(4)                    :: LeapOccurring                  ! Leap Second is being applied at Time
INTEGER(8)                    :: TmSeconds                      ! Time minus leaps seconds                (seconds)
INTEGER(4)                    :: TmSeconds_w(2)                 ! Word equivalence to TmSeconds           (seconds)
EQUIVALENCE                     (TmSeconds,TmSeconds_w)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(.not.Initialized) THEN
  TmUnix = 0
  RETURN
END IF

IF(Time < TimeJanuary1970) THEN
  TmUnix = 0
  RETURN
END IF

CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeap, LeapOccurring)
TmSeconds = (Time - TimeJanuary1979 - (dTimeLeap - dTimeLeapJanuary1979))/TimePerSecond

IF(TmSeconds_w(2) /= 0) THEN
  TmUnix =  INT(z'7FFFFFFF', KIND=4)
ELSE
  TmUnix = MAX(TmSeconds_w(1), 0)
END IF

RETURN
END FUNCTION Time1970FromTime

FUNCTION TimeFromTimeWindows(TimeWindows) RESULT(Time)
!+
! Description: This function converts a time value from Window Time to Modified Julian date
!              ToDo Move dTime1601 to Global and set it during initializtion
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
! 06-FEB-2025     | MA    | SCR 150: Add capability to use Window Time (Ephoch Jan 1, 1601)
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(8)   , INTENT(IN)     :: TimeWindows                    ! Windows Time (10^-7 seconds)
INTEGER(MJD)                  :: Time                           ! Modified Julian Date (10^-7 seconds)
!
!                                Local
!-------------------------------------
INTEGER(8)   , PARAMETER      :: dTime1601 = 81377568 * 10**7   ! Time elapsed at MJD Ephoch since Windows Ephoch
INTEGER(MJD)                  :: dTimeLeapBefore                ! Leap Seconds before Initial Time   (10^-7 sec)
INTEGER(MJD)                  :: dTimeLeapAfter                 ! Leap Seconds before Adjusted Time  (10^-7 sec)
LOGICAL(4)                    :: LeapOccurring                  ! Leap Second is being applied at Time
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(.not.Initialized) THEN
  Time = TimeWindows - dTime1601
  RETURN
END IF

! Adjust for Epoch delta between Windows Time and Modified Julian Date
Time = TimeWindows - dTime1601
CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeapBefore, LeapOccurring)
Time = Time + dTimeLeapBefore
CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeapAfter, LeapOccurring)
Time = Time + dTimeLeapAfter - dTimeLeapBefore
RETURN
END FUNCTION TimeFromTimeWindows

FUNCTION TimeWindowsFromTime(Time) RESULT(TimeWindows)
!+
! Description: This function converts a time value from Modified Julian Date to Windows Time
!              ToDo Move dTime1601 to Global and set it during initializtion
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
! 06-FEB-2025     | MA    | SCR 150: Add capability to use Window Time (Ephoch Jan 1, 1601)
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN)     :: Time                           ! Modified Julian Date (10^-7 seconds)
INTEGER(8)                    :: TimeWindows                    ! Windows Time (10^-7 seconds)
!
!                                Local
!-------------------------------------
INTEGER(MJD) , PARAMETER      :: dTime1601 = 81377568 * 10**7   ! Time elapsed at MJD Ephoch since Windows Ephoch
INTEGER(MJD)                  :: dTimeLeapBefore                ! Leap Seconds before Time   (10^-7 sec)
!INTEGER(MJD)                 :: dTimeLeapAfter                 ! Leap Seconds before Adjusted Time  (10^-7 sec)
LOGICAL(4)                    :: LeapOccurring                  ! Leap Second is being applied at Time
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(.not.Initialized) THEN
  TimeWindows = Time + dTime1601
  RETURN
END IF

! Adjust for Epoch delta between Windows Time and Modified Julian Date
CALL CalculateLeapSecondsBeforeTime(Time, dTimeLeapBefore, LeapOccurring)
TimeWindows = Time + dTime1601 - dTimeLeapBefore
RETURN
END FUNCTION TimeWindowsFromTime

FUNCTION TimeFromTiesolDateAndTime(DateAndTime) RESULT(TimeMJD)
!+
! Description: Converts a Tiesol time to a Modified Julian Date for use in the Ephemeris Module
!
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
! 03-MAR-2025     | MA    | SCR 150: Added to facilitate integration with Tietronix's Tiesol software
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(TimeDateTiesol)          :: DateAndTime                    ! Tiesol Time/Date Structure
INTEGER(MJD)                  :: TimeMJD                        ! Modified Julian Date                   (10^-7 seconds)

!                                Local
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: Year                           ! Tiesol Date/Time value for year                (years)
INTEGER(4)                    :: Month                          ! Tiesol Date/Time value for month              (months)
INTEGER(8)                    :: Day                            ! Tiesol Date/Time value for day of the month     (days)
INTEGER(8)                    :: Hour                           ! Tiesol Date/Time value for hour                (hours)
INTEGER(8)                    :: Minute                         ! Tiesol Date/Time value for minute           (minutes)
INTEGER(8)                    :: Second                         ! Integer part of seconds                     (seconds)
INTEGER(8)                    :: dTime                          ! Fraction part of seconds              (10^-7 seconds)
LOGICAL(4), PARAMETER         :: debugLocal = .false.           ! Local diagnostic flag for testing
CHARACTER(24)                 :: TimeStamp                      ! Timestamp for testing
CHARACTER(4)                  :: Zone                           ! Timezone for testing
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  IF(.not.Initialized) THEN
     TimeMJD = 0
     RETURN
  END IF
  Year = DateAndTime%Year
  IF(Year .ne. YearFromTiesol) THEN
    YearFromTiesol = Year
    IF(LeapYear(Year)) THEN
      dTimeTiesol_month = dTimeLeapYear_month
    ELSE
      dTimeTiesol_month = dTimeNormalYear_month
    END IF
    CALL AdjustMonthTimesForLeapSeconds(Year, dTimeTiesol_month)
  END IF
  Month   = DateAndTime%Month
  Day     = DateAndTime%Day
  Hour    = DateAndTime%Hour
  Minute  = DateAndTime%Minute
  Second  = INT( DateAndTime%Seconds)
  dTime   = INT((DateAndTime%Seconds - DBLE(Second))*DBLE(TimePerSecond))
  TimeMJD = TimeStart_Year(Year)       &
          + dTimeTiesol_Month(Month-1) &
          + TimePerDay    *(Day - 1)   &
          + TimePerHour   * Hour       &
          + TimePerMinute * Minute     &
          + TimePerSecond * Second     &
          + dTime

  IF(debugLocal) THEN
    CALL ConvertTimeToLocalTimeStamp(TimeMJD, TimeStamp, Zone)
    WRITE(*,FMT='(A)')"TimeFromTiesolDateAndTime: TimeLocal: "//TimeStamp//" "//Zone
  END IF
  RETURN
END FUNCTION TimeFromTiesolDateAndTime

FUNCTION TimeDateFromTimeHCU(TimeHCU, YearHCU) RESULT(DateAndTime)
!+
! Description: Converts a Delta HCU time to UTC time stored in a TimeDate Type.
!
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: TimeHCU                        ! HCU Time tag
INTEGER(4),INTENT(IN)         :: YearHCU                        ! Year
TYPE(TimeDate)                :: DateAndTime

!                                Local
!-------------------------------------
INTEGER(4)                    :: Ival_w                         ! Word for tranferring
INTEGER(2)                    :: Ival_h(2)                      ! Halfword equivalence to Ival_w
EQUIVALENCE                     (Ival_h,Ival_w)
INTEGER(4)                    :: Jval_w                         ! Word for tranferring
INTEGER(2)                    :: Jval_h(2)                      ! Halfword equivalence to Ival_w
EQUIVALENCE                     (Jval_h,Jval_w)
INTEGER(MJD)                  :: TimeMJD                        ! Modified Julian Date (10^-7 sec)
LOGICAL(4)                    :: Error
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
IF(.not.Initialized) THEN
  RETURN
END IF

Ival_w                 = TimeHCU
Jval_w                 = 0
Jval_h(1)              = Ival_h(1)
DateAndTime%Hour       = IAND(Jval_w,z'0000001F')
DateAndTime%Day        = IAND(Jval_w,z'000003E0')/2**5
DateAndTime%Month      = IAND(Jval_w,z'0000FC00')/2**10
DateAndTime%Year       = YearHCU
Jval_h(1)              = Ival_h(2)
DateAndTime%Second     = IAND(Jval_w,z'0000007F')
DateAndTime%Minute     = IAND(Jval_w,z'00007F00')/2**8
DateAndTime%dHourZone  = UTC

CALL GetDayOfYear(DateAndTime%Day,DateAndTime%Month,DateAndTime%Year,DateAndTime%DayOfYear,Error)
TimeMJD = TimeStart_Year(YearHCU) + (DateAndTime%DayOfYear-1)*TimePerDay + TimePerDay/2
CALL GetDayOfWeek(TimeMJD,DateAndTime%DayOfWeek)
CALL GetDayOfYear(DateAndTime%Year,DateAndTime%Month,DateAndTime%Day,DateAndTime%DayOfYear,Error)
RETURN
END FUNCTION TimeDateFromTimeHCU

FUNCTION TimeFromTimeDateStringForRules(TimeDateString,Error) RESULT(Time)
!+
!
! Description: This function calculates the Modified Julian Date from a Time-Date String used for processing the rules
!              for Daylight Savings Time transitions.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: timeDateString                 !Time and date string
LOGICAL(4),   INTENT(OUT)     :: Error                          !Invalid date and time
INTEGER(MJD)                  :: Time                           !Modified Julian Date (10^-7 sec)
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTime                    !Date and Time structure
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Error = .true.
  Time  = 0
  IF(.not.Initialized) THEN
    RETURN
  END IF

  CALL ExtractTimesForRules(TimeDateString, DateAndTime%Year        &
                                          , DateAndTime%Month       &
                                          , DateAndTime%Day         &
                                          , DateAndTime%Hour        &
                                          , DateAndTime%Minute      &
                                          , DateAndTime%Second      &
                                          , DateAndTime%Msec        &
                                          , DateAndTime%dHourZone   &
                                          , Error)

  IF(Error) THEN
    Time = 0
  ELSE
    DateAndTime%uSec = 0
    DateAndTime%nSec = 0
    CALL ConvertDateAndTimeToTimezone(UTC,DateAndTime,DateAndTime)
    CALL CalculateTimeFromDate(Time, DateAndTime)
  END IF

  RETURN
END FUNCTION TimeFromTimeDateStringForRules
!
FUNCTION TimeFromTimeDateString(TimeDateString,Error) RESULT(Time)
!+
!
! Description: This function calculates the Modified Julian Date from a Time-Date String
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: timeDateString                 !Time and date string
LOGICAL(4),   INTENT(OUT)     :: Error                          !Invalid date and time
INTEGER(MJD)                  :: Time                           !Modified Julian Date (10^-7 sec)
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTime                    !Date and Time structure
LOGICAL(4)   ,PARAMETER       :: DebugLocal = .false.           !Diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Time  = 0
  Error = .true.
  IF(.not.Initialized) THEN
    RETURN
  END IF

  CALL ExtractTimes(TimeDateString, DateAndTime%Year        &
                                  , DateAndTime%Month       &
                                  , DateAndTime%Day         &
                                  , DateAndTime%Hour        &
                                  , DateAndTime%Minute      &
                                  , DateAndTime%Second      &
                                  , DateAndTime%Msec        &
                                  , DateAndTime%dHourZone   &
                                  , Error)

  IF(.not.Error) THEN
    DateAndTime%uSec = 0
    DateAndTime%nSec = 0
    IF(DebugLocal) WRITE(*,FMT='("ExtractTimes: Y,M,D,h,m,s,dh = ",6(I4,1X),F6.2)')DateAndTime%Year        &
                                                                                 , DateAndTime%Month       &
                                                                                 , DateAndTime%Day         &
                                                                                 , DateAndTime%Hour        &
                                                                                 , DateAndTime%Minute      &
                                                                                 , DateAndTime%Second      &
                                                                                 , DateAndTime%dHourZone


    CALL ConvertDateAndTimeToTimezone(UTC,DateAndTime,DateAndTime)
    IF(DebugLocal) WRITE(*,FMT='("ConvertToUTC: Y,M,D,h,m,s,dh = ",6(I4,1X),F6.2)')DateAndTime%Year        &
                                                                                 , DateAndTime%Month       &
                                                                                 , DateAndTime%Day         &
                                                                                 , DateAndTime%Hour        &
                                                                                 , DateAndTime%Minute      &
                                                                                 , DateAndTime%Second      &
                                                                                 , DateAndTime%dHourZone

    CALL CalculateTimeFromDate(Time, DateAndTime)

  END IF

  RETURN
END FUNCTION TimeFromTimeDateString
!
FUNCTION TimeDateStringLocalFromTime(TimeInput,Error) RESULT(TimeDateStringLocal)
!+
!
! Description: This function calculates the Time-Date string of Local Time for a Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4)  , INTENT(OUT)     :: Error                          !Invalid date and time
INTEGER(MJD), INTENT(IN)      :: TimeInput                      !Modified Julian Date (10^-7 sec)
CHARACTER(25)                 :: TimeDateStringLocal            !Time and date string in local time zone
!
!                                Local
!-------------------------------------
CHARACTER(24)                 :: TimeStampLocal                 !Time Stamp in local format
CHARACTER(4)                  :: ZoneLocal                      !Time Zone
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
IF(TimeInput < 0) THEN
  Error = .true.
  TimeDateStringLocal = "17-NOV-1858 00:00:00 UTC "
ELSE
  Error = .false.
  CALL CONVERT_TIME_TO_LOCAL_TIMESTAMP(TimeInput, TimeStampLocal, ZoneLocal)
  TimeDateStringLocal = TimeStampLocal(1:20)//" "//TRIM(ADJUSTL(ZoneLocal))
END IF
RETURN
END FUNCTION TimeDateStringLocalFromTime

FUNCTION MonthLookup(MonthChr, Error) RESULT(Month)
!
! Description: This routine returns the Month number (1 - 12) from the Month name
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: MonthChr                       ! Month Name                        (string)
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Error found in finding the month
INTEGER(4)                    :: Month                          ! Month number returned             [1 ... 12]
!
!                                Local Variables
! ----------------------------+-------------------
CHARACTER(256)                :: MonthUpperCase                 ! First 3 character of month name raused to uppercase
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!

  Error = .false.
  MonthUpperCase = (UpperCaseAscii(MonthChr))
  SELECT CASE(MonthUpperCase(1:3))
  CASE("JAN")  ; Month = January
  CASE("FEB")  ; Month = February
  CASE("MAR")  ; Month = March
  CASE("APR")  ; Month = April
  CASE("MAY")  ; Month = May
  CASE("JUN")  ; Month = June
  CASE("JUL")  ; Month = July
  CASE("AUG")  ; Month = August
  CASE("SEP")  ; Month = September
  CASE("OCT")  ; Month = October
  CASE("NOV")  ; Month = November
  CASE("DEC")  ; Month = December
  CASE DEFAULT ; Month = 0; Error = .true.
  END SELECT
  RETURN
END FUNCTION MonthLookup

FUNCTION RuleLookup(RuleChr, Error) RESULT(Rule)
!
! Description: This routine returns the Rule index from the Rule name
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
!
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: RuleChr                       ! Rule Name                        (string)
LOGICAL(4)  , INTENT(OUT)     :: Error                         ! Error found in finding the month
INTEGER(4)                    :: Rule                          ! Rule number returned             [1 ... 12]
!
!                                Local Variables
! ----------------------------+-------------------
INTEGER(4)                    :: RuleTz                        ! Local index through Time Zone Rules
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Rule = 0
  IF(TRIM(RuleChr) == "-") THEN
    Error = .false.
    RETURN
  END IF

  DO RuleTz = 1, LV$RuleTz
    Error = .not.(TRIM(RuleChr) == TRIM(Tz_Rule(RuleTz)%Name))
    IF(.not.Error) THEN
      Rule = RuleTz
      EXIT
    END IF
  END DO
  RETURN
END FUNCTION RuleLookup

FUNCTION LocaleLookup(LocaleChr, Error) RESULT(Locale)
!
! Description: This routine returns the Locale index from the Locale name
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
!
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: LocaleChr                      ! Locale Name                        (string)
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Error found in finding the month
INTEGER(4)                    :: Locale                         ! Locale number returned             [1 ... 12]
!
!                                Local Variables
! ----------------------------+-------------------
INTEGER(4)                    :: LocaleTz                       ! Local index through Time Zone Locales
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Locale = 0
  Error  = .true.
  DO LocaleTz = 1, LV$LocaleTz
    Error = .not.(TRIM(LocaleChr) == TRIM(Tz_Locale(LocaleTz)%Name))
    IF(.not.Error) THEN
      Locale = LocaleTz
      EXIT
    END IF
  END DO
  RETURN
END FUNCTION LocaleLookup
!
FUNCTION DayFromSolarYearStartForTime(Time) RESULT(Day)
!+
!
! Description: This function calculates the Day Delta from December 21.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN)      :: Time                           ! Modified Julian Date (10^-7 seconds)
INTEGER(4)                    :: Day                            ! Day from Solar Year Start, Dec 21.
!
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTime                    ! Time Date Structure
LOGICAL(4)                    :: Error                          ! Error retrieving time
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL GetLocalDateOfTime(DateAndTime, Time, Error)
Day = DayFromSolarYearStartForDate(DateAndTime)
RETURN
END FUNCTION DayFromSolarYearStartForTime

FUNCTION DayFromSolarYearStartForDate(DateAndTime) RESULT(Day)
!+
!
! Description: This function calculates the Day Delta from December 21 from a TimeDate structure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(TimeDate), INTENT(IN)    :: DateAndTime                    ! Time Date Structure
INTEGER(4)                    :: Day                            ! Day from Solar Year Start, Dec 21.
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
Day = DayInSolarYear(DateAndTime)
IF(Day > 183) Day = 367 - Day
RETURN
END FUNCTION DayFromSolarYearStartForDate

FUNCTION DayInSolarYearForTime(Time) RESULT(Day)
!+
!
! Description: This function calculates the Day Delta from December 21 from a Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN)      :: Time                           ! Modified Julian Date (10^-7 seconds)
INTEGER(4)                    :: Day                            ! Day of Solar Year [Dec21 - Dec20]
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTime                    ! Time Date Structure
LOGICAL(4)                    :: Error                          ! Error retrieving time
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL GetLocalDateOfTime(DateAndTime, Time, Error)
Day = DateAndTime%DayOfYear + 11
IF(LeapYear(DateAndTime%Year)) THEN
  IF(Day > 366) Day = Day - 366
ELSE
  IF(Day > 365) Day = Day - 365
END IF
RETURN
END FUNCTION DayInSolarYearForTime
!

FUNCTION DayInSolarYear(DateAndTime) RESULT(Day)
!+
!
! Description: This function calculates the Day Delta from December 21 from a TimeDate structure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(TimeDate), INTENT(IN)    :: DateAndTime                    ! Time Date Structure
INTEGER(4)                    :: Day                            ! Day of Solar Year [Dec21 - Dec20]
!
!                                Local
!-------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
Day = DateAndTime%DayOfYear + 11
IF(LeapYear(DateAndTime%Year)) THEN
  IF(Day > 366) Day = Day - 366
ELSE
  IF(Day > 365) Day = Day - 365
END IF
RETURN
END FUNCTION DayInSolarYear
!
SUBROUTINE ApproximateDailySolarData(Time, Location, HoursOfSunLight, TimeSolarNoon, TimeSunrise, TimeSunset)
!+
!
! Description: This function calculates approximate sunrise, solar noon and sunset times for latitude, longitude
!              and elevation on the day corresponding to the input Time.  It follows the Sunrise Equation in Wikipedia
!
!              https://Wikipedia.org/wiki/Sunrise_equation
!              https://en.wikipedia.org/wiki/Sunrise_equation#Complete_calculation_on_Earth
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
!
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD)   , INTENT(IN)   :: Time                           ! Modified Julian Data               (10^-7 seconds)
TYPE(GeoData)  , INTENT(IN)   :: Location                       ! WGS84 Latitude, Longitude and Eleation
REAL(8)        , INTENT(OUT)  :: HoursOfSunLight                ! Approximate sunlight time                  (hours)
INTEGER(MJD)   , INTENT(OUT)  :: TimeSolarNoon                  ! Modified Julian Data for SolarNoon (10^-7 seconds)
INTEGER(MJD)   , INTENT(OUT)  :: TimeSunrise                    ! Modified Julian Data for Sunrise   (10^-7 seconds)
INTEGER(MJD)   , INTENT(OUT)  :: TimeSunset                     ! Modified Julian Data for Sunset    (10^-7 seconds)
!
!                                Local
!-----------------------------+--------
REAL(8)                       :: Longitude                      ! Geodetic Longitude WGS84 (                degrees)
REAL(8)                       :: Latitude                       ! Geodetic Longitude WGS84 (                degrees)
REAL(8)                       :: Elevation                      ! Surface elevation                             (km)
REAL(8)                       :: Jdt                            ! Julain Date corresponging to Time           (days)
INTEGER(MJD)                  :: dTimeCorrection                ! Leap seconds include TT correction (100 nSec tics)
LOGICAL(4)                    :: LeapOccurring                  ! Leap second transition is occuring at Time
REAL(8)                       :: dJdtCorrection                 ! Leap seconds include TT correction          (days)
REAL(8)                       :: Jday2000                       ! Julian Days since January 1, 2000           (days)
REAL(8)                       :: Jmean                          ! Approximation of mean solar time            (days)
REAL(8)                       :: M                              ! Solar mean anomaly                       (degrees)
REAL(8)                       :: C                              ! Equation of the center value             (degrees)
REAL(8)                       :: Lambda                         ! Ecliptic longitude                       (degrees)
REAL(8)                       :: Jtransit                       ! Julian Date of Solar Transit                (days)
REAL(8)                       :: delta                          ! Declination of the sun                   (degrees)
REAL(8)                       :: dElev                          ! Refration correction for hour angle      (degrees)
REAL(8)                       :: omega                          ! Hour angle                               (degrees)
REAL(8)                       :: Jrise                          ! Julian Date of Sunrise                      (days)
REAL(8)                       :: Jset                           ! Julian Date of Sunset                       (days)

!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !Ensure the Time/Date Calculation subsystem is initialized
  IF(.not.Initialized) THEN
    RETURN
  END IF
  !
  ! Unpack the Location
  Longitude = Location%Longitude
  Latitude  = Location%Latitude
  Elevation = Location%Elevation

  ! Compute Current Julian Date / Cycle including leap seconds
  Jdt = JulianDateFromTime(Time)

  ! Leap seconds and Terrestrail Time (TT) correction
  CALL CalculateLeapSecondsBeforeTime(Time, dTimeCorrection, LeapOccurring)
  dJdtCorrection = DBLE(dTimeCorrection)/DBLE(TimePerDay)

  ! Current Julian Day
  Jday2000 = DINT(Jdt - Jdt2000  + dJdtCorrection + 0.5d0)  ! Force roundup to the next integer by adding 0.5

  ! Approximation of mean solar day
  Jmean = Jday2000 - Longitude/360d0

  ! Solar mean anomaly
  M = MOD(357.5291d0 + 0.98560028d0 * Jmean, 360d0)

  ! Equation of the Center
  C = 1.9148d0 * SIND(M) + 0.02d0 * SIND(2d0*M) + 0.0003d0 * SIND(3d0*M)

  ! Ecliptic Longitude
  lambda = MOD( M + C + 180d0 + 102.9372d0, 360d0)

  ! Solar Transit
  Jtransit = Jdt2000 + Jmean + 0.0053d0 * SIND(M) - 0.0069d0 * SIND(2d0*lambda)

  ! Declination of the Sun
  delta = ASIND(SIND(lambda) * SIND(23.4397d0))

  ! Hour Angle
  dElev = 2.076d0 * SQRT(Elevation/1d3) / 60d0
  omega = ACOSD((SIND(-0.833d0 - dElev) - DSIND(Latitude) * DSIND(delta))/(COSD(Latitude) * COSD(delta)))

  ! Julian Date for Sunrise and sunset
  Jrise = Jtransit - omega/360d0
  Jset  = Jtransit + omega/360d0

  ! Convert to Modified Julian Date for Solar Noon, Sunrise and Sunset
  TimeSolarNoon = TimeFromJulianDate(Jtransit)
  TimeSunset    = TimeFromJulianDate(Jset)
  TimeSunrise   = TimeFromJulianDate(Jrise)

  ! Hours of sunlight
  HoursOfSunlight = DBLE(TimeSunset - TimeSunrise)/DBLE(TimePerHour)

  RETURN
END SUBROUTINE ApproximateDailySolarData
!
!===================================================================================================
!Routines for retrieveing internal time (Modified Julian Date) and to perform conversions between
!internal time and timestamps (ascii)
!
!Internal times in these these routine are INTEGER(8) and count the number of 100 nanosecond tics
!since the Modified Julian Epoch of Midnight, Wednesday, November 17, 1858.
!
!Timestamps in these routines are CHARACTER(*) strings, 1 to 24 characters long.  The format of
!time in the routine is as follows:
!
! DD-MON-YEAR HH:MM:SS.FFF
!
! As an example
!
! 24-JUN-2009 12:00:00.000
!
!
!MODIFICATION HISTORY:
!----------------+-------+--------------------------------------------------------------------------
!----------------+-------+--------------------------------------------------------------------------
!
!===================================================================================================
SUBROUTINE LIB$WAIT_R8(seconds)
!+
!
! Description: This function paused for a specified number of seconds.  It processes REAL(8) inputs
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
USE Portability
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)           :: seconds                        !time to wait (seconds)
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: milliseconds                   !time to wait local (milliseconds)
  Milliseconds = INT(seconds*1000.0d0)
  CALL SleepMsec(Milliseconds)
  RETURN
END SUBROUTINE LIB$WAIT_R8
SUBROUTINE LIB$WAIT_R4(seconds)
!+
!
! Description: This function paused for a specified number of seconds.  It processes REAL(4) inputs
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(4), INTENT(IN)           :: seconds                        !time to wait (seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL LIB$WAIT_R8(DBLE(seconds))
RETURN
END SUBROUTINE LIB$WAIT_R4
!
!
SUBROUTINE LIB$WAIT_I2(seconds)
!+
!
! Description: This function paused for a specified number of seconds.  It processes INTEGER(2) inputs.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)        :: seconds                        !time to wait (seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL LIB$WAIT_R8(DBLE(seconds))
RETURN
END SUBROUTINE LIB$WAIT_I2
!
SUBROUTINE LIB$WAIT_I4(seconds)
!+
!
! Description: This function paused for a specified number of seconds.  It processes INTEGER(4) inputs.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)        :: seconds                        !time to wait (seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL LIB$WAIT_R8(DBLE(seconds))
RETURN
END SUBROUTINE LIB$WAIT_I4
!
SUBROUTINE LIB$WAIT_I8(seconds)
!+
!
! Description: This function paused for a specified number of seconds.  It processes INTEGER(2) inputs.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(8)  , INTENT(IN)      :: seconds                        !time to wait (seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL LIB$WAIT_R8(DBLE(seconds))
RETURN
END SUBROUTINE LIB$WAIT_I8
!
!
SUBROUTINE LIB$DATE_TIME(TimeStamp)
!+
!
! Description: This subroutine returns the current time as Time Stamp in UTC format.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: TimeStamp                      !Current system time stamp
!
!                                Local
!-----------------------------+-------
CHARACTER(24)                 :: TimeStampLocal                 !Local copy of time stamp
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL get_time_stamp_now(timeStampLocal)
timeStamp = timeStampLocal
RETURN
END SUBROUTINE LIB$DATE_TIME

SUBROUTINE LIB$CONVERT_DATE_STRING_I8(TimeStamp, Time)
!+
!
! Description: This subroutine converts a Time Stamp in UTC format to a Modified Julian Date as an INTEGER(8).
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN )     :: TimeStamp                      !Current system time stamp
INTEGER(MJD), INTENT(OUT)     :: Time                           !Modified Julian Date Now(10^-7 sec)
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: status                         !return code from sys$bintim
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
status = sys$bintim(timeStamp, time)
RETURN
END SUBROUTINE LIB$CONVERT_DATE_STRING_I8
!
SUBROUTINE LIB$CONVERT_DATE_STRING_I4(TimeStamp, Time_w)
!+
!
! Description: This subroutine converts a Time Stamp in UTC format to a Modified Julian Date as an INTEGER(4) array.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN )     :: TimeStamp                      !Current system time stamp
INTEGER(4)  , INTENT(OUT)     :: Time_w(2)                      !Modified Julian Date Now(10^-7 sec)
!
!                                Local
!-----------------------------+-------
INTEGER(MJD)                  :: Time
INTEGER(4)                    :: TimeLocal_w(2)
EQUIVALENCE                     (Time,TimeLocal_w)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL LIB$CONVERT_DATE_STRING_I8(timeStamp, Time)
Time_w = TimeLocal_w
RETURN
END SUBROUTINE LIB$CONVERT_DATE_STRING_I4

FUNCTION SYS$GETTIM_I8(time) RESULT(status)
!+
!
! Description: This function returns the system time as a Modified Julian Date stored in an INTEGER(8) word.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(OUT)     :: Time                           !Modified Julian Date Now(10^-7 sec)
INTEGER(4)                    :: status                         !return code
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Get the Binary Time (Number of 100 nanosecond tics since the Modified Julian Day 0: 17-NOV-1858 00:00:00)
call get_time_now(Time)
status = TIME_S_NORMAL

RETURN
END FUNCTION SYS$GETTIM_I8
!
FUNCTION SYS$GETTIM_I4(time_w) RESULT(status)
!+
!
! Description: This function returns the system time as a Modified Julian Date stored in an array of 2 INTEGER(4) words.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(OUT)     :: Time_w(2)                      !Modified Julian Date Now(10^-7 sec)
INTEGER(4)                    :: status                         !return code

!                                Local
!-----------------------------+-------
INTEGER(MJD)                  :: Time
INTEGER(4)                    :: TimeLocal_w(2)
EQUIVALENCE                     (Time,TimeLocal_w)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
status = sys$gettim_i8(Time)
Time_w = TimeLocal_w
RETURN
END FUNCTION SYS$GETTIM_I4
!
FUNCTION SYS$ASCTIM_I8( size, timeStamp, time) RESULT(status)
!+
!
! Description: This function formats a timestamp from a Modified Julian Date stored in an INTEGER(8) word.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN )     :: size                           ! Number of characters desired [1,24]
INTEGER(MJD), INTENT(IN )     :: Time                           ! Modified Julian Date Now(10^-7 sec)
CHARACTER(*), INTENT(OUT)     :: TimeStamp                      ! Timestamp returned
INTEGER(4)                    :: status                         ! Return status
!
!                                Local
!-----------------------------+-------
TYPE(TimeDate)                :: DateAndTime                    ! Date and Time structure
TYPE(TimeDelta)               :: DeltaTime                      ! Delta time structure
CHARACTER(24)                 :: TimeStampAbsolute              ! Absolute time stamp
CHARACTER(17)                 :: TimeStampDelta                 ! Delta time stamp
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Ensure the Time/Date Calculation subsystem is initialized
IF(.not.Initialized) THEN
  status = TIME_E_NOTINITIALIZED
  TimeStamp = "17-NOV-1858 00:00:00.000"
  RETURN
END IF

IF(time >= 0 .and. time < 10000*TimePerDay .and. size < 18) THEN
  TimeStampDelta    = '0000 00:00:00.000'

  CALL CalculateDeltaTime(time, DeltaTime)

  IF(DeltaTime%days > 9999) THEN
    timeStampDelta = '9999 23:59:59.999'
    status = TIME_E_INVALID

  ELSE IF(DeltaTime%days < 0) THEN
    status = TIME_E_INVALID

  ELSE
    timeStampDelta(1 : 4) = ascii_days  (DeltaTime%days   )
    timeStampDelta(6 : 7) = ascii_hour  (DeltaTime%hours  )
    timeStampDelta(9 :10) = ascii_minute(DeltaTime%minutes)
    timeStampDelta(12:13) = ascii_second(DeltaTime%seconds)
    timeStampDelta(15:17) = ascii_msec  (DeltaTime%msec   )
    status = TIME_S_NORMAL
  END IF
  timeStamp = timeStampDelta(1:MIN(size,17))

ELSE
  TimeStampAbsolute = '01-JAN-1977 00:00:00.000'

  CALL CalculateDateFromTime(Time, DateAndTime)
  IF(DateAndTime%year > yearMax) THEN
    TimeStampAbsolute = '31-DEC-9999 23:59:59.999'
    status = TIME_E_INVALID

  ELSE IF(DateAndTime%year < yearMin) THEN
    TimeStampAbsolute = '17-NOV-1958 00:00:00.000'
    status = TIME_E_INVALID

  ELSE
    timeStampAbsolute(1 : 2)  = ascii_day   (DateAndTime%day   )
    timeStampAbsolute(4 : 6)  = ascii_month (DateAndTime%month )
    timeStampAbsolute(8 :11)  = Ascii_year  (DateAndTime%year  )
    timeStampAbsolute(13:14)  = ascii_hour  (DateAndTime%hour  )
    timeStampAbsolute(16:17)  = ascii_minute(DateAndTime%minute)
    timeStampAbsolute(19:20)  = ascii_second(DateAndTime%second)
    timeStampAbsolute(22:24)  = ascii_msec  (DateAndTime%msec  )
    status = TIME_S_NORMAL
  END IF
  timeStamp = timeStampAbsolute(1:MIN(size,24))

END IF

RETURN
END FUNCTION SYS$ASCTIM_I8
!
FUNCTION SYS$ASCTIM_I4( size, timeStamp, time_w) RESULT(status)
!+
!
! Description: This function formats a timestamp from a Modified Julian Date stored an array of 2 INTEGER(4) words.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN )     :: size                           !Number of characters desired [1,24]
INTEGER(4)  , INTENT(IN )     :: Time_w(2)                      !Modified Julian Date Now(10^-7 sec)
CHARACTER(*), INTENT(OUT)     :: TimeStamp                      !Timestamp returned
INTEGER(4)                    :: status                         !Return status
!
!                                Local
!-----------------------------+-------
INTEGER(MJD)                  :: Time
INTEGER(4)                    :: TimeLocal_w(2)
EQUIVALENCE                     (Time,TimeLocal_w)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
TimeLocal_w = Time_w
status = sys$asctim_i8(size, TimeStamp, Time)
RETURN
END FUNCTION SYS$ASCTIM_I4

FUNCTION SYS$BINTIM_I8(timeStamp, time) RESULT(status)
!+
!
! Description: This function processes a timestamp and returns the corresponding Modified Julian Date stored an
!              INTEGER(8) word.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*) , INTENT(IN)     :: timeStamp                      !Timestamp input
INTEGER(MJD) , INTENT(OUT)    :: time                           !Modified Julian Date (10^-7 sec)
INTEGER(4)                    :: status                         !Return status
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTime                    !Date and Time structure
TYPE(TimeDelta)               :: DeltaTime                      !Delta time structure
CHARACTER(24)                 :: timeStampCopy                  !Local copy of time stamp
INTEGER(4)                    :: copy                           !Index through timeStampCopy
LOGICAL(4)                    :: Error                          !Invalid date and time
LOGICAL(4)                    :: readSuccess                    !Successful read of date components
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Ensure the Time/Date Calculation subsystem is initialized
IF(.not.Initialized) THEN
  status = TIME_E_NOTINITIALIZED
  Time   = 0
  RETURN
END IF

!Copy to local
timeStampCopy = timeStamp


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!Format for converted time        !!
!!  Absolute:                      !!
!!  DD-MMM-YYYY HR:MN:SC.FFF       !!
!!                                 !!
!!  Relative                       !!
!!  DDDD HH:MN:SC.FFF              !!
!!                                 !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Test for Absolute or Relative Time
IF(timeStampCopy(5:5).ne.' ') THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!  Absolute Time:                 !!
  !!  DD-MMM-YYYY HR:MN:SC.FFF       !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  status = TIME_E_INVALID
  time   = 0
  DO WHILE(status==TIME_E_INVALID)

    !User must supply at least to the second resolution
    IF(LEN(timeStamp) < 20) EXIT

    !Zero out the fractional part of the seconds if it is blank.
    DO copy = 22,24
      IF(timeStampCopy(copy:copy).eq.' ') timeStampCopy(copy:copy) = '0'
    END DO

    !Parse the components of the time
    readSuccess = .false.
    IF( DateNumberASCII(timeStampCopy( 1: 2), DateAndTime%day   ) .and. &
        DateNumberASCII(timeStampCopy( 4: 6), DateAndTime%month)  .and. &
        DateNumberASCII(timeStampCopy( 8:11), DateAndTime%year  ) .and. &
        DateNumberASCII(timeStampCopy(13:14), DateAndTime%hour  ) .and. &
        DateNumberASCII(timeStampCopy(16:17), DateAndTime%minute) .and. &
        DateNumberASCII(timeStampCopy(19:20), DateAndTime%second) .and. &
        DateNumberASCII(timeStampCopy(22:24), DateAndTime%msec  )       &
      ) THEN
      readSuccess = .true.
    END IF

    !Return if parse error
    IF(.not.readSuccess) EXIT

    !Convert to Binary Time (Number of 100 nanosecond tics since the Modified Julian Day 0: 17-NOV-1858 00:00:00)
    DateAndTime%dHourZone = UTC
    DateAndTime%usec      = 0
    DateAndTime%nsec      = 0
    CALL GetTimeOfDate(DateAndTime, time, error)

    !Exit if invalid DateAndTime
    IF(Error) EXIT

    !Good time
    status  = TIME_S_NORMAL

  END DO

ELSE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!  Relative time                  !!
  !!  DDDD HH:MN:SC.FFF              !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  status = TIME_E_INVALID
  time   = 0
  DO WHILE(status==TIME_E_INVALID)

    !User must supply at least to the second resolution
    IF(LEN(timeStamp) < 13) EXIT

    !Zero out the fractional part of the seconds if it is blank.
    DO copy = 15,17
       IF(timeStampCopy(copy:copy).eq.' ') timeStampCopy(copy:copy) = '0'
    END DO

    !Parse the components of the time
    readSuccess = .false.
    IF( DateNumberASCII(timeStampCopy( 1: 4), DeltaTime%days   ) .and. &
        DateNumberASCII(timeStampCopy( 6: 7), DeltaTime%hours  ) .and. &
        DateNumberASCII(timeStampCopy( 9:10), DeltaTime%minutes) .and. &
        DateNumberASCII(timeStampCopy(12:13), DeltaTime%seconds) .and. &
        DateNumberASCII(timeStampCopy(15:17), DeltaTime%msec   )       &
      ) THEN
      readSuccess = .true.
    END IF

    !Return if parse error
    IF(.not.readSuccess) EXIT

    time = DeltaTime%days    * TimePerDay    &
         + DeltaTime%hours   * TimePerHour   &
         + DeltaTime%minutes * TimePerMinute &
         + DeltaTime%seconds * TimePerSecond &
         + DeltaTime%msec    * TimePerMsec

    !Good delta time
    status  = TIME_S_NORMAL
  END DO
END IF

RETURN
END FUNCTION SYS$BINTIM_I8
!
FUNCTION SYS$BINTIM_I4(timeStamp, time_w) RESULT(status)
!+
!
! Description: This function processes a timestamp and returns the corresponding Modified Julian Date stored an array of
!              2 INTEGER(4) words.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: timeStamp                      !Timestamp input
INTEGER(4)  , INTENT(OUT)     :: time_w(2)                      !Modified Julian Date Now(10^-7 sec)
INTEGER(4)                    :: status                         !Return status
!
!                                Local
!-------------------------------------
INTEGER(MJD)                  :: Time
INTEGER(4)                    :: TimeLocal_w(2)
EQUIVALENCE                     (Time,TimeLocal_w)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
TimeLocal_w = Time_w
status = sys$bintim_I8(TimeStamp, Time)
Time_w = TimeLocal_w
RETURN

RETURN
END FUNCTION SYS$BINTIM_I4

SUBROUTINE GetTimeNow(TimeNow)
!+
!
! Description: This subroutine returns the current Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD), INTENT(OUT)     :: TimeNow                        ! Modified Julian Date of current time (10^-7 sec)
!
!
! -------------------------------------------------------------------------------------_---------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL Get_Time_Now_I8(TimeNow)
  RETURN
END SUBROUTINE GetTimeNow

SUBROUTINE GET_TIME_NOW_I4(TIMENOW_W)
!+
!
! Description: This subroutine returns the current Modified Julian Date as INTEGER(4) array of two words.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(OUT)       :: TimeNow_W(2)                   !Modified Julian Date Now(10^-7 sec)

!                                Local
!-------------------------------------
INTEGER(MJD)                  :: TimeLocal                      !Modified Julian Date Now(10^-7 sec)
INTEGER(4)                    :: TimeLocal_W(2)                 !Equivalence to TimeLocal
EQUIVALENCE                     (TimeLocal,TimeLocal_W)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL GET_TIME_NOW_I8(TimeLocal)
TimeNow_W = TimeLocal_W
RETURN
END SUBROUTINE GET_TIME_NOW_I4
!
SUBROUTINE GET_TIME_NOW_I8(TIMENOW)
!+
!
! Description: This subroutine returns the current Modified Julian Date as an INTEGER(8)%
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD), INTENT(OUT)     :: TimeNow                        !Modified Julian Date Now(10^-7 sec)
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTime                    !Date and Time structure
TYPE(TimeDate)                :: DateAndTimeUTC                 !Date and Time Converted to UTC
LOGICAL(4)                    :: Error                          !Invalid date and time
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Ensure the Time/Date Calculation subsystem is initialized
IF(.not.Initialized) THEN
  TimeNow = 0
  RETURN
END IF

!Initialize
TimeNow = TimeMax

!Get current time
CALL GetDateAndTime(DateAndTime)
CALL ConvertDateAndTimeToTimeZone(UTC, DateAndTime, DateAndTimeUTC)
CALL GetTimeOfDate (DateAndTimeUTC, timeNow, error)
RETURN
END SUBROUTINE GET_TIME_NOW_I8

SUBROUTINE GetTheTimeNow(TimeNow)
!+
!
! Description: This subroutine returns the current Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD), INTENT(OUT)     :: TimeNow                        ! Modified Julian Date of current time (10^-7 sec)
!
!                                Local
!-----------------------------+------
INTEGER(4)                    :: Values_rcv(8)                  ! System Date and Time (Array)
INTEGER(4)                    :: year,month,day,dMinuteZone     ! Date components from DATE_AND_TIME
INTEGER(4)                    :: Hour,Minute,Second,msec        ! Time components from DATE_AND_TIME
EQUIVALENCE                     (values_rcv(1),year         )
EQUIVALENCE                     (values_rcv(2),month        )
EQUIVALENCE                     (values_rcv(3),day          )
EQUIVALENCE                     (values_rcv(4),dMinuteZone  )
EQUIVALENCE                     (values_rcv(5),hour         )
EQUIVALENCE                     (values_rcv(6),minute       )
EQUIVALENCE                     (values_rcv(7),second       )
EQUIVALENCE                     (values_rcv(8),msec         )
TYPE(TimeDateISO)             :: DateAndTimeLocal
EQUIVALENCE                    (DateAndTimeLocal, Values_rcv)
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time elapsed in year for each month (10^-7 seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CALL DATE_AND_TIME(Values = Values_rcv)

  IF(LeapYear(year)) THEN
   dTime_month = dTimeLeapYear_month
  ELSE
   dTime_month = dTimeNormalYear_month
  END IF
  CALL AdjustMonthTimesForLeapSeconds(year,dTime_month)
  TimeNow = TimeStart_Year(Year) + dTime_month(month-1) + 1    &
                                 + TimePerDay    * (Day-1)     &
                                 + TimePerHour   * Hour        &
                                 + TimePerMinute * Minute      &
                                 - TimePerMinute * dMinuteZone &
                                 + TimePerSecond * Second      &
                                 + TimePerMsec   * msec
  RETURN
END SUBROUTINE GetTheTimeNow

SUBROUTINE GetTimeStampNow(TimeStampNow)
!+
!
! Description: This subroutine returns the current time formatted as a UTC time stamp.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: TimeStampNow                   ! Time stamp (UTC Time Zone)
!
!
! -------------------------------------------------------------------------------------_---------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL Get_Time_Stamp_Now(TimeStampNow)
  RETURN
END SUBROUTINE GetTimeStampNow

SUBROUTINE GET_TIME_STAMP_NOW(TimeStampNow)
!+
!
! Description: This subroutine returns the current time formatted as a UTC time stamp.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: TimeStampNow                   ! Time stamp (UTC Time Zone)
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTimeSystem              ! Date and Time structure from OS
TYPE(TimeDate)                :: DateAndTimeUTC                 ! Date and Time Converted to UTC
INTEGER(MJD)                  :: Time                           ! Modified Julian Date (10^-7 seconds)
LOGICAL(4)                    :: Error                          ! Invalid date and time
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Ensure the Time/Date Calculation subsystem is initialized
IF(.not.Initialized) THEN
  TimeStampNow = "17-NOV-1858 00:00:00.000"
  RETURN
END IF

!Get current time
CALL GetDateAndTime(DateAndTimeSystem)
CALL GetTimeOfDate(DateAndTimeSystem,Time,Error)
CALL CalculateDateFromTime(Time,DateAndTimeUTC)
CALL FormTimeStamp(DateAndTimeUTC, TimeStampNow, Error)
RETURN
END SUBROUTINE GET_TIME_STAMP_NOW

SUBROUTINE GetLocalTimeStampNow(TimeStampLocalNow)
!+
!
! Description: This subroutine gets the the local time stamp formatted in the Local time zone.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: TimeStampLocalNow              ! Time stamp (Local Time Zone)
!
!
! -------------------------------------------------------------------------------------_---------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL Get_Local_Time_Stamp_Now(TimeStampLocalNow)
  RETURN
END SUBROUTINE GetLocalTimeStampNow

SUBROUTINE GET_LOCAL_TIME_STAMP_NOW(TimeStampLocalNow)
!+
!
! Description: This subroutine gets the the local time stamp formatted in the Local time zone.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: TimeStampLocalNow              !Time stamp (Local Time Zone)
!
!                                Local
!-------------------------------------
TYPE(TimeDate)                :: DateAndTimeSystem              !Date and Time structure from OS
TYPE(TimeDate)                :: DateAndTimeLocal               !Date and Time structure Local Time
INTEGER(MJD)                  :: Time                           !Modified Julian Date (10^-7 seconds)
LOGICAL(4)                    :: Error                          !Invalid date and time
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Error if Time/Date Calculation subsystem are not initialized
  IF(.not.Initialized) THEN
    Error = .true.
    RETURN
  END IF
  !
  ! Get current time
  CALL GetDateAndTime(DateAndTimeSystem)
  CALL GetTimeOfDate(DateAndTimeSystem,Time,Error)
  CALL CalculateLocalDateFromTime(Time,DateAndTimeLocal)
  CALL FormTimeStamp(DateAndTimeLocal, TimeStampLocalNow, Error)
  RETURN
!
END SUBROUTINE GET_LOCAL_TIME_STAMP_NOW

SUBROUTINE GET_SYSTEM_TIME_STAMP_NOW(TimeStampSystemNow)
!+
!
! Description: This subroutine gets the the time stamp now formatted in the System time zone.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: TimeStampSystemNow             ! Time stamp (System Time Zone)
!
!                                Local
!-------------------------------------
INTEGER(MJD)                  :: TimeNow                        ! Modified Julian Date (10^-7 seconds)
CHARACTER(4)                  :: ZoneSystem                     ! Time zone abbreviation
LOGICAL(4)                    :: Error                          ! System not initialized
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Error if Time/Date Calculation subsystem are not initialized
  IF(.not.Initialized) THEN
    Error = .true.
    RETURN
  END IF
  !
  ! Get current time
  CALL GetTimeNow(TimeNow)
  CALL ConvertTimeToSystemTimeStamp(TimeNow, TimeStampSystemNow, ZoneSystem)
  RETURN
!
END SUBROUTINE GET_SYSTEM_TIME_STAMP_NOW

FUNCTION YearFromTime(Time) RESULT(Year)
!+
!
! Description: This function returns the year of a modified julian date
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 05-MAR-2025     | MA    | SCR 150: Add this to improve traversing through the Tz data
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed Variables             Description (Units)
!-----------------------------+-----------------------------+-----------------------------------------------------------
INTEGER(MJD)  , INTENT(IN)    :: Time                       ! Modified Julian Date source for DateAndTime (10^-7 seconds)
INTEGER(4)                    :: Year                       ! Year of Time                                         (year)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Year = YearEpoch + INT(Time/TimePerYear, KIND=4)
  IF(Time > TimeEnd_Year(Year)) THEN
     Year = Year + 1
  ELSE IF(Time < TimeStart_Year(Year)) THEN
     Year = Year - 1
  END IF
  RETURN
END FUNCTION YearFromTime


FUNCTION StatusTimeAndDateInitialization() RESULT(StatusInitialize)
!+
!
! Description: This function returns the status of the TimeAndDate Package Initialization:
!              TIME_S_NORMAL: Time and Date Initialized
!              TIME_E_NOTINITIALIZED - Time and Data not Initialized
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 29-APR-2025     | MA    | SCR 150: Provide public access to check TimeAndData Initialization
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed Variables                Description (Units)
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(4)                    :: StatusInitialize              ! Return status
!
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(Initialized) THEN
    StatusInitialize = TIME_S_NORMAL
  ELSE
    StatusInitialize = TIME_E_NOTINITIALIZED
  END IF
  RETURN
END FUNCTION StatusTimeAndDateInitialization

FUNCTION InitializeTimeAndDateCalculations() RESULT(StatusInitialize)
!+
!
! Description: This subroutine performs and saves the results of calculations and initializations needed for the
!              TimeDatePackage Module.  It only needs to be performed once at that start of any task that uses the
!              Module.
!
! Audit Trail
! ----------------+-------+-----------------------------------------------------------------
! ----------------+-------+-----------------------------------------------------------------
IMPLICIT NONE
!                                Passed Variables                Description (Units)
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(4)                    :: StatusInitialize              ! Return status
!
!                                Local                           Description / (units)
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(4)                    :: dayOfYear                     ! Day of the year counter
INTEGER(4)                    :: year                          ! Index through the years
INTEGER(4)                    :: month                         ! Index through month of the year
INTEGER(4)                    :: day                           ! Index through days of the month
INTEGER(4)                    :: days                          ! Index through days of a time duration (maximum 9999)
INTEGER(4)                    :: hour                          ! Index through hours of the day
INTEGER(4)                    :: minute                        ! Index through minutes of the hour
INTEGER(4)                    :: second                        ! Index through seconds of the minute
INTEGER(4)                    :: msec                          ! Index through month of the year
INTEGER(4)                    :: StatusLocale                  ! Status of setting up the Locales
INTEGER(MJD)                  :: dTime_month(0:13)             ! Time elapsed in year for each month     (10^-7 seconds)
LOGICAL(4)                    :: LeapOccurring                 ! Set true if a leap second is occurring
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Initialized = .false.
  IF(.not.LocaleSpecified) THEN
    statusInitialize = TIME_E_FAILEDINIT
    WRITE(*,FMT='(A)')"InitializeTimeAndDateCalculations.Failure: The Current Locale is not specified"
    RETURN
  END IF

  dayOfYear = 0
  DO month = January,December
    dayOfYear = dayOfYear + daysLeapYear_month(month)
    dTimeLeapYear_month(month) = dayOfYear*TIME_PER_DAY - 1
  END DO
  dTimeLeapYear_month(DecemberLastYear) = dTimeLeapYear_month(January ) - 31*TIME_PER_DAY
  dTimeLeapYear_month(JanuaryNextYear ) = dTimeLeapYear_month(December) + 31*TIME_PER_DAY
  dayOfYear = 0
  DO month = January,December
    dayOfYear = dayOfYear + daysNormalYear_month(month)
    dTimeNormalYear_month(month) = dayOfYear*TIME_PER_DAY - 1
  END DO
  dTimeNormalYear_month(DecemberLastYear) = dTimeNormalYear_month(January ) - 31*TIME_PER_DAY
  dTimeNormalYear_month(JanuaryNextYear ) = dTimeNormalYear_month(December) + 31*TIME_PER_DAY

  IF(LeapYear(year_epoch)) THEN
    DaysEpochYear_month = DaysLeapYear_month
  ELSE
    DaysEpochYear_month = DaysNormalYear_month
  END IF
  dTimeEpochYear_month(month_epoch) = (DaysEpochYear_month(month_epoch) - day_epoch + 1) * TIME_PER_DAY - 1
  month = month_epoch - 1
  DO month = month_epoch-1, January, -1
    dTimeEpochYear_month(month)= dTimeEpochYear_month(month+1) - DaysEpochYear_month(month+1) * TIME_PER_DAY
  END DO

  DO month = month_epoch+1, December
    dTimeEpochYear_month(month) = dTimeEpochYear_month(month-1) + DaysEpochYear_month(month) * TIME_PER_DAY
  END DO
  dTimeEpochYear_month(DecemberLastYear) = dTimeEpochYear_month(January ) - 31*TIME_PER_DAY
  dTimeEpochYear_month(JanuaryNextYear ) = dTimeEpochYear_month(December) + 31*TIME_PER_DAY

  dTime_month = dTimeEpochYear_month
  !CALL AdjustMonthTimesForLeapSeconds(year_epoch,dTime_month(January:December))

  timeStart_year(year_epoch) = dTime_month(January) - 31*TIME_PER_DAY + 1
  timeEnd_year  (year_epoch) = dTime_month(December)
  DO year = year_epoch+1, year_end+1
    IF(LeapYear(year)) THEN
      dTime_month = dTimeLeapYear_month
    ELSE
      dTime_month = dTimeNormalYear_month
    END IF
    !CALL AdjustMonthTimesForLeapSeconds(year,dTime_month(January:December))
    timeStart_year(year) = timeEnd_year(year-1) + 1
    timeEnd_year  (year) = timeStart_year(year) + dTime_month(December)
  END DO
  DO year = year_epoch-1, year_start-1, -1
    IF(LeapYear(year)) THEN
     dTime_month = dTimeLeapYear_month
    ELSE
     dTime_month = dTimeNormalYear_month
    END IF
    !CALL AdjustMonthTimesForLeapSeconds(year,dTime_month(January:December))
    timeEnd_year  (year) = timeStart_year(year+1) - 1
    timeStart_year(year) = timeEnd_year  (year) - dTime_month(December)
  END DO
  !
  Notice_Year = 0
  CALL GetLeapSecondNotices()

  DO year = 0, 9999
    WRITE(Ascii_year  (year  ),FMT='(I4.4)')year
  END DO
  DO days = 0, 9999
    WRITE(ascii_days  (days  ),FMT='(I4.4)')days
  END DO
  DO day = 1,31
    WRITE(ascii_day   (day   ),FMT='(I2.2)')day
  END DO
  DO hour = 0, 23
    WRITE(ascii_hour  (hour  ),FMT='(I2.2)')hour
  END DO
  DO minute = 0, 59
    WRITE(ascii_minute(minute),FMT='(I2.2)')minute
  END DO
  DO second = 0, 99
    WRITE(ascii_second(second),FMT='(I2.2)')second
  END DO
  DO msec = 0, 999
    WRITE(ascii_msec   (msec ),FMT='(I3.3)')msec
  END DO

  IF(.not.TzInfoProcessed) THEN
    CALL ProcessTzInfo()
    TzInfoProcessed = .true.
  END IF

  StatusLocale = SetupLocales()
  IF(StatusLocale .ne. TIME_S_NORMAL) THEN
    WRITE(*,FMT='(A)')"InitializeTimeAndDateCalculations.Failure: The Current/System Locale failed Setup."
    statusInitialize = TIME_E_FAILEDINIT
    RETURN
  END IF

  USRules%TimeDateString_Switch  = (/"2ndSunday-March-YYYY 02:00:00 SDT   ", "1stSunday-November-YYYY 01:00:00 SDT"/)
  USRules%DaylightSavings_Switch = (/.FALSE.,.TRUE.,.FALSE./)
  USRules%Year                   = -9999

  EURules%TimeDateString_Switch  = (/"LastSunday-March-YYYY 02:00:00 UTC  ", "LastSunday-October-YYYY 01:00:00 UTC"/)
  EURules%DaylightSavings_Switch = (/.FALSE.,.TRUE.,.FALSE./)
  EURules%Year                   = -9999

  AURules%TimeDateString_Switch  = (/"1stSunday-April-YYYY 02:00:00 UTC   ", "1stSunday-October-YYYY 01:00:00 UTC "/)
  AURules%DaylightSavings_Switch = (/.TRUE.,.FALSE.,.TRUE./)
  AURules%Year    = -9999

  Initialized = .true.

  !ToDo: SCR: 150 Convert these to parameters so we can move them up before the Initialized = true statement.
  CALL lib$convert_date_string("01-JAN-1970 00:00:00.000",TimeJanuary1970)
  CALL CalculateLeapSecondsBeforeTime(TimeJanuary1970, dTimeLeapJanuary1970,LeapOccurring)
  CALL lib$convert_date_string("01-JAN-1979 00:00:00.000",TimeJanuary1979)
  CALL CalculateLeapSecondsBeforeTime(TimeJanuary1979, dTimeLeapJanuary1979,LeapOccurring)

  StatusInitialize = TIME_S_NORMAL
  RETURN
END FUNCTION InitializeTimeAndDateCalculations

FUNCTION SetupLocales() RESUlt(StatusLocale)
!
! Description: This function sets up the Current Locale and the System Locale
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!                               Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: StatusLocale                   ! Return status
!
!                               Local
!-----------------------------+---------        -
INTEGER(4)                    :: Locale                         ! Index through the Locales
CHARACTER(32)                 :: NameLocale                     ! Name of Current/System Locale
INTEGER(4)                    :: StatusCurrent                  ! Status of checking the Current Locale
INTEGER(4)                    :: StatusSystem                   ! Status of checking the System Locale
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Initialize the status return
  StatusLocale = TIME_S_NORMAL

  !Initialize the current and system Locale
  LocaleCurrent = 0
  LocaleSystem  = 0

  ! Look up the LocaleCurrent and LocaleSystem
  DO Locale = 1, LV$LocaleTz
    NameLocale = TRIM(Tz_Locale(Locale)%Name)
    IF(TRIM(NameLocale) == TRIM(NameCurrentLocale)) LocaleCurrent = Locale
    IF(TRIM(NameLocale) == TRIM(NameSystemLocale )) LocaleSystem  = Locale
    IF(LocaleCurrent > 0 .and. LocaleSystem > 0) EXIT
  END DO

  DO
    IF(LocaleCurrent > 0) THEN
      StatusCurrent = SetupLocale(LocaleCurrent)
    ELSE
      StatusCurrent = TIME_E_SETUPLOCALE
    END IF
    IF(StatusCurrent .ne. TIME_S_NORMAL) THEN
      WRITE(*, FMT='(A)')" SetupLocales: Error seting up Locale "//TRIM(NameCurrentLocale)
      StatusLocale = TIME_E_SETUPLOCALE
    END IF

    IF(TRIM(NameSystemLocale) == TRIM(NameCurrentLocale)) EXIT

    IF(LocaleSystem > 0) THEN
      StatusSystem = SetupLocale(LocaleSystem)
    ELSE
      StatusSystem = TIME_E_SETUPLOCALE
    END IF
    IF(StatusSystem .ne. TIME_S_NORMAL) THEN
      WRITE(*, FMT='(A)')" SetupLocales: Error seting up Locale "//TRIM(NameSystemLocale)
      StatusLocale = TIME_E_SETUPLOCALE
    END IF
    EXIT
  END DO
  RETURN
END FUNCTION SetupLocales

FUNCTION SetupLocale(Locale) RESUlt(StatusLocale)
!
! Description: This function sets up the input Locale for time/date calculations
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN)      :: Locale                         ! Locale in which we are looking for timezone switches
INTEGER(4)                    :: StatusLocale                   ! Return status
!
!                                Local
!-----------------------------+---------
INTEGER(MJD)                  :: TimeNow                        ! Modified Julian Date of current time (10^-7 sec)
INTEGER(4)                    :: Zone                           ! Timezone index for getting applicable switches
INTEGER(4)                    :: SwitchEnd                      ! Number of applicable timezone rule switches
INTEGER(MJD)                  :: Time_Switch       (SwitchTz$MX)! Starting time of each timezone switch  (10^-7 seconds)
REAL(8)                       :: dHoursLocal_Switch(SwitchTz$MX)! Hours shift from UTC                           (hours)
CHARACTER(8)                  :: AbbrZone_Switch   (SwitchTz$MX)! Time Zone abbreviation (e.g. PDT, PST)        (string)
INTEGER(4)                    :: RuleInfo_Switch   (SwitchTz$MX)! Zone information index for each switch
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Get the current Time from the system and the year
  CALL GetTheTimeNow (TimeNow)
  CALL GetTimeZoneSwitchesForLocale(Locale, TimeNow, Zone, SwitchEnd, Time_Switch, dHoursLocal_Switch, AbbrZone_Switch, RuleInfo_Switch)
  IF(SwitchEnd < 2) THEN
    StatusLocale = TIME_E_TZSWITCHES
    RETURN
  END IF
  Tz_Locale(Locale)%YearNow         = YearFromTime(TimeNow)
  Tz_Locale(Locale)%ZoneNow         = Zone
  Tz_Locale(Locale)%SwitchEnd       = SwitchEnd
  Tz_Locale(Locale)%Time_Switch     = Time_Switch
  Tz_Locale(Locale)%dHours_Switch   = dHoursLocal_Switch
  Tz_Locale(Locale)%AbbrZone_Switch = AbbrZone_Switch
  Tz_Locale(Locale)%RuleInfo_Switch = RuleInfo_Switch
  StatusLocale = TIME_S_NORMAL
  RETURN
END FUNCTION SetupLocale

SUBROUTINE GetTimeOfDateTimeArrayISO(DateAndTime_ISO, Time, Error)
!+
!
! Description: Converts TimeValue array conforming to ISO 8601 to the equivalent Time (Modified Julian Date).
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(4)  , INTENT(IN)      :: DateAndTime_ISO(8)             ! Array of Date and Time Values per ISO 8601
INTEGER(MJD), INTENT(OUT)     :: Time
LOGICAL(4)  , INTENT(OUT)     :: Error
!
!                                Local
!-----------------------------+--------
TYPE(TimeDate)                :: DateAndTime

DateAndTime%Year      = DateAndTime_ISO(1)
DateAndTime%Month     = DateAndTime_ISO(2)
DateAndTime%Day       = DateAndTime_ISO(3)
DateAndTime%Hour      = DateAndTime_ISO(5)
DateAndTime%Minute    = DateAndTime_ISO(6)
DateAndTime%Second    = DateAndTime_ISO(7)
DateAndTime%Msec      = DateAndTime_ISO(8)
DateAndTime%dHourZone = FLOAT(DateAndTime_ISO(4))/60.0
CALL GetTimeOfTimeDate(DateAndTime, Time, Error)

END SUBROUTINE GetTimeOfDateTimeArrayISO

SUBROUTINE GetTimeOfDateISO(DateAndTimeISO, Time, Error)
!+
!
! Description: This subroutine converts a Civil Time ISO in a DateAndTime structure to Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
TYPE(TimeDateISO),INTENT(IN)  :: DateAndTimeISO
INTEGER(MJD)     ,INTENT(OUT) :: Time
LOGICAL(4)       ,INTENT(OUT) :: Error
!
!                                Local
!-----------------------------+--------
TYPE(TimeDate)                :: DateAndTime

  DateAndTime%Year      = DateAndTimeISO%Year
  DateAndTime%Month     = DateAndTimeISO%Month
  DateAndTime%Day       = DateAndTimeISO%Day
  DateAndTime%Hour      = DateAndTimeISO%Hour
  DateAndTime%Minute    = DateAndTimeISO%Minute
  DateAndTime%Second    = DateAndTimeISO%Second
  DateAndTime%Msec      = DateAndTimeISO%Msec
  DateAndTime%dHourZone = FLOAT(DateAndTimeISO%dMinuteZone)/60.0
  CALL GetTimeOfTimeDate(DateAndTime, Time, Error)
  RETURN
END SUBROUTINE GetTimeOfDateISO

SUBROUTINE GetTimeOfTimeDate(DateAndTime, Time, Error)
!+
!
! Description: This subroutine converts Civil Time UTC in a TimeDate structure to Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
TYPE(TimeDate), INTENT(IN)    :: DateAndTime                    ! TimeDate struture to calculate Time
INTEGER(MJD)  , INTENT(OUT)   :: Time                           ! Modified Julian Date calculated       (10^-7 seconds)
LOGICAL(4)    , INTENT(OUT)   :: Error                          ! Error in the calculation
!
!                                Local
!-----------------------------+--------
TYPE(TimeDate)                :: DateAndTimeUTC                 ! DateAndTime translated to UTC from local time
INTEGER(8)                    :: DayMax_month(0:13)             ! Maximum number of days in each month of the year
INTEGER(4)                    :: Notice                         ! Index through leap second notices
INTEGER(MJD)                  :: dTime                          ! Seconds needed for this DateAndTime    (10^-7 seconds)
INTEGER(MJD)                  :: dTimeMax                       ! Maximum time in seconds for this date  (10^-7 seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
Error = .true.
DO WHILE (Initialized .and. Error)
  IF(.not.WithinClosedSet(    0, DateAndTime%hour     , 23    )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%minute   , 59    )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%second   , 99    )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%msec     , 999   )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%usec     , 999   )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%nsec     , 999   )) EXIT
  IF(.not.WithinClosedSet(    1, DateAndTime%day      , 31    )) EXIT
  IF(.not.WithinClosedSet(    1, DateAndTime%month    , 12    )) EXIT
  IF(.not.WithinClosedSet(-12.0, DateAndTime%dHourZone, 12.0  )) EXIT

  DayMax_month = DaysNormalYear_month
  IF(LeapYear(DateAndTime%year)) DayMax_month(February) = 29
  IF(DateAndTime%day > DayMax_month(DateAndTime%month)) EXIT

  CALL ConvertDateAndTimeToTimezone(UTC, DateAndTime, DateAndTimeUTC)
  IF(DateAndTimeUTC%year < YearMin .or. DateAndTimeUTC%year > yearMax) EXIT

  ! Ensure that maximum seconds cannot deviate from 59.99999 unless a leap second is in progress
  dTimeMax = 599999999
  Notice   = Notice_Year(DateAndTimeUTC%Year)
  DO WHILE(Notice /= 0 .and. Notice <= LV$Notice)
    IF(DateAndTimeUTC%Month  >  LeapSecond_Notice(notice)%Month .or.  &
       DateAndTimeUTC%Year   /= LeapSecond_Notice(notice)%Year) EXIT

    IF(DateAndTimeUTC%Month  == LeapSecond_Notice(notice)%Month .and. &
       DateAndTimeUTC%Day    == LeapSecond_Notice(notice)%Day   .and. &
       DateAndTimeUTC%Hour   == LeapSecond_Notice(notice)%Hour  .and. &
       DateAndTimeUTC%Minute == LeapSecond_Notice(notice)%Minute) THEN

       ! Adjust the maximum seconds value due to the leap second
       dTimeMax = dTimeMax    + LeapSecond_Notice(notice)%dTime
       EXIT
    END IF
    Notice = Notice + 1
  END DO
  dTime = TimePerSecond*DateAndTimeUTC%Second + TimePerMsec*DateAndTimeUTC%Msec + TimePeruSec*DateAndTimeUTC%uSec + DateAndTimeUTC%nSec/100
  IF(dTime > dTimeMax) EXIT

  IF(DateAndTimeUTC%year == year_epoch) THEN
    IF(DateAndTimeUTC%month <  month_epoch) EXIT
    IF(DateAndTimeUTC%month == month_epoch .and. DateAndTimeUTC%day < day_epoch) EXIT
  END IF

  Error = .false.
  CALL CalculateTimeFromDate(Time, DateAndTimeUTC)

END DO
RETURN
END SUBROUTINE GetTimeOfTimeDate

SUBROUTINE GetTimeOfDateWithZone(DateAndTime, Time, Error)
!+
!
! Description: This subroutine onverts Civil Time stored in a DateAndTime structure to Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
TYPE(TimeDate), INTENT(IN)    :: DateAndTime                    ! TimeDate struture to calculate Time
INTEGER(MJD)  , INTENT(OUT)   :: Time                           ! Modified Julian Date calculated       (10^-7 seconds)
LOGICAL(4)    , INTENT(OUT)   :: Error                          ! Error in the calculation
!
!                                Local
!-----------------------------+--------
TYPE(TimeDate)                :: DateAndTimeUTC                 ! DateAndTime translated to UTC from local time
INTEGER(8)                    :: DayMax_month(0:13)             ! Maximum number of days in each month of the year
INTEGER(4)                    :: Notice                         ! Index through leap second notices
INTEGER(MJD)                  :: dTime                          ! Seconds needed for this DateAndTime    (10^-7 seconds)
INTEGER(MJD)                  :: dTimeMax                       ! Maximum time in seconds for this date  (10^-7 seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
Error = .true.
DO WHILE (Initialized .and. Error)
  IF(.not.WithinClosedSet(    0, DateAndTime%hour     , 23    )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%minute   , 59    )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%second   , 99    )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%msec     , 999   )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%usec     , 999   )) EXIT
  IF(.not.WithinClosedSet(    0, DateAndTime%nsec     , 999   )) EXIT
  IF(.not.WithinClosedSet(    1, DateAndTime%day      , 31    )) EXIT
  IF(.not.WithinClosedSet(    1, DateAndTime%month    , 12    )) EXIT
  IF(.not.WithinClosedSet(-12.0, DateAndTime%dHourZone, 12.0  )) EXIT

  DayMax_month = DaysNormalYear_month
  IF(LeapYear(DateAndTime%year)) DayMax_month(February) = 29
  IF(DateAndTime%day > DayMax_month(DateAndTime%month)) EXIT

  CALL ConvertDateAndTimeToTimezone(UTC, DateAndTime, DateAndTimeUTC)
  IF(DateAndTimeUTC%year < YearMin .or. DateAndTimeUTC%year > yearMax) EXIT

  ! Ensure that maximum seconds cannot deviate from 59.99999 unless a leap second is in progress
  dTimeMax = 599999999
  Notice   = Notice_Year(DateAndTimeUTC%Year)
  DO WHILE(Notice /= 0 .and. Notice <= LV$Notice)
    IF(DateAndTimeUTC%Month  >  LeapSecond_Notice(notice)%Month .or.  &
       DateAndTimeUTC%Year   /= LeapSecond_Notice(notice)%Year) EXIT

    IF(DateAndTimeUTC%Month  == LeapSecond_Notice(notice)%Month .and. &
       DateAndTimeUTC%Day    == LeapSecond_Notice(notice)%Day   .and. &
       DateAndTimeUTC%Hour   == LeapSecond_Notice(notice)%Hour  .and. &
       DateAndTimeUTC%Minute == LeapSecond_Notice(notice)%Minute) THEN
       ! Adjust the maximum seconds value due to the leap second
         dTimeMax = dTimeMax    + LeapSecond_Notice(notice)%dTime
       EXIT
    END IF
    Notice = Notice + 1
  END DO
  dTime = TimePerSecond*DateAndTimeUTC%Second + TimePerMsec*DateAndTimeUTC%Msec + TimePeruSec*DateAndTimeUTC%uSec + DateAndTimeUTC%nSec/100
  IF(dTime > dTimeMax) EXIT

  IF(DateAndTimeUTC%year == year_epoch) THEN
    IF(DateAndTimeUTC%month <  month_epoch) EXIT
    IF(DateAndTimeUTC%month == month_epoch .and. DateAndTimeUTC%day < day_epoch) EXIT
  END IF

  Error = .false.
  CALL CalculateTimeFromDate(Time, DateAndTimeUTC)

END DO
RETURN
END SUBROUTINE GetTimeOfDateWithZone

SUBROUTINE GetDateOfTime(DateAndTime, Time, Error)
!+
!
! Description: This subroutine converts Modified Julian Date to Civil Time UTC in a TimeDate structure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
TYPE(TimeDate), INTENT(OUT)   :: DateAndTime
INTEGER(MJD)  , INTENT(IN)    :: Time
LOGICAL(4)    , INTENT(OUT)   :: Error
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Check for time_of_date beyond capability of I*8 type
IF (Time > TimeMax .or. Time < timeMin .or. .not.Initialized) THEN
  Error = .true.
ELSE
  CALL CalculateDateFromTime(Time, DateAndTime)
  Error = .false.
END IF
RETURN
END SUBROUTINE GetDateOfTime

SUBROUTINE GetLocalDateOfTime(DateAndTime, Time, Error)
!+
!
! Description: This function formats a Modified Julian Date input as a TimeDate structure with the local time zone
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------+-----------------------------------------------------------_
TYPE(TimeDate), INTENT(OUT)   :: DateAndTime              ! Time and Date elements for local timezone
INTEGER(MJD)  , INTENT(IN)    :: Time                     ! Modified Julian Date source for DateAndTime (10^-7 seconds)
LOGICAL(4)    , INTENT(OUT)   :: Error                    ! Input Time is outside range for Modified Julian Date
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!Check for time_of_date beyond capability of I*8 type
IF (Time > TimeMax .or. Time < TimeMin .or. .not.Initialized) THEN
  Error = .true.
ELSE
  CALL CalculateLocalDateFromTime(Time, DateAndTime)
  Error = .false.
END IF
RETURN
END SUBROUTINE GetLocalDateOfTime

SUBROUTINE CalculateTimeFromDate(Time,DateAndTime)
!+
!
! Description: This subroutine converts Civil Time UTC in a TimeDate structure to Modified Julian Date.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
TYPE(TimeDate), INTENT(IN)    :: DateAndtime
INTEGER(MJD)  , INTENT(OUT)   :: Time
!
!                                Local
!-----------------------------+--------
INTEGER(4)                    :: year
INTEGER(4)                    :: month

INTEGER(MJD)                  :: dTime_month(0:13)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  year = DateAndTime%year
  IF(LeapYear(year)) THEN
    dTime_month  = dTimeLeapYear_month
  ELSE
    dTime_month  = dTimeNormalYear_month
  END IF
  CALL AdjustMonthTimesForLeapSeconds(year, dTime_month)

  !Calculate the time
  month = DateAndTime%month
  Time  = TimeStart_year(year)                &
        + dTime_month(month-1) + 1            &
        + TimePerDay    *(DateAndTime%day-1)  &
        + TimePerHour   * DateAndTime%hour    &
        + TimePerMinute * DateAndTime%minute  &
        + TimePerSecond * DateAndTime%second  &
        + TimePerMsec   * DateAndTime%msec    &
        + TimePerUsec   * DateAndTime%usec    &
        + DateAndTime%nsec/100                &
        - TimePerMinute * INT(DateAndTime%dHourZone*60.0, KIND=8)

  RETURN
END SUBROUTINE CalculateTimeFromDate

SUBROUTINE CalculateLocalDateFromTime(Time,DateAndTime)
!+
!
! Description: This subroutine converts Modified Julian Date to Civil Time Local Time Zone in a TimeDate structure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 11-MAR-2025     | MA    | SCR 150: Add the Locale as an argument to accommodate different locations
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD)  , INTENT(IN)    :: Time
TYPE(TimeDate), INTENT(OUT)   :: DateAndtime
!
!                                Local
!-----------------------------+--------
CHARACTER(LEN=4)              :: FormatTime
REAL(4)                       :: dHoursLocal
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL CalculateDateFromTime(Time,DateAndTime)

  CALL AdjustDaylightSavings(Time, LocaleCurrent, FormatTime, dHoursLocal)
  CALL ConvertDateAndTimeToTimezone(dHoursLocal,DateAndTime,DateAndTime)

  RETURN
END SUBROUTINE CalculateLocalDateFromTime

SUBROUTINE CalculateDateFromTime(Time,DateAndTime)
!+
!
! Description: This subroutine converts Modified Julian Date to Civil UTC in a TimeDate structure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Ensure routine is thread safe by removing saved local variables
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD)  , INTENT(IN)    :: Time                           ! Modified Julian Date (10^-7 seconds)
TYPE(TimeDate), INTENT(OUT)   :: DateAndTime                    ! Date and Time structure Local Time
!
!                                Local
!-----------------------------+--------
INTEGER(4)                    :: year                           ! year component of DateAndTime
INTEGER(8)                    :: month                          ! month component of DateAndTime
INTEGER(8)                    :: day                            ! day component of DateAndTime
INTEGER(8)                    :: hour                           ! hour component of DateAndTime
INTEGER(8)                    :: minute                         ! minute component of DateAndTime
INTEGER(8)                    :: second                         ! second component of DateAndTime
INTEGER(8)                    :: mSec                           ! mSec component of DateAndTime
INTEGER(8)                    :: uSec                           ! uSec component of DateAndTime
INTEGER(8)                    :: nSec                           ! nSec component of DateAndTime
INTEGER(MJD)                  :: dTimeLeft                      ! Running total of time left  (10^-7 seconds)

INTEGER(4)                    :: DayMax_month(0:13)             ! Last day of each month
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time elapsed in year for each month (10^-7 seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  year = yearEpoch + INT(Time/TimePerYear, KIND=4)
  IF(TimeEnd_year(year) < 0) THEN
     IF(TimeEnd_year(year-1) >= Time) THEN
        year = year-1
     END IF
  ELSE IF(Time > TimeEnd_year(year)) THEN
     year = year + 1
  ELSE IF(Time < TimeStart_year(year)) THEN
     year = year - 1
  END IF

  IF(LeapYear(year)) THEN
    DayMax_month = DaysLeapYear_month
    dTime_month = dTimeLeapYear_month
  ELSE
    DayMax_month = DaysNormalYear_month
    dTime_month = dTimeNormalYear_month
  END IF

  CALL AdjustMonthTimesForLeapSeconds(year,dTime_month)

  dTimeLeft = Time - TimeStart_year(year)
  DO month = January, December
    IF(dTime_month(month) < dTimeLeft) CYCLE
    dTimeLeft = dTimeLeft - (dTime_month(month-1) + 1)
    Day       = MIN((dTimeLeft+TimePerDay)/TimePerDay, DayMax_month(month))
    dTimeLeft = dTimeLeft - TimePerDay    * (Day-1)
    Hour      = MIN(dTimeLeft/TimePerHour,  23)
    dTimeLeft = dTimeLeft - TimePerHour   * Hour
    Minute    = MIN(dTimeLeft/TimePerMinute,59)
    dTimeLeft = dTimeLeft - TimePerMinute * Minute
    Second    = dTimeLeft/TimePerSecond
    dTimeLeft = dTimeLeft - TimePerSecond * Second
    mSec      = dTimeLeft/TimePerMsec
    dTimeLeft = dTimeLeft - TimePerMsec * mSec
    uSec      = dTimeLeft/TimePerUsec
    dTimeLeft = dTimeLeft - TimePerUsec * uSec
    nSec      = dTimeLeft * 100
    DateAndTime%Year      = Year
    DateAndTime%Month     = INT(Month , KIND=4)
    DateAndTime%Day       = INT(Day   , KIND=4)
    DateAndTime%dHourZone = UTC
    DateAndTime%Hour      = INT(Hour  , KIND=4)
    DateAndTime%Minute    = INT(Minute, KIND=4)
    DateAndTime%Second    = INT(Second, KIND=4)
    DateAndTime%mSec      = INT(mSec  , KIND=4)
    DateAndTime%uSec      = INT(uSec  , KIND=4)
    DateAndTime%nSec      = INT(nSec  , KIND=4)
    DateAndTime%DayOfYear = INT(Day + SUM(DayMax_month(1:month-1)), KIND=4)
    CALL GetDayOfWeek(Time,DateAndTime%DayOfWeek)
    EXIT
  END DO
  RETURN
END SUBROUTINE CalculateDateFromTime

SUBROUTINE CalculateDeltaTime(dTime,DeltaTime)
!+
!
! Description: This subroutine converts Modified Julian Date Delta Time to a TimeDelta structure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD)  , INTENT(IN)    :: dTime
TYPE(TimeDelta),INTENT(OUT)   :: DeltaTime
!
!                                Local
!-----------------------------+--------
INTEGER(MJD)                  :: dTimeLeft
INTEGER(8)                    :: Days
INTEGER(8)                    :: Hours
INTEGER(8)                    :: Minutes
INTEGER(8)                    :: Seconds
INTEGER(8)                    :: mSec
INTEGER(8)                    :: uSec
INTEGER(8)                    :: nSec
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! Calculate components of DeltaTime
  Days      = (dTime/TimePerDay)
  dTimeLeft =  dTime - TimePerDay*Days
  Hours     =  dTimeLeft / TimePerHour
  dTimeLeft =  dTimeLeft - TimePerHour*Hours
  Minutes   =  dTimeLeft / TimePerMinute
  dTimeLeft =  dTimeLeft - TimePerMinute*Minutes
  Seconds   =  dTimeLeft / TimePerSecond
  dTimeLeft =  dTimeLeft - TimePerSecond*Seconds
  mSec      =  dTimeLeft / TimePerMsec
  dTimeLeft =  dTimeLeft - TimePerMsec*mSec
  uSec      =  dTimeLeft / TimePerUsec
  dTimeLeft =  dTimeLeft - TimePerUsec*uSec
  nSec      =  dTimeLeft / 100
  !
  ! Return the values
  DeltaTime%days    = INT(days   , KIND=4)
  DeltaTime%hours   = INT(hours  , KIND=4)
  DeltaTime%minutes = INT(minutes, KIND=4)
  DeltaTime%seconds = INT(seconds, KIND=4)
  DeltaTime%mSec    = INT(mSec   , KIND=4)
  DeltaTime%uSec    = INT(uSec   , KIND=4)
  DeltaTime%nSec    = INT(nSec   , KIND=4)

  RETURN
END SUBROUTINE CalculateDeltaTime

SUBROUTINE CalculateLeapSecondsBeforeTime(Time, dTimeLeap, LeapOccurring)
!+
!
! Description: This subroutine determines the number of leap seconds that have ocurred before a Modified Julian Date.
!              It is used to correct calculations using Unix Time, Habitat Time, Windows Time or Linux Time which do not
!              account for leap seconds.
!
!              The 1958 correction of 32.184 seconds and the subsequest accumulation of leap seconds are included. This
!              correction currently stands at 69.184 seconds for Modified Julian Dates after Janury 1, 2017
!
! ToDo SCR 150: Make more efficient by accumulating dTime_notice (sum of all correction up to and including the notice)
!               and then going from the DO lv$notice, 1, -1 until Time >= LeapSecondNotics%Time
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(MJD), INTENT(IN)      :: Time                           ! Modified Julian Date               (10^-7 seconds)
INTEGER(MJD), INTENT(OUT)     :: dTimeLeap                      ! Number of leap seconds before Time (10^-7 seconds)
LOGICAL(4)  , INTENT(OUT)     :: LeapOccurring                  ! Set true if a leap second was ocurring at Time.
!
!                                Local
!-----------------------------+--------
INTEGER(4)                    :: notice
INTEGER(MJD)                  :: TimeStart
INTEGER(MJD)                  :: TimeEnd
INTEGER(MJD)                  :: dTime
TYPE(TimeLeap)                :: LeapSecond
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  LeapOccurring = .false.
  dTimeLeap     = 0
  IF(.not.initialized) RETURN

  DO notice = 1, lv$notice
    LeapSecond = LeapSecond_notice(notice)
    dTime = LeapSecond_notice(notice)%dTime
    TimeEnd   = LeapSecond_notice(notice)%Time
    IF(dTime > 0) THEN
      TimeStart = TimeEnd - dTime
      IF(TimeStart <= Time) THEN
        dTimeLeap = dTimeLeap + MIN(dTime, Time-TimeStart)
        IF(Time<=TimeEnd) THEN
          LeapOccurring = .true.
        END IF
      END IF
    ELSE
      IF(TimeEnd < Time) THEN
        dTimeLeap = dTimeLeap + dTime
      END IF
    END IF
  END DO
  RETURN
END SUBROUTINE CalculateLeapSecondsBeforeTime

SUBROUTINE AdjustMonthTimesForLeapSeconds(year,dTimeAdjusted_month)
!+
! Description: This subroutine adjusts the ending Modified Julian Date for the months in a given year for leap seconds
!              scheduled during the given year.
!
! ToDo: SCR 150: Ensure that we never need dTime_mont(0) e.g.DecemberLastYear, otherwise we have to ensure that as well
! Also we may have to process Year-1, Year and Year+1 to ensure DecemberLastYear and JanuaryNextYear are proper
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Traverse leapsecond notice from latest to earliest to improve execution time
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(4)  , INTENT(IN)      :: year                           ! Year to adjust month Time.
INTEGER(MJD), INTENT(INOUT)   :: dTimeAdjusted_month(0:13)      ! Ending Modified Julian Dates for each month (10^-7 seconds)
!
!                                Local
!-----------------------------+--------
INTEGER(4)                    :: notice                         ! Index through leap second notices
INTEGER(4)                    :: month                          ! Month of a leap second notice
INTEGER(4)                    :: yearNotice                     ! Year of a leap second notice
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  DO notice = lv$notice, 1, -1
    yearNotice = LeapSecond_Notice(notice)%year
    IF(year > yearNotice) EXIT
    IF(year== yearNotice) THEN
      month = LeapSecond_Notice(notice)%month
      dTimeAdjusted_month(month:13) = dTimeAdjusted_month(month:13) &
                                    + LeapSecond_notice(notice)%dTime
    END IF
  END DO
  RETURN
END SUBROUTINE AdjustMonthTimesForLeapSeconds

FUNCTION RetrieveUpdatedLeapSecondList() RESULT(Status)
!
! Description: This function retrieves the leap second notice file from the URL below and makes it available to other CSP Programs
!              https://www.iana.org/timezones/data/leap-seconds.list
!
!              Note: This function should only be run on a machine with internet access.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!                                Passed Variables                Description (Units)
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(4)                    :: Status                        ! Return status
!
!                                Local Variables
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(4)                    :: StatusCmd                     ! Status return from SystemCommand
INTEGER(4)                    :: UnitCheckCommand              ! Fortran I/O unit for the command status file
CHARACTER(80)                 :: AsciiChr                      ! Buffer used for file reads
LOGICAL(1)                    :: SuccessfullBackup             ! Successfully backed up the leap-seconds.list file
INTEGER(4)   ,PARAMETER       :: CSP_S_NORMAL            =   & ! Normal Status Return
                                                                 INT(z'31FF0009', KIND=4)
INTEGER(4)   ,PARAMETER       :: CSP_E_FAILED            =   & ! Failed Status Return
                                                                 INT(z'31FF0012', KIND=4)
CHARACTER(53),PARAMETER       :: URLLeapSecondList       =   & ! URL of the file containing the list of leap seconds
                                                                 "https://www.iana.org/timezones/data/leap-seconds.list"
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  DO
    ! Backup current copy of the leap-seconds file
    StatusCmd = SystemCommand("copy /y D:\CSP\leap-seconds.list D:\CSP\leap-seconds-backup.list")
    IF(StatusCmd==-1) THEN
      SuccessfullBackup = .false.
      WRITE(*,FMT='(A)') "RetrieveUpdatedLeapSecondList: Error backing leap-seconds.list "
      EXIT
    END IF
    SuccessfullBackup = .true.
    !
    ! Issue the command to retrieve the file and pipe the command status to the out file:
    StatusCmd = SystemCommand("D:\CSP\Batch\winhttpjs.bat https://www.iana.org/timezones/data/leap-seconds.list -saveTo d:\csp\leap-seconds.list > d:\csp\temp\leap-seconds.out")
    IF(StatusCmd==-1) THEN
      WRITE(*,FMT='(A)') "RetrieveUpdatedLeapSecondList: Error retrieving "//URLLeapSecondList
      EXIT
    END IF
    !
    ! Ensure the command was successful
    IF(.not.OpenFileAndGetUnit(UnitCheckCommand, "d:\csp\temp\leap-seconds.out", "old")) THEN
      WRITE(*,FMT='(A)') "RetrieveUpdatedLeapSecondList: Error opening d:\csp\temp\leap-seconds.out"
      EXIT
    END IF
    !
    ! Read the result status
    IF(.not.ReadLineFromUnit(UnitCheckCommand,AsciiChr)) THEN
      WRITE(*,FMT='(A)') "RetrieveUpdatedLeapSecondList: Error reading d:\csp\temp\leap-seconds.out"
      EXIT
    END IF
    !
    ! Ensure that it was successful
    IF(TRIM(AsciiChr) /= "Status: 200 OK") THEN
      WRITE(*,FMT='(A)') "RetrieveUpdatedLeapSecondList: Error retrieving "//URLLeapSecondList
      WRITE(*,FMT='(A)') "RetrieveUpdatedLeapSecondList:                  "//TRIM(AsciiChr)
      EXIT
    END IF
    !
    ! Copy the file to the EMS Servers
    StatusCmd = SystemCommand("copy /y d:\csp\leap-seconds.list \\EMS-1\CSP\leap-seconds.list > d:\csp\temp\copy.out")
    StatusCmd = SystemCommand("copy /y d:\csp\leap-seconds.list \\EMS-2\CSP\leap-seconds.list > d:\csp\temp\copy.out")

    CLOSE(UnitCheckCommand)
    Status = CSP_S_NORMAL
    RETURN
  END DO
!
! ----------------------------------------------------------------------------------------------------------------------
!                         E R R O R   R E T U R N
! ----------------------------------------------------------------------------------------------------------------------
!
  !
  ! Restore the backup if it was successfully saved
  IF(SuccessfullBackup) THEN
    StatusCmd = SystemCommand("copy /y D:\CSP\leap-seconds-backup.list D:\CSP\leap-seconds.list")
  END IF

  CLOSE(UnitCheckCommand)
  Status = CSP_E_FAILED
  RETURN
END FUNCTION RetrieveUpdatedLeapSecondList

FUNCTION ReadNewLeapSecondNotices() RESULT(Status)
!
! Description: This function reads leap second notices from the file %CSP_DIR%\leap-seconds.list and adds any new notices
!              to the list of IERS leap second notices.
!
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!                                Passed Variables                Description (Units)
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(4)                    :: Status                        ! Status Return
!
!                                Local Variables
!-----------------------------+--------------------------------+--------------------------------------------------------
INTEGER(8)                    :: TmNTPBulletinCFile            ! IERS Bulletin C NTP Time of the file           (seconds)
CHARACTER(16)                 :: CSPPath                       ! Base path for CSP
INTEGER(4)                    :: UnitLeapSeconds               ! Fortran I/O unit to acces the leap seconds file
CHARACTER(96)                 :: AsciiChr                      ! Buffer used to read the leap second notices
INTEGER(4)                    :: Chr                           ! Character index into AsciiChr
CHARACTER(24)                 :: StringTime                    ! Time/Date string of leap second notice
CHARACTER(6)                  :: StringSeconds                 ! Seconds portion of time/date string
INTEGER(4)                    :: Notice                        ! Leap second notice number
LOGICAL(4)                    :: IncludedNotice                ! Set true if a new leap second notice has to be added
TYPE(TimeLeap)                :: LeapSecond                    ! Leap Second data stored in the system
TYPE(TimeDate)                :: DateAndTime                   ! Time data structure
LOGICAL(4)                    :: ErrorNumAscii                 ! Error in Ascii numeric conversion
INTEGER(4)   ,PARAMETER       :: sepEnd   = 4                  ! Number of separators
INTEGER(4)   ,PARAMETER       :: parseMax = 6                  ! Maximum number of tokens
CHARACTER(1) ,PARAMETER       :: a_sep(sepEnd)           = &   ! List of valid separator characters
                                                                 (/NULL,SPACE,TAB,"."/)
INTEGER(4)                    :: start$chr_parse(6)            ! Parsed field in leap second notice start
INTEGER(4)                    :: end$chr_parse(6)              ! Parsed field in leap second notice end
INTEGER(4)                    :: parseEnd                      ! Number of parsed fields
INTEGER(4)                    :: Day                           ! Day of leap second notice
INTEGER(4)                    :: Year                          ! Year of leap second notice
CHARACTER(3)                  :: AsciiMonth                    ! Month of leap second notice (Ascii)
INTEGER(4)                    :: Month                         ! Month of leap second notice
INTEGER(4)                    :: SecondsLast                   ! Last processed accumulated leap seconds (seconds)
INTEGER(4)                    :: Seconds                       ! Current accumulated leap seconds (seconds)
INTEGER(4)                    :: SecondLeap                    ! Leap seconds for a notice to be added (seconds)
INTEGER(4)                    :: LineRead                      ! Line read in file
LOGICAL(4)                    :: ErrorLeapSecond               ! Error trying to add a leap second notice
INTEGER(4)   ,PARAMETER       :: CSP_S_NORMAL            = &   ! Normal Status Return
                                                                 INT(z'31FF0009', KIND=4)
INTEGER(4)   ,PARAMETER       :: CSP_E_FAILED            = &   ! Failed Status Return
                                                                 INT(z'31FF0012', KIND=4)
LOGICAL(4)   ,PARAMETER       :: Debug = .false.                ! Diagnostic output flag
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(Debug) WRITE(*,FMT='(A)')"ReadNewLeapSecondNotices.Enter"
  DO
    !
    IF(Debug) WRITE(*,FMT='(A)')" ReadNewLeapSecondNotices.Reading and incorporating any new Leap Second Notices."
    !
    ! Now process the Leap Second List
    CALL GET_ENVIRONMENT_VARIABLE("CSP_DIR", CSPPath)
    OPEN(NewUnit=UnitLeapSeconds,FILE=TRIM(CSPPath)//"\leap-seconds.list",Status='old',ERR=3)
    SecondsLast = 0
    LineRead    = 0
    DO
      !Read a new line
      LineRead = LineRead + 1
      READ(UnitLeapSeconds,FMT='(A)',END=1,ERR=2) AsciiChr
      IF(Debug) WRITE(*,FMT='(A)') "   "//TRIM(AsciiChr)
      !
      ! Get the NTP Seconds Timestamp for the file
      Chr = INDEX(AsciiChr,"#$")
      IF(Chr > 0) THEN
        TmNTPBulletinCFile = INT(NumAscii(AsciiChr(Chr+3:Chr+12),ErrorNumAscii), KIND=8)
        IF(ErrorNumAscii) THEN
          WRITE(*,FMT='(A)')"ReadLeapSecondNotices: Unable to Decode NTP Timestamp"//AsciiChr(Chr+3:Chr+12)
          EXIT
        END IF
        IF(Debug) WRITe(*,FMT='("TmNTPFile = ",I11," TmNTPSystem = ",I11)') TmNTPBulletinCFile, TmNTPBulletinC
        !
        ! Return if the system IERS Bulletin C NTP times match (we have already processed this file)
        IF(TmNTPBulletinCFile==TmNTPBulletinC) THEN
          CLOSE(UnitLeapSeconds)
          Status = CSP_S_NORMAL
          IF(Debug) WRITE(*,FMT='(A)')" ReadNewLeapSecondNotices.Success - No new information"
          RETURN
        END IF
        TmNTPBulletinC = TmNTPBulletinCFile
      END IF
      IF(AsciiChr(1:1)=="#") CYCLE
      CALL ParseString(AsciiChr,a_sep,sepEnd,parseMax,start$chr_parse,end$chr_parse,parseEnd)
      IF(ParseEnd < 5) THEN
        WRITE(*,FMT='(" Error Processing, ParseEnd < 5, Line: ",I5)') LineRead
        CYCLE
      END IF
      StringSeconds  = AsciiChr(start$chr_parse(2):end$chr_parse(2))
      StringTime     = AsciiChr(Start$chr_parse(4):end$chr_parse(4))//"-"// &
                       AsciiChr(Start$chr_parse(5):end$chr_parse(5))//"-"// &
                       AsciiChr(Start$chr_parse(6):end$chr_parse(6))//" 00:00:00"
      Seconds        = INT(NumAscii(StringSeconds, ErrorNumAscii), KIND=4)
      IF(ErrorNumAscii) THEN
        WRITE(*,FMT='(" Error Decoding Seconds, NumAscii Line: ",I5)') LineRead
        CYCLE
      END IF
      Day        = INT(NumAscii(AsciiChr(start$chr_parse(5):end$chr_parse(5)),ErrorNumAscii),KIND=4) ; IF(ErrorNumAscii) CYCLE
      Year       = INT(NumAscii(AsciiChr(start$chr_parse(6):end$chr_parse(6)),ErrorNumAscii),KIND=4) ; IF(ErrorNumAscii) CYCLE
      AsciiMonth = TRIM(UpperCaseAscii(AsciiChr(start$chr_parse(4):end$chr_parse(4))))
      Month = January
      DO WHILE(Month <= December)
        IF(Ascii_Month(Month) == AsciiMonth) EXIT
        Month = Month + 1
      END DO
      IF(Month > December) CYCLE
      CALL TransformLeapSecondSchedule(Year, Month, Day)
      DateAndTime%Year    = Year
      DateAndTime%Month   = Month
      DateAndTime%Day     = Day
      DateAndTime%Hour    = 23
      DateAndTime%Minute  = 59
      IncludedNotice = .false.
      DO notice = 1, lv$notice
        LeapSecond = LeapSecondNotice(Notice)
        IF(LeapSecond%year  == DateAndTime%year .and. &
           LeapSecond%month == DateAndTime%month.and. &
           LeapSecond%day   == DateAndTime%day) THEN
           !IF(Debug) WRITE(*,FMT='(A)'," Notice Already Included: "//TRIM(AsciiChr)
           IncludedNotice = .true.
           EXIT
        END IF
      END DO
      IF(.not.IncludedNotice) THEN
         WRITE(*,FMT='(A)') " Need to Add Leap Second Notice: "//StringTime
         SecondLeap = Seconds-SecondsLast + 60
         WRITE(*,FMT='("  CALL AddLeapSecondNotice(",I4,5(",",I2.2),", 000, 000, ErrorLeap)")') DateAndTime%year, DateAndTime%month, DateAndTime%Day, DateAndTime%Hour, DateAndTime%Minute, SecondLeap
         CALL AddLeapSecondNotice(DateAndTime%year, DateAndTime%month, DateAndTime%Day, DateAndTime%Hour, DateAndTime%Minute, SecondLeap, 000, 000, ErrorLeapSecond)
         IF(ErrorLeapSecond) EXIT
      ELSE
         IF(Debug) WRITE(*,FMT='(A)') " Already Include Leap Second Notice: "//StringTime
      END IF
      SecondsLast = Seconds
    END DO
1   CLOSE(UnitLeapSeconds)
    Status = CSP_S_NORMAL
    IF(Debug) WRITE(*,FMT='(A)')" ReadNewLeapSecondNotices.Successfull return"
    RETURN
  END DO
!
! ----------------------------------------------------------------------------------------------------------------------
!                         E R R O R   R E T U R N
! ----------------------------------------------------------------------------------------------------------------------
!
  WRITE(*,FMT='("ReadNewLeapSecondNotices: Error Processing %CSP_DIR%\leap-seconds.list at line, ",I5)') LineRead
  CLOSE(UnitLeapSeconds); Status = CSP_E_FAILED
  RETURN

2 WRITE(*,FMT='("ReadNewLeapSecondNotices: Error Reading %CSP_DIR%\leap-seconds.list at line, ",I5)') LineRead
  CLOSE(UnitLeapSeconds); Status = CSP_E_FAILED
  RETURN

3 WRITE(*,FMT='(A)') "ReadNewLeapSecondNotices: Error Opening %CSP_DIR%\leap-seconds.list"
  CLOSE(UnitLeapSeconds); Status = CSP_E_FAILED
  RETURN
END FUNCTION ReadNewLeapSecondNotices

SUBROUTINE TransformLeapSecondSchedule(Year, Month, Day)
!
! Description: This subroutine transforms the NIST leap second schedule to a CSP leap second schedule.
!
!                  NIST records the leap second schedule as the first instant of a day
!                  CSP  records the leap second schedule as the last instant of a day
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                            Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN OUT)  :: Year
INTEGER(4)  , INTENT(IN OUT)  :: Month
INTEGER(4)  , INTENT(IN OUT)  :: Day
!
!                                Local                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: DayMax_Month(0:13)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         E R R O R   R E T U R N
! ----------------------------------------------------------------------------------------------------------------------
!
  DayMax_month = DaysNormalYear_month
  IF(LeapYear(Year)) DayMax_month(February) = 29
  Day  =  Day - 1
  IF(Day == 0) THEN
    Month = Month - 1
    IF(Month == 0) THEN
      Month = December
      Year  = Year - 1
    END IF
    Day = DayMax_Month(Month)
  END IF
  RETURN
END SUBROUTINE TransformLeapSecondSchedule

SUBROUTINE GetLeapSecondNotices()
!
! Description: This function retrieves the list of IERS leap second notices.  If the have been new updates from NIST,
!              then those new leap second notices are appended to the list.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Local                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(8)                  :: DateCurrently                  ! Date Currently (CCYYMMDD)
INTEGER(4)                    :: Status                         ! Return from ReadNewLeapSecondNotices
LOGICAL(4)  ,PARAMETER        :: Debug = .false.                ! Diagnostic for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL AddLeapSecondNoticesFromIERS
  CALL DATE_AND_TIME(DateCurrently)
  IF(DateCurrently /= DateLeapSeconds) THEN
    IF(Debug) WRITE(*,FMT='(A)') " Need to Read New Leap Second Notices: DateCurrently, DateLeapSeconds: "//DateCurrently//" .ne. "//DateLeapSeconds
    Status = ReadNewLeapSecondNotices()
    DateLeapSeconds = DateCurrently
  END IF
  RETURN
  END SUBROUTINE GetLeapSecondNotices

SUBROUTINE AddLeapSecondNoticesFromIERS
!
! Description: This subroutine processes the list of IERS leap second notices.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!                                Local                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4)                    :: ErrorLeapSecond
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  IF(AddedNoticesFromIERS) RETURN
  AddedNoticesFromIERS = .true.
  !
  ! Add leap seconds defined before January 1, 1988 by the Bureau International de I'Heure (BIH)
  !                        Year  Mo Day  Hr Min Sec mSec uSec
  CALL AddLeapSecondNotice(1958, 12, 31, 23, 59, 92, 184, 000, ErrorLeapSecond) !Initial TT-TAI delta (32.184 sec, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1959, 12, 31, 23, 59, 61, 422, 818, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1961, 07, 31, 23, 59, 59, 950, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1961, 12, 31, 23, 59, 60, 473, 040, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1963, 10, 31, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1963, 12, 31, 23, 59, 61, 294, 272, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1964, 03, 31, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1964, 08, 31, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1964, 12, 31, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1965, 02, 28, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1965, 06, 30, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1965, 08, 31, 23, 59, 60, 100, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1965, 12, 31, 23, 59, 60, 473, 040, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1966, 01, 31, 23, 59, 59, 900, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1971, 12, 31, 23, 59, 65, 786, 830, ErrorLeapSecond)
  !
  ! Decision to wait until UTC-UT1 is close to 0.6 seconds and then shedule 1 second leaps
  CALL AddLeapSecondNotice(1972, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1972, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1973, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1974, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1975, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1976, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1977, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1978, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1979, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1981, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1982, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1983, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1985, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  !
  ! IERS Jurisdiction starts here
  CALL AddLeapSecondNotice(1987, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1989, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1990, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1992, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1993, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1994, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1995, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1997, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(1998, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(2005, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(2008, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(2012, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(2015, 06, 30, 23, 59, 61, 000, 000, ErrorLeapSecond)
  CALL AddLeapSecondNotice(2016, 12, 31, 23, 59, 61, 000, 000, ErrorLeapSecond)
  RETURN
END SUBROUTINE AddLeapSecondNoticesFromIERS

SUBROUTINE AddLeapSecondNotice(year, month, day, hour, minute, second, msec, usec, error)
!
! Description: This function processes a leap second notice from the IERS
!
!               Note:
!               The time component arguments passed to this routine represent the moment in time when the clock
!               will rollover to midnight of the following day.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)        :: year                           ! notice time - year
INTEGER(4), INTENT(IN)        :: month                          ! notice time - month
INTEGER(4), INTENT(IN)        :: day                            ! notice time - day
INTEGER(4), INTENT(IN)        :: hour                           ! notice time - hour
INTEGER(4), INTENT(IN)        :: minute                         ! notice time - minute
INTEGER(4), INTENT(IN)        :: second                         ! notice time - second
INTEGER(4), INTENT(IN)        :: msec                           ! notice time - milliseconds
INTEGER(4), INTENT(IN)        :: usec                           ! notice time - microseconds
LOGICAL(4), INTENT(OUT)       :: error                          ! entry unable to be added
!
!                                Local
!-----------------------------+--------
INTEGER(4)                    :: Notice                         ! Index through leap second notices
INTEGER(4)                    :: Days_Month(0:13)               ! Number of days in each month (including DecemberLastYear and January Next Year
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time for each month last day of each month relative to the start of the year
INTEGER(4)                    :: Order                          ! Order of the notice to be added
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
IF(LeapYear(year)) THEN
  Days_Month  = DaysLeapYear_Month
  dTime_month = dTimeLeapYear_month
ELSE
  Days_Month  = DaysNormalYear_Month
  dTime_month = dTimeNormalYear_month
END IF
notice = 1
DO WHILE(notice <= lv$notice)
  IF(LeapSecond_notice(notice)%year == year) THEN
    dTime_month(LeapSecond_Notice(notice)%Month:) = dTime_month(LeapSecond_Notice(notice)%Month:) + LeapSecond_Notice(notice)%dTime
  END IF
  notice = notice + 1
END DO

Error = .true.
DO WHILE(error)
  !
  !check for no space
  IF(lv$notice == mx$notice) EXIT

  !check for invalid moment to rollover to midnight the following day
  IF(year      < yearStart.or.year   > yearEnd          ) EXIT
  IF(month     < January  .or.month  > December         ) EXIT
  IF(day       < 1        .or.day    > Days_month(month)) EXIT
  IF(second    < 0        .or.second > 99               ) EXIT
  IF(msec      < 0        .or.msec   > 999              ) EXIT
  IF(usec      < 0        .or.usec   > 999              ) EXIT
  Order = Year*10000 + Month*100 + Day
  error = .false.
END DO

notice = 1
DO WHILE(.not.error)
  IF(notice <= lv$notice) THEN
    !
    ! Check for duplicate entry
    IF(LeapSecond_Notice(notice)%year == year .and. LeapSecond_Notice(notice)%month == month .and. LeapSecond_Notice(notice)%day == day) THEN
      EXIT
    END IF
    !
    ! Check for notices not being added in sequential order (earliest to latest)
    IF(Order_Notice(Notice) > Order) THEN
      Error = .true.
      EXIT
    END IF
    notice = notice + 1
  ELSE
    !
    ! PRINT *,' Adding LeapSecond Notice, Index = ',notice
    LeapSecond_notice(notice)%DateAndTime%year      = year
    LeapSecond_notice(notice)%DateAndTime%month     = month
    LeapSecond_notice(notice)%DateAndTime%day       = day
    LeapSecond_notice(notice)%DateAndTime%hour      = hour
    LeapSecond_notice(notice)%DateAndTime%minute    = minute
    LeapSecond_notice(notice)%DateAndTime%second    = second
    LeapSecond_notice(notice)%DateAndTime%msec      = msec
    LeapSecond_notice(notice)%DateAndTime%usec      = usec
    LeapSecond_notice(notice)%DateAndTime%nsec      = 0
    LeapSecond_notice(notice)%DateAndTime%dHourZone = UTC
    LeapSecond_notice(notice)%year                  = year
    LeapSecond_notice(notice)%month                 = month
    LeapSecond_notice(notice)%day                   = day
    LeapSecond_notice(notice)%hour                  = hour
    LeapSecond_notice(notice)%minute                = minute
    LeapSecond_notice(notice)%second                = second
    LeapSecond_notice(notice)%msec                  = msec
    LeapSecond_notice(notice)%usec                  = usec
    LeapSecond_notice(notice)%nsec                  = 0
    LeapSecond_notice(notice)%dHourZone             = UTC
    LeapSecond_notice(notice)%dTime                 = (second -60) * TimePerSecond &
                                                    +  msec        * TimePermSec   &
                                                    +  usec        * TimePeruSec
    !
    ! Set the time one tic before the transition to midnight
    LeapSecond_notice(notice)%Time                  = TimeStart_year(year)         &
                                                    + dTime_month(month-1) + 1      &
                                                    + TimePerDay    *(day-1)       &
                                                    + TimePerHour   * hour         &
                                                    + TimePerMinute * minute       &
                                                    + TimePerSecond * second       &
                                                    + TimePerMsec   * msec         &
                                                    + TimePerUsec   * usec         &
                                                    - 1
    Order_Notice(Notice) = Order
    lv$notice = notice
    IF(notice_year(year)==0) notice_year(year) = notice
    !
    ! This updates were moved here from Time/Date initialization to allow the incorporation of new leap second notices during run-Time.
    TimeStart_Year(Year+1:YearEnd) = TimeStart_Year(Year+1:YearEnd) + LeapSecond_Notice(notice)%dTime
    TimeEnd_Year  (Year  :YearEnd) = TimeEnd_Year  (Year  :YearEnd) + LeapSecond_Notice(notice)%dTime
    CALL CalculateDateFromTime(LeapSecond_notice(notice)%Time, LeapSecond_notice(notice)%DateAndTime)
    EXIT
  END IF
END DO
IF(Error) PRINT*,' Bad Leap Second Data: ',year,month,day
RETURN
END SUBROUTINE AddLeapSecondNotice

FUNCTION LeapSecondNotice(Notice) RESULT(LeapSecond)
!
! Description: This function processes a leap second notice from the IERS list of leap seconds and returns the leap
!              second notice data in a LeapSecond structure.
!
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(4)                    :: notice                         ! Leap second notice index
TYPE(TimeLeap)                :: LeapSecond                     ! Leap second data
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
   IF(notice > 0 .and. notice <=LV$Notice) THEN
     LeapSecond = LeapSecond_Notice(Notice)
   ELSE
     LeapSecond%DateAndTime%year      = 0
     LeapSecond%DateAndTime%month     = 0
     LeapSecond%DateAndTime%day       = 0
     LeapSecond%DateAndTime%hour      = 0
     LeapSecond%DateAndTime%minute    = 0
     LeapSecond%DateAndTime%second    = 0
     LeapSecond%DateAndTime%msec      = 0
     LeapSecond%DateAndTime%usec      = 0
     LeapSecond%DateAndTime%nsec      = 0
     LeapSecond%DateAndTime%dHourZone = 0
     LeapSecond%Time                  = 0
     LeapSecond%dTime                 = 0
     LeapSecond%Year                  = 0
     LeapSecond%Month                 = 0
     LeapSecond%Day                   = 0
     LeapSecond%Hour                  = 0
     LeapSecond%Minute                = 0
     LeapSecond%Second                = 0
     LeapSecond%msec                  = 0
     LeapSecond%usec                  = 0

   END IF
END FUNCTION LeapSecondNotice

SUBROUTINE AddIERSBulletinD(NumberNotice, DateStringAnnounce, DateStringStart, dUT1IERS, Error)
!
! Description: This function processes a IERS Bulletin D notice.  Bulletin D contains the difference between UTC and UT1.
!              UTC is based on atomic time with periodic leap second updates to keep UTC sychronized with time based on
!              the rotation of the earth.  UTC has a inserted leap second whenever the difference betwen UTC and atomic
!              time exceeds 0.9 seconds. Bulletin D publishes the instataneous difference.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
INTEGER(4)   , INTENT(IN)     :: NumberNotice                   ! Bulletin D Number
CHARACTER(10), INTENT(IN)     :: DateStringAnnounce             ! Issue Date
CHARACTER(10), INTENT(IN)     :: DateStringStart                ! Start Date
REAL(8)      , INTENT(IN)     :: dUT1IERS                       ! Difference between Terrestrial Time and UT1 (sec)
LOGICAL(4)   , INTENT(OUT)    :: Error                          ! entry unable to be added
!
!                                Local
!----------------------------+--------
INTEGER(4)                    :: BulletinD                      ! Index through the IERS Bulleting D Notices
LOGICAL(4)                    :: ErrorTimeSupplied              ! Error in either of the
INTEGER(MJD)                  :: TimeStartNotice                ! Modified Julian Date of the start   (10^-7 seconds)
INTEGER(MJD)                  :: TimeAnnounceNotice             ! Modified Julian Date of announcemnt (10^-7 seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Error    = .false.

  !check for no space
  IF(lv$BulletinD == mx$BulletinD) THEN
    PRINT *,' AddIERSBulletin-D, mx$BulletinD sized too small'
    Error = .true.
  END IF
  TimeStartNotice = TimeFromTimeDateString(DateStringStart//" UTC", errorTimeSupplied)
  IF(ErrorTimeSupplied) THEN
    PRINT *,' AddIERSBulletin-D, invalid starting date: ',DateStringStart
    Error = .true.
  END IF
  TimeAnnounceNotice = TimeFromTimeDateString(DateStringAnnounce//" UTC", errorTimeSupplied)
  IF(ErrorTimeSupplied) THEN
    PRINT *,' AddIERSBulletin-D, invalid announce date: ',DateStringAnnounce
    Error = .true.
  END IF
  IF(NumberNotice < 0 .or. NumberNotice > 9999          ) THEN
    PRINT *,' AddIERSBulletin-D, invalid notice number:',NumberNotice
    Error = .true.
  END IF

  BulletinD = 1
  DO WHILE(.not.Error)
    IF(BulletinD <= lv$BulletinD) THEN
      IF(IERS_BulletinD(BulletinD)%TimeStart == TimeStartNotice) THEN
        Error = .true.
        PRINT *,' AddIERSBulletin-d, conflicting notices:',NumberNotice,IERS_BulletinD(BulletinD)%Number
        EXIT
      END IF
      BulletinD = BulletinD + 1
      IF(BulletinD > mx$BulletinD) THEN
        Error = .true.
        PRINT *,' AddIERSBulletin-d, mx$BulletinD sizing too small'
        EXIT
      END IF
    ELSE
      !PRINT *,' Adding IERS BulletinD = ',NumberNotice,' Index = ', BulletinD
      IERS_BulletinD(BulletinD)%Number       = NumberNotice
      IERS_BulletinD(BulletinD)%TimeStart    = TimeStartNotice
      IERS_BulletinD(BulletinD)%StartDate    = DateStringStart
      IERS_BulletinD(BulletinD)%AnnounceDate = DateStringAnnounce
      IERS_BulletinD(BulletinD)%dUT1         = dUT1IERS
      lv$BulletinD = BulletinD
      EXIT
    END IF
  END DO
  RETURN
END SUBROUTINE AddIERSBulletinD

FUNCTION dUT1FromIERSBulletins(Time) RESULT(dUT1)
!
!  Description: This function retrieves the difference between UTC and UT1 valid for the input Time from the applicable
!               IERS Bulletin-D notices
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD),INTENT(IN)       :: Time                          ! Modified Julian Date (10^-7 seconds)
REAL(8)                       :: dUT1                           ! UTC-UT1 (seconds)

!                               Local
!-----------------------------+--------
INTEGER(4)                    :: BulletinD                      ! Index through Bulletin-D notices
LOGICAL(4)                    :: ErrorBulletinD
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!

  ! Bulletin-D notices
  IF(.not.InitializedBulletinD) THEN
    InitializedBulletinD = .true.
    CALL AddIERSBulletinD   (109, "2011-09-29", "2011-11-04", -0.400d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (110, "2012-01-16", "2012-02-09", -0.500d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (111, "2012-04-11", "2012-04-30", -0.600d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (112, "2012-06-15", "2012-07-01",  0.400d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (113, "2012-09-21", "2012-10-05",  0.300d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (114, "2012-12-21", "2013-01-01",  0.200d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (115, "2013-03-18", "2013-04-01",  0.100d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (116, "2013-07-16", "2013-08-02",  0.000d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (117, "2013-10-18", "2013-11-01", -0.100d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (118, "2025-03-16", "2025-03-16",  0.100d0,  ErrorBulletinD)
    CALL AddIERSBulletinD   (119, "2025-03-17", "2025-03-17", -1.57d-3, ErrorBulletinD)
  END IF

  IF(LV$BulletinD < 1) THEN
    dUT1 = 0d0
  ELSE IF(Time < IERS_BulletinD(1)%TimeStart) THEN
    dUT1 = 0d0
  ELSE
    dUT1 = 0d0
    DO BulletinD = LV$BulletinD, 1, -1
      IF(Time >= IERS_BulletinD(BulletinD)%TimeStart) THEN
        dUT1 = IERS_BulletinD(BulletinD)%dUT1
        EXIT
      END IF
    END DO
  END IF
  RETURN
END FUNCTION dUT1FromIERSBulletins

SUBROUTINE ConvertDateAndTimeToTimezone(dHourZone,DateAndTime,DateAndTimeConverted)
!
!  Description: This subroutine converts a TimeDate structure from to an equivalent TimeDate structure in a different
!               time zone.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!  Note: Must add Validity check for DateAndTime and TimeZone and an error return
!        to guard against ridiculous input (or make this private and ensure no bad calls are made)
!
!                               Passed                           Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4)       ,INTENT(IN)     :: dHourZone                      ! Timezone /(hours)
TYPE(TimeDate)                :: DateAndTime                    ! Date and time structure input
TYPE(TimeDate)                :: DateAndTimeConverted           ! Data and time structure output

!                               Local
!----------------------------+--------
INTEGER(4)                    :: Year
INTEGER(4)                    :: Month
INTEGER(4)                    :: Day
INTEGER(4)                    :: Hour
INTEGER(4)                    :: Minute
REAL(4)                       :: dHours
INTEGER(4)                    :: dHour
INTEGER(4)                    :: dMinute
INTEGER(MJD)                  :: TimeLocal
LOGICAL(4)                    :: Error
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
IF(DateAndTime%dHourZone == dHourZone) THEN
  DateAndTimeConverted = DateAndTime
  Day   = DateAndTimeConverted%Day
  Month = DateAndTimeConverted%Month
  Year  = DateAndTimeConverted%Year
  CALL GetDayOfYear(Day,Month,Year,DateAndTimeConverted%DayOfYear,Error)
  TimeLocal = TimeStart_Year(Year) + (DateAndTimeConverted%DayOfYear-1)*TimePerDay + TimePerDay/2
  CALL GetDayOfWeek(TimeLocal,DateAndTimeConverted%DayOfWeek)
ELSE
  dHours  = dHourZone - DateAndTime%dHourZone
  dHour   = INT(dHours)
  dMinute = INT(60.0d0*(dHours - FLOAT(dHour)))

  Year    = DateAndTime%Year
  Month   = DateAndTime%Month
  Day     = DateAndTime%Day
  Hour    = DateAndTime%Hour
  Minute  = DateAndTime%Minute + dMinute
  IF(Minute > 59) THEN
    Minute = Minute - 60
    Hour   = Hour + 1
  ELSE IF(Minute < 0) THEN
    Minute = Minute + 60
    Hour   = Hour - 1
  END IF
  Hour = Hour + dHour
  IF(Hour > 23) THEN
    Hour = Hour - 24
    Day  = Day + 1
  ELSE IF(Hour < 0) THEN
    Hour = Hour + 24
    Day  = Day - 1
  END IF
  IF(LeapYear(year)) THEN
    IF(Day > DaysLeapYear_month(month)) THEN
      Day = 1
      Month = Month + 1
    ELSE IF(Day < 1) THEN
      Month = Month - 1
      Day = DaysLeapYear_month(month)
    END IF
  ELSE
    IF(Day > DaysNormalYear_month(month)) THEN
      Day = 1
      Month = Month + 1
    ELSE IF(Day < 1) THEN
      Month = Month - 1
      Day = DaysNormalYear_month(month)
    END IF
  END IF
  IF(month > 12) THEN
     month = 1
     year  = year + 1
  ELSE IF(month < 1) THEN
    month = 12
    year  = year - 1
  END IF
  DateAndTimeConverted%Year      = Year
  DateAndTimeConverted%Month     = Month
  DateAndTimeConverted%Day       = Day
  DateAndTimeConverted%dHourZone = dHourZone
  DateAndTimeConverted%Hour      = Hour
  DateAndTimeConverted%Minute    = Minute
  DateAndTimeConverted%Second    = DateAndTime%Second
  DateAndTimeConverted%mSec      = DateAndTime%mSec
  DateAndTimeConverted%uSec      = DateAndTime%uSec
  DateAndTimeConverted%nSec      = DateAndTime%nSec

  CALL GetDayOfYear(Day,Month,Year,DateAndTimeConverted%DayOfYear,Error)
  TimeLocal = TimeStart_year(Year) + (DateAndTimeConverted%DayOfYear-1)*TimePerDay + TimePerDay/2
  CALL GetDayOfWeek(TimeLocal,DateAndTimeConverted%DayOfWeek)
END IF
RETURN
END SUBROUTINE ConvertDateAndTimeToTimezone

SUBROUTINE GetDayOfWeek(Time, dayOfWeek)
!
!  Description: This subroutine determines the day of the week (1-7) of a Modified Julian Date.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Update comments to clarify why we add a Wednesday offset to TimeAdjusted
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD),INTENT(IN)       :: Time                           !Modified Julian Date  (10^-7 sec)
INTEGER(4)  ,INTENT(OUT)      :: DayOfWeek                      !Day of the week [1,7] (days)
!
!                               Local
!-----------------------------+---------
INTEGER(MJD)                  :: dTime                          !Leap seconds before Time
INTEGER(MJD)                  :: TimeAdjusted                   !Time adjusted for leap seconds
LOGICAL(4)                    :: LeapOccurring                  !Set true if during a leap second
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! Adjust time for leap seconds
CALL CalculateLeapSecondsBeforeTime(Time, dTime, LeapOccurring)
IF(LeapOccurring) THEN
  TimeAdjusted = Time - dTime - TimePerMinute
ELSE
  TimeAdjusted = Time - dTime
END IF

! SCR 150: Nov 17, 1858 was a Wednesday that's why we add 3 * TimePerDay below
IF(TimeAdjusted > 0) THEN
  dayOfWeek = INT( MODULO(TimeAdjusted+3*TimePerDay,TimePerWeek)/TimePerDay + 1 , KIND=4)
ELSE
  dayOfWeek = INT( MODULO(TimeAdjusted+3*TimePerDay,TimePerWeek)/TimePerDay + 1 , KIND=4)
END IF
RETURN
END SUBROUTINE GetDayOfWeek

SUBROUTINE GetDayOfYear(Day, Month, Year, DayOfYear, Error)
!
!  Description: This subroutine determines the day of the year (1-366) for a given day of the month, month and year.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN )       :: day
INTEGER(4), INTENT(IN )       :: month
INTEGER(4), INTENT(IN )       :: year
INTEGER(4), INTENT(OUT)       :: dayOfYear
LOGICAL(4), INTENT(OUT)       :: Error
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: DayMax_month(0:13)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
IF(LeapYear(year)) THEN
  DayMax_month = DaysLeapYear_month
ElSE
  DayMax_month = DaysNormalYear_month
END IF

dayOfYear = 0
Error     = .true.
IF(month > 0 .and. month <= 13) THEN
  IF(day > 0 .and. day   <= DayMax_month(month)) THEN
    dayOfYear = day + SUM(DayMax_month(1:month-1))
    Error = .false.
  END IF
END IF
RETURN
END SUBROUTINE GetDayOfYear

SUBROUTINE GetMonthAndDay(year, dayOfYear, month, day, error)
!
!  Description: This subroutine determines the month and day of the month for a given year and day of the year (1-366).
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN )       :: year                           ! Year input
INTEGER(4), INTENT(IN )       :: dayOfYear                      ! Day of the year (1-366)
INTEGER(4), INTENT(OUT)       :: month                          ! Month (1-12)
INTEGER(4), INTENT(OUT)       :: day                            ! Day of the month (1-31)
LOGICAL(4), INTENT(OUT)       :: error                          ! Set false for invalid input (e.g. DayOfYear 390)
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: DayMax_month(0:13)
INTEGER(4)                    :: dayOfYearSum
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
IF(LeapYear(year)) THEN
  DayMax_month = DaysLeapYear_month
ElSE
  DayMax_month = DaysNormalYear_month
END IF

dayOfYearSum = 0
Error        = .true.
DO month = January, December
  IF(dayOfYearSum + DayMax_month(month)>=dayOfYear) THEN
    day   = dayOfYear - dayOfYearSum
    error = .false.
    EXIT
  END IF
  dayOfYearSum = dayOfYearSum + DayMax_month(month)
END DO
IF(error) THEN
  day   = 0
  month = 0
END IF
RETURN
END SUBROUTINE GetMonthAndDay

SUBROUTINE FormTimeStamp(DateAndTime, TimeStamp , Error)
!
!  Description: This subroutine forms a timestamp, DD-MMM-YYYY hh:mm:ss.mmm, from a TimeDate structure
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(TimeDate), INTENT(IN )   :: DateAndTime
CHARACTER(*)  , INTENT(OUT)   :: TimeStamp
LOGICAL(4)    , INTENT(OUT)   :: error
!
!                                Local
!-------------------------------------
CHARACTER(24)                 :: TimeSTampLocal
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
TimeStampLocal = Ascii_day   (DateAndTime%day   )//'-'// &
                 Ascii_month (DateAndTime%month )//'-'// &
                 Ascii_year  (DateAndTime%year  )//' '// &
                 Ascii_hour  (DateAndTime%hour  )//':'// &
                 Ascii_minute(DateAndTime%minute)//':'// &
                 Ascii_second(DateAndTime%second)//'.'// &
                 Ascii_msec  (DateAndTime%msec  )
TimeStamp     = TimeStampLocal
error         = .false.
RETURN
END SUBROUTINE FormTimeStamp

SUBROUTINE GetMonthAscii(asciiForMonth, monthFromAscii)
!
!  Description: This subroutine determines the month number from the month ascii (e.g. input "MAR" returns 3)
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!                                Variable                        Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)       :: asciiForMonth                  !month name
INTEGER(4),  INTENT(OUT)      :: monthFromAscii                 !month [1,12] (month index)
!
INTEGER(4)                    :: month                          !Index through months
CHARACTER(16)                 :: ascii                          !Local ascii for month
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
monthFromAscii = -1
CALL UpperCase(asciiForMonth,ascii)
DO month = January, December
  IF( TRIM(ascii_month(month))/=TRIM(ascii).and.TRIM(asciiFull_month(month))/=TRIM(ascii)) CYCLE
  monthFromAscii = month
  EXIT
END DO
RETURN
END SUBROUTINE GetMonthAscii

SUBROUTINE GetDateAndTimeISO(DateAndTime)
!+
! Description: This function retrieves the current local time information from the operating system and packages
!              it into the TimeDateISO structure.
!
!
!MODIFICATION HISTORY:
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                            Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------
TYPE(TimeDateISO), INTENT(OUT):: DateAndTime                    ! ISO 8601:1988 compatible Time and Date structure
!
!                                Local
!-----------------------------+--------------
INTEGER(4)                    :: values_rcv(8)                  ! Return value from DATE_AND_TIME
TYPE(TimeDateISO)             :: DateAndTimeLocal               ! ISO 8601:1988 compatible Time and Date structure
EQUIVALENCE                     (DateAndTimeLocal, Values_rcv)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CALL DATE_AND_TIME(Values = Values_rcv)
  DateAndTime = DateAndTimeLocal
END SUBROUTINE GetDateAndTimeISO

SUBROUTINE GetDateAndTime(DateAndTime)
!+
! Description: This function retrieves the current local time information from the operating system and packages
!              it into the TimeDate structure.
!
!
!MODIFICATION HISTORY:
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!------------------------------+--------------------------------+--------------------------------------------------------
TYPE(TimeDate), INTENT(OUT)    :: DateAndTime                   ! Date and Time structure
!
!                               Local
!-----------------------------+---------------------------
INTEGER(4)                    :: DayOfYear                      ! Day of the year
INTEGER(4)                    :: DayOfWeek                      ! Day of the week
INTEGER(MJD)                  :: TimeNoon                       ! Time at Noon
LOGICAL(4)                    :: ErrorTime                      ! Error from GetDayOfYear
INTEGER(4)                    :: Values_rcv(9)                  ! System Date and Time from DATE_AND_TIME (Array)
INTEGER(4)                    :: year                           ! Current value for year
INTEGER(4)                    :: month                          ! Current value for month
INTEGER(4)                    :: day                            ! Current value for day
INTEGER(4)                    :: dMinuteZone                    ! Minute shift from UTC for Local time
INTEGER(4)                    :: hour                           ! Current value for hour
INTEGER(4)                    :: minute                         ! Current value for minute
INTEGER(4)                    :: second                         ! Current value for second
INTEGER(4)                    :: mSec                           ! Current value for mSec
EQUIVALENCE                     (values_rcv(1),year  )
EQUIVALENCE                     (values_rcv(2),month )
EQUIVALENCE                     (values_rcv(3),day   )
EQUIVALENCE                     (values_rcv(4),dMinuteZone  )
EQUIVALENCE                     (values_rcv(5),hour  )
EQUIVALENCE                     (values_rcv(6),minute)
EQUIVALENCE                     (values_rcv(7),second)
EQUIVALENCE                     (values_rcv(8),msec  )
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  !
  IF(.not.Initialized) THEN
    RETURN
  END IF
  CALL DATE_AND_TIME(VALUES = Values_rcv(1:8))
  DateAndTime%Year      = Year
  DateAndTime%Month     = Month
  DateAndTime%Day       = Day
  DateAndTime%dHourZone = FLOAT(dMinuteZone )/60.0
  DateAndTime%Hour      = Hour
  DateAndTime%Minute    = Minute
  DateAndTime%Second    = Second
  DateAndTime%msec      = msec
  DateAndTime%uSec      = 0
  DateAndTime%nSec      = 0
  CALL GetDayOfYear(DateAndTime%Day, DateAndTime%Month, DateAndTime%Year, DayOfYear, ErrorTime)
  TimeNoon = TimeStart_Year(DateAndTime%year) + TimePerDay*DayOfYear + TimePerDay/2
  CALL GetDayOfWeek(TimeNoon, DayOfWeek)
  DateAndTime%DayOfYear = DayOfYear
  DateAndTime%DayOfWeek = DayOfWeek
  RETURN
END SUBROUTINE GetDateAndTime
!
FUNCTION LeapYear(Year) RESULT(Leap)
!+
! Description: This function returns true if the input year is a leap year; false otherwise.
!
!
!MODIFICATION HISTORY:
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                               Passed                                           Description / (units)
!-----------------------------+------------------------------------------------+----------------------------------------
INTEGER(4), INTENT(IN)        :: year                                          ! year being checked (years)
LOGICAL(4)                    :: Leap                                          ! Set true if year is a leap year
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  !
  IF(MOD(year,400)==0) THEN
    Leap = .true.
  ELSE IF(MOD(year,100)==0) THEN
    Leap = .false.
  ELSE IF(MOD(year,4)==0) THEN
    Leap = .true.
  ELSE
    Leap = .false.
  END IF
  RETURN
END FUNCTION LeapYear

FUNCTION DateNumberASCII(ASCII_chr, number) RESULT(valid)
!
! Description: This subroutine converts a date/time substring to a number.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN )     :: ASCII_chr                      !date/time string
INTEGER(4)  , INTENT(OUT)     :: number                         !date/time number
LOGICAL(4)                    :: valid                          !Set true if valid date/time string
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: chr
INTEGER(4)                    :: chrStart
INTEGER(4)                    :: chrEnd
INTEGER(1)                    :: digit
CHARACTER(1)                  :: ascii
EQUIVALENCE                     (ascii,digit)
CHARACTER(1)                  :: asciiZero
INTEGER(1)                    :: zero
EQUIVALENCE                     (zero,asciiZero)
CHARACTER(1)                  :: asciiNine
INTEGER(1)                    :: nine
EQUIVALENCE                     (nine,asciiNine)
INTEGER(4)                    :: month
INTEGER(4)                    :: num
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  asciiZero    = '0'
  asciiNine    = '9'
  chrEnd       = LEN(ascii_chr)
  number       = 0
  num          = 0
  chrStart     = 1

  valid = .true.

  !Find the non-blank starting character
  DO WHILE(ascii_chr(chrStart:chrStart).eq.' ')
     chrStart = chrStart+1
     IF(chrStart.gt.chrEnd) THEN
        valid = .false.
        EXIT
     END IF
  END DO

  !Find the non-blank ending character
  DO WHILE(ascii_chr(chrEnd:chrEnd).eq.' ')
     chrEnd = chrEnd - 1
     IF(chrStart.gt.chrEnd) THEN
        valid = .false.
        EXIT
     END IF
  END DO

  IF(valid) THEN
    DO chr = chrStart, chrEnd
       ascii = ascii_chr(chr:chr)
       IF(digit >= zero .and. digit <= nine) THEN
          num = num*10 + digit - zero
       ELSE
          valid = .false.
          EXIT
       END IF
    END DO

    !
    IF(.not.valid) THEN
      DO month = January, December
        IF(ascii_month(month)==ascii_chr(chrStart:chrEnd)) THEN
          valid = .true.
          num   = month
          EXIT
        END IF
      END DO
    END IF
  END IF

  IF(valid) THEN
    number = num
  ELSE
    number = -1
  END IF

  RETURN
END FUNCTION DateNumberASCII

SUBROUTINE SET_SYSTEM_LOCALE(Name, StandardTimeString, dHoursSTD, DayLightSavingsString, dHoursDST)
!
! Description: This subroutine sets the Locale for the current system (e.g. where the HFCS is running).
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN )     :: Name                           ! Name of the Locale (e.g. America/Los_Angeles
CHARACTER(*), INTENT(IN )     :: StandardTimeString             ! String appended for standard time (e.g. PST)
REAL(4),      INTENT(IN )     :: dHoursSTD                      ! Hour shift from UTC standard time (hours)
CHARACTER(*), INTENT(IN )     :: DayLightSavingsString          ! String appended for daylight savings (e.g. PDT)
REAL(4),      INTENT(IN )     :: dHoursDST                      ! Hour shift from UTC daylight savings (hours)
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: slash                          ! Location of first "/" in the locale name
INTEGER(4)                    :: slashLast                      ! Location of last "/" in the locale name
CHARACTER(16)                 :: Continent                      ! Continent or Ocean part of the locale name
CHARACTER(16)                 :: City                           ! City part of the locale name

!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  slash                            = INDEX(Name,'/')
  slashLast                        = INDEX(Name,'/',BACK=.true.)
  Continent                        = Name(1:slash-1)
  City                             = Name(slash+1: )

  SystemLocale%Name                = TRIM(Name)
  SystemLocale%DayLightSavingsTime = DayLightSavingsString
  SystemLocale%StandardTime        = StandardTimeString
  SystemLocale%dHoursSTD           = dHoursSTD
  SystemLocale%dHoursDST           = dHoursDST
  SystemLocale%Continent           = Continent
  SystemLocale%City                = City

  NameSystemLocale                 = TRIM(Name)

  RETURN
END SUBROUTINE SET_SYSTEM_LOCALE

SUBROUTINE SET_TIME_LOCALE(Name, StandardTimeString, dHoursSTD, DaylightSavingsString, dHoursDST)
!
! Description: This subroutine sets the Locale for the local system (e.g. where the HFCS is to be installed).
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN )     :: Name                           ! Name of the Locale (e.g. America/Los_Angeles
CHARACTER(*), INTENT(IN )     :: StandardTimeString             ! String appended for standard time (e.g. PST)
REAL(4),      INTENT(IN )     :: dHoursSTD                      ! Hour shift from UTC standard time (hours)
CHARACTER(*), INTENT(IN )     :: DayLightSavingsString          ! String appended for daylight savings (e.g. PDT)
REAL(4),      INTENT(IN )     :: dHoursDST                      ! Hour shift from UTC daylight savings (hours)
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: slash                          ! Location of first "/" in the locale name
INTEGER(4)                    :: slashLast                      ! Location of last "/" in the locale name
CHARACTER(16)                 :: Continent                      ! Continent or Ocean part of the locale name
CHARACTER(16)                 :: City                           ! City part of the locale name
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  slash                             = INDEX(Name,'/')
  slashLast                         = INDEX(Name,'/',BACK=.true.)
  Continent                         = Name(1:slash-1)
  City                              = Name(slash+1: )

  CurrentLocale%Name                = TRIM(Name)
  CurrentLocale%DayLightSavingsTime = DayLightSavingsString
  CurrentLocale%StandardTime        = StandardTimeString
  CurrentLocale%dHoursSTD           = dHoursSTD
  CurrentLocale%dHoursDST           = dHoursDST
  CurrentLocale%Continent           = Continent
  CurrentLocale%City                = City

  NameCurrentLocale                = TRIM(Name)

  LocaleSpecified = .true.

  RETURN
END SUBROUTINE SET_TIME_LOCALE

SUBROUTINE GetTheLatestFileInPath(Path, PathLatest, TimeLatest, FoundFile)
!+
! Description: This subroutine uses GetFileTimes to determine the latest file among a group of files
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE Portability
USE Utilities
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: Path                           ! Path to search for the latest file
CHARACTER(*), INTENT(OUT)     :: PathLatest                     ! Path for the latest file
INTEGER(MJD), INTENT(OUT)     :: TimeLatest                     ! Modified Julian Date of the latest file (10^-7 seconds)
LOGICAL(4)  , INTENT(OUT)     :: FoundFile                      ! Set true if a file was found
!
!                             Local Variables
!-----------------------------+---------------
INTEGER(4)                    :: BackSlash                      ! Last backlash character in Path
CHARACTER(64)                 :: PathBase                       ! Path above the candidate files (cannot contain wildcards
CHARACTER(64)                 :: FileTemplate                   ! Template of eligible files (can contain wildcards)
CHARACTER(1),ALLOCATABLE      :: File_m_f(:,:)                  ! List of Files found in a Directory
INTEGER(4)                    :: fEnd                           ! Number of Files found in a Directory
INTEGER(4)                    :: f                              ! Index through the files found in a Directory
CHARACTER(64)                 :: File                           ! Candidate File
CHARACTER(1)                  :: File_m(64)                     ! Candidate File equivalence
EQUIVALENCE                     (File, File_m)
INTEGER(8)                    :: TmSystemLatest                 ! Windows time of latest write (10^-7 seconds)
INTEGER(8)                    :: TmSystemCreate                 ! System Time of creation      (10^-7 seconds)
INTEGER(8)                    :: TmSystemWrite                  ! System Time of last write    (10^-7 seconds)
INTEGER(8)                    :: TmSystemAccess                 ! Windows Time of last access  (10^-7 seconds)
INTEGER(4)                    :: StatusResult                   ! Status return from GetTimesForFile
CHARACTER(128)                :: PathFile                       ! Path of a candidate file
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  !
  ! Initialize return values
  PathLatest  = " "
  TimeLatest  = 0
  FoundFile   = .false.

  BackSlash    = INDEX(Path,SlashBackward,BACK=.true.);   IF(BackSlash < 3) RETURN
  PathBase     = TRIM(Path(1:BackSlash-1))
  IF(LEN_TRIM(Path) > BackSlash) THEN
    FileTemplate = Path(BackSlash+1:)
  ELSE
    FileTemplate = "*"
  END IF
  ALLOCATE(File_m_f(64,64), SOURCE = " ")
  CALL GetFilesInPathBase(TRIM(PathBase), TRIM(FileTemplate), File_m_f, fEnd)

  !Initialize the latest file time
  TmSystemLatest = 0
  !
  DO f = 1, fEnd
    File_m   = File_m_f(:,f)
    PathFile = TRIM(PathBase)//SlashBackward//TRIM(File)
    CALL GetTimesForFile(PathFile, TmSystemCreate, TmSystemWrite, TmSystemAccess, StatusResult)
    IF (StatusResult == PORT_S_NORMAL) THEN
      IF (TmSystemWrite > TmSystemLatest) THEN
        FoundFile  = .true.
        PathLatest = PathFile
        TmSystemLatest = TmSystemWrite
      END IF
    END IF
  END DO
  DEALLOCATE(File_m_f)
  !
  ! If we found a time then convert from Windows Time to Modified Julian Data
  IF (FoundFile) THEN
    !
    ! Convert to Modified Julian Date
    TimeLatest = TimeFromTimeWindows(TmSystemLatest)
  END IF
  RETURN
END SUBROUTINE GetTheLatestFileInPath

SUBROUTINE GetTheLatestFileInPathTree(PathTree, PathLatest, TimeLatest, FoundFile)
!+
! Description: This subroutine takes as input a PathTree which is assumed to have the following structure:
!
!               PathBase\FolderTemplate\FileTemplate
!
!               The PathBase\FolderTemplate is retrieved into a local string, DirectoryTemplate
!
!               No wildcards are allowed in PathBase
!
!               The structure of storage assumed is two levels below PathBase:
!
!                                      PathBase
!                Level 1 (Directories)    |__ Directory1  ......   __ DirectoryN
!                Level 2 (Files  )          |__ File(s)         |__ File(s)
!
!               The Path to the File with the latest timestamp is returned in PathLatest
!
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE Portability
USE Utilities
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: PathTree                       ! Path to search for the latest file
CHARACTER(*), INTENT(OUT)     :: PathLatest                     ! Path for the latest file
INTEGER(MJD), INTENT(OUT)     :: TimeLatest                     ! Modified Julian Date of the latest file (10^-7 seconds)
LOGICAL(4)  , INTENT(OUT)     :: FoundFile                      ! Set true if a file was found



!
!                             Local Variables
!-----------------------------+---------------
INTEGER(4)                    :: BackSlashLast                  ! Location of last backslash in a Path
INTEGER(4)                    :: BackSlashNext                  ! Location of next to last backslash in a Path
CHARACTER(64)                 :: FileTemplate                   ! Template of eligible files (can contain wildcards)
CHARACTER(80)                 :: DirectoryTemplate              ! Template of eligible directories (can contain wildcards)
CHARACTER(80)                 :: PathBase                       ! Path above the directories (cannot contain wildcards
CHARACTER(96)                 :: PathBaseFiles                  ! Path of Directory above the files
CHARACTER(1),ALLOCATABLE      :: Directory_m_d(:,:)             ! List of Directories found below PathBase
INTEGER(4)                    :: dEnd                           ! Number of Directories found below PathBase
INTEGER(4)                    :: d                              ! Index through the Directories found below PathBase
CHARACTER(80)                 :: Directory                      ! Candidate Directory
CHARACTER(1)                  :: Directory_m(80)                ! Candidate Directory equivalence
EQUIVALENCE                     (Directory, Directory_m)
CHARACTER(1),ALLOCATABLE      :: File_m_f(:,:)                  ! List of Files found in a Directory
INTEGER(4)                    :: fEnd                           ! Number of Files found in a Directory
INTEGER(4)                    :: f                              ! Index through the files found in a Directory
CHARACTER(64)                 :: File                           ! Candidate File
CHARACTER(1)                  :: File_m(64)                     ! Candidate File equivalence
EQUIVALENCE                     (File, File_m)
INTEGER(8)                    :: TmSystemCreate                 ! System Time of creation    (10^-7 seconds)
INTEGER(8)                    :: TmSystemWrite                  ! System Time of last write  (10^-7 seconds)
INTEGER(8)                    :: TmSystemAccess                 ! System Time of last access (10^-7 seconds)
INTEGER(4)                    :: statusResult                   ! Status return from GetTimesForFile
INTEGER(8)                    :: TmSystemLatest                 ! Time of latest File write  (10^-7 seconds)
CHARACTER(128)                :: PathFile                       ! Path of a candidate file
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
  !
  ! Initialize return values
  PathLatest  = " "
  TimeLatest  = 0
  FoundFile   = .false.
  DO
    ! Separate the File Template and the Directory Template
    BackSlashLast      = INDEX(PathTree,SlashBackward,BACK=.true.)                   ; IF(BackslashLast < 2) EXIT
    FileTemplate       = PathTree(BackSlashLast+1:)
    BackSlashNext      = INDEX(PathTree(1:BackSlashLast-1),SlashBackward,BACK=.true.); IF(BackSlashNext < 2) EXIT
    DirectoryTemplate  = PathTree(BackSlashNext+1:BackSlashLast-1)
    !
    ! Get the Base Path.  It cannot contain wildcards.
    PathBase           = PathTree(1:BackSlashNext-1)                                 ; IF(INDEX(PathBase,"*") > 0) EXIT

    ! Allocate the list of Directories and retrieve them
    ALLOCATE(Directory_m_d(80,64), SOURCE = " ")
    CALL GetFoldersInPathBase(TRIM(PathBase), TRIM(DirectoryTemplate), Directory_m_d, dEnd)

    !Initialize the latest file time
    TmSystemLatest = 0
    !
    DO d = 1, dEnd

      Directory_m = Directory_m_d(:,d)
      PathBaseFiles = TRIM(PathBase)//SlashBackward//TRIM(Directory)

      ALLOCATE(File_m_f(64,64), SOURCE = " ")
      CALL GetFilesInPathBase(TRIM(PathBaseFiles), TRIM(FileTemplate), File_m_f, fEnd)
      !
      DO f = 1, fEnd
        File_m   = File_m_f(:,f)
        PathFile = TRIM(PathBaseFiles)//SlashBackward//TRIM(File)
        CALL GetTimesForFile(PathFile, TmSystemCreate, TmSystemWrite, TmSystemAccess, statusResult)
        IF (StatusResult == PORT_S_NORMAL) THEN
          IF (TmSystemWrite > TmSystemLatest) THEN
            FoundFile  = .true.
            PathLatest = PathFile
            TmSystemLatest = TmSystemWrite
          END IF
        END IF
      END DO
      DEALLOCATE(File_m_f)
    END DO
    DEALLOCATE(Directory_m_d)
    !
    ! If we found a time then convert from Windows Time to Modified Julian Data
    IF (FoundFile) THEN
      !
      ! Convert to Modified Julian Date
      TimeLatest = TimeFromTimeWindows(TmSystemLatest)
    END IF
    EXIT
  END DO
  RETURN

END SUBROUTINE GetTheLatestFileInPathTree

SUBROUTINE GetFileInfo(PathFile, FileInformation, Found)
!
! Description: This subroutine get the file creation, last write and last accessed times for a file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Portability
USE Utilities
IMPLICIT NONE
!                                 Passed                             Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*)                  :: PathFile                         ! Fully qualified path of the file
TYPE(FileInfo)                :: FileInformation                  ! File information
LOGICAL(4)                    :: Found                            ! True if the file exists
!
!                                Local
!-------------------------------+---------------
INTEGER(8)                    :: TmSystemCreate                   ! Windows Time of creation    (10^-7 seconds)
INTEGER(8)                    :: TmSystemWrite                    ! Windows Time of last write  (10^-7 seconds)
INTEGER(8)                    :: TmSystemAccess                   ! Windows Time of last access (10^-7 seconds)
INTEGER(4)                    :: StatusResult                     ! Return status form GetTimesForFil
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
!
  CALL GetTimesForFile(PathFile, TmSystemCreate, TmSystemWrite, TmSystemAccess, StatusResult)

  Found = PORT_S_NORMAL == StatusResult

  IF(.not.Found) THEN
    FileInformation%TimeCreation   = 0
    FileInformation%TimeLastWrite  = 0
    FileInformation%TimeLastAccess = 0
    FileInformation%Length         = 0
    FileInformation%IsDirectory    = .false.
    FileInformation%Name           = " "
    RETURN
  END IF

  FileInformation%TimeCreation   = TimeFromTimeWindows(TmSystemCreate)
  FileInformation%TimeLastWrite  = TimeFromTimeWindows(TmSystemWrite)
  FileInformation%TimeLastAccess = TimeFromTimeWindows(TmSystemAccess)
  FileInformation%Name           = TRIM(PathFile)

  CALL InquireDirectory(TRIM(PathFile), FileInformation%IsDirectory)

  IF(FileInformation%IsDirectory) THEN
    FileInformation%Length = 0
    RETURN
  END IF

  INQUIRE(FILE = TRIM(PathFile), SIZE = FileInformation%Length, EXIST = Found, ERR = 1)
  RETURN

1 Found = .false.
  RETURN
END SUBROUTINE GetFileInfo

SUBROUTINE ChangeFileTime(PathFile, TimeNew)
!+
!
! Description: This subroutine updates the last accessed time stamp of a file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Portability
USE Utilities
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: PathFile                       ! File path
INTEGER(MJD), INTENT(IN)      :: TimeNew                        ! Modified Julian Date for the file (10^-7 seconds)
!
!                              Local
!-----------------------------+---------------
LOGICAL(4)                    :: Found                          ! Found the file information
INTEGER(8)                    :: TmSystemCreate                 ! Windows Time of creation    (10^-7 seconds)
INTEGER(8)                    :: TmSystemWrite                  ! Windows Time of last write  (10^-7 seconds)
INTEGER(8)                    :: TmSystemAccess                 ! Windows Time of last access (10^-7 seconds)
INTEGER(4)                    :: StatusResult                   ! Return status form GetTimesForFil
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
!
  INQUIRE(FILE=PathFile, EXIST = Found)
  IF(.not.Found) THEN
    WRITE(*,FMT='(A)') "ChangeFileTime%Error for "//TRIM(PathFile)//" File Does not exist."
    RETURN
  ELSE IF(TimeNew < 0) THEN
    WRITE(*,FMT='(A)') "ChangeFileTime%Error for "//TRIM(PathFile)//" TimeNew is less than 0."
    RETURN
  END IF
  IF(.not.Found) THEN
    RETURN
  END IF

  TmSystemWrite  = TimeWindowsFromTime(TimeNew)
  TmSystemCreate = 0
  TmSystemAccess = 0

  CALL SetTimesForFile(PathFile, TmSystemCreate, TmSystemWrite, TmSystemAccess, StatusResult)

  IF(StatusResult == PORT_E_FAILURE) THEN
    WRITE(*,FMT='(A)') "ChangeFileTime: Error changing the write time."
  ENDIF

END SUBROUTINE ChangeFileTime

SUBROUTINE ChangeFileTimeAndDate(Filename, Year, Month, Day, Hour, Minute, Second, Error)
!+
!
! Description: This subroutine updates the last accessed time stamp of a file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: Filename                       ! File path
INTEGER(4)  , INTENT(IN)      :: Year                           ! Change time Year   (Year  )
INTEGER(4)  , INTENT(IN)      :: Month                          ! Change time Month  (Month )
INTEGER(4)  , INTENT(IN)      :: Day                            ! Change time Day    (Day   )
INTEGER(4)  , INTENT(IN)      :: Hour                           ! Change time Hour   (Hour  )
INTEGER(4)  , INTENT(IN)      :: Minute                         ! Change time Minute (Minute)
INTEGER(4)  , INTENT(IN)      :: Second                         ! Change time Second (Second)
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Error in changing file
!
!                              Local
!-----------------------------+---------------
INTEGER(MJD)                  :: TimeTarget                     ! New time, Modified Julian Date
INTEGER(4)                    :: DayMax_Month(0:13)
CHARACTER(24)                 :: TimeDateString
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
!
  DO
    Error = .not.(WithinClosedSet(1601, Year, 2601) .and. WithinClosedSet(1, Month, 12)) ; IF(Error) EXIT

    IF(LeapYear(year)) THEN
      DayMax_month = INT(DaysLeapYear_Month  , KIND=4)
    ELSE
      DayMax_month = INT(DaysNormalYear_Month, KIND=4)
    END IF

    Error = .not.WithinClosedSet(1, Day, DayMax_Month(Month))   .or. &
            .not.WithinClosedSet(0, Hour  , 23)                 .or. &
            .not.WithinClosedSet(0, Minute, 59)                 .or. &
            .not.WithinClosedSet(0, Second, 99)                                          ; IF(Error) EXIT

    TimeDateString = Ascii_Day   (Day   )//"-"//  &
                     Ascii_Month (Month )//"-"//  &
                     Ascii_year  (Year  )//" "//  &
                     Ascii_Hour  (Hour  )//":"//  &
                     Ascii_Minute(Minute)//":"//  &
                     Ascii_Second(Second)//" SDT"

    TimeTarget = TimeFromTimeDateString(TimeDateString,Error)                            ; IF(Error) EXIT
    CALL ChangeFileTime(FileName, TimeTarget)
    EXIT
  END DO
  IF(Error) THEN
    WRITE(*,FMT='(A)') "ChangeFileTimeAndDate.Failed"
  END IF
  RETURN
END SUBROUTINE ChangeFileTimeAndDate

SUBROUTINE SetGeoLocationForTime(Latitude, Longitude, Elevation, Error)
!+
!
! Description: This subroutine sets the latitude and longitude for calculations in the TimeDatePackage.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 01-MAR-2025     | MA    | SCR 150: Updat the moduls WGS84 Location
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
USE EphemerisTypes
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8),    INTENT(IN)        :: Latitude                       ! Geocentric Latitude +North    (degrees)
REAL(8),    INTENT(IN)        :: Longitude                      ! Geocentric Longitude +East    (degrees)
REAL(8),    INTENT(IN)        :: Elevation                      ! Elevation above mean sea level     (km)
LOGICAL(4), INTENT(OUT)       :: Error                          ! Invalid Inputs
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! Ensure the inputs are with bounds
  Error = .not. WithinClosedSet(-180d0 , Longitude, +180d0) .or. &
          .not. WithinClosedSet( -90d0 , Latitude ,  +90d0) .or. &
          .not. WithinClosedSet( -0.5d0, Elevation,  +10d0)
  IF(Error) RETURN

  ! Update the Location
  GeoLocationSpecified      = .true.
  LocationForTime%Latitude  = Latitude
  LocationForTime%Longitude = Longitude
  LocationForTime%Elevation = Elevation
  IF(Longitude==-180d0) THEN
   LocationForTime%Longitude = 180d0
  END IF
  RETURN
END SUBROUTINE SetGeoLocationForTime
!
SUBROUTINE ConvertTimeToLocalTimeStamp(TimeInput, TimeStamp, FormatTime)
!+
!
! Description: This subroutine formats a local timestamp and zone for a Modified Julian Date..
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN )    :: TimeInput
CHARACTER(24), INTENT(OUT)    :: TimeStamp
CHARACTER(4) , INTENT(OUT)    :: FormatTime
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL Convert_Time_To_Local_Timestamp(TimeInput, TimeStamp, FormatTime)
  RETURN
END SUBROUTINE ConvertTimeToLocalTimeStamp
!
SUBROUTINE ConvertTimeToSystemTimeStamp(TimeInput, TimeStamp, FormatTime)
!+
!
! Description: This subroutine formats a system timestamp and zone for a Modified Julian Date..
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 11-MAR-2025     | MA    | SCR 150: Add the Locale as an argument to accommodate different locations
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN )    :: TimeInput
CHARACTER(24), INTENT(OUT)    :: TimeStamp
CHARACTER(4) , INTENT(OUT)    :: FormatTime
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL Convert_Time_To_System_Timestamp(TimeInput, TimeStamp, FormatTime)
  RETURN
END SUBROUTINE ConvertTimeToSystemTimeStamp
!
SUBROUTINE ConvertTimeToUTCTimeStamp(TimeInput, TimeStamp, Zone)
!+
!
! Description: This subroutine formats a UTC timestamp and zone for a Modified Julian Date..
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN )     :: TimeInput
CHARACTER(*), INTENT(OUT)     :: TimeStamp
CHARACTER(*), INTENT(OUT)     :: Zone
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL Convert_Time_To_UTC_Timestamp(TimeInput, TimeStamp, Zone)
  RETURN
END SUBROUTINE ConvertTimeToUTCTimeStamp

SUBROUTINE CONVERT_TIME_TO_LOCAL_TIMESTAMP(Time,TimeStamp,FormatTime)
!+
!
! Description: This subroutine formats a local timestamp and zone for a Modified Julian Date..
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN )     :: Time
CHARACTER(*), INTENT(OUT)     :: TimeStamp
CHARACTER(*), INTENT(OUT)     :: FormatTime
!
!                                Local
!-----------------------------+-------
TYPE(TimeDate)                :: DateAndTime
TYPE(TimeDate)                :: DateAndTimeConverted
REAL(4)                       :: dHoursLocal
LOGICAL(4)                    :: Error
LOGICAL(4)  ,PARAMETER        :: DebugLocal = .false. !
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL CalculateDateFromTime(Time, DateAndTime)
  IF(DebugLocal) WRITE(*,FMT='("ConvertLocal: Locale = ",I4)') LocaleCurrent
  IF(DebugLocal) WRITE(*,FMT='("ConvertLocal: Y,M,D,h,m,s,dh = ",6(I4,1X),F6.2)')DateAndTime%Year        &
                                                                               , DateAndTime%Month       &
                                                                               , DateAndTime%Day         &
                                                                               , DateAndTime%Hour        &
                                                                               , DateAndTime%Minute      &
                                                                               , DateAndTime%Second      &
                                                                               , DateAndTime%dHourZone
  CALL AdjustDaylightSavings(Time, LocaleCurrent, FormatTime, dHoursLocal)
  IF(DebugLocal) WRITE(*,FMT='("ConvertLocal: dHoursLocal, FormatTime = ",F6.3,1X,A4)') dHoursLocal, FormatTime

  CALL ConvertDateAndTimeToTimezone(dHoursLocal, DateAndTime, DateAndTimeConverted)
  CALL FormTimeStamp(DateAndTimeConverted,TimeStamp,Error)
RETURN
END SUBROUTINE CONVERT_TIME_TO_LOCAL_TIMESTAMP

SUBROUTINE CONVERT_TIME_TO_SYSTEM_TIMESTAMP(Time,TimeStamp,FormatTime)
!+
!
! Description: This subroutine formats a system timestamp and zone for a Modified Julian Date..
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 11-MAR-2025     | MA    | SCR 150: Add the Locale as an argument to accommodate different locations
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!USE IFPOSIX
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN )     :: Time
CHARACTER(*), INTENT(OUT)     :: TimeStamp
CHARACTER(*), INTENT(OUT)     :: FormatTime
!
!                                Local
!-----------------------------+-------
TYPE(TimeDate)                :: DateAndTime
TYPE(TimeDate)                :: DateAndTimeConverted
REAL(4)                       :: dHoursLocal
LOGICAL(4)                    :: Error
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL CalculateDateFromTime(Time, DateAndTime)
  CALL AdjustDaylightSavings(Time, LocaleSystem, FormatTime, dHoursLocal)

  CALL ConvertDateAndTimeToTimezone(dHoursLocal, DateAndTime, DateAndTimeConverted)
  CALL FormTimeStamp(DateAndTimeConverted,TimeStamp,Error)
RETURN
END SUBROUTINE CONVERT_TIME_TO_SYSTEM_TIMESTAMP
!
SUBROUTINE CONVERT_TIME_TO_UTC_TIMESTAMP(Time,TimeStamp,FormatTime)
!+
!
! Description: This subroutine formats a UTC timestamp and zone for a Modified Julian Date..
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN )     :: Time
CHARACTER(*), INTENT(OUT)     :: TimeStamp
CHARACTER(*), INTENT(OUT)     :: FormatTime
!
!                                Local
!-----------------------------+-------
TYPE(TimeDate)                :: DateAndTime
LOGICAL(4)                    :: error
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL CalculateDateFromTime(Time,DateAndTime)
DateAndTime%dHourZone = 0.0
CALL FormTimeStamp(DateAndTime, TimeStamp, Error)
FormatTime = 'UTC'
RETURN
END SUBROUTINE CONVERT_TIME_TO_UTC_TIMESTAMP
!
SUBROUTINE CalculateISODateFromTime(Time,DateAndTime)
!+
!
! Description: This subroutine converts Modified Julian Date to an ISO 8601:1988 time in the current Locale
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Ensure routine is thread safe by removing saved local variables
! 21-MAR-2025     | MA    | SCR 150: Added support to calculate an ISO 8601:1988 date and time from MJD time.
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD)     ,INTENT(IN)  :: Time                           ! Modified Julian Date                    (10^-7 seconds)
TYPE(TimeDateISO),INTENT(OUT) :: DateAndTime                    ! Date and Time ISO structure Local Time
!
!                                Local
!-----------------------------+--------
INTEGER(MJD)                  :: TimeLocal                      ! Modified Julian Date shifted to local  (10^-7 seconds)
REAL(8)                       :: dHoursLocal                    ! Hours shift from UTC                           (hours)
INTEGER(4)                    :: ZoneUse                        ! Timezone of the locale that is used
CHARACTER(8)                  :: AbbrZone                       ! Time Zone abbreviation (e.g. PDT, PST)        (string)
INTEGER(4)                    :: RuleInfo                       ! Index of Zone Rule information used
INTEGER(4)                    :: StatusAdjust                   ! Status returned from AdjustTimeZoneForLocale
INTEGER(4)                    :: year                           ! year component of DateAndTime                   (year)
INTEGER(8)                    :: month                          ! month component of DateAndTime                 (month)
INTEGER(8)                    :: day                            ! day component of DateAndTime                     (day)
INTEGER(8)                    :: hour                           ! hour component of DateAndTime                   (hour)
INTEGER(8)                    :: minute                         ! minute component of DateAndTime               (minute)
INTEGER(8)                    :: second                         ! second component of DateAndTime               (second)
INTEGER(8)                    :: mSec                           ! mSec component of DateAndTime           (milliseconds)
INTEGER(MJD)                  :: dTimeLeft                      ! Running total of time left             (10^-7 seconds)
INTEGER(4)                    :: DayMax_month(0:13)             ! Last day of each month                           (day)
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time elapsed in year for each month    (10^-7 seconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Shift Time to the local time zone
  CALL AdjustTimeZoneForLocale(LocaleCurrent, Time, dHoursLocal, ZoneUse, AbbrZone, RuleInfo, StatusAdjust)
  TimeLocal = Time + INT(dHoursLocal * 60d0, KIND=8) * TimePerMinute

  year = yearEpoch + INT(TimeLocal/TimePerYear, KIND=4)
  IF(TimeEnd_year(year) < 0) THEN
    IF(TimeEnd_year(year-1) >= TimeLocal) THEN
      year = year-1
    END IF
  ELSE IF(TimeLocal > TimeEnd_year(year)) THEN
    year = year + 1
  ELSE IF(TimeLocal < TimeStart_year(year)) THEN
    year = year - 1
  END IF

  IF(LeapYear(year)) THEN
    DayMax_month = DaysLeapYear_month
    dTime_month = dTimeLeapYear_month
  ELSE
    DayMax_month = DaysNormalYear_month
    dTime_month = dTimeNormalYear_month
  END IF

  CALL AdjustMonthTimesForLeapSeconds(year,dTime_month)

  dTimeLeft = TimeLocal - TimeStart_year(year)
  DO month = January, December
    IF(dTime_month(month) < dTimeLeft) CYCLE
    dTimeLeft = dTimeLeft - (dTime_month(month-1) + 1)
    Day       = MIN((dTimeLeft+TimePerDay)/TimePerDay, DayMax_month(month))
    dTimeLeft = dTimeLeft - TimePerDay    * (Day-1)
    Hour      = MIN(dTimeLeft/TimePerHour,  23)
    dTimeLeft = dTimeLeft - TimePerHour   * Hour
    Minute    = MIN(dTimeLeft/TimePerMinute,59)
    dTimeLeft = dTimeLeft - TimePerMinute * Minute
    Second    = dTimeLeft/TimePerSecond
    dTimeLeft = dTimeLeft - TimePerSecond * Second
    mSec      = dTimeLeft/TimePerMsec
    dTimeLeft = dTimeLeft - TimePerMsec * mSec
    IF(dTimeLeft >= TimePerMsec/2) mSec = mSec+1
    DateAndTime%Year        = Year
    DateAndTime%Month       = INT(Month , KIND=4)
    DateAndTime%Day         = INT(Day   , KIND=4)
    DateAndTime%dMinuteZone = INT(dHoursLocal * 60d0, KIND=4)
    DateAndTime%Hour        = INT(Hour  , KIND=4)
    DateAndTime%Minute      = INT(Minute, KIND=4)
    DateAndTime%Second      = INT(Second, KIND=4)
    DateAndTime%mSec        = INT(mSec  , KIND=4)
    EXIT
  END DO
  RETURN
END SUBROUTINE CalculateISODateFromTime
!
FUNCTION DateAndTimeLocalFormat(Time) RESULT(DateAndTimeLocal)
!+
!
! Description: This function formats a Modified Julian Date input as a TimeDate structure with the local time zone
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!USE IFPOSIX
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN )    :: Time                           ! Modified Julian Date (10^-7 seconds)
TYPE(TimeAndDate)             :: DateAndTimeLocal               ! Date and Time structure set to local time zone
!
!                                Local
!-----------------------------+-------
TYPE(TimeDate)                :: DateAndTime
TYPE(TimeDate)                :: DateAndTimeConverted
CHARACTER(4)                  :: FormatTime
CHARACTER(24)                 :: TimeStamp
REAL(4)                       :: dHoursLocal
LOGICAL(4)                    :: Error
!
  CALL CalculateDateFromTime(Time, DateAndTime)
  CALL AdjustDaylightSavings(Time, LocaleCurrent, FormatTime, dHoursLocal)
  CALL ConvertDateAndTimeToTimezone(dHoursLocal, DateAndTime, DateAndTimeConverted)
  CALL FormTimeStamp(DateAndTimeConverted,TimeStamp,Error)
  DateAndTimeLocal%Year       =  DateAndTimeConverted%Year
  DateAndTimeLocal%Month      =  DateAndTimeConverted%Month
  DateAndTimeLocal%Day        =  DateAndTimeConverted%Day
  DateAndTimeLocal%dHourZone  =  DateAndTimeConverted%dHourZone
  DateAndTimeLocal%Hour       =  DateAndTimeConverted%Hour
  DateAndTimeLocal%Minute     =  DateAndTimeConverted%Minute
  DateAndTimeLocal%Second     =  DateAndTimeConverted%Second
  DateAndTimeLocal%msec       =  DateAndTimeConverted%msec
  DateAndTimeLocal%usec       =  DateAndTimeConverted%usec
  DateAndTimeLocal%nsec       =  DateAndTimeConverted%nsec
  DateAndTimeLocal%dayOfWeek  =  DateAndTimeConverted%dayOfWeek
  DateAndTimeLocal%dayOfYear  =  DateAndTimeConverted%dayOfYear
  DateAndTimeLocal%TimeStamp  =  TimeStamp
  DateAndTimeLocal%FormatTime =  FormatTime
  RETURN
END FUNCTION DateAndTimeLocalFormat
!
FUNCTION DateAndTimeSystemFormat(Time) RESULT(DateAndTimeSystem)
!+
!
! Description: This function formats a Modified Julian Date input as a TimeDate structure with the system time zone
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!USE IFPOSIX
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(8),   INTENT(IN )     :: Time                           ! Modified Julian Date (10^-7 seconds)
TYPE(TimeAndDate)             :: DateAndTimeSystem              ! Date and Time structure set to system time zone
!
!                                Local
!-----------------------------+-------
TYPE(TimeDate)                :: DateAndTime
TYPE(TimeDate)                :: DateAndTimeConverted
CHARACTER(4)                  :: FormatTime
CHARACTER(24)                 :: TimeStamp
REAL(4)                       :: dHoursLocal
LOGICAL(4)                    :: Error                          !Invalid date and time

  !
  CALL CalculateDateFromTime(Time, DateAndTime)

  CALL AdjustDaylightSavings(Time, LocaleSystem, FormatTime, dHoursLocal)

  CALL ConvertDateAndTimeToTimezone(dHoursLocal, DateAndTime, DateAndTimeConverted)

  CALL FormTimeStamp(DateAndTimeConverted,TimeStamp,Error)

  DateAndTimeSystem%Year       =  DateAndTimeConverted%Year
  DateAndTimeSystem%Month      =  DateAndTimeConverted%Month
  DateAndTimeSystem%Day        =  DateAndTimeConverted%Day
  DateAndTimeSystem%dHourZone  =  DateAndTimeConverted%dHourZone
  DateAndTimeSystem%Hour       =  DateAndTimeConverted%Hour
  DateAndTimeSystem%Minute     =  DateAndTimeConverted%Minute
  DateAndTimeSystem%Second     =  DateAndTimeConverted%Second
  DateAndTimeSystem%msec       =  DateAndTimeConverted%msec
  DateAndTimeSystem%usec       =  DateAndTimeConverted%usec
  DateAndTimeSystem%nsec       =  DateAndTimeConverted%nsec
  DateAndTimeSystem%dayOfWeek  =  DateAndTimeConverted%dayOfWeek
  DateAndTimeSystem%dayOfYear  =  DateAndTimeConverted%dayOfYear
  DateAndTimeSystem%TimeStamp  =  TimeStamp
  DateAndTimeSystem%FormatTime =  FormatTime
  RETURN
END FUNCTION DateAndTimeSystemFormat
!
FUNCTION TimesForDate(DateString) RESULT(TimesForDay_n)
!
! Description: Retrieve the character code of standard time for the Locale
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(11),INTENT(IN)      :: DateString                     ! Date string (either dd-MMM-YYYY or dd-mm-yyyy
INTEGER(8)                    :: TimesForDay_n(3)               ! Modified Julian Day for Midnight, Noon and End of Day (10^-7 seconds)
!
!                                Local
!-----------------------------+-----------
INTEGER(4)                    :: Year                           ! Year  number (Year )
INTEGER(4)                    :: Month                          ! Month number (Month)
INTEGER(4)                    :: Day                            ! Day   number (Day  )
LOGICAL(4)                    :: Error                          ! Error return from ParseDate
INTEGER(MJD)                  :: TimeMidnight                   ! Modified Julian Day for Midnight (10^-7 seconds)
INTEGER(MJD)                  :: TimeNoon                       ! Modified Julian Day for Noon (10^-7 seconds)
INTEGER(MJD)                  :: TimeDayEnd                     ! Modified Julian Day for Day end(10^-7 seconds)
CHARACTER(24)                 :: TimeStamp                      ! Time stamp created
CHARACTER(4)                  :: AbbrZone                       ! Time Zone
!
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL ParseDate(DateString, Year, Month, Day, error)
  TimeNoon = TimeFromTimeDateString(Ascii_Day(Day)//"-"//Ascii_Month(Month)//"-"//Ascii_year(Year)//" 12:00:00SDT", Error)

  CAlL CONVERT_TIME_TO_LOCAL_TIMESTAMP(TimeNoon, TimeStamp, AbbrZone)
  IF(TimeStamp(13:14)=='11')      THEN ; TimeNoon = TimeNoon + TimePerHour
  ELSE IF(TimeStamp(13:14)=='13') THEN ; TimeNoon = TimeNoon - TimePerHour
  END IF

  TimeMidnight = TimeNoon - 12*TimePerHour

  CAlL CONVERT_TIME_TO_LOCAL_TIMESTAMP(TimeMidnight, TimeStamp, AbbrZone)
  IF(TimeStamp(13:14)=='11')      THEN ; TimeMidnight = TimeMidnight + TimePerHour
  ELSE IF(TimeStamp(13:14)=='13') THEN ; TimeMidnight = TimeMidnight - TimePerHour
  END IF

  TimeDayEnd = TimeNoon + 12*TimePerHour

  TimesForDay_n = (/TimeMidnight, TimeNoon, TimeDayEnd/)
  RETURN
END FUNCTION TimesForDate

SUBROUTINE ExtractTimesForRules(TimeDateString, Year, Month, Day, Hour, Minute, Second, Msec, dHour, Error)
!
! Description: This subroutin extracts the numeric time parts from a Time/Date string.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
USE NumericalConstants
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: TimeDateString                 ! Timestamp and possible timezone
INTEGER(4),   INTENT(OUT)     :: Year                           ! Year number
INTEGER(4),   INTENT(OUT)     :: Month                          ! Month number
INTEGER(4),   INTENT(OUT)     :: Day                            ! Day of Month number
INTEGER(4),   INTENT(OUT)     :: Hour                           ! Hour number
INTEGER(4),   INTENT(OUT)     :: Minute                         ! Hour number
INTEGER(4),   INTENT(OUT)     :: Second                         ! Second number
INTEGER(4),   INTENT(OUT)     :: Msec                           ! Millisecond number
REAL(4),      INTENT(OUT)     :: dHour                          ! Hour Offset for timezone
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Set true for invalid timestamp
!
!                                Local
!-------------------------------------
CHARACTER(32)                 :: DateString
CHARACTER(16)                 :: TimeString
CHARACTER(16)                 :: ZoneString
INTEGER(4)                    :: monthNum
INTEGER(4)                    :: yearNum
INTEGER(4)                    :: dayNum
INTEGER(4)                    :: hourNum
INTEGER(4)                    :: minuteNum
INTEGER(4)                    :: secondNum
INTEGER(4)                    :: msecNum
REAL(4)                       :: dHourNum
REAL(4)                       :: signZone
LOGICAL(4)                    :: ErrorZone
LOGICAL(4)                    :: ErrorTime
LOGICAL(4)                    :: ErrorDate
INTEGER(4)                    :: HourZone
INTEGER(4)                    :: MinuteZone
INTEGER(4)                    :: SecondZone
INTEGER(4)                    :: MsecZone
TYPE(TimeDate)                :: DateAndTime                    !Local date and time structure
INTEGER(MJD)                  :: Time
INTEGER(MJD)                  :: TimeSolarNoon
INTEGER(MJD)                  :: TimeSunrise
INTEGER(MJD)                  :: TimeSunset
REAL(8)                       :: HoursOfSunlight
CHARACTER(4)                  :: FormatTime
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!

!-- Initialize the return values
Error  = .true.
Year   = YearEpoch
Month  = MonthEpoch
Day    = DayEpoch
Hour   = 0
Minute = 0
Second = 0
Msec   = 0
dHour  = 0.0
IF(.not.Initialized .or. .not.GeoLocationSpecified) THEN
  RETURN
END IF

!-- Parse into Date, Time and TimeZone parts
CALL GetDateAndTimeParts(TimeDateString,DateString, TimeString, ZoneString)

!--Process the Date
CALL ParseDate(TRIM(DateString), YearNum, MonthNum , DayNum, errorDate)
   !
   !-- Return if date is invalid
   IF(ErrorDate) RETURN

!-- Process the Time
CALL ParseTime(TRIM(TimeString), HourNum, MinuteNum, SecondNum, MsecNum, errorTime)
   !
   !-- Return if time is invalid
   IF(ErrorTime) RETURN

!-- Parse the timeZone
errorZone = .false.

IF(TRIM(ZoneString)==TRIM(CurrentLocale%DaylightSavingsTime).or.TRIM(ZoneString)=='DST') THEN
  dHourNum = CurrentLocale%dHoursDST

ELSE IF(TRIM(ZoneString)==TRIM(CurrentLocale%StandardTime).or.TRIM(ZoneString)=='SDT'.or.TRIM(ZoneString)=='STD') THEN
  dHourNum = CurrentLocale%dHoursSTD


ELSE IF(TRIM(ZoneString)=='LMT') THEN
  DateAndTime%Year      = YearNum
  DateAndTime%Month     = MonthNum
  DateAndTime%Day       = DayNum
  DateAndTime%Hour      = 12
  DateAndTime%Minute    = 0
  DateAndTime%Second    = 0
  DateAndTime%msec      = 0
  DateAndTime%usec      = 0
  DateAndTime%nsec      = 0
  DateAndTime%dHourZone = 0.0
  CALL CalculateTimeFromDate(Time, DateAndTime)
  CALL ApproximateDailySolarData(Time, LocationForTime, &
                                   HoursOfSunLight, TimeSolarNoon, TimeSunrise, TimeSunset)
  Time  = TimeSolarNoon + (HourNum-12)*TimePerHour &
                        +  MinuteNum*TimePerMinute &
                        +  SecondNum*TimePerSecond &
                        +  MsecNum*TimePerMsec
  CALL AdjustDaylightSavings(Time, LocaleCurrent, FormatTime, dHourNum)
  CALL CalculateDateFromTime(Time,DateAndTime)
  CALL ConvertDateAndTimeToTimeZone(dHourNum,DateAndTime,DateAndTime)
  YearNum   = DateAndTime%Year
  MonthNum  = DateAndTime%Month
  DayNum    = DateAndTime%Day
  HourNum   = DateAndTime%Hour
  MinuteNum = DateAndTime%Minute
  SecondNum = DateAndTime%Second
  msecNum   = DateAndTime%msec
  dHourNum  = DateAndTime%dHourZone

ELSE
  IF(TRIM(ZoneString)=='UTC' .or. TRIM(ZoneString)=='Z') THEN
    dHourNum = 0.0
  ELSE
    IF(ZoneString(1:3)=='UTC') THEN
      ZoneString = ZoneString(4:)
    ELSE IF(ZoneString(1:1)=='Z') THEN
      ZoneString = ZoneString(2:)
    END IF
    signZone = 1.0
    IF(ZoneString(1:1)=='-') THEN
      ZoneString = ZoneString(2:)
      signZone   = -1.0
    END IF
    IF(INDEX(ZoneString,':')>0) THEN
      CALL ParseTime(TRIM(ZoneString),HourZone,MinuteZone,SecondZone,MsecZone, ErrorZone)
      IF(SecondZone/=0 .or. msecZone/=0) ErrorZone = .true.
      dHourNum = signZone * SNGL(DBLE(HourZone)         &
                              + DBLE(MinuteZone)/6.0d1 &
                              + DBLE(SecondZone)/3.6D3 &
                              + DBLE(MsecZone  )/3.6D6 )
    ELSE
      dHourNum = signZone * SNGL(NumAscii(ZoneString, errorZone))
    END IF
    IF(ABS(dHourNum)>12.0) errorZone = .true.
  END IF
END IF
!
!-- Return if invalid offset
IF(errorZone) RETURN

Year   = yearNum
Month  = monthNum
Day    = dayNum
Hour   = hourNum
Minute = minuteNum
Second = secondNum
Msec   = msecNum
dHour  = dHourNum
Error  = .false.


!DO WHILE(.not.Error)
!  IF(parseEnd /= 3) THEN
!    Error = .true.
!    EXIT
!  END If
!  DateParse(1) = DateString(start$chr_parse(1):end$chr_parse(1))
!  DateParse(2) = DateString(start$chr_parse(2):end$chr_parse(2))
!  DateParse(3) = DateString(start$chr_parse(3):end$chr_parse(3))
!  DO parse = 1, parseEnd
!    StringStart = start$chr_parse(parse)
!    StringEnd  =  end$chr_parse  (parse)
!  END DO
!
!
!END DO

RETURN
END SUBROUTINE ExtractTimesForRules
!
!
SUBROUTINE ExtractTimes(TimeDateString, Year, Month, Day, Hour, Minute, Second, Msec, dHour, Error)
!
! Description: This subroutine extracts the numeric time parts from a Time/Date string.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
USE NumericalConstants
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: TimeDateString                 ! Timestamp and possible timezone
INTEGER(4),   INTENT(OUT)     :: Year                           ! Year number
INTEGER(4),   INTENT(OUT)     :: Month                          ! Month number
INTEGER(4),   INTENT(OUT)     :: Day                            ! Day of Month number
INTEGER(4),   INTENT(OUT)     :: Hour                           ! Hour number
INTEGER(4),   INTENT(OUT)     :: Minute                         ! Hour number
INTEGER(4),   INTENT(OUT)     :: Second                         ! Second number
INTEGER(4),   INTENT(OUT)     :: Msec                           ! Millisecond number
REAL(4),      INTENT(OUT)     :: dHour                          ! Hour Offset for timezone
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Set true for invalid timestamp
!
!                                Local
!-------------------------------------
CHARACTER(32)                 :: DateString
CHARACTER(16)                 :: TimeString
CHARACTER(16)                 :: ZoneString
CHARACTER(64)                 :: TimeDateLocal
INTEGER(4)                    :: monthNum
INTEGER(4)                    :: yearNum
INTEGER(4)                    :: dayNum
INTEGER(4)                    :: hourNum
INTEGER(4)                    :: minuteNum
INTEGER(4)                    :: secondNum
INTEGER(4)                    :: msecNum
REAL(4)                       :: dHourNum
REAL(4)                       :: signZone
LOGICAL(4)                    :: ErrorZone
LOGICAL(4)                    :: ErrorTime
LOGICAL(4)                    :: ErrorDate
INTEGER(4)                    :: HourZone
INTEGER(4)                    :: MinuteZone
INTEGER(4)                    :: SecondZone
INTEGER(4)                    :: MsecZone
TYPE(TimeDate)                :: DateAndTime                    !Local date and time structure
INTEGER(MJD)                  :: Time
INTEGER(MJD)                  :: TimeSolarNoon
INTEGER(MJD)                  :: TimeSunrise
INTEGER(MJD)                  :: TimeSunset
REAL(8)                       :: HoursOfSunlight
CHARACTER(4)                  :: FormatTime
INTEGER(MJD)                  :: TimeLocal

INTEGER(MJD)                  :: TimeNum
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time elapsed in year for each month (10^-7 seconds)
LOGICAL(4)  ,PARAMETER        :: DebugLocal = .false.           ! Diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !-- Initialize the return values
  Error  = .true.
  Year   = YearEpoch
  Month  = MonthEpoch
  Day    = DayEpoch
  Hour   = 0
  Minute = 0
  Second = 0
  Msec   = 0
  dHour  = 0.0
  IF(.not.Initialized .or. .not.GeoLocationSpecified) THEN
    RETURN
  END IF

  !-- Parse into Date, Time and TimeZone parts
  CALL GetDateAndTimeParts(TimeDateString,DateString, TimeString, ZoneString) ; IF(DebugLocal) WRITE(*,FMT='(A)') " ExtractTimes: "//TRIM(DateString)//" "//TRIM(TimeString)//" "//ZoneString


  !--Parse the Date
  CALL ParseDate(TRIM(DateString), YearNum, MonthNum , DayNum, errorDate)
     !
     !-- Return if date is invalid
     IF(ErrorDate) RETURN

  !-- Parse the Time
  CALL ParseTime(TRIM(TimeString), HourNum, MinuteNum, SecondNum, MsecNum, errorTime)
     !
     !-- Return if time is invalid
     IF(ErrorTime) RETURN

  !-- Parse the timeZone
  errorZone = .false.

  ! ToDo SCR 150: Update this based on the new Locale design
  ! Get a time range for the String
  IF(LeapYear(year)) THEN
   dTime_month = dTimeLeapYear_month
  ELSE
   dTime_month = dTimeNormalYear_month
  END IF
  CALL AdjustMonthTimesForLeapSeconds(year,dTime_month)
  TimeNum = TimeStart_Year(YearNum) + dTime_Month(MonthNum-1) + TimePerDay*(DayNum-1)
  ! ToDo SCR: 150

  IF(TRIM(ZoneString)==TRIM(CurrentLocale%DaylightSavingsTime).or.TRIM(ZoneString)=='DST') THEN
    dHourNum = CurrentLocale%dHoursDST

  ELSE IF(TRIM(ZoneString)==TRIM(CurrentLocale%StandardTime).or.TRIM(ZoneString)=='SDT'.or.TRIM(ZoneString)=='STD') THEN
    dHourNum = CurrentLocale%dHoursSTD

  ELSE IF(TRIM(ZoneString)=="LOC" .or. LEN_TRIM(ZoneString)==0) THEN
    IF(CurrentLocale%dHoursSTD == CurrentLocale%dHoursDST) THEN
      dHourNum = CurrentLocale%dHoursSTD
    ELSE
      DateAndTime%Year      = YearNum
      DateAndTime%Month     = MonthNum
      DateAndTime%Day       = DayNum
      DateAndTime%Hour      = HourNum
      DateAndTime%Minute    = MinuteNum
      DateAndTime%Second    = SecondNum
      DateAndTime%msec      = MsecNum
      DateAndTime%usec      = 0
      DateAndTime%nsec      = 0

      DateAndTime%dHourZone = CurrentLocale%dHoursSTD
      CALL CalculateTimeFromDate(TimeLocal,DateAndTime)
      IF(.not.DaylightSavingsActive(TimeLocal, LocaleCurrent)) THEN
        dHourNum = DateAndTime%dHourZone
      ELSE
        DateAndTime%dHourZone = CurrentLocale%dHoursDST
        CALL CalculateTimeFromDate(TimeLocal, DateAndTime)
        IF(DaylightSavingsActive(TimeLocal, LocaleCurrent)) THEN
          dHourNum = DateAndTime%dHourZone
        ELSE
          ErrorZone = .true.
        END IF
      END IF
      !CALL CalculateLocalDateFromTime(TimeLocal,DateAndTime)
      !
      !DateAndTime%dHourZone = CurrentLocale%dHoursDST
      !CALL CalculateTimeFromDate      (TimeLocal,DateAndTime)
      !CALL CalculateLocalDateFromTime (TimeLocal,DateAndTimeSavings)
      !
      !IF(DateAndTimeSavings.Hour==DateAndTime%Hour) THEN
      !  ErrorZone = .true.
      !ELSE IF(DateAndTime%Hour==HourNum) THEN
      !  dHourNum = CurrentLocale%dHoursSTD
      !ELSE IF(DateAndTimeSavings.Hour==HourNum) THEN
      !  dHourNum = CurrentLocale%dHoursDST
      !ELSE
      !  ErrorZone = .true.
      !END IF
    END IF

  ELSE IF(TRIM(ZoneString)=='LMT') THEN
    DateAndTime%Year      = YearNum
    DateAndTime%Month     = MonthNum
    DateAndTime%Day       = DayNum
    DateAndTime%Hour      = 12
    DateAndTime%Minute    = 0
    DateAndTime%Second    = 0
    DateAndTime%msec      = 0
    DateAndTime%usec      = 0
    DateAndTime%nsec      = 0
    DateAndTime%dHourZone = 0.0
    CALL CalculateTimeFromDate(Time, DateAndTime)
    CALL ApproximateDailySolarData(Time, LocationForTime, &
                                 HoursOfSunLight, TimeSolarNoon, TimeSunrise, TimeSunset)
    Time  = TimeSolarNoon + (HourNum-12)*TimePerHour &
                          +  MinuteNum*TimePerMinute &
                          +  SecondNum*TimePerSecond &
                          +  MsecNum*TimePerMsec
    CALL AdjustDaylightSavings(Time, LocaleCurrent, FormatTime, dHourNum)
    CALL CalculateDateFromTime(Time,DateAndTime)
    CALL ConvertDateAndTimeToTimeZone(dHourNum, DateAndTime, DateAndTime)
    YearNum   = DateAndTime%Year
    MonthNum  = DateAndTime%Month
    DayNum    = DateAndTime%Day
    HourNum   = DateAndTime%Hour
    MinuteNum = DateAndTime%Minute
    SecondNum = DateAndTime%Second
    msecNum   = DateAndTime%msec
    dHourNum  = DateAndTime%dHourZone

  ELSE
    IF(TRIM(ZoneString)=='UTC' .or. TRIM(ZoneString)=='Z') THEN
      dHourNum = 0.0
    ELSE
      IF(ZoneString(1:3)=='UTC') THEN
        ZoneString = ZoneString(4:)
      ELSE IF(ZoneString(1:1)=='Z') THEN
        ZoneString = ZoneString(2:)
      END IF
      signZone = 1.0
      IF(ZoneString(1:1)=='-') THEN
        ZoneString = ZoneString(2:)
        signZone   = -1.0
      END IF
      IF(INDEX(ZoneString,':')>0) THEN
        CALL ParseTime(TRIM(ZoneString),HourZone,MinuteZone,SecondZone,MsecZone, ErrorZone)
        IF(SecondZone/=0 .or. msecZone/=0) ErrorZone = .true.
        dHourNum = signZone * SNGL( DBLE(HourZone)         &
                                  + DBLE(MinuteZone)/6.0D1 &
                                  + DBLE(SecondZone)/3.6D3 &
                                  + DBLE(MsecZone  )/3.6D6 )
      ELSE
        dHourNum = signZone*SNGL(NumAscii(ZoneString, errorZone))
      END IF
      IF(ABS(dHourNum)>12.0) errorZone = .true.
    END IF
  END IF
  !
  !-- Return if invalid offset
  IF(errorZone) RETURN

  Year   = yearNum
  Month  = monthNum
  Day    = dayNum
  Hour   = hourNum
  Minute = minuteNum
  Second = secondNum
  Msec   = msecNum
  dHour  = dHourNum
  Error  = .false.

  RETURN
END SUBROUTINE ExtractTimes
!
SUBROUTINE GetDateAndTimeParts(TimeDateChr, DateChr, TimeChr, ZoneChr)
!+
!
! Description: This subroutine extracts the Date, Time and Zone portions of a TimeDate Character String
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),  INTENT(IN)     :: TimeDateChr                    ! Timestamp and possible timezone
CHARACTER(32), INTENT(OUT)    :: DateChr                        ! Date portion extracted from TimeDateChr
CHARACTER(16), INTENT(OUT)    :: TimeChr                        ! Time portion extracted from TimeDateChr
CHARACTER(16), INTENT(OUT)    :: ZoneChr                        ! Zone portion extracted from TimeDateChr
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: ChrComma                       ! Location of first "," in the character string
INTEGER(4)                    :: ChrSpace                       ! Location of first " " in the character string
INTEGER(4)                    :: ChrAtSign                      ! Location of first "@" in the character string
INTEGER(4)                    :: ChrZulu                        ! Location of UTC character in the Zulu character string
INTEGER(4)                    :: ChrUTC                         ! Location of first UTC character comma in the string
INTEGER(4)                    :: zulu                           ! Index through the Zulu timezone character strings
INTEGER(4)  ,PARAMETER        :: zuluEnd = 4                    ! Number of Zulu timezone strings
CHARACTER(3),PARAMETER        :: UTCchr_zulu(zuluEnd)         & ! Zulu timezone strings
                                                                 = (/'Z  ','UTC','+  ','-  '/)
CHARACTER(32)                 :: TimeAndZoneChr                 ! Time and Timezone portion of TimeDateCjr
INTEGER(4)                    :: Chr                            ! Index thru characters in TimeAndZoneChr
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  DateChr = ' '
  TimeChr = ' '
  ZoneChr = ' '

  ChrComma  = INDEX(TRIM(TimeDateChr),COMMA)
  ChrSpace  = INDEX(TRIM(TimeDateChr),SPACE)
  ChrAtSign = INDEX(TRIM(TimeDateChr),'@')
  IF(ChrAtSign > 1) THEN
    DateChr        = TimeDateChr(1:ChrAtSign-1)
    TimeAndZoneChr = TimeDateChr(ChrAtSign+1:)
  ELSE IF(ChrComma > 0) THEN
    DateChr        = TimeDateChr(1:ChrComma+5)
    TimeAndZoneChr = TimeDateChr(ChrComma+7:)
  ELSE IF(ChrSpace > 0) THEN
    DateChr        = TimeDateChr(1:ChrSpace-1)
    TimeAndZoneChr = TimeDateChr(ChrSpace+1:)
  ELSE
    DateChr        = TimeDateChr
    TimeAndZoneChr = '00:00:00 UTC'
  END IF
  CALL UpperCase(TRIM(TimeAndZoneChr),TimeAndZoneChr)

  ChrSpace = INDEX(TRIM(TimeAndZoneChr),SPACE)

  ChrUTC = 0
  DO zulu = 1, zuluEnd
    ChrZulu = INDEX(TRIM(TimeAndZoneChr),TRIM(UTCchr_zulu(zulu)))
    IF(ChrZulu < 1) CYCLE
    IF(ChrUTC==0) THEN
      ChrUTC = ChrZulu
    ELSE
      ChrUTC = MIN(ChrUTC,ChrZulu)
    END IF
  END DO

  IF(ChrSpace > 0) THEN
    TimeChr = TimeAndZoneChr(1:ChrSpace-1)
    ZoneChr = TimeAndZoneChr(ChrSpace+1:)
    IF(TimeChr==' ') TimeChr = '00:00:00'

  ELSE IF(ChrUTC>0) THEN
    TimeChr = TimeAndZoneChr(1:ChrUTC-1)
    ZoneChr = TimeAndZoneChr(ChrUTC:)
    IF(TimeChr==' ') TimeChr = '00:00:00'

  ELSE
    IF(Numeric(TimeAndZoneChr(1:1))) THEN
      Chr = LEN_TRIM(TimeAndZoneChr)
      IF(Numeric(TimeAndZoneChr(Chr:Chr))) THEN
        TimeChr = TRIM(TimeAndZoneChr)
        ZoneChr = 'UTC'
      ELSE
        DO WHILE(.not.Numeric(TimeAndZoneChr(Chr:Chr)))
          Chr = Chr - 1
        END DO
        TimeChr = TRIM(TimeAndZoneChr(1:Chr))
        ZoneChr = TRIM(TimeAndZoneChr(Chr+1:))
      END IF
    ELSE
      TimeChr = '00:00:00.000'
      ZoneChr = TRIM(TimeAndZoneChr)
    END IF
  END IF
  RETURN
END SUBROUTINE GetDateAndTimeParts

!
SUBROUTINE ParseDate(DateString, Year, Month, Day, error)
!+
!
! Description: This subroutine extracts the Year, Month and Day from a Date String
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
USE NumericalConstants
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: DateString                     ! Date string
INTEGER(4),   INTENT(OUT)     :: Year                           ! Year number
INTEGER(4),   INTENT(OUT)     :: Month                          ! Month number
INTEGER(4),   INTENT(OUT)     :: Day                            ! Day of Month number
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Set true for invalid timestamp
!
!                                Local
!-----------------------------+-------
INTEGER(4)  ,PARAMETER        :: sepEnd = 4                     ! Number of separators for parsing the Date string
CHARACTER(1),PARAMETER        :: a_DateSep(sepEnd)            & ! Date component separators
                                                                  = (/ ",", "/", "-", " "/)
INTEGER(4)                    :: start$chr_parse(4)             ! Starting character of each parsed date token
INTEGER(4)                    :: end$chr_parse(4)               ! Ending character of each parsed date token
INTEGER(4)                    :: parseEnd                       ! Number of parsed components
CHARACTER(32)                 :: ascii_part(3)
INTEGER(4)                    :: PartDay
INTEGER(4)                    :: PartYear
REAL(8)                       :: num_part(3)
INTEGER(4)                    :: chr_part(3)
LOGICAL(4)                    :: numeric_part(3)
LOGICAL(4)                    :: ErrorNumeric
INTEGER(4)                    :: parse
INTEGER(4)                    :: part
INTEGER(4)                    :: partMonth
INTEGER(4)                    :: dayMax
INTEGER(4)                    :: monthNum
INTEGER(4)                    :: yearNum
INTEGER(4)                    :: dayNum
INTEGER(4)                    :: DayOfWeek
INTEGER(4)                    :: DayOfMonth
INTEGER(4)                    :: occurrence
INTEGER(4)                    :: occurrenceCheck
INTEGER(4)   ,PARAMETER       :: occurrenceEnd   = 18
CHARACTER(13),PARAMETER       :: Ascii_Occurrence (18) = (/'1st          ','2nd          ','3rd          ','4th          ','5th          ','Last         ' &
                                                          ,'DayBefore1st ','DayBefore2nd ','DayBefore3rd ','DayBefore4th ','DayBefore5th ','DayBeforeLast' &
                                                          ,'DayAfter1st  ','DayAfter2nd  ','DayAfter3rd  ','DayAfter4th  ','DayAfter5th  ','DayAfterLast ' &
                                                         /)
INTEGER(4)   ,PARAMETER       :: size_Occurrence  (18) = (/ 3, 3, 3, 3, 3, 4,12,12,12,12,12,13,11,11,11,11,11,12/)
INTEGER(4)   ,PARAMETER       :: order_Occurrence (18) = (/ 1, 2, 3, 4, 5,-1, 1, 2, 3, 4, 5,-1, 1, 2, 3, 4, 5,-1/)
INTEGER(4)   ,PARAMETER       :: offset_Occurrence(18) = (/ 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1, 1, 1, 1, 1, 1, 1/)
INTEGER(4)                    :: SizeCheck
INTEGER(4)                    :: YearAtStart
INTEGER(4)                    :: YearAtEnd
CHARACTER(32)                 :: AsciiPart
CHARACTER(13)                 :: AsciiOccur
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !-- Initialize return
  Error  = .true.
  Year   = YearEpoch
  Month  = MonthEpoch
  Day    = DayEpoch
  !
  !Process the Date
  CALL ParseString(TRIM(DateString), a_DateSep, sepEnd, 4, start$chr_parse,end$chr_parse,parseEnd)
  !
  ! Special processing if there are no separators
  IF(ParseEnd==1) THEN
    IF(Numeric(TRIM(DateString))) THEN
      !-- All numeric -> Either YYYYMMDD or DDMMYYYY
      YearAtStart = INT(NumAscii(DateString(1:4),ErrorNumeric)); IF(ErrorNumeric) RETURN
      YearAtEnd   = INT(NumAscii(DateString(5:8),ErrorNumeric)); IF(ErrorNumeric) RETURN
      IF(YearAtStart <  YearEpoch .and. YearAtEnd < YearEpoch) RETURN
      ParseEnd = 3
      IF(YearAtStart >= YearEpoch) THEN
        start$chr_parse(1:3) = (/1,5,7/)
        end$chr_parse  (1:3) = (/4,6,8/)
      ELSE
        start$chr_parse(1:3) = (/1,3,5/)
        end$chr_parse  (1:3) = (/2,4,8/)
      END IF
    END IF
  END IF

  !-- Return if all date parts not provided (year, month and day)
  IF(parseEnd /= 3) RETURN

  ! Default is DDD-MMM-YYYY
  partDay   = 1
  partMonth = 2
  partYear  = 3

  DO parse = 1, parseEnd
    part = parse
    ascii_part  (part) = DateString(start$chr_parse(parse):end$chr_parse(parse))
    chr_part    (part) = end$chr_parse(parse) - start$chr_parse(parse) + 1
    Num_part    (part) = NumAscii(ascii_part(part),ErrorNumeric)
    Numeric_part(part) = .not.ErrorNumeric
    IF(ErrorNumeric) partMonth = part
  END DO

  !-- Get the month information
  IF(Numeric_part(partMonth)) THEN
    monthNum = INT(Num_part(partMonth))
  ELSE
    CALL GetMonthAscii(ascii_part(partMonth), monthNum)
  END IF

  !-- Return if invalid month
  IF(monthNum < January .or. monthNum > December) RETURN

  SELECT CASE (partMonth)
    CASE (1)
      partDay  = 2
      partYear = 3
    CASE (2)
      IF(chr_part(3)==4) THEN
        partYear = 3
        partDay  = 1
      ELSE
        partYear = 1
        partDay  = 3
      END IF
    CASE DEFAULT
  END SELECT

  !-- Return if invalid year
  yearNum =INT( num_part(partYear) , KIND=4)
  IF(.not.Numeric_part(partYear) .or. yearNum < YearMin.or.yearNum > YearMax) RETURN

  !-- Check for day occurrence (eg FirstSunday, 2ndWednesday
  IF(.not.Numeric_part(partDay)) THEN
    Occurrence = 0
    DO OccurrenceCheck = 1, OccurrenceEnd
      sizeCheck  = size_Occurrence(OccurrenceCheck)
      AsciiPart  = TRIM(Ascii_Part(PartDay))
      AsciiOccur = TRIM(Ascii_Occurrence(OccurrenceCheck))
      IF(AsciiPart(1:sizeCheck)==AsciiOccur(1:sizeCheck)) THEN
        Occurrence = OccurrenceCheck
        DO DayOfWeek = Sunday, Saturday
          IF(AsciiPart(sizeCheck+1:sizeCheck+9)==Ascii_DayOfWeek(DayOfWeek)) THEN
            CALL GetDayOccurrence(DayOfMonth,Order_Occurrence(Occurrence),Offset_Occurrence(Occurrence),DayOfWeek,MonthNum,YearNum)
            num_part    (partDay) = DayOfMonth
            Numeric_part(partDay) = TRUE
            EXIT
          END IF
        END DO
        EXIT
      END IF
    END DO
  END IF

  !-- Return if invalid day
  dayNum = INT(num_part(partDay))
  IF(LeapYear(yearNum)) THEN
    DayMax = DaysLeapYear_month(monthNum)
  ELSE
    DayMax = DaysNormalYear_month(monthNum)
  END If
  IF(.not.Numeric_part(partDay) .or. dayNum < 1 .or. dayNum > DayMax) RETURN

  error = .false.
  year  = yearNum
  month = monthNum
  day   = dayNum
  RETURN
END SUBROUTINE ParseDate
!
SUBROUTINE ParseTime(TimeString, Hour, Minute, Second, Msec, error)
!+
!
! Description: This subroutine extracts the Hour, Minute, Second and Milliseconds from a Time String
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
USE NumericalConstants
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: TimeString                     ! Timestring
INTEGER(4),   INTENT(OUT)     :: Hour                           ! Hour number
INTEGER(4),   INTENT(OUT)     :: Minute                         ! Hour number
INTEGER(4),   INTENT(OUT)     :: Second                         ! Second number
INTEGER(4),   INTENT(OUT)     :: Msec                           ! Millisecond number
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Set true for invalid timestamp
!
!                                Local
!-----------------------------+-------
CHARACTER(1),PARAMETER        :: a_TimeSep(1) = (/ ":" /)
INTEGER(4)  ,PARAMETER        :: sepEnd = 1
INTEGER(4)  ,PARAMETER        :: parseMax = 3
INTEGER(4)                    :: start$chr_parse(4)
INTEGER(4)                    :: end$chr_parse(4)
INTEGER(4)                    :: parseEnd
CHARACTER(12)                 :: ascii_part(3)
REAL(8)                       :: num_part(3)
INTEGER(4)                    :: chr_part(3)
LOGICAL(4)                    :: numeric_part(3)
LOGICAL(4)                    :: NumericError
INTEGER(4)                    :: parse
INTEGER(4)                    :: part
INTEGER(4)                    :: hourNum
INTEGER(4)                    :: minuteNum
INTEGER(4)                    :: secondNum
INTEGER(4)                    :: msecNum
LOGICAL(4)                    :: ErrorTime
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
!-- Initialize return
Error  = .true.
hour   = 0
minute = 0
second = 0
msec   = 0

!-- Parse the time parts
CALL ParseString(TRIM(TimeString), a_TimeSep, sepEnd, parseMax, start$chr_parse,end$chr_parse,parseEnd)

!-- Return if no time specified
IF(parseEnd < 1) RETURN

! Special processing if there are no separators
IF(ParseEnd==1) THEN
  IF(LEN_TRIM(TimeString)>5) THEN
    start$chr_parse(1:3) = (/1,3,5/)
    end$chr_parse  (1:3) = (/2,4,LEN_TRIM(TimeString)/)
    parseEnd = 3
  END IF
END IF

Num_part = 0.0d0
ErrorTime = .false.
DO parse = 1, parseEnd
  part = parse
  ascii_part  (part) = TimeString(start$chr_parse(parse):end$chr_parse(parse))
  chr_part    (part) = end$chr_parse(parse) - start$chr_parse(parse) + 1
  Num_part    (part) = NumAscii(ascii_part(part),NumericError)
  Numeric_part(part) = .not.NumericError
  IF(NumericError) ErrorTime = .true.
END DO

!-- Return if any numeric error
IF(ErrorTime) RETURN

hourNum   = INT(num_part(1))
minuteNum = INT(num_part(2))
secondNum = INT(num_part(3))
msecNum   = INT((num_part(3) - DBLE(secondNum))*1000.0d0 + epsilon)

!-- Check for 1st level time errors
IF(hourNum   < 0 .or. hourNum   >  23) RETURN
IF(minuteNum < 0 .or. minuteNum >  59) RETURN
IF(secondNum < 0 .or. secondNum > 100) RETURN

error = .false.
hour   = hourNum
minute = minuteNum
second = secondNum
msec   = msecNum
END SUBROUTINE ParseTime
!
SUBROUTINE GetDayOccurrence(DayOfMonth,Order,DayOffset,DayOfWeek,MonthAt,Year)
!+
!
! Description: This subroutine calculates starting and ending day of the week for each month based on the year input.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(OUT)        :: DayOfMonth
INTEGER(4),INTENT(IN)         :: Order
INTEGER(4),INTENT(IN)         :: DayOffset
INTEGER(4),INTENT(IN)         :: DayOfWeek
INTEGER(4),INTENT(IN)         :: MonthAt
INTEGER(4),INTENT(IN)         :: Year
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: DayMax_month(0:13)
INTEGER(4)                    :: WeekMax_month       (January:December)
INTEGER(4)                    :: DayOfWeekStart_month(January:December)
INTEGER(4)                    :: DayOfWeekEnd_month  (January:December)
INTEGER(4)                    :: DayOfWeekStart
INTEGER(4)                    :: month
INTEGER(4)                    :: week
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  DayMax_month = DaysNormalYear_month
  IF(LeapYear(Year)) THEN
    DayMax_month(February) = 29
  END IF
  CALL GetDayOfWeek(TimeStart_year(year), DayOfWeekStart)
  DO month = January, December
    WeekMax_month       (month) = (DayOfWeekStart - 1 + DayMax_month(month) + 6)/7
    DayOfWeekStart_month(month) = DayOfWeekStart
    DayOfWeekEnd_month  (month) = MOD(DayOfWeekStart + DayMax_month(month)-2,7)+1
    DayOfWeekStart              = MOD(DayOfWeekStart + DayMax_month(month)-1,7)+1
  END DO

  month = monthAt
  SELECT CASE (order)
  CASE (1,2,3,4,5)
    IF(DayOfWeek >= DayOfWeekStart_month(month)) THEN
      week = order
    ELSE
      week = order+1
    END IF
  CASE (-1)
    IF(DayOfWeek <= DayOfWeekEnd_month(month)) THEN
      week = WeekMax_month(month)
    ELSE
      week = WeekMax_month(month) - 1
    END IF
  CASE DEFAULT
    week = WeekMax_month(month) + 1
  END SELECT
  DayOfMonth = DayOfWeek - DayOfWeekStart_month(month) + 7*(week-1) + 1 + DayOffset
  IF(DayOfMonth < 0 .or. DayOfMonth > DayMax_month(month)) THEN
    DayOfMonth = 0
  END IF
  RETURN
END SUBROUTINE GetDayOccurrence

SUBROUTINE AdjustDaylightSavings(Time, Locale, FormatTime, dHoursLocal)
!+
!
! Description: This subroutine returns the time zone string (e.g. "PDT" or "PST") and the hours from GMT for a given
!              Modified Julian Date in a Locale
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 11-MAR-2025     | MA    | SCR 150: Add the Locale as an argument to accommodate different locations
! ----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD), INTENT(IN)      :: Time                           ! Modified Julian Date                   (10^-7 seconds)
INTEGER(4)  , INTENT(IN)      :: Locale                         ! Timezone Locale
CHARACTER(4), INTENT(OUT)     :: FormatTime                     ! Time Zone format at Time (eg PST or PDT)
REAL(4)     , INTENT(OUT)     :: dHoursLocal                    ! Hour shift from UTC                            (hours)
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: Zone                           ! Timezone instance of the locale that is used
INTEGER(4)                    :: RuleInfo                       ! Index of Zone Rule information used
CHARACTER(8)                  :: AbbrZone                       ! Time Zone abbreviation (e.g. PDT, PST)        (string)
REAL(8)                       :: dHoursZone                     ! Hour shift from UTC                            (hours)
INTEGER(4)                    :: StatusAdjust                   ! Return status code
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL AdjustTimeZoneForLocale(Locale, Time, dHoursZone, Zone, AbbrZone, RuleInfo, StatusAdjust)
  IF(StatusAdjust .eq. TIME_S_NORMAL) THEN
    dHoursLocal = REAL(dHoursZone, KIND=4)
    FormatTime  = TRIM(AbbrZone)
  ELSE
    WRITE(*,FMT='("AdjustDaylightSavings: Status ",I4," Returned")') StatusAdjust
  END IF
  RETURN

END SUBROUTINE AdjustDaylightSavings

SUBROUTINE DetermineDaylightSavingsByRules(Rules,Time,DaylightSavings)
!+
!
! Description: This subroutine determines if daylight savings is in effect.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
TYPE(DaylightSavingsRule)     :: Rules
INTEGER(MJD)                  :: Time
LOGICAL(4)                    :: DayLightSavings
!
!                                    Local
!-----------------------------+---------
INTEGER(4)                    :: switch
INTEGER(MJD)                  :: Time_Switch(2)
TYPE(TimeDate)                :: DateAndTime
INTEGER(4)                    :: indexYear
CHARACTER(42)                 :: TimeDateString
LOGICAL(4)                    :: Error
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL CalculateDateFromTime(Time, DateAndTime)
  DO switch = 1, 2
    TimeDateString  = Rules%TimeDateString_Switch(switch)
    indexYear      = INDEX(TimeDateString,'YYYY')
    WRITE(TimeDateString(indexYear:indexYear+3),FMT='(I4.4)') DateAndTime%Year
    Time_switch(switch) = TimeFromTimeDateStringForRules(TimeDateString, Error)
  END DO
  IF(Time < Time_Switch(1)) THEN
    DayLightSavings = Rules%DaylightSavings_Switch(0)
  ElSE IF(Time < Time_Switch(2)) THEN
    DayLightSavings = Rules%DaylightSavings_Switch(1)
  ELSE
    DayLightSavings = Rules%DaylightSavings_Switch(2)
  END IF
  RETURN
END SUBROUTINE DetermineDaylightSavingsByRules

FUNCTION DaylightSavingsActive(Time, Locale) RESULT(DaylightSavings)
!+
! Description: Returns true if daylight saving is active for Time
!              ToDo SCR 150: Add Locale as an INTENT(IN) to generalize this.
!              ToDo SCR 150: We could also return the ZoneUse, AbbrZone and dHoursLocal
!              ToDo SCR 150: This may allow us to combine this Function with
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
! 12-MAR-2025     | MA    | SCR 150: Upgrade to use the Olsen Tz Database
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN)     :: Time                           ! Modified Julian Date                   (10^-7 seconds)
INTEGER(4)   , INTENT(IN)     :: Locale                         ! Index of the Time Zone Locale
LOGICAL(4)                    :: DayLightSavings                ! True if daylight savings is active
!
!                                Local
!-----------------------------+---------
REAL(8)                       :: dHoursLocal                    ! Hours shift from UTC                           (hours)
INTEGER(4)                    :: ZoneUse                        ! Timezone of the locale that is used
CHARACTER(8)                  :: AbbrZone                       ! Time Zone abbreviation (e.g. PDT, PST)        (string)
INTEGER(4)                    :: RuleInfo                       ! Index of Zone Rule information used
INTEGER(4)                    :: StatusAdjust                   ! Status returned from AdjustTimeZoneForLocale
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL AdjustTimeZoneForLocale(Locale, Time, dHoursLocal, ZoneUse, AbbrZone, RuleInfo, StatusAdjust)
    IF(StatusAdjust .ne. TIME_S_NORMAL) THEN
    WRITE(*,FMT='(" DaylightSavingsActive: For Locale ",I4," Time ",Z16.16," StatusAdjust ",I4)') Locale, Time, StatusAdjust
  END IF
  IF(INDEX(AbbrZone,"DT") > 0) THEN
    DaylightSavings = .true.
  ELSE IF( WithinClosedSet(1, RuleInfo, MX$RuleInfoTz)) THEN
    IF(Tz_RuleInfo(RuleInfo)%dHourShift > 0 .or. Tz_RuleInfo(RuleInfo)%dMinuteShift > 0) THEN
      DaylightSavings = .true.
    ELSE
      DaylightSavings = .false.
    END IF
  ELSE
    DaylightSavings = .false.
  END IF

  !IF(ABS(dHoursLocal - DBLE(CurrentLocale%dHoursDST)) < 0.0001) THEN
  !  DayLightSavings = .true.
  !ELSE
  !  DayLightSavings = .false.
  !END IF

  RETURN
END FUNCTION DaylightSavingsActive

SUBROUTINE AdjustDaylightSavingsUS(Time, DST, SDT, dHourDST, dHourSDT, FormatTime, dHour)
!+
! Description: This subroutine adjusts the daylight savings time transition for United States Rules.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD)                  :: Time
CHARACTER(LEN=*)              :: DST
CHARACTER(LEN=*)              :: SDT
CHARACTER(LEN=*)              :: FormatTime
REAL(4)                       :: dHourDST
REAL(4)                       :: dHourSDT
REAL(4)                       :: dHour
!
!                                Local
!-----------------------------+---------
TYPE(TimeDate)                :: DateAndTime
INTEGER(4)                    :: DayOfWeek
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL GetDayOfWeek(Time, DayOfWeek)
CALL CalculateDateFromTime(Time, DateAndTime)

IF(DateAndTime%Month < 3) THEN
  FormatTime   = SDT
ELSE IF(DateAndTime%Month>11) THEN
  FormatTime   = SDT
ELSE IF(DateAndTime%Month==3) THEN
  CALL ChooseTimesUS(DateAndTime%day, DateAndTime%hour, DayOfWeek, SDT, DST, FormatTime)

ELSE IF(DateAndTime%Month==11) THEN
  CALL ChooseTimesUS(DateAndTime%day, DateAndTime%hour, DayOfWeek, DST, SDT, FormatTime)

ELSE
  FormatTime   = DST

END IF
IF(FormatTime==DST) THEN
  dHour = dHourDST
ELSE
  dHour = dHourSDT
END IF
RETURN
END SUBROUTINE AdjustDaylightSavingsUS

SUBROUTINE AdjustDayLightSavingsEU(Time, DST, SDT, dHourDST, dHourSDT, FormatTime, dHour)
!+
! Description: This subroutine adjusts the daylight savings time transition for European Union Rules.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD)                  :: Time
CHARACTER(LEN=*)              :: DST
CHARACTER(LEN=*)              :: SDT
CHARACTER(LEN=*)              :: FormatTime
REAL(4)                       :: dHourDST
REAL(4)                       :: dHourSDT
REAL(4)                       :: dHour
!
!                                Local
!-----------------------------+---------
TYPE(TimeDate)                :: DateAndTime
INTEGER(4)                    :: DayOfWeek
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
CALL GetDayOfWeek(Time, DayOfWeek)
CALL CalculateDateFromTime(Time, DateAndTime)

IF(DateAndTime%Month < 3) THEN
  FormatTime   = SDT
ELSE IF(DateAndTime%Month>10) THEN
  FormatTime   = SDT
ELSE IF(DateAndTime%Month==3) THEN
  CALL ChooseTimesEU(DateAndTime%day, DateAndTime%hour, DayOfWeek, SDT, DST, FormatTime)

ELSE IF(DateAndTime%Month==10) THEN
  CALL ChooseTimesEU(DateAndTime%day, DateAndTime%hour, DayOfWeek, DST, SDT, FormatTime)

ELSE
  FormatTime   = DST

END IF
IF(FormatTime==DST) THEN
  dHour = dHourDST
ELSE
  dHour = dHourSDT
END IF

RETURN
END SUBROUTINE AdjustDaylightSavingsEU

SUBROUTINE ChooseTimesUS(DayOfMonth, HourOfDay, DayOfWeek, FormatTime1, FormatTime2, FormatTime)
!+
! Description: This subroutine chooses the time zone format (e.g. PST or PDT) based on United States Rules.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: DayOfMonth
INTEGER(4)                    :: HourOfDay
INTEGER(4)                    :: DayOfWeek
CHARACTER(LEN=*)              :: FormatTime1
CHARACTER(LEN=*)              :: FormatTime2
CHARACTER(LEN=*)              :: FormatTime
INTEGER(4)                    :: Week
!
!                                Local
!-----------------------------+---------
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
Week = (DayOfMonth - DayOfWeek + 6)/7 + 1

IF(DayOfMonth - DayOfWeek < 7) THEN
  FormatTime = FormatTime1
ELSE
  IF(DayOfWeek == 1) THEN
    IF(DayOfMonth-7 < 8) THEN
      IF(HourOfDay >= 2) THEN
        FormatTime = FormatTime2
      ELSE
        FormatTime = FormatTime1
      END IF
    ELSE
      FormatTime = FormatTime2
    END IF
  ELSE
    FormatTime = FormatTime1
  END IF
END IF
RETURN
END SUBROUTINE ChooseTimesUS

SUBROUTINE ChooseTimesEU(DayOfMonth, HourOfDay, DayOfWeek, FormatTime1, FormatTime2, FormatTime)
!+
! Description: This subroutine chooses the time zone format (e.g. GMT or BST) based on European Union Rules.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                    :: DayOfMonth
INTEGER(4)                    :: HourOfDay
INTEGER(4)                    :: DayOfWeek
CHARACTER(LEN=*)              :: FormatTime1
CHARACTER(LEN=*)              :: FormatTime2
CHARACTER(LEN=*)              :: FormatTime
!
!                                Local
!-----------------------------+---------
INTEGER(4)                    :: LastSunday
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
LastSunday = 29 - ABS(MOD(DayOfMonth-DayOfWeek,7))
IF(LastSunday <= 24) LastSunday = LastSunday + 7
IF(DayOfMonth < LastSunday) THEN
  FormatTime = FormatTime1
ELSE
  IF(DayOfWeek == 1) THEN
    IF(HourOfDay < 1) THEN
      FormatTime = FormatTime1
    ELSE
      FormatTime = FormatTime2
    END IF
  ELSE
    FormatTime = FormatTime2
  END IF
END IF
RETURN
END SUBROUTINE ChooseTimesEU

FUNCTION HolidayTimeCheck(TimeCheck) RESULT(HolidayTime)
IMPLICIT NONE
!
! Description: This function checks a Modified Julian Date to see if it falls within a Holiday.
!
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(MJD) , INTENT(IN)     :: TimeCheck                      ! Modified Julian Date to check (10^-7 sec)
LOGICAL(4)                    :: HolidayTime                    ! Set true if time is a holiday
!
!
!-----------------------------+---------------------
INTEGER(MJD)                  :: TimeHoliday
CHARACTER(24)                 :: TimeStamp
CHARACTER(24)                 :: TimeStampHoliday
CHARACTER(4)                  :: FormatTime
INTEGER(4)                    :: Holiday
CHARACTER(42)                 :: TimeDateString
LOGICAL(4)                    :: ErrorTimeDate
TYPE(TimeDate)                :: DateAndTime
INTEGER(4)                    :: DayOfWeek
INTEGER(8)                    :: DayIncrement
INTEGER(4)                    :: IndexYear
LOGICAL(4)                    :: ErrorHoliday
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
HolidayTime = .false.
IF(HolidayEnd == 0)  THEN
  CALL GetHolidayDefinitionsPath(ErrorHoliday)
  IF(ErrorHoliday) RETURN
END IF
CALL Convert_Time_To_Local_TimeStamp(TimeCheck, TimeStamp, FormatTime)
IF(YearHoliday /= TimeStamp(8:11)) THEN
  YearHoliday = TimeStamp(8:11)
  DO Holiday = 1, HolidayEnd
    TimeDateString = TRIM(ADJUSTL(Rule_Holiday(Holiday)%TimeDateString))
    indexYear = INDEX(TimeDateString,'YYYY')
    IF(indexYear > 0) THEN
      TimeDateString(indexYear:indexYear+3)=YearHoliday
    END IF
    TimeHoliday = TimeFromTimeDateString(TimeDateString, ErrorTimeDate)
    IF(ErrorTimeDate) CYCLE
    CALL GetLocalDateOfTime(DateAndTime, TimeHoliday, ErrorTimeDate)
    IF(ErrorTimeDate) CYCLE
    DayOfWeek    = DateAndTime%DayOfWeek
    DayIncrement = 0
    SELECT CASE(Rule_Holiday(Holiday)%Substitution)
    CASE('NextWeekDay')
      IF(DayOfWeek == Saturday) THEN
        DayIncrement = 2
      ELSE IF(DayOfWeek == Sunday) THEN
        DayIncrement = 1
      END IF

    CASE('PreviousWeekDay')
      IF(DayOfWeek == Saturday) THEN
        DayIncrement = -1
      ELSE IF(DayOfWeek == Sunday) THEN
        DayIncrement = -2
      END IF

    CASE('NearestWeekday')
      IF(DayOfWeek == Saturday) THEN
        DayIncrement = -1
      ELSE IF(DayOfWeek == Sunday) THEN
        DayIncrement = 1
      END IF

    CASE DEFAULT
    END SELECT
    TimeHoliday = TimeHoliday + TimePerDay*DayIncrement
    CALL Convert_Time_To_Local_TimeStamp(TimeHoliday, TimeStampHoliday, FormatTime)
    Rule_Holiday(Holiday)%Time      = TimeHoliday
    Rule_Holiday(Holiday)%TimeStamp = TimeStampHoliday(1:12)
  END DO
END IF
DO Holiday = 1, HolidayEnd
  IF(Rule_Holiday(Holiday)%TimeStamp(1:12) == TimeStamp(1:12)) THEN
    HolidayTime = .true.
    EXIT
  END IF
END DO
RETURN
!
END FUNCTION HolidayTimeCheck

SUBROUTINE GetHolidayDefinitionsPath(Error)
!
! Description: This subroutine reads in the holiday definitions for the current system and sets up the Holiday test data
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4)                    :: Error                          ! Error in holiday definitions file
!
!
!-----------------------------+---------------------
CHARACTER(128)                :: PathHoliday                    ! Holiday definitions path
CHARACTER(1),PARAMETER        :: a_Sep(1) = (/ ","/)
INTEGER(4)  ,PARAMETER        :: sepEnd = 1
INTEGER(4)                    :: start$chr_parse(3)
INTEGER(4)                    :: end$chr_parse(3)
INTEGER(4)                    :: parseEnd
CHARACTER(128)                :: Ascii_Chr
CHARACTER(40)                 :: Description
CHARACTER(42)                 :: TimeDateString
CHARACTER(16)                 :: Substitution
LOGICAL(4)                    :: ErrorSubstitution
LOGICAL(4)                    :: ErrorTimeDateString
INTEGER(4)                    :: Holiday
CHARACTER(24)                 :: TimeStamp
CHARACTER(4)                  :: FormatTime
INTEGER(4)                    :: indexYear
INTEGER(MJD)                  :: TimeTest
INTEGER(4)                    :: UnitHoliday
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
Error = .true.
IF(.not.Initialized) THEN
  RETURN
END IF

IF(.not.HolidayFileRead) THEN
  CALL GET_ENVIRONMENT_VARIABLE('JPL_DIR',PathHoliday)
  Holiday = 0
  OPEN(NewUnit=UnitHoliday, FILE=TRIM(PathHoliday)//"\HolidayDefinition.dat", STATUS="old", ERR=90)
  DO
    READ(UnitHoliday, FMT='(A)', ERR=91, END=1) Ascii_Chr
    IF(Ascii_Chr(1:1)=="!") CYCLE

    !Process the Date
    CALL ParseString(TRIM(Ascii_Chr), a_Sep, sepEnd, 3, start$chr_parse,end$chr_parse,parseEnd)
    Description    = Ascii_Chr(start$chr_parse(1):end$chr_parse(1))
    TimeDateString = TRIM(ADJUSTL(Ascii_Chr(start$chr_parse(2):end$chr_parse(2))))
    Substitution   = TRIM(ADJUSTL(Ascii_Chr(start$chr_parse(3):end$chr_parse(3))))
    SELECT CASE(TRIM(ADJUSTL(Substitution)))
    CASE("None")
      ErrorSubstitution = .false.
    CASE("PreviousWeekday")
      ErrorSubstitution = .false.
    CASE("NextWeekday")
      ErrorSubstitution = .false.
    CASE("NearestWeekday")
      ErrorSubstitution = .false.
    CASE DEFAULT
      ErrorSubstitution = .true.
      PRINT *,' Error In Substitution: '//Substitution
    END SELECT
    indexYear = INDEX(TimeDateString,'YYYY')
    IF(indexYear > 0) THEN
      TimeDateString(indexYear:indexYear+3)="2000"
      TimeTest = TimeFromTimeDateString(TimeDateString, ErrorTimeDateString)
      TimeDateString = Ascii_Chr(start$chr_parse(2):end$chr_parse(2))
    ELSE
      TimeTest = TimeFromTimeDateString(TimeDateString, ErrorTimeDateString)
    END IF
    IF(ErrorTimeDateString) THEN
      PRINT *,' Error in TimeDateString'//TimeDateString
    END IF
    IF(ErrorTimeDateString .or. ErrorSubstitution) CYCLE
    Holiday = Holiday + 1
    CALL Convert_Time_To_Local_TimeStamp(TimeTest, TimeStamp, FormatTime)
    Rule_Holiday(Holiday) = HolidayRule(TimeTest, Description, Substitution, TimeStamp, TimeDateString)

  END DO
1 CLOSE(UnitHoliday)
  HolidayFileRead   = .true.
  HolidayEnd        = Holiday
END IF
IF(HolidayEnd>0) THEN
  Error = .false.
END IF
RETURN

90 WRITE(*,FMT='(A)') ' Error Opening '//PathHoliday
CLOSE(UnitHoliday)
RETURN

91 WRITE(*,FMT='(A)') ' Error Reading '//PathHoliday
CLOSE(UnitHoliday)
RETURN

END SUBROUTINE GetHolidayDefinitionsPath

SUBROUTINE ProcessTzInfo()
!+
!
! Description: This subroutine processes the Time Zone information database
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 19-Feb-2025     | MA    | SCR 150: Use the TzInfo database for Time Zone information
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!                                Local                            Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(16)                 :: PathTzInfo                     ! Path for the TzInfo data
INTEGER(4)                    :: StatusEnvironment              ! Return status from GET_ENVIRONMENT_VARIABLE
INTEGER(4)                    :: length                         ! Length of TZ_DIR environment variable
CHARACTER(32)                 :: NameFile_List(8)               ! List of TzInfo tiles
INTEGER(4)                    :: List                           ! Index through list of TzInfo Files
INTEGER(4)                    :: ListEnd                        ! Number of TzInfo files
INTEGER(4)                    :: UnitInfo                       ! Fortran I/O for the extracted information files
INTEGER(4)                    :: Info                           ! Index through the extracted information
CHARACTER(64)                 :: PathFile                       ! File to hold the extracted information
CHARACTER(80)                 :: AsciiChr                       ! Time Zone information
INTEGER(4)  ,PARAMETER        :: parseMax = 16                  ! Maximum number of tokens
INTEGER(4)                    :: parseEnd                       ! Number of parsed tokens
CHARACTER(1)                  :: Ascii_sep(8)                   ! List of separator characters
INTEGER(4)                    :: sepEnd                         ! Number of separator chartacters
INTEGER(4)                    :: start$chr_parse(16)            ! Start character of the tokens
INTEGER(4)                    :: end$chr_parse(16)              ! End character of the tokens
CHARACTER(16)                 :: NameRule                       ! Name of a TZInfo Rule
CHARACTER(8)                  :: YearFromChr                    ! Year a rule start
CHARACTER(8)                  :: YearToChr                      ! Year a rule ends
CHARACTER(8)                  :: InMonthChr                     ! Month on which a rule is activated
CHARACTER(8)                  :: OnDayChr                       ! Day on which a rule is activated
CHARACTER(8)                  :: AtTimeChr                      ! Time at which a rule is activated
CHARACTER(8)                  :: dTimeShiftChr                  ! Time shift when the rule is applied
CHARACTER(8)                  :: dAbbrZoneChr                   ! Time Zone abbreviation change
INTEGER(4)                    :: Rule                           ! Rule for applying a timezone shift
INTEGER(4)                    :: RulePrior                      ! Index through previous Rules
INTEGER(4)                    :: RuleInfo                       ! Rule information defining the shift
LOGICAL(4)                    :: FoundPriorRule                 ! Found that the rule has already been defined
INTEGER(4)                    :: Locale                         ! Index of the Time Zone Locale
CHARACTER(32)                 :: NameLocale                     ! Country/City of the Locale             (string)
INTEGER(4)                    :: Zone                           ! Index of the Time Zone specification
CHARACTER(8)                  :: dTimeUTCChr                    ! Time shift from UTC                    (string)
CHARACTER(16)                 :: RuleChr                        ! Time zone rule that is used            (string)
CHARACTER(8)                  :: AbbrZoneChr                    ! Time Zone abbreviation Template        (string)
CHARACTER(8)                  :: YearUntilChr                   ! Year when this specification expires   (string)
CHARACTER(8)                  :: MonthUntilChr                  ! Mont hwhen this specification expires  (string)
CHARACTER(8)                  :: DayUntilChr                    ! Day owhen this specification expires   (string)
CHARACTER(8)                  :: TimeUntilChr                   ! Time when this specification expires   (string)
LOGICAL(4)  , PARAMETER       :: DebugLocal    = .false.        ! Local diagnostic flag for in-house testing
LOGICAL(4)                    :: RelativePath                   ! Find the TzInfo files in the execution directory
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  !
  ! Set the Path to the Tz Info files:
  INQUIRE(FILE="TzInfo\backward.txt",EXIST=RelativePath)
  IF(RelativePath) THEN
    PathTzInfo = "TzInfo"
  ELSE
    CALL GET_ENVIRONMENT_VARIABLE("TZ_DIR",PathTzInfo, Length, StatusEnvironment, TRIM_NAME=.true.)
    IF(statusEnvironment /= 0 .or. length == 0) THEN
      PathTzInfo = "C:\CSP\TzInfo"
    END IF
  END IF
  !
  ! Setup the list of files to be processed
  List = 1
  NameFile_List(List) = "northamerica.txt"; List = List + 1
  NameFile_List(List) = "southamerica.txt"; List = List + 1
  NameFile_List(List) = "africa.txt      "; List = List + 1
  NameFile_List(List) = "asia.txt        "; List = List + 1
  NameFile_List(List) = "europe.txt      "; List = List + 1
  NameFile_List(List) = "australasia.txt "; List = List + 1
  NameFile_List(List) = "antarctica.txt  "; List = List + 1
  NameFile_List(List) = "backward.txt    "; ListEnd = List

  ! Process the files
  DO List = 1, ListEnd
    CALL ProcessTimeZoneFile(TRIM(PathTzInfo)//SlashBackward//TRIM(NameFile_List(List)))
  END DO

  ! Save the extracted Zone information
  PathFile = TRIM(PathTzInfo)//SlashBackward//"ZoneInfo.txt"
  OPEN(NewUnit=UnitInfo, FILE=PathFile,status="UNKNOWN")
  DO Info = 1, LV$TimeZoneInfo
    WRITE(UnitInfo,FMT='(A)') TRIM(Ascii_TimeZoneInfo(Info))
  END DO
  CLOSE(UnitInfo)

  ! Save the extracted Rule information
  PathFile = TRIM(PathTzInfo)//"RuleInfo.txt"
  OPEN(NewUnit=UnitInfo, FILE=PathFile,status="UNKNOWN")
  DO Info = 1, LV$TimeRuleInfo
    WRITE(UnitInfo,FMT='(A)') TRIM(Ascii_TimeRuleInfo(Info))
  END DO
  CLOSE(UnitInfo)

  ! Save the extracted Link information
  PathFile = TRIM(PathTzInfo)//"LinkInfo.txt"
  OPEN(NewUnit=UnitInfo, FILE=PathFile,status="UNKNOWN")
  DO Info = 1, LV$TimeLinkInfo
    WRITE(UnitInfo,FMT='(A)') TRIM(Ascii_TimeLinkInfo(Info))
  END DO
  CLOSE(UnitInfo)

  !Set up the Rules
  Rule = 0
  RuleInfo = 0
  DO Info = 1, LV$TimeRuleInfo
    ascii_sep(1) = SPACE
    sepEnd       = 1
    AsciiChr = Ascii_TimeRuleInfo(Info)
    CALL ParseString(AsciiChr,Ascii_sep,sepEnd, parseMax, start$chr_parse, end$chr_parse, parseEnd)
    NameRule      = AsciiChr(Start$chr_parse(2) :End$chr_parse(2))
    YearFromChr   = AsciiChr(Start$chr_parse(3) :End$chr_parse(3))
    YearToChr     = AsciiChr(Start$chr_parse(4) :End$chr_parse(4))
    InMonthChr    = AsciiChr(Start$chr_parse(6) :End$chr_parse(6))
    OnDayChr      = AsciiChr(Start$chr_parse(7) :End$chr_parse(7))
    AtTimeChr     = AsciiChr(Start$chr_parse(8) :End$chr_parse(8))
    dTimeShiftChr = AsciiChr(Start$chr_parse(9) :End$chr_parse(9))
    dAbbrZoneChr  = AsciiChr(Start$chr_parse(10):End$chr_parse(10))

    IF(DebugLocal) &
      WRITE(*,FMT='(A)') TRIM(NameRule)//" "//TRIM(YearFromChr)//" "//TRIM(YearToChr)//" "//TRIM(InMonthChr)//" "//TRIM(OnDayChr)//" "//TRIM(AtTimeChr)//" "//TRIM(dTimeShiftChr)//" "//TRIM(dAbbrZoneChr)

    IF(Rule==0) THEN
      CALL AddTzRuleAndInfo(Rule, NameRule, RuleInfo, YearFromChr, YearToChr, InMonthChr, OnDayChr, AtTimeChr, dTimeShiftChr, dAbbrZoneChr)
    ELSE
      FoundPriorRule = .false.
      DO RulePrior = Rule, 1, -1
        IF(TRIM(Tz_Rule(Rule)%Name) == TRIM(NameRule)) THEN
          CALL AddTzRuleInfo(RulePrior, RuleInfo, YearFromChr, YearToChr, InMonthChr, OnDayChr, AtTimeChr, dTimeShiftChr, dAbbrZoneChr)
          FoundPriorRule = .true.
          EXIT
        END IF
      END DO
      IF(.not.FoundPriorRule) THEN
        CALL AddTzRuleAndInfo(Rule, NameRule, RuleInfo, YearFromChr, YearToChr, InMonthChr, OnDayChr, AtTimeChr, dTimeShiftChr, dAbbrZoneChr)
      END IF
    END IF
  END DO
  LV$RuleTz     = Rule
  LV$RuleInfoTz = RuleInfo

  ! Determine the Locales and time zones
  Locale = 0
  Zone   = 0
  DO Info = 1, LV$TimeZoneInfo
    ascii_sep(1) = SPACE
    sepEnd       = 1
    AsciiChr = Ascii_TimeZoneInfo(Info)
    CALL ParseString(AsciiChr,Ascii_sep,sepEnd, parseMax, start$chr_parse, end$chr_parse, parseEnd)
    IF(AsciiChr(1:4) == "Zone") THEN
      IF(parseEnd >= 2) THEN ; NameLocale    = AsciiChr(Start$chr_parse(2) :End$chr_parse(2)); ELSE   ; NameLocale    = SPACE; END IF
      IF(parseEnd >= 3) THEN ; dTimeUTCChr   = AsciiChr(Start$chr_parse(3) :End$chr_parse(3)); ELSE   ; dTimeUTCChr   = SPACE; END IF
      IF(parseEnd >= 4) THEN ; RuleChr       = AsciiChr(Start$chr_parse(4) :End$chr_parse(4)); ELSE   ; RuleChr       = SPACE; END IF
      IF(parseEnd >= 5) THEN ; AbbrZoneChr   = AsciiChr(Start$chr_parse(5) :End$chr_parse(5)); ELSE   ; AbbrZoneChr   = SPACE; END IF
      IF(parseEnd >= 6) THEN ; YearUntilChr  = AsciiChr(Start$chr_parse(6) :End$chr_parse(6)); ELSE   ; YearUntilChr  = SPACE; END IF
      IF(parseEnd >= 7) THEN ; MonthUntilChr = AsciiChr(Start$chr_parse(7) :End$chr_parse(7)); ELSE   ; MonthUntilChr = SPACE; END IF
      IF(parseEnd >= 8) THEN ; DayUntilChr   = AsciiChr(Start$chr_parse(8) :End$chr_parse(8)); ELSE   ; DayUntilChr   = SPACE; END IF
      IF(parseEnd >= 9) THEN ; TimeUntilChr  = AsciiChr(Start$chr_parse(9) :End$chr_parse(9)); ELSE   ; TimeUntilChr  = SPACE; END IF
      CALL AddTzLocaleAndZoneInfo(Locale, NameLocale, Zone, dTimeUTCChr, RuleChr, AbbrZoneChr, YearUntilChr, MonthUntilChr, DayUntilChr, TimeUntilChr)
    ELSE
      IF(parseEnd >= 1) THEN ; dTimeUTCChr   = AsciiChr(Start$chr_parse(1) :End$chr_parse(1)); ELSE   ; dTimeUTCChr   = SPACE; END IF
      IF(parseEnd >= 2) THEN ; RuleChr       = AsciiChr(Start$chr_parse(2) :End$chr_parse(2)); ELSE   ; RuleChr       = SPACE; END IF
      IF(parseEnd >= 3) THEN ; AbbrZoneChr   = AsciiChr(Start$chr_parse(3) :End$chr_parse(3)); ELSE   ; AbbrZoneChr   = SPACE; END IF
      IF(parseEnd >= 4) THEN ; YearUntilChr  = AsciiChr(Start$chr_parse(4) :End$chr_parse(4)); ELSE   ; YearUntilChr  = SPACE; END IF
      IF(parseEnd == 5) THEN ; MonthUntilChr = AsciiChr(Start$chr_parse(5) :End$chr_parse(5)); ELSE   ; MonthUntilChr = SPACE; END IF
      IF(parseEnd == 6) THEN ; DayUntilChr   = AsciiChr(Start$chr_parse(6) :End$chr_parse(6)); ELSE   ; DayUntilChr   = SPACE; END IF
      IF(parseEnd == 7) THEN ; TimeUntilChr  = AsciiChr(Start$chr_parse(7) :End$chr_parse(7)); ELSE   ; TimeUntilChr  = SPACE; END IF
      CALL AddTzZoneInfo(Locale, Zone, dTimeUTCChr, RuleChr, AbbrZoneChr, YearUntilChr, MonthUntilChr, DayUntilChr, TimeUntilChr)
    END IF
    IF(DebugLocal) &
      WRITE(*,FMT='(A)') TRIM(NameLocale)//" "//TRIM(dTimeUTCChr)//" "//TRIM(RuleChr)//" "//TRIM(AbbrZoneChr)//" "//TRIM(YearUntilChr)//" "//TRIM(MonthUntilChr)//" "//TRIM(DayUntilChr)//" "//TRIM(TimeUntilChr)
  END DO
  LV$LocaleTz = Locale
  LV$ZoneTz   = Zone
  RETURN
END SUBROUTINE ProcessTzInfo

SUBROUTINE ProcessTimeZoneFile(PathFile)
!
! Description: This subroutine processes all the daylight savings time rules in an Olson tz info file.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)       :: PathFile                       ! Path to the TzInfo File
!
!                                Local Variables
! ----------------------------+-------------------
INTEGER(1)                    :: TypeInput = 0
INTEGER(1),PARAMETER          :: RuleInformation = 1
INTEGER(1),PARAMETER          :: ZoneInformation = 2
INTEGER(1),PARAMETER          :: LinkInformation = 3
CHARACTER(256)                :: AsciiChr                       ! line read from time zone info
CHARACTER(256)                :: AsciiClean                     ! Ascii line cleaned of control characters and tabs
CHARACTER(256)                :: AsciiSingleChr                 ! Ascii line witn only single spaces
INTEGER(4)                    :: UnitTz                         ! Fortran I/O unit for the TZINFO file
CHARACTER(8)                  :: Ascii1stChr                    ! First 8 chartacter of the line.
INTEGER(4)                    :: chrKeep                        ! Number of characters to keep in a line
LOGICAL(4),PARAMETER          :: DebugLocal = .false.           ! Diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(DebugLocal) &
    WRITE(*,FMT='(A)') "Processing File "//TRIM(PathFile)
  OPEN(NewUnit=UnitTz,File=PathFile,ERR=3,status='old')

  DO
    AsciiChr = " "
    READ(UnitTz,FMT='(A)',END=1, ERR=2)AsciiChr
    AsciiClean  = TRIM(CleanString(AsciiChr))
    Ascii1stChr = ADJUSTL(AsciiClean(1:8)); IF( Ascii1stChr(1:1) == "#") CYCLE
    AsciiSingleChr = RemoveMultipleSpaces(AsciiClean)
    chrKeep      = INDEX(AsciiSingleChr,"#",BACK=.true.)
    if(chrKeep > 1) THEN
      chrKeep = chrKeep-1
    ELSE
      chrKeep = 80
    END IF

    IF(AsciiChr(1:4)=="Rule") THEN
      TypeInput       = RuleInformation
      LV$TimeRuleInfo = LV$TimeRuleInfo + 1
      Ascii_TimeRuleInfo(LV$TimeRuleInfo) = AsciiSingleChr(1:chrKeep)

    ELSE IF(AsciiChr(1:4)=="Zone") THEN
      TypeInput       = ZoneInformation
      LV$TimeZoneInfo = LV$TimeZoneInfo + 1
      Ascii_TimeZoneInfo(LV$TimeZoneInfo) = AsciiSingleChr(1:chrKeep)

    ELSE IF(AsciiChr(1:4)=="Link") THEN
      TypeInput       = ZoneInformation
      LV$TimeLinkInfo = LV$TimeLinkInfo + 1
      Ascii_TimeLinkInfo(LV$TimeLinkInfo) = AsciiSingleChr(1:chrKeep)

    ELSE IF(AsciiChr /= " ") THEN
      IF(TypeInput == ZoneInformation) THEN
         LV$TimeZoneInfo = LV$TimeZoneInfo + 1
         Ascii_TimeZoneInfo(LV$TimeZoneInfo) = AsciiSingleChr(1:chrKeep)
      END IF
    END IF
  END DO

1 CLOSE(UnitTz)
  RETURN

2 WRITE(*,FMT='(A)') 'ProcessTimeZoneFile: Error reading file '//TRIM(PathFile)
  CLOSE(UnitTz)
  RETURN

3 WRITE(*,FMT='(A)') 'ProcessTimeZoneFile: Error opening file '//TRIM(PathFile)
  CLOSE(UnitTz)
  RETURN

END SUBROUTINE ProcessTimeZoneFile

SUBROUTINE AddTzRuleAndInfo(Rule, NameRule, RuleInfo, YearFromChr, YearToChr, InMonthChr, OnDayChr, AtTimeChr, dTimeShiftChr, dAbbrZoneChr)
!
! Description: This subroutine adds a Tz Rule and an associated Tz RuleInfo
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   , INTENT(INOUT)  :: Rule                           ! Rule for applying a timezone shift
INTEGER(4)   , INTENT(INOUT)  :: RuleInfo                       ! Rule information defining the shift
CHARACTER(16), INTENT(IN)     :: NameRule                       ! Name of a TZInfo Rule                  (string)
CHARACTER(8) , INTENT(IN)     :: YearFromChr                    ! Year a rule start                      (string)
CHARACTER(8) , INTENT(IN)     :: YearToChr                      ! Year a rule ends                       (string)
CHARACTER(8) , INTENT(IN)     :: InMonthChr                     ! Month on which a rule is activated     (string)
CHARACTER(8) , INTENT(IN)     :: OnDayChr                       ! Day on which a rule is activated       (string)
CHARACTER(8) , INTENT(IN)     :: AtTimeChr                      ! Time at which a rule is activated      (string)
CHARACTER(8) , INTENT(IN)     :: dTimeShiftChr                  ! Time shift when the rule is applied    (string)
CHARACTER(8) , INTENT(IN)     :: dAbbrZoneChr                   ! Time Zone abbreviation change          (string)
!
!                                Local Variables
! ----------------------------+-------------------
LOGICAL(4)                    :: ErrorNameRule                  ! Error in input NameRule
LOGICAL(4)                    :: ErrorYearFrom                  ! Error in input YearFromChr
LOGICAL(4)                    :: ErrorYearTo                    ! Error in input YearToChr
LOGICAL(4)                    :: ErrorInMonth                   ! Error in input InMonthChr
LOGICAL(4)                    :: ErrorOnDay                     ! Error in input OnDayChr
LOGICAL(4)                    :: ErrorAtTime                    ! Error in input AtTimeChr
LOGICAL(4)                    :: ErrordTimeShift                ! Error in input dTimeSaveChr
LOGICAL(4)                    :: ErrordAbbrZone                 ! Error in input AbbrZoneChr
INTEGER(4)                    :: YearFrom                       ! Year a rule start                     (years)
INTEGER(4)                    :: YearTo                         ! Year a rule ends                      (years)
INTEGER(4)                    :: When                           ! When the rule is applied
INTEGER(4)                    :: Month                          ! Month in which a rule is activated    (month)
INTEGER(4)                    :: Day                            ! Day of month when a rule is active    [1 ... 31]
INTEGER(4)                    :: DayOfWeek                      ! Day of the week when a rule is active [1 ... 7]
INTEGER(4)                    :: Hour                           ! Hour at which a rule is activated     (hours)
INTEGER(4)                    :: Minute                         ! Minute at which a rule is activated   (minutes)
INTEGER(4)                    :: Second                         ! Second at which a rule is activated   (seconds)
INTEGER(4)                    :: dHour                          ! Hour at which a rule is activated     (hours)
INTEGER(4)                    :: dMinute                        ! Minute at which a rule is activated   (minutes)
LOGICAL(4)                    :: AtUTC                          ! Time At is UTC time, not local time
INTEGER(4)                    :: Chr                            ! Character index for testing
INTEGER(4)                    :: ChrColon                       ! Location of colon in time strings
INTEGER(4)                    :: ChrColon2                      ! Location of 2nd colon in time strings
CHARACTER(128)                :: StringResult                   ! Result of checking the inputs
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ErrorNameRule = LEN_TRIM(NameRule) < 2

  YearFrom = INT(NumAscii(TRIM(YearFromChr),ErrorYearFrom), KIND=4)

  ErrorYearTo = .false.
  SELECT CASE(TRIM(YearToChr))
  CASE("only") ; YearTo = YearFrom
  CASE("max")  ; YearTo = YearMax
  CASE DEFAULT ; YearTo = INT(NumAscii(TRIM(YearFromChr),ErrorYearTo), KIND=4)
  END SELECT

  ErrorInMonth = .false.
  SELECT CASE(TRIM(InMonthChr))
  CASE("Jan")  ; Month = January
  CASE("Feb")  ; Month = February
  CASE("Mar")  ; Month = March
  CASE("Apr")  ; Month = April
  CASE("May")  ; Month = May
  CASE("Jun")  ; Month = June
  CASE("Jul")  ; Month = July
  CASE("Aug")  ; Month = August
  CASE("Sep")  ; Month = September
  CASE("Oct")  ; Month = October
  CASE("Nov")  ; Month = November
  CASE("Dec")  ; Month = December
  CASE DEFAULT ; Month = 0; ErrorInMonth = .true.
  END SELECT

  chr = LEN_TRIM(OnDayChr)

  When = OnWhatDay
  IF(WithinClosedSet(1,chr,2))   When = OnDayOfMonth
  IF(INDEX(OnDayChr,"last") > 0) When = OnLastWeekday
  IF(INDEX(OnDayChr,">="  ) > 0) When = OnWeekDayAfter
  IF(INDEX(OnDayChr,"<="  ) > 0) When = OnWeekDayBefore

  ErrorOnDay = .false.
  SELECT CASE(When)
  CASE(OnDayOfMonth)   ; Day = INT(NumAscii(OnDayChr(1:2), ErrorOnDay), KIND=4)
  CASE(OnLastWeekday)  ; Day = 31
  CASE(OnWeekDayAfter) ; Day = INT(NumAscii(OnDayChr(6:7), ErrorOnDay), KIND=4)
  CASE(OnWeekDayBefore); Day = INT(NumAscii(OnDayChr(6:7), ErrorOnDay), KIND=4)
  CASE DEFAULT         ; Day = 0; ErrorOnDay = .true.
  END SELECT

  IF(When == OnDayOfMonth) THEN            ; DayOfWeek = 0
  ELSE IF(INDEX(OnDayChr,"Sun") > 0 ) THEN ; DayOfWeek = Sunday
  ELSE IF(INDEX(OnDayChr,"Mon") > 0 ) THEN ; DayOfWeek = Monday
  ELSE IF(INDEX(OnDayChr,"Tue") > 0 ) THEN ; DayOfWeek = Tuesday
  ELSE IF(INDEX(OnDayChr,"Wed") > 0 ) THEN ; DayOfWeek = Wednesday
  ELSE IF(INDEX(OnDayChr,"Thu") > 0 ) THEN ; DayOfWeek = Thursday
  ELSE IF(INDEX(OnDayChr,"Fri") > 0 ) THEN ; DayOfWeek = Friday
  ELSE IF(INDEX(OnDayChr,"Sat") > 0 ) THEN ; DayOfWeek = Saturday
  ELSE ; ErrorOnDay = .true.               ; DayOfWeek = 0
  END IF

  Chr = LEN_TRIM(AtTimeChr)
  DO
    Hour   = 0
    Minute = 0
    Second = 0
    AtUTC  = .false.
    ErrorAtTime = Chr < 4                                                    ;  IF(ErrorAtTime) EXIT
    AtUTC = AtTimeChr(Chr:Chr)=="u"
    IF(AtTimeChr(Chr:Chr) == "s" .or. AtTimeChr(Chr:Chr) == "u")  Chr = Chr-1
    ErrorAtTime = Chr < 4                                                    ;  IF(ErrorAtTime) EXIT
    ChrColon    = INDEX(AtTimeChr,":")
    ErrorAtTime = ChrColon < 2                                               ;  IF(ErrorAtTime) EXIT
    Hour        = INT(NumAscii(AtTimeChr(1:ChrColon-1), ErrorAtTime))        ;  IF(ErrorAtTime )EXIT
    ChrColon2   = INDEX(AtTimeChr,":",BACK = .true.)
    IF(ChrColon2 == ChrColon) THEN
      Minute  = INT(NumAscii(AtTimeChr(ChrColon+1:Chr), ErrorAtTime))        ;  IF(ErrorAtTime ) EXIT
      Second  = 0
    ELSE
      Minute  = INT(NumAscii(AtTimeChr(ChrColon+1:ChrColon2-1), ErrorAtTime));  IF(ErrorAtTime ) EXIT
      Second  = INT(NumAscii(AtTimeChr(ChrColon2+1:Chr), ErrorAtTime))       ;  IF(ErrorAtTime ) EXIT
    END IF
    ErrorAtTime = .false.
    EXIT
  END DO

  Chr = LEN_TRIM(dTimeShiftChr)
  DO
    dHour   = 0
    dMinute = 0
    ErrordTimeShift = Chr < 1                                                ;  IF(ErrordTimeShift)  EXIT
    ChrColon  = INDEX(dTimeShiftChr,":")
    IF(ChrColon < 1) THEN
      dMinute = INT(NumAscii(dTimeShiftChr(1:Chr), ErrordTimeShift))         ;  IF(ErrordTimeShift ) EXIT
    ELSE
      ErrordTimeShift = ChrColon < 2                                         ;  IF(ErrordTimeShift ) EXIT
      dHour   = INT(NumAscii(dTimeShiftChr(1:ChrColon-1), ErrordTimeShift))  ;  IF(ErrordTimeShift ) EXIT
      dMinute = INT(NumAscii(dTimeShiftChr(ChrColon+1:Chr), ErrordTimeShift));  IF(ErrordTimeShift ) EXIT
    END IF
    ErrordTimeShift = .false.
    EXIT
  END DO

  Chr = LEN_TRIM(dAbbrZoneChr)
  ErrordAbbrZone = Chr < 1

  StringResult = "Rule"
  IF(ErrorNameRule  ) StringResult = TRIM(StringResult)//" E-1:"//TRIM(NameRule)
  IF(ErrorYearFrom  ) StringResult = TRIM(StringResult)//" E-2:"//TRIM(YearFromChr)
  IF(ErrorYearTo    ) StringResult = TRIM(StringResult)//" E-3:"//TRIM(YearToChr)
  IF(ErrorInMonth   ) StringResult = TRIM(StringResult)//" E-4:"//TRIM(InMonthChr)
  IF(ErrorOnDay     ) StringResult = TRIM(StringResult)//" E-5:"//TRIM(OnDayChr)
  IF(ErrorAtTime    ) StringResult = TRIM(StringResult)//" E-6:"//TRIM(AtTimeChr)
  IF(ErrordTimeShift) StringResult = TRIM(StringResult)//" E-7:"//TRIM(dTimeShiftChr)
  IF(ErrordAbbrZone ) StringResult = TRIM(StringResult)//" E-8:"//TRIM(dAbbrZoneChr)

  IF(LEN_TRIM(StringResult) > 4) THEN
    WRITE(*,FMT='(A)') "AddTzRuleAndIfo: "//StringResult
    RETURN
  END IF

  Rule = Rule+1
  Tz_Rule(Rule)%Name = NameRule
  Tz_Rule(Rule)%Start$RuleInfo = 0

  RuleInfo = RuleInfo + 1
  Tz_RuleInfo(RuleInfo)%YearFrom       = YearFrom
  Tz_RuleInfo(RuleInfo)%YearTo         = YearTo
  Tz_RuleInfo(RuleInfo)%Month          = Month
  Tz_RuleInfo(RuleInfo)%Day            = Day
  Tz_RuleInfo(RuleInfo)%DayOfWeek      = DayOfWeek
  Tz_RuleInfo(RuleInfo)%When           = When
  Tz_RuleInfo(RuleInfo)%Hour           = Hour
  Tz_RuleInfo(RuleInfo)%Minute         = Minute
  Tz_RuleInfo(RuleInfo)%Second         = Second
  Tz_RuleInfo(RuleInfo)%dHourShift     = dHour
  Tz_RuleInfo(RuleInfo)%dMinuteShift   = dMinute
  Tz_RuleInfo(RuleInfo)%AtUTC          = AtUTC
  Tz_RuleInfo(RuleInfo)%dAbbrZone      = TRIM(dAbbrZoneChr)

  Tz_RuleInfo(RuleInfo)%Next$RuleInfo  = Tz_Rule(Rule)%Start$RuleInfo
  Tz_Rule(Rule)%Start$RuleInfo         = RuleInfo
  RETURN
END SUBROUTINE AddTzRuleAndInfo

SUBROUTINE AddTzRuleInfo(Rule, RuleInfo, YearFromChr, YearToChr, InMonthChr, OnDayChr, AtTimeChr, dTimeShiftChr, dAbbrZoneChr)
!
! Description: This subroutine adds a Tz RuleInfo
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   , INTENT(IN)     :: Rule                           ! Rule for applying a time zone shift
INTEGER(4)   , INTENT(INOUT)  :: RuleInfo                       ! Rule information defining the shift
CHARACTER(8) , INTENT(IN)     :: YearFromChr                    ! Year a rule start                      (string)
CHARACTER(8) , INTENT(IN)     :: YearToChr                      ! Year a rule ends                       (string)
CHARACTER(8) , INTENT(IN)     :: InMonthChr                     ! Month on which a rule is activated     (string)
CHARACTER(8) , INTENT(IN)     :: OnDayChr                       ! Day on which a rule is activated       (string)
CHARACTER(8) , INTENT(IN)     :: AtTimeChr                      ! Time at which a rule is activated      (string)
CHARACTER(8) , INTENT(IN)     :: dTimeShiftChr                  ! Time shift when the rule is applied    (string)
CHARACTER(8) , INTENT(IN)     :: dAbbrZoneChr                   ! Time Zone abbreviation change          (string)
!
!                                Local Variables
! ----------------------------+-------------------
LOGICAL(4)                    :: ErrorYearFrom                  ! Error in input YearFromChr
LOGICAL(4)                    :: ErrorYearTo                    ! Error in input YearToChr
LOGICAL(4)                    :: ErrorInMonth                   ! Error in input InMonthChr
LOGICAL(4)                    :: ErrorOnDay                     ! Error in input OnDayChr
LOGICAL(4)                    :: ErrorAtTime                    ! Error in input AtTimeChr
LOGICAL(4)                    :: ErrordTimeShift                ! Error in input dTimeSaveChr
LOGICAL(4)                    :: ErrordAbbrZone                 ! Error in input AbbrZoneChr
INTEGER(4)                    :: YearFrom                       ! Year a rule start                     (years)
INTEGER(4)                    :: YearTo                         ! Year a rule ends                      (years)
INTEGER(4)                    :: When                           ! When the rule is applied
INTEGER(4)                    :: Month                          ! Month in which a rule is activated    (month)
INTEGER(4)                    :: Day                            ! Day of month when a rule is active    [1 ... 31]
INTEGER(4)                    :: DayOfWeek                      ! Day of the week when a rule is active [1 ... 7]
INTEGER(4)                    :: Hour                           ! Hour at which a rule is activated     (hours)
INTEGER(4)                    :: Minute                         ! Minute at which a rule is activated   (minutes)
INTEGER(4)                    :: Second                         ! Second at which a rule is activated   (seconds)
INTEGER(4)                    :: dHour                          ! Hour   time shift for the rule          (hours)
INTEGER(4)                    :: dMinute                        ! Minute time shift for the rule        (minutes)
LOGICAL(4)                    :: AtUTC                          ! Time At is UTC time, not local time
INTEGER(4)                    :: Chr                            ! Character index for testing
INTEGER(4)                    :: ChrColon                       ! Location of colon in time strings
INTEGER(4)                    :: ChrColon2                      ! Location of 2nd colon in time strings
CHARACTER(128)                :: StringResult                   ! Result of checking the inputs
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  YearFrom = INT(NumAscii(TRIM(YearFromChr),ErrorYearFrom), KIND=4)

  ErrorYearTo = .false.
  SELECT CASE(TRIM(YearToChr))
  CASE("only") ; YearTo = YearFrom
  CASE("max")  ; YearTo = YearMax
  CASE DEFAULT ; YearTo = INT(NumAscii(TRIM(YearFromChr),ErrorYearTo), KIND=4)
  END SELECT

  ErrorInMonth = .false.
  SELECT CASE(TRIM(InMonthChr))
  CASE("Jan")  ; Month = January
  CASE("Feb")  ; Month = February
  CASE("Mar")  ; Month = March
  CASE("Apr")  ; Month = April
  CASE("May")  ; Month = May
  CASE("Jun")  ; Month = June
  CASE("Jul")  ; Month = July
  CASE("Aug")  ; Month = August
  CASE("Sep")  ; Month = September
  CASE("Oct")  ; Month = October
  CASE("Nov")  ; Month = November
  CASE("Dec")  ; Month = December
  CASE DEFAULT ; Month = 0; ErrorInMonth = .true.
  END SELECT

  chr = LEN_TRIM(OnDayChr)

  When = OnWhatDay
  IF(WithinClosedSet(1,chr,2))   When = OnDayOfMonth
  IF(INDEX(OnDayChr,"last") > 0) When = OnLastWeekday
  IF(INDEX(OnDayChr,">="  ) > 0) When = OnWeekDayAfter
  IF(INDEX(OnDayChr,"<="  ) > 0) When = OnWeekDayBefore

  ErrorOnDay = .false.
  SELECT CASE(When)
  CASE(OnDayOfMonth)   ; Day = INT(NumAscii(OnDayChr(1:2), ErrorOnDay), KIND=4)
  CASE(OnLastWeekday)  ; Day = 31
  CASE(OnWeekDayAfter) ; Day = INT(NumAscii(OnDayChr(6:7), ErrorOnDay), KIND=4)
  CASE(OnWeekDayBefore); Day = INT(NumAscii(OnDayChr(6:7), ErrorOnDay), KIND=4)
  CASE DEFAULT         ; Day = 0; ErrorOnDay = .true.
  END SELECT

  IF(When == OnDayOfMonth) THEN            ; DayOfWeek = 0
  ELSE IF(INDEX(OnDayChr,"Sun") > 0 ) THEN ; DayOfWeek = Sunday
  ELSE IF(INDEX(OnDayChr,"Mon") > 0 ) THEN ; DayOfWeek = Monday
  ELSE IF(INDEX(OnDayChr,"Tue") > 0 ) THEN ; DayOfWeek = Tuesday
  ELSE IF(INDEX(OnDayChr,"Wed") > 0 ) THEN ; DayOfWeek = Wednesday
  ELSE IF(INDEX(OnDayChr,"Thu") > 0 ) THEN ; DayOfWeek = Thursday
  ELSE IF(INDEX(OnDayChr,"Fri") > 0 ) THEN ; DayOfWeek = Friday
  ELSE IF(INDEX(OnDayChr,"Sat") > 0 ) THEN ; DayOfWeek = Saturday
  ELSE ; ErrorOnDay = .true.               ; DayOfWeek = 0
  END IF

  Chr = LEN_TRIM(AtTimeChr)
  DO
    Hour   = 0
    Minute = 0
    Second = 0
    AtUTC  = .false.
    ErrorAtTime = Chr < 4                                                    ;  IF(ErrorAtTime) EXIT
    AtUTC = AtTimeChr(Chr:Chr)=="u"
    IF(AtTimeChr(Chr:Chr) == "s" .or. AtTimeChr(Chr:Chr) == "u")  Chr = Chr-1
    ErrorAtTime = Chr < 4                                                    ;  IF(ErrorAtTime) EXIT
    ChrColon    = INDEX(AtTimeChr,":")
    ErrorAtTime = ChrColon < 2                                               ;  IF(ErrorAtTime) EXIT
    Hour        = INT(NumAscii(AtTimeChr(1:ChrColon-1), ErrorAtTime))        ;  IF(ErrorAtTime )EXIT
    ChrColon2   = INDEX(AtTimeChr,":",BACK = .true.)
    IF(ChrColon2 == ChrColon) THEN
      Minute  = INT(NumAscii(AtTimeChr(ChrColon+1:Chr), ErrorAtTime))        ;  IF(ErrorAtTime ) EXIT
      Second  = 0
    ELSE
      Minute  = INT(NumAscii(AtTimeChr(ChrColon+1:ChrColon2-1), ErrorAtTime));  IF(ErrorAtTime ) EXIT
      Second  = INT(NumAscii(AtTimeChr(ChrColon2+1:Chr), ErrorAtTime))       ;  IF(ErrorAtTime ) EXIT
    END IF
    ErrorAtTime = .false.
    EXIT
  END DO

  Chr = LEN_TRIM(dTimeShiftChr)
  DO
    dHour   = 0
    dMinute = 0
    ErrordTimeShift = Chr < 1                                                ;  IF(ErrordTimeShift)  EXIT
    ChrColon  = INDEX(dTimeShiftChr,":")
    IF(ChrColon < 1) THEN
      dMinute = INT(NumAscii(dTimeShiftChr(1:Chr), ErrordTimeShift))         ;  IF(ErrordTimeShift ) EXIT
    ELSE
      ErrordTimeShift = ChrColon < 2                                         ;  IF(ErrordTimeShift ) EXIT
      dHour   = INT(NumAscii(dTimeShiftChr(1:ChrColon-1), ErrordTimeShift))  ;  IF(ErrordTimeShift ) EXIT
      dMinute = INT(NumAscii(dTimeShiftChr(ChrColon+1:Chr), ErrordTimeShift));  IF(ErrordTimeShift ) EXIT
    END IF
    ErrordTimeShift = .false.
    EXIT
  END DO

  Chr = LEN_TRIM(dAbbrZoneChr)
  ErrordAbbrZone = Chr < 1

  StringResult = "Rule"
  IF(ErrorYearFrom  ) StringResult = TRIM(StringResult)//" E-"//TRIM(YearFromChr)
  IF(ErrorYearTo    ) StringResult = TRIM(StringResult)//" E-"//TRIM(YearToChr)
  IF(ErrorInMonth   ) StringResult = TRIM(StringResult)//" E-"//TRIM(InMonthChr)
  IF(ErrorOnDay     ) StringResult = TRIM(StringResult)//" E-"//TRIM(OnDayChr)
  IF(ErrorAtTime    ) StringResult = TRIM(StringResult)//" E-"//TRIM(AtTimeChr)
  IF(ErrordTimeShift) StringResult = TRIM(StringResult)//" E-"//TRIM(dTimeShiftChr)
  IF(ErrordAbbrZone ) StringResult = TRIM(StringResult)//" E-"//TRIM(dAbbrZoneChr)

  IF(LEN_TRIM(StringResult) > 4) THEN
    WRITE(*,FMT='(A)') "AddTzRuleIfo: "//StringResult
    RETURN
  END IF

  RuleInfo = RuleInfo + 1
  Tz_RuleInfo(RuleInfo)%YearFrom       = YearFrom
  Tz_RuleInfo(RuleInfo)%YearTo         = YearTo
  Tz_RuleInfo(RuleInfo)%Month          = Month
  Tz_RuleInfo(RuleInfo)%Day            = Day
  Tz_RuleInfo(RuleInfo)%DayOfWeek      = DayOfWeek
  Tz_RuleInfo(RuleInfo)%When           = When
  Tz_RuleInfo(RuleInfo)%Hour           = Hour
  Tz_RuleInfo(RuleInfo)%Minute         = Minute
  Tz_RuleInfo(RuleInfo)%Second         = Second
  Tz_RuleInfo(RuleInfo)%dHourShift     = dHour
  Tz_RuleInfo(RuleInfo)%dMinuteShift   = dMinute
  Tz_RuleInfo(RuleInfo)%AtUTC          = AtUTC
  Tz_RuleInfo(RuleInfo)%dAbbrZone      = TRIM(dAbbrZoneChr)

  Tz_RuleInfo(RuleInfo)%Next$RuleInfo  = Tz_Rule(Rule)%Start$RuleInfo
  Tz_Rule(Rule)%Start$RuleInfo         = RuleInfo
  RETURN
END SUBROUTINE AddTzRuleInfo

SUBROUTINE AddTzLocaleAndZoneInfo(Locale, NameLocale, Zone, dTimeUTCChr, RuleChr, AbbrZone, YearUntilChr, MonthUntilChr, DayUntilChr, TimeUntilChr)
!
! Description: This routine adds a new Locale and its associated time zone information
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   , INTENT(INOUT)  :: Locale                         ! Index of the Time Zone Locale
CHARACTER(32), INTENT(IN)     :: NameLocale                     ! Continent/City of the Locale           (string)
INTEGER(4)   , INTENT(INOUT)  :: Zone                           ! Index of the Time Zone specification
CHARACTER(8) , INTENT(IN)     :: dTimeUTCChr                    ! Time shift from UTC                    (string)
CHARACTER(16), INTENT(IN)     :: RuleChr                        ! Time zone rule that is used            (string)
CHARACTER(8) , INTENT(IN)     :: AbbrZone                       ! Time Zone abbreviation                 (string)
CHARACTER(8) , INTENT(IN)     :: YearUntilChr                   ! Year when this specification expires   (string)
CHARACTER(8) , INTENT(IN)     :: MonthUntilChr                  ! Month hwhen this specification expires (string)
CHARACTER(8) , INTENT(IN)     :: DayUntilChr                    ! Day owhen this specification expires   (string)
CHARACTER(8) , INTENT(IN)     :: TimeUntilChr                   ! Time when this specification expires   (string)
!
!                                Local Variables
! ----------------------------+-------------------
LOGICAL(4)                    :: ErrorNameLocale                ! Error in input NameLocale
LOGICAL(4)                    :: ErrordTimeUTC                  ! Error in input dTimeUTCChr
LOGICAL(4)                    :: ErrorRule                      ! Error in input RuleChr
LOGICAL(4)                    :: ErrorAbbrZone                  ! Error in input AbbrZoneChr
LOGICAL(4)                    :: ErrorYearUntil                 ! Error in input YearUntilChr
LOGICAL(4)                    :: ErrorMonthUntil                ! Error in input MonthUntilChr
LOGICAL(4)                    :: ErrorDayUntil                  ! Error in input DayUntilChr
LOGICAL(4)                    :: ErrorTimeUntil                 ! Error in input TimeUntilChr
INTEGER(4)                    :: ChrSlash1st                    ! Location of slash in time strings
INTEGER(4)                    :: ChrSlash2nd                    ! Location of 2nd slash in time strings
CHARACTER(16)                 :: Continent                      ! Continent (or Ocean) of the Locale
CHARACTER(16)                 :: City                           ! City of the Locale
INTEGER(4)                    :: dHour                          ! UTC delta hours                       (hours)
INTEGER(4)                    :: dMinute                        ! UTC delta minutes                     (minutes)
REAL(8)                       :: dSeconds                       ! UTC delta seconds                     (seconds)
INTEGER(4)                    :: dSecond                        ! dSeconds cast as an integer           (seconds)
INTEGER(4)                    :: dHourRule                      ! Time shift delta hours                (hours)
INTEGER(4)                    :: dMinuteRule                    ! Time shift delta minutes              (minutes)
REAL(8)                       :: dSecondsRule                   ! Time shift delta seconds              (seconds)
LOGICAL(4)                    :: UseRule                        ! Use rule for time shift
LOGICAL(4)                    :: AtUTCRule                      ! Use UTC for time shift
INTEGER(4)                    :: Rule                           ! Rule for applying a time zone
INTEGER(4)                    :: YearUntil                      ! Year a time zone definition expires   (years)
INTEGER(4)                    :: MonthUntil                     ! Month in which a rule is activated    (month)
INTEGER(4)                    :: DayUntil                       ! Day of month when a rule is active    [1 ... 31]
INTEGER(4)                    :: Hour                           ! Hour at which a rule is activated     (hours)
INTEGER(4)                    :: Minute                         ! Minute at which a rule is activated   (minutes)
REAL(8)                       :: Seconds                        ! Second at which a rule is activated   (seconds)
INTEGER(4)                    :: Second                         ! Seconds cast as an integer            (seconds)
LOGICAL(4)                    :: AtUTC                          ! Time At is UTC time, not local time
CHARACTER(128)                :: StringResult                   ! Result of checking the inputs
INTEGER(4)                    :: DayMax_month(0:13)             ! Last day of each month
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time elapsed in year for each month   (10^-7 seconds)
LOGICAL(4)    ,PARAMETER      :: DebugLocal = .false.           ! Local diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ChrSlash1st     = INDEX(NameLocale, "/")
  ChrSlash2nd     = INDEX(NameLocale, "/", BACK=.true.)
  ErrorNameLocale = ChrSlash1st > 4 .and. ChrSlash2nd == LEN_TRIM(NameLocale)
  IF(.not.ErrorNameLocale) THEN
    Continent = NameLocale(1:ChrSlash1st-1)
    City      = NameLocale(ChrSlash2nd+1:)
  END IF

  CALL GetHourMinuteAndSeconds(dTimeUTCChr, dHour, dMinute, dSeconds, AtUTC, ErrordTimeUTC)

  Rule = RuleLookup(RuleChr, ErrorRule)
  IF(ErrorRule) THEN
    CALL GetHourMinuteAndSeconds(RuleChr, dHourRule, dMinuteRule, dSecondsRule, AtUTCRule, ErrorRule)
    IF(DebugLocal) &
        WRITE(*,FMT='("RuleChr:",A16," dHourRule: ",I4," dMinuteRule: ",I4," ErrorRule",L1)') &
                       RuleChr, dHourRule, dMinuteRule,ErrorRule
    UseRule = .false.
  ELSE
    UseRule = Rule > 0
    IF(.not.UseRule) THEN
      dHourRule    = 0
      dMinuteRule  = 0
      dSecondsRule = 0d0
    END IF
  END IF

  ErrorAbbrZone = LEN_TRIM(AbbrZone) < 1

  IF(LEN_TRIM(YearUntilChr) < 1) THEN
    YearUntil  = YearMax ;                       ErrorYearUntil  = .false.
    MonthUntil = January ;                       ErrorMonthUntil = .false.
    DayUntil   = 1       ;                       ErrorDayUntil   = .false.
  ELSE IF(LEN_TRIM(MonthUntilChr) < 1) THEN
    YearUntil  = INT(NumAscii(TRIM(YearUntilChr),ErrorYearUntil), KIND=4)
    MonthUntil = January ;                       ErrorMonthUntil = .false.
    DayUntil   = 1       ;                       ErrorDayUntil   = .false.
  ELSE IF(LEN_TRIM(DayUntilChr) < 1) THEN
    YearUntil  = INT(NumAscii(TRIM(YearUntilChr),ErrorYearUntil), KIND=4)
    MonthUntil = MonthLookup(MonthUntilChr,      ErrorMonthUntil)
    DayUntil   = 1       ;                       ErrorDayUntil   = .false.
  ELSE
    YearUntil  = INT(NumAscii(TRIM(YearUntilChr),ErrorYearUntil), KIND=4)
    MonthUntil = MonthLookup(MonthUntilChr,      ErrorMonthUntil)
    DayUntil   = INT(NumAscii(TRIM(DayUntilChr) ,ErrorDayUntil ), KIND=4)
  END IF

  IF(LEN_TRIM(TimeUntilChr) < 1) THEN
    Hour    = 0
    Minute  = 0
    Seconds = 0.0
    AtUTC   = .false.
    ErrorTimeUntil = .false.
  ELSE
    CALL GetHourMinuteAndSeconds(TimeUntilChr, Hour, Minute, Seconds, AtUTC, ErrorTimeUntil)
  END IF

  StringResult = "Zone"

  IF(ErrorNameLocale) StringResult = TRIM(StringResult)//" E-1:"//TRIM(NameLocale)
  IF(ErrordTimeUTC  ) StringResult = TRIM(StringResult)//" E-2:"//TRIM(dTimeUTCChr)
  IF(ErrorRule      ) StringResult = TRIM(StringResult)//" E-3:"//TRIM(RuleChr)
  IF(ErrorAbbrZone  ) StringResult = TRIM(StringResult)//" E-4:"//TRIM(AbbrZone)
  IF(ErrorYearUntil ) StringResult = TRIM(StringResult)//" E-5:"//TRIM(YearUntilChr)
  IF(ErrorMonthUntil) StringResult = TRIM(StringResult)//" E-6:"//TRIM(MonthUntilChr)
  IF(ErrorDayUntil  ) StringResult = TRIM(StringResult)//" E-7:"//TRIM(DayUntilChr)
  IF(ErrorTimeUntil ) StringResult = TRIM(StringResult)//" E-8:"//TRIM(TimeUntilChr)

  IF(LEN_TRIM(StringResult) > 4) THEN
    WRITE(*,FMT='(A)') "AddTzLocaleAndZoneInfo: "//StringResult
    RETURN
  END IF
  Second  = INT(Seconds , KIND=4)
  dSecond = INT(dSeconds, KIND=4)

  Locale = Locale+1
  Zone   = Zone + 1
  Tz_Locale(Locale)%Name      = NameLocale
  Tz_Locale(Locale)%Continent = Continent
  Tz_Locale(Locale)%City      = City
  Tz_Locale(Locale)%ZoneStart = Zone
  Tz_Locale(Locale)%ZoneEnd   = Zone

  Tz_Zone(Zone)%Locale        = Locale
  Tz_Zone(Zone)%dHourUTC      = dHour
  Tz_Zone(Zone)%dMinuteUTC    = dMinute
  Tz_Zone(Zone)%dSecondUTC    = dSecond
  Tz_Zone(Zone)%dHoursUTC     = DBLE(dHour) + DBLE(dMinute)/60d0 + DBLE(dSecond)/3600d0
  Tz_Zone(Zone)%UseRule       = UseRule
  Tz_Zone(Zone)%Rule          = Rule
  Tz_Zone(Zone)%AbbrZone      = AbbrZone
  Tz_Zone(Zone)%YearUntil     = YearUntil
  Tz_Zone(Zone)%MonthUntil    = MonthUntil
  Tz_Zone(Zone)%DayUntil      = DayUntil
  Tz_Zone(Zone)%AtUTC         = AtUTC
  Tz_Zone(Zone)%HourUntil     = Hour
  Tz_Zone(Zone)%MinuteUntil   = Minute
  Tz_Zone(Zone)%SecondUntil   = Second
  Tz_Zone(Zone)%dHourRule     = dHourRule
  Tz_Zone(Zone)%dMinuteRule   = dMinuteRule
  IF(.not.UseRule) THEN
    Tz_Zone(Zone)%dHour       = dHourRule
    Tz_Zone(Zone)%dMinute     = dMinuteRule
    Tz_Zone(Zone)%dSecond     = INT(dSecondsRule, KIND=4)
  END IF

  IF(LeapYear(YearUntil)) THEN
    DayMax_month = DaysLeapYear_month
    dTime_month  = dTimeLeapYear_month
  ELSE
    DayMax_month = DaysNormalYear_month
    dTime_month  = dTimeNormalYear_month
  END IF
  CALL AdjustMonthTimesForLeapSeconds(YearUntil, dTime_month)

  IF(.not.AtUTC) THEN
    Hour   = Hour   - dHour
    Minute = Minute - dMinute
    Second = Second - dSecond
  END IF

  Tz_Zone(Zone)%TimeUntil = TimeStart_Year(YearUntil) + dTime_month(MonthUntil-1) + 1 &
                          + TimePerDay      * (DayUntil - 1)                          &
                          + TimePerHour     * Hour                                    &
                          + TimePerMinute   * Minute                                  &
                          + TimePerSecond   * Second
  RETURN
END SUBROUTINE AddTzLocaleAndZoneInfo

SUBROUTINE AddTzZoneInfo(Locale, Zone, dTimeUTCChr, RuleChr, AbbrZone, YearUntilChr, MonthUntilChr, DayUntilChr, TimeUntilChr)
!
! Description: This routine adds new time zone information
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   , INTENT(IN)     :: Locale                         ! Index of the Time Zone Locale
INTEGER(4)   , INTENT(INOUT)  :: Zone                           ! Index of the Time Zone specification
CHARACTER(8) , INTENT(IN)     :: dTimeUTCChr                    ! Time shift from UTC                    (string)
CHARACTER(16), INTENT(IN)     :: RuleChr                        ! Time zone rule that is used            (string)
CHARACTER(8) , INTENT(IN)     :: AbbrZone                       ! Time Zone abbreviation                 (string)
CHARACTER(8) , INTENT(IN)     :: YearUntilChr                   ! Year when this specification expires   (string)
CHARACTER(8) , INTENT(IN)     :: MonthUntilChr                  ! Mont hwhen this specification expires  (string)
CHARACTER(8) , INTENT(IN)     :: DayUntilChr                    ! Day owhen this specification expires   (string)
CHARACTER(8) , INTENT(IN)     :: TimeUntilChr                   ! Time when this specification expires   (string)
!
!                                Local Variables
! ----------------------------+-------------------
!
!                                Local Variables
! ----------------------------+-------------------
LOGICAL(4)                    :: ErrordTimeUTC                  ! Error in input dTimeUTCChr
LOGICAL(4)                    :: ErrorRule                      ! Error in input RuleChr
LOGICAL(4)                    :: ErrorAbbrZone                  ! Error in input AbbrZoneChr
LOGICAL(4)                    :: ErrorYearUntil                 ! Error in input YearUntilChr
LOGICAL(4)                    :: ErrorMonthUntil                ! Error in input MonthUntilChr
LOGICAL(4)                    :: ErrorDayUntil                  ! Error in input DayUntilChr
LOGICAL(4)                    :: ErrorTimeUntil                 ! Error in input TimeUntilChr
INTEGER(4)                    :: dHour                          ! UTC delta hours                       (hours)
INTEGER(4)                    :: dMinute                        ! UTC delta minutes                     (minutes)
REAL(8)                       :: dSeconds                       ! UTC delta seconds                     (seconds)
INTEGER(4)                    :: dSecond                        ! dSeconds cast as an integer           (seconds)
INTEGER(4)                    :: dHourRule                      ! Time shift delta hours                (hours)
INTEGER(4)                    :: dMinuteRule                    ! Time shift delta minutes              (minutes)
REAL(8)                       :: dSecondsRule                   ! Time shift delta seconds              (seconds)
LOGICAL(4)                    :: UseRule                        ! Use rule for time shift
LOGICAL(4)                    :: AtUTCRule                      ! Use UTC for time shift
INTEGER(4)                    :: Rule                           ! Rule for applying a time zone
INTEGER(4)                    :: YearUntil                      ! Year a time zone definition expires   (years)
INTEGER(4)                    :: MonthUntil                     ! Month in which a rule is activated    (month)
INTEGER(4)                    :: DayUntil                       ! Day of month when a rule is active    [1 ... 31]
INTEGER(4)                    :: Hour                           ! Hour at which a rule is activated     (hours)
INTEGER(4)                    :: Minute                         ! Minute at which a rule is activated   (minutes)
REAL(8)                       :: Seconds                        ! Second at which a rule is activated   (seconds)
INTEGER(4)                    :: Second                         ! Seconds cast as an integer            (seconds)
LOGICAL(4)                    :: AtUTC                          ! Time At is UTC time, not local time
CHARACTER(128)                :: StringResult                   ! Result of checking the inputs
INTEGER(4)                    :: DayMax_month(0:13)             ! Last day of each month
INTEGER(MJD)                  :: dTime_month(0:13)              ! Time elapsed in year for each month   (10^-7 seconds)
LOGICAL(4)    ,PARAMETER      :: DebugLocal = .false.           ! Local diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL GetHourMinuteAndSeconds(dTimeUTCChr, dHour, dMinute, dSeconds, AtUTC, ErrordTimeUTC)

  Rule = RuleLookup(RuleChr, ErrorRule)
  IF(ErrorRule) THEN
    CALL GetHourMinuteAndSeconds(RuleChr, dHourRule, dMinuteRule, dSecondsRule, AtUTCRule, ErrorRule)
    IF(DebugLocal) &
        WRITE(*,FMT='("RuleChr:",A16," dHourRule: ",I4," dMinuteRule: ",I4," ErrorRule",L1)') &
                       RuleChr, dHourRule, dMinuteRule,ErrorRule
    UseRule = .false.
  ELSE
    UseRule = Rule > 0
    IF(.not.UseRule) THEN
      dHourRule    = 0
      dMinuteRule  = 0
      dSecondsRule = 0.0d0
    END IF
  END IF

  ErrorAbbrZone = LEN_TRIM(AbbrZone) < 1

  IF(LEN_TRIM(YearUntilChr) < 1) THEN
    YearUntil  = YearMax ;                       ErrorYearUntil  = .false.
    MonthUntil = January ;                       ErrorMonthUntil = .false.
    DayUntil   = 1       ;                       ErrorDayUntil   = .false.
  ELSE IF(LEN_TRIM(MonthUntilChr) < 1) THEN
    YearUntil  = INT(NumAscii(TRIM(YearUntilChr),ErrorYearUntil), KIND=4)
    MonthUntil = January ;                       ErrorMonthUntil = .false.
    DayUntil   = 1       ;                       ErrorDayUntil   = .false.
  ELSE IF(LEN_TRIM(DayUntilChr) < 1) THEN
    YearUntil  = INT(NumAscii(TRIM(YearUntilChr),ErrorYearUntil), KIND=4)
    MonthUntil = MonthLookup(MonthUntilChr,      ErrorMonthUntil)
    DayUntil   = 1       ;                       ErrorDayUntil   = .false.
  ELSE
    YearUntil  = INT(NumAscii(TRIM(YearUntilChr),ErrorYearUntil), KIND=4)
    MonthUntil = MonthLookup(MonthUntilChr,      ErrorMonthUntil)
    DayUntil   = INT(NumAscii(TRIM(DayUntilChr) ,ErrorDayUntil ), KIND=4)
  END IF

  IF(LEN_TRIM(TimeUntilChr) < 1) THEN
    Hour    = 0
    Minute  = 0
    Seconds = 0.0
    AtUTC   = .false.
    ErrorTimeUntil = .false.
  ELSE
    CALL GetHourMinuteAndSeconds(TimeUntilChr, Hour, Minute, Seconds, AtUTC, ErrorTimeUntil)
  END IF

  StringResult = "Zone"

  IF(ErrordTimeUTC  ) StringResult = TRIM(StringResult)//" E-1:"//TRIM(dTimeUTCChr)
  IF(ErrorRule      ) StringResult = TRIM(StringResult)//" E-2:"//TRIM(RuleChr)
  IF(ErrorAbbrZone  ) StringResult = TRIM(StringResult)//" E-3:"//TRIM(AbbrZone)
  IF(ErrorYearUntil ) StringResult = TRIM(StringResult)//" E-4:"//TRIM(YearUntilChr)
  IF(ErrorMonthUntil) StringResult = TRIM(StringResult)//" E-5:"//TRIM(MonthUntilChr)
  IF(ErrorDayUntil  ) StringResult = TRIM(StringResult)//" E-6:"//TRIM(DayUntilChr)
  IF(ErrorTimeUntil ) StringResult = TRIM(StringResult)//" E-7:"//TRIM(TimeUntilChr)

  IF(LEN_TRIM(StringResult) > 4) THEN
    WRITE(*,FMT='(A)') "AddTzZoneInfo: "//StringResult
    RETURN
  END IF
  Second  = INT(Seconds , KIND=4)
  dSecond = INT(dSeconds, KIND=4)

  Zone   = Zone + 1
  Tz_Locale(Locale)%ZoneEnd   = Zone

  Tz_Zone(Zone)%Locale        = Locale
  Tz_Zone(Zone)%dHourUTC      = dHour
  Tz_Zone(Zone)%dMinuteUTC    = dMinute
  Tz_Zone(Zone)%dSecondUTC    = dSecond
  Tz_Zone(Zone)%dHoursUTC     = DBLE(dHour) + DBLE(dMinute)/60d0 + DBLE(dSecond)/3600d0
  Tz_Zone(Zone)%UseRule       = UseRule
  Tz_Zone(Zone)%Rule          = Rule
  Tz_Zone(Zone)%AbbrZone      = AbbrZone
  Tz_Zone(Zone)%YearUntil     = YearUntil
  Tz_Zone(Zone)%MonthUntil    = MonthUntil
  Tz_Zone(Zone)%DayUntil      = DayUntil
  Tz_Zone(Zone)%AtUTC         = AtUTC
  Tz_Zone(Zone)%HourUntil     = Hour
  Tz_Zone(Zone)%MinuteUntil   = Minute
  Tz_Zone(Zone)%SecondUntil   = Second
  Tz_Zone(Zone)%UseRule       = UseRule
  Tz_Zone(Zone)%dHourRule     = dHourRule
  Tz_Zone(Zone)%dMinuteRule   = dMinuteRule
  IF(.not.UseRule) THEN
    Tz_Zone(Zone)%dHour       = dHourRule
    Tz_Zone(Zone)%dMinute     = dMinuteRule
    Tz_Zone(Zone)%dSecond     = INT(dSecondsRule, KIND=4)
  END IF

  IF(LeapYear(YearUntil)) THEN
    DayMax_month = DaysLeapYear_month
    dTime_month  = dTimeLeapYear_month
  ELSE
    DayMax_month = DaysNormalYear_month
    dTime_month  = dTimeNormalYear_month
  END IF
  CALL AdjustMonthTimesForLeapSeconds(YearUntil, dTime_month)

  IF(.not.AtUTC) THEN
    Hour   = Hour   - dHour
    Minute = Minute - dMinute
    Second = Second - dSecond
  END IF


  Tz_Zone(Zone)%TimeUntil = TimeStart_Year(YearUntil) + dTime_month(MonthUntil-1) + 1 &
                          + TimePerDay      * (DayUntil - 1)                          &
                          + TimePerHour     * Hour                                    &
                          + TimePerMinute   * Minute                                  &
                          + TimePerSecond   * Second
  RETURN
END SUBROUTINE AddTzZoneInfo

SUBROUTINE GetHourMinuteAndSeconds(AsciiTimeChr, Hour, Minute, Seconds, AtUTC, Error)
!
! Description: This routine parses the hour, minute and second from a TZ time string.
!              The logical AtUTC is true if a "u" is appended to the string; false otherwise
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: AsciiTimeChr                   ! Ascii time string                 (string)
INTEGER(4)  , INTENT(OUT)     :: Hour                           ! Hour decoded from string          (hours)
INTEGER(4)  , INTENT(OUT)     :: Minute                         ! Minute decoded from string        (minutes)
REAL(8)     , INTENT(OUT)     :: Seconds                        ! Second decoded from string        (seconds)
LOGICAL(4)  , INTENT(OUT)     :: AtUTC                          ! Time is relative to UTC
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Error found in decoding
!
!                                Local Variables
! ----------------------------+-------------------

INTEGER(4)                    :: Chr                            ! Character index for testing
INTEGER(4)  ,PARAMETER        :: parseMax = 3                   ! Maximum number of tokens
INTEGER(4)                    :: parseEnd                       ! Number of parsed tokens
CHARACTER(1)                  :: Ascii_sep(1)                   ! List of separator characters
INTEGER(4)                    :: sepEnd                         ! Number of separator chartacters
INTEGER(4)                    :: start$chr_parse(3)             ! Start character of the tokens
INTEGER(4)                    :: end$chr_parse(3)               ! End character of the tokens
LOGICAL(4)                    :: ErrorHour                      ! Error found in decoding the hour
LOGICAL(4)                    :: ErrorMinute                    ! Error found in decoding the minute
LOGICAL(4)                    :: ErrorSeconds                   ! Error found in decoding the seconds
CHARACTER(8)                  :: HourChr                        ! Hour string
CHARACTER(8)                  :: MinuteChr                      ! Minute string
CHARACTER(8)                  :: SecondChr                      ! Second string
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Hour    = 0
  Minute  = 0
  Seconds = 0d0
  AtUTC   = .false.
  Error   = .false.

  Chr = LEN_TRIM(AsciiTimeChr, KIND=4)
  !
  ! The empty string is and error
  IF(Chr < 1) THEN
    Error = .true.
    RETURN
  END IF

  AtUTC = AsciiTimeChr(Chr:Chr) == "u"
  IF(AtUTC .or. AsciiTimeChr(Chr:Chr) == "s") THEN
    Chr = Chr - 1
    IF(Chr == 0) THEN
      Error = .true.
      RETURN
    END IF
  END IF

  Ascii_sep(1) = ":"
  sepEnd       = 1
  CALL ParseString(AsciiTimeChr(1:Chr),Ascii_sep,sepEnd, parseMax, start$chr_parse, end$chr_parse, parseEnd)

  SELECT CASE(parseEnd)
  CASE(1)
    HourChr   = AsciiTimeChr(Start$chr_parse(1):End$chr_parse(1))
    Hour      = INT(NumAscii(HourChr,   ErrorHour)  ,KIND=4)
    Error     = ErrorHour

  CASE(2)
    HourChr   = AsciiTimeChr(Start$chr_parse(1):End$chr_parse(1))
    MinuteChr = AsciiTimeChr(start$chr_parse(2):end$chr_parse(2))
    Hour      = INT(NumAscii(HourChr,   ErrorHour)  ,KIND=4)
    Minute    = INT(NumAscii(MinuteChr, ErrorMinute),KIND=4)
    Error     = ErrorHour .or. ErrorMinute

  CASE(3)
    HourChr   = AsciiTimeChr(Start$chr_parse(1):End$chr_parse(1))
    MinuteChr = AsciiTimeChr(start$chr_parse(2):end$chr_parse(2))
    SecondChr = AsciiTimeChr(start$chr_parse(3):end$chr_parse(3))
    Hour      = INT(NumAscii(HourChr,   ErrorHour)  ,KIND=4)
    Minute    = INT(NumAscii(MinuteChr, ErrorMinute),KIND=4)
    Seconds   = NumAscii(SecondChr,     ErrorSeconds)
    Error     = ErrorHour .or. ErrorMinute .or. ErrorSeconds

  CASE DEFAULT
    Error = .true.

  END SELECT

  ! Ensure a leading negative sign on Hour propogates through to Minute and Second
  IF(.not.Error) THEN
    IF(AsciiTimeChr(1:1) == "-") THEN
      Minute  = -Minute
      Seconds = -Seconds
    END IF
  END IF
  RETURN
END SUBROUTINE GetHourMinuteAndSeconds

SUBROUTINE AdjustTimeZoneForLocale(Locale, Time, dHoursLocal, ZoneUse, AbbrZone, RuleInfo, StatusAdjust)
!+
! Description: This routine determines the hour shift from UTC, the Timezone and the Abbreviation for that TimeZone
!              that are to be used for the input Locale at a specific time
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
! 04-MAR-2025    | MA    | STR 150: Update to use the Olson TZ Info files for time and date information
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN)      :: Locale                         ! Index of the Time Zone Locale
INTEGER(MJD), INTENT(IN )     :: Time                           ! Modified Julian Date                   (10^-7 seconds)
REAL(8)     , INTENT(OUT)     :: dHoursLocal                    ! Hours shift from UTC                           (hours)
INTEGER(4)  , INTENT(OUT)     :: ZoneUse                        ! Timezone of the locale that is used
CHARACTER(8), INTENT(OUT)     :: AbbrZone                       ! Time Zone abbreviation (e.g. PDT, PST)        (string)
INTEGER(4)  , INTENT(OUT)     :: RuleInfo                       ! Index of Zone Rule information used
INTEGER(4)  , INTENT(OUT)     :: StatusAdjust                   ! Return status code
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: SwitchEnd                      ! Number of cached timezone switch data entries
INTEGER(4)                    :: Switch                         ! Index through cached timezone switch data entries
INTEGER(4)                    :: SwitchUse                      ! Index of timezone switch data that is active
INTEGER(4)                    :: Zone                           ! Index through timezones of the Locale
INTEGER(MJD)                  :: Time_Switch       (SwitchTz$MX)! Ending time of each timezone switch    (10^-7 seconds)
REAL(8)                       :: dHoursLocal_Switch(SwitchTz$MX)! Hours shift from UTC                           (hours)
CHARACTER(8)                  :: AbbrZone_Switch   (SwitchTz$MX)! Time Zone abbreviation (e.g. PDT, PST)        (string)
INTEGER(4)                    :: RuleInfo_Switch   (SwitchTz$MX)! Zone information index for each switch
LOGICAL(4)  ,PARAMETER        :: DebugLocal = .false.           ! Diagnostic flag for in-house testing
LOGICAL(4)  ,PARAMETER        :: DebugRule  = .false.
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! ! Initialize Return
  StatusAdjust = TIME_E_INVALID

  ! Check if timezone switch information has been cached
  SwitchEnd   = Tz_Locale(Locale)%SwitchEnd
  IF(SwitchEnd > 1) THEN
    !
    ! Get the cached list of time zone switches
    Time_Switch = Tz_Locale(Locale)%Time_Switch
    IF(DebugLocal) THEN
      AbbrZone_Switch = Tz_Locale(Locale)%AbbrZone_Switch
      WRITE(*,FMT='(" Time: ",Z16.16)')Time
      DO Switch = 1, SwitchEnd
        WRITE(*,FMT='(" Time: ",Z16.16,1X,A8,1X,"dDay ",I5)') Time_Switch(Switch),AbbrZone_Switch(Switch),(Time - Time_Switch(Switch))/TimePerDay
      END DO
    END IF
    !
    ! See if the cached list encompasses the current time
    IF(WithinClosedSet(Time_Switch(1), Time, Time_Switch(SwitchEnd))) THEN
      !
      ! Find the timezone switch to use
      SwitchUse = SwitchEnd
      DO Switch = 1, SwitchEnd-1
        IF(Time >= Time_Switch(Switch) .and. Time < Time_Switch(Switch+1)) THEN
          SwitchUse = Switch; EXIT
        END IF
      END DO
      !
      ! Return the Hour shift and the Zone Abbreviation for this time and the Zone used
      dHoursLocal  = Tz_Locale(Locale)%dHours_Switch(SwitchUse)
      AbbrZone     = Tz_Locale(Locale)%AbbrZone_Switch(SwitchUse)
      RuleInfo     = Tz_Locale(Locale)%RuleInfo_Switch(SwitchUse)
      ZoneUse      = Tz_Locale(Locale)%ZoneNow
      StatusAdjust = TIME_S_NORMAL
      IF(DebugLocal) THEN
        WRITE(*,FMT='("For Locale: ",I4," ZoneUse ",I4,1X,A8,1X," dHoursLocal ",2(1X,F7.2))') Locale        &
                                                                                            , ZoneUse       &
                                                                                            ,TRIM(AbbrZone) &
                                                                                            ,dHoursLocal    &
                                                                                            ,Tz_Zone(ZoneUse)%dHoursUTC
      END IF
      RETURN
    END IF
  END IF
  !
  ! Ensure that the starting and ending timezones have been setup
  IF(DebugLocal) WRITE(*,FMT='("For Locale ",I4," Zone Start and End ",I4,1X,I4)') Tz_Locale(Locale)%ZoneStart, Tz_Locale(Locale)%ZoneEnd
  IF(.not.WithinClosedSet(1, Tz_Locale(Locale)%ZoneStart, Tz_Locale(Locale)%ZoneEnd)) THEN
    StatusAdjust = TIME_E_TZSETUP
    RETURN
  END IF
  CALL GetTimeZoneSwitchesForLocale(Locale, Time, Zone, SwitchEnd, Time_Switch, dHoursLocal_Switch, AbbrZone_Switch, RuleInfo_Switch)
  IF(SwitchEnd < 2) THEN
    !
    ! If this timezone does not switch its Abbreviation then we can use the default
    IF(INDEX(Tz_Zone(Zone)%AbbrZone,"%") == 0) THEN
      dHoursLocal = DBLE(Tz_Zone(Zone)%dHourUTC)          &
                  + DBLE(Tz_Zone(Zone)%dMinuteUTC)/60d0   &
                  + DBLE(Tz_Zone(Zone)%dSecondUTC)/3600d0
      AbbrZone = Tz_Zone(Zone)%AbbrZone
      ZoneUse  = Zone
      RuleInfo = 0
      StatusAdjust = TIME_S_NORMAL
      IF(DebugLocal) WRITE(*,FMT='(" No TimeZone Switch, AbbrZone: ",A8," dHoursLocal: ",F8.4)')AbbrZone, dHoursLocal
    ELSE
      StatusAdjust = TIME_E_TZSWITCHES
    END IF
    RETURN
  END IF
  IF(DebugLocal) THEN
    AbbrZone_Switch = Tz_Locale(Locale)%AbbrZone_Switch
    WRITE(*,FMT='(" Time: ",Z16.16)')Time
    DO Switch = 1, SwitchEnd
      WRITE(*,FMT='(" Time: ",Z16.16,1X,A8,1X,"dDay ",I5)') Time_Switch(Switch),AbbrZone_Switch(Switch),(Time - Time_Switch(Switch))/TimePerDay
    END DO
  END IF
  !
  ! See if the applicable switches encompasses the current time
  IF(WithinClosedSet(Time_Switch(1), Time, Time_Switch(SwitchEnd))) THEN
    !
    ! Find the timezone Switch to use
    SwitchUse = SwitchEnd
    DO Switch = 1, SwitchEnd-1
      IF(Time >= Time_Switch(Switch) .and. Time < Time_Switch(Switch+1)) THEN
        SwitchUse = Switch; EXIT
      END IF
    END DO
    !
    ! Return the hour shift, the timezone and the timezone abbreviation
    dHoursLocal = dHoursLocal_Switch(SwitchUse)
    AbbrZone    = AbbrZone_Switch   (SwitchUse)
    RuleInfo    = RuleInfo_Switch   (SwitchUse)
    ZoneUse     = Zone
    StatusAdjust = TIME_S_NORMAL
    IF(DebugRule) WRITE(*,FMT='(" For Locale, ",I4," ZoneUse, ",I4, "Rule",Z8.8)')Locale, ZoneUse, RuleInfo
    IF(DebugLocal) THEN
        WRITE(*,FMT='("For Locale: ",I4," ZoneUse ",I4,1X,A8,1X," dHoursLocal ",2(1X,F7.2))') Locale        &
                                                                                            , ZoneUse       &
                                                                                            ,TRIM(AbbrZone) &
                                                                                            ,dHoursLocal    &
                                                                                            ,Tz_Zone(ZoneUse)%dHoursUTC
    END IF
    RETURN
  ELSE
    StatusAdjust = TIME_E_TZRULES
    RETURN
  END IF
  RETURN
END SUBROUTINE AdjustTimeZoneForLocale

SUBROUTINE GetTimeZoneSwitchesForLocale(Locale, Time, Zone, SwitchEnd, Time_Switch, dHoursLocal_Switch, AbbrZone_Switch, RuleInfo_Switch)
!+
! Description: This function searches the applicable rules for a timezone and returns the absolute Time and the change
!              in hours shift from UTC and the abbreviation of the timezone for each switch in time.
!
! Audit Trail
!----------------+-------+----------------------------------------------------------------------------------------------
! 04-MAR-2025    | MA    | STR 150: Update to use the Olson TZ Info files for time and date information
!----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Utilities
IMPLICIT NONE
!
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  , INTENT(IN)      :: Locale                         ! Locale in which we are looking for timezone switches
INTEGER(MJD), INTENT(IN)      :: Time                           ! Time for finding the timezone switches (10^-7 seconds)
INTEGER(4)  , INTENT(OUT)     :: Zone                           ! Timezone index for getting applicable switches
INTEGER(4)  , INTENT(OUT)     :: SwitchEnd                      ! Number of applicable timezone rule switches
INTEGER(MJD), INTENT(OUT)     :: Time_Switch       (SwitchTz$MX)! Ending time of each timezone switch    (10^-7 seconds)
REAL(8)     , INTENT(OUT)     :: dHoursLocal_Switch(SwitchTz$MX)! Hours shift from UTC                           (hours)
CHARACTER(8), INTENT(OUT)     :: AbbrZone_Switch   (SwitchTz$MX)! Time Zone abbreviation (e.g. PDT, PST)        (string)
INTEGER(4)  , INTENT(OUT)     :: RuleInfo_Switch   (SwitchTz$MX)! Zone information index for each switch
!
!                                Local
!-----------------------------+---------
LOGICAL(4)                    :: FoundZone                      ! Found a timezone active during Time
INTEGER(4)                    :: Rule                           ! Rule for applying a timezone shift
INTEGER(4)                    :: YearAt                         ! Year for which to get applicable timezone switches
INTEGER(4)                    :: Info                           ! Index through the extracted information
INTEGER(4)                    :: Switch                         ! Index through applicable timezone rule applications
INTEGER(4)                    :: When                           ! When a rule is applied
INTEGER(4)                    :: Month                          ! Month in which a rule is activated    (month)
INTEGER(4)                    :: Year_Switch       (SwitchTz$MX)! Year of each Switch
INTEGER(4)                    :: Month_Switch      (SwitchTz$MX)! Month of each Switch
INTEGER(4)                    :: Order_Switch      (SwitchTz$MX)! Temporal Order of each Switch
INTEGER(4)                    :: Switch_SwitchSort (SwitchTz$MX)! Switch indices in ascending order
INTEGER(4)                    :: dHour_Switch      (SwitchTz$MX)! Hour shift for the timezone switches           (hours)
INTEGER(4)                    :: dMinute_Switch    (SwitchTz$MX)! Minute shift for the timezone switches       (minutes)
INTEGER(4)                    :: dSecond_Switch    (SwitchTz$MX)! Second shift for the timezone switches       (seconds)
CHARACTER(8)                  :: dAbbr_Switch      (SwitchTz$MX)! Change in abbreviation for the timezone switches
INTEGER(4)                    :: Info_Switch       (SwitchTz$MX)! Zone information index for each switch
INTEGER(4)                    :: SwitchSort                     ! Switch order index
INTEGER(4)                    :: YearApplied                    ! Year when values were calculated for a timezone switch
INTEGER(4)                    :: DayMax_month(0:13)             ! Last day of each month
INTEGER(MJD)                  :: dTime_month (0:13)             ! Time elapsed in year for each month    (10^-7 seconds)
INTEGER(4)                    :: Year                           ! Index from the Year before and the Year after YearAt
INTEGER(4)                    :: Day                            ! Day of the month on which a timezone switch occurs
INTEGER(4)                    :: dDay                           ! Increment (+1 or -1) in search for day of the month
INTEGER(MJD)                  :: dTime                          ! Delta time into month for a switch     (10^-7 seconds)
INTEGER(MJD)                  :: TimeWhen                       ! Time when a timezone switch occurs     (10^-7 seconds)
INTEGER(4)                    :: DayOfWeek                      ! Day of the week on which a timezone switch occurs
INTEGER(4)                    :: Hour                           ! Hour when a timezone switch occurs             (hours)
INTEGER(4)                    :: Minute                         ! Minute when a timezone switch occurs         (minutes)
INTEGER(4)                    :: Second                         ! Second when a timezone switch occurs         (seconds)
LOGICAL(4)                    :: FoundWhen                      ! Found a day to match the day when a switch occurs
INTEGER(4)                    :: Chr                            ! Location in AbbrZoneChr of "%s" substitution
INTEGER(4)                    :: dHour                          ! Hour shift  for a timezone switch              (hours)
INTEGER(4)                    :: dMinute                        ! Minute shift  for a timezone switch          (minutes)
CHARACTER(8)                  :: AbbrZoneChr                    ! Time Zone abbreviation template               (string)
CHARACTER(8)                  :: AbbrZone                       ! Time Zone abbreviation                        (string)
LOGICAL(4), PARAMETER         :: DebugLocal = .false.           ! Diagnostic flag for in-house testing
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Initialize the return Values
  Time_Switch        = 0
  dHoursLocal_Switch = 0
  AbbrZone_Switch    = SPACE
  RuleInfo_Switch    = 0

  ! Find the timezone that we need to use
  FoundZone = .false.
  DO Zone = Tz_Locale(Locale)%ZoneStart, Tz_Locale(Locale)%ZoneEnd
    FoundZone = Time <= Tz_Zone(Zone)%TimeUntil
    IF(.not.FoundZone) CYCLE
    EXIT
  END DO
  SwitchEnd = 0

  IF(.not.FoundZone) RETURN

  ! Find all the timezone switches for the previous year, this year and the next year
  IF(.not.Tz_Zone(Zone)%UseRule .or. Tz_Zone(Zone)%Rule == 0) THEN
    !
    ! This timezone does not use any rules
    IF(Tz_Locale(Locale)%ZoneStart == Zone) THEN
      !
      ! The very first timezone switch for this Locale applies all the way back to the epoch
      Time_Switch(1) = 0
    ELSE
      !
      ! Timezones switches after the first one for this Locale start right where the previous one left off
      Time_Switch(1) = Tz_Zone(Zone-1)%TimeUntil-1
    END IF
    !
    ! Fill in the remaining data for this timezone switch
    Time_Switch(2)      = Tz_Zone(Zone)%TimeUntil
    dHour_Switch  (1:2) = Tz_Zone(Zone)%dHourUTC
    dMinute_Switch(1:2) = Tz_Zone(Zone)%dMinuteUTC
    dSecond_Switch(1:2) = Tz_Zone(Zone)%dSecondUTC
    dAbbr_Switch  (1:2) = " "
    Info_Switch   (1:2) = 0
    SwitchEnd = 2

  ELSE
    !
    ! Here we have to follow the rules to find the relevant timezone switches
    Rule = Tz_Zone(Zone)%Rule
    Info = Tz_Rule(Rule)%Start$RuleInfo
    DO WHILE(Info > 0)
      !
      ! Get the year we are at
      YearAt = YearFromTime(Time)
      !
      ! We need all switches from the year before to the year after to ensure that we bracket all the timezone switches
      DO Year = YearAt-1, YearAt+1
        IF(WithinClosedSet(Tz_RuleInfo(Info)%YearFrom, Year, Tz_RuleInfo(Info)%YearTo)) THEN
          SwitchEnd = SwitchEnd + 1
          Info_Switch (SwitchEnd) = Info
          Year_Switch (SwitchEnd) = Year
          Month_Switch(SwitchEnd) = Tz_RuleInfo(Info)%Month
          Order_Switch(SwitchEnd) = Year * 12 +  Month_Switch(SwitchEnd)-1
        ENDIF
      END DO
      Info = Tz_RuleInfo(Info)%Next$RuleInfo
    END DO
    !
    ! Return if no applicable rules are found
    IF(SwitchEnd == 0) RETURN
    !
    ! Ensure that all the rules that appply are in temporal order
    CALL SortByAttribute(Order_Switch, Switch_SwitchSort, SwitchEnd)

    ! Now calculate the time of each application of a time zone change
    YearApplied = 0
    DO SwitchSort = 1, SwitchEnd
      Switch = Switch_SwitchSort(SwitchSort)
      Year   = Year_Switch(Switch)
      IF(YearApplied /= Year) THEN
        IF(LeapYear(Year)) THEN
          DayMax_month = DaysLeapYear_month
          dTime_month  = dTimeLeapYear_month
        ELSE
          DayMax_month = DaysNormalYear_month
          dTime_month  = dTimeNormalYear_month
        END IF
        CALL AdjustMonthTimesForLeapSeconds(year,dTime_month)
        YearApplied = Year
      END IF
      Info = Info_Switch(Switch)
      When = Tz_RuleInfo(Info)%When
      month = month_Switch(Switch)
      SELECT CASE(When)
      CASE(OnDayOfMonth)   ; Day = Tz_RuleInfo(Info)%Day ; dDay =  0 ; dTime = dTime_month(month-1) + 1       !dTime = dTime_month(month)
      CASE(OnLastWeekday)  ; Day = DayMax_month(month)   ; dDay = -1 ; dTime = dTime_month(month)             !dTime = dTime_month(month+1) - 1
      CASE(OnWeekDayAfter) ; Day = Tz_RuleInfo(Info)%Day ; dDay = +1 ; dTime = dTime_month(month-1) + 1       !dTime = dTime_month(month)
      CASE(OnWeekDayBefore); Day = DayMax_month(month)   ; dDay = -1 ; dTime = dTime_month(month)             !dTime = dTime_month(month+1) - 1
      CASE DEFAULT         ; Day = Tz_RuleInfo(Info)%Day ; dDay =  0 ; dTime = dTime_month(month-1) + 1       !dTime = dTime_month(month)
      END SELECT
      TimeWhen  = TimeStart_Year(Year) + dTime
      FoundWhen = dDay == 0
      IF(FoundWhen) THEN
        TimeWhen = TimeWhen + TimePerDay*(Day-1)
      ELSE
        TimeWhen = TimeWhen + TimePerDay*(Day-1)
        IF(DebugLocal) THEN
          WRITE(*,FMT='("For RuneInfo ",I4," Starting Day= ",I4," Looking for Weekday= ",I4)') Info, Day, Tz_RuleInfo(Info)%DayOfWeek
        END IF
        DO WHILE(WithinClosedSet(1, Day, DayMax_month(month)))
          CALL GetDayOfWeek(TimeWhen, DayOfWeek)
          IF(DebugLocal) THEN
            WRITE(*,FMT='("                    Checking Day= ",I4," Which is at Weekday= ",I4)') Day, DayOfWeek
          END IF
          FoundWhen = DayOfWeek == Tz_RuleInfo(Info)%DayOfWeek
          IF(FoundWhen) EXIT
          TimeWhen = TimeWhen + TimePerDay * dDay
          Day = Day + dDay
        END DO
      END IF
      IF(.not.FoundWhen) THEN
        IF(debugLocal) WRITE(*,FMT='("For RuleInfo ",I4," did not find day match")') Info
      END IF
      Hour   = Tz_RuleInfo(Info)%Hour
      Minute = Tz_RuleInfo(Info)%Minute
      Second = Tz_RuleInfo(Info)%Second
      IF(.not.Tz_RuleInfo(Info)%AtUTC) THEN
        Hour   = Hour   - Tz_Zone(Zone)%dHourUTC
        Minute = Minute - Tz_Zone(Zone)%dMinuteUTC
        Second = Second - Tz_Zone(Zone)%dSecondUTC
      END IF
      TimeWhen = TimeWhen + TimePerHour*Hour + TimePerMinute*Minute + TimePerSecond*Second

      Time_Switch    (SwitchSort) = TimeWhen
      dHour_Switch   (SwitchSort) = Tz_Zone(Zone)%dHourUTC    + Tz_RuleInfo(Info)%dHourShift
      dMinute_Switch (SwitchSort) = Tz_Zone(Zone)%dMinuteUTC  + Tz_RuleInfo(Info)%dMinuteShift
      dSecond_Switch (SwitchSort) = 0
      dAbbr_Switch   (SwitchSort) = Tz_RuleInfo(Info)%dAbbrZone
      RuleInfo_Switch(SwitchSort) = Info
    END DO
    IF(DebugLocal) THEN
      DO Switch = 1, SwitchEnd
        WRITE(*,FMT='(" For Switch ",I2," Time ",Z16.16," dHour ",I4, " Zone ",A8," Info ",Z8.8)') Switch, Time_Switch(Switch),dHour_Switch(Switch),dAbbr_Switch(Switch), RuleInfo_Switch(Switch)
      END DO
    END IF
  END IF
  DO Switch = 1, SwitchEnd
    !
    ! Return the hour shift, the timezone and the timezone abbreviation
    dHoursLocal_Switch(Switch)  = DBLE(dHour_Switch  (Switch))       &
                                + DBLE(dMinute_Switch(Switch))/60d0  &
                                + DBLE(dSecond_Switch(Switch))/3600d0

    AbbrZoneChr = TRIM(Tz_Zone(Zone)%AbbrZone)
    dHour       = dHour_Switch(Switch)
    dMinute     = dMinute_Switch(Switch)

    IF(TRIM(AbbrZoneChr) == "%z") THEN
      IF(dHour < 0d0) THEN
        WRITE(AbbrZone,FMT='("UTC-",I2.2,I2.2)') ABS(dHour),ABS(dMinute)
      ELSE
        WRITE(AbbrZone,FMT='("UTC+",I2.2,I2.2)') ABS(dHour),ABS(dMinute)
      END IF
    ELSE
      chr = INDEX(AbbrZoneChr,"%s")
      IF(chr == 1) THEN
        AbbrZone = TRIM(dAbbr_Switch(Switch))//AbbrZoneChr(3:)
      ELSE IF(chr == 7) THEN
        AbbrZone = TRIM(AbbrZoneChr(1:6))//TRIM(dAbbr_Switch(Switch))
      ELSE IF(chr >  1) THEN
        AbbrZone = TRIM(AbbrZoneChr(1:chr-1))//TRIM(dAbbr_Switch(Switch))//AbbrZoneChr(chr+2:)
      ELSE
        AbbrZone = TRIM(AbbrZoneChr)
      END IF
    END IF
    AbbrZone_Switch(Switch) = AbbrZone
  END DO
  RETURN
END SUBROUTINE GetTimeZoneSwitchesForLocale

FUNCTION TimeOfFileModification(When, File, Error) RESULT(TimeModified)
!
! Description: This subroutine returns the Modified Julian Dates of the file time of creation, access or modification.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: File                           ! File for which the timestamp is desired
CHARACTER(*), INTENT(IN)      :: When                           ! When the timestamp is for: Created, Modified or Accessed
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Error accessing the file's information.
INTEGER(MJD)                  :: TimeModified                   ! Modified Julian Date of files update. (10^-7 seconds)
!
!                                Local
!-----------------------------+---------
TYPE(FileInfo)                :: FileInformation                ! Current information about the file.
LOGICAL(4)                    :: Found                          ! Found the file information
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Error        = .true.
  TimeModified = 0
  IF(.not.Initialized) RETURN
  CALL GetFileInfo(File, FileInformation, Found)

  IF(.not.Found) THEN
    RETURN
  END IF

  SELECT CASE(TRIM(LowerCaseAscii(squish(When))))

  CASE("creation","created")
    TimeModified = FileInformation%TimeCreation

  CASE("accessed","lastread","lastaccess")
    TimeModified = FileInformation%TimeLastAccess

  CASE("updated","lastwrite","modified")
    TimeModified = FileInformation%TimeLastWrite

  CASE DEFAULT
    TimeModified = FileInformation%TimeLastWrite

  END SELECT
  Error = .false.

  RETURN
END FUNCTION TimeOfFileModification

FUNCTION AsciiNumberOfMonth(AsciiMonth) RESULT(AsciiNumber)
!
! Description: This subroutine returns the ascii numeric value for a month (e.g. "JAN" is "01" , ... , "DEC" is "12"
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: AsciiMonth                     ! Month name
CHARACTER(2)                  :: AsciiNumber                    ! Month number
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  SELECT CASE(TRIM(UpperCaseAscii(AsciiMonth)))
  CASE("JAN","JANUARY"  ) ; AsciiNumber = "01"
  CASE("FEB","FEBRUARY" ) ; AsciiNumber = "02"
  CASE("MAR","MARCH"    ) ; AsciiNumber = "03"
  CASE("APR","APRIL"    ) ; AsciiNumber = "04"
  CASE("MAY"            ) ; AsciiNumber = "05"
  CASE("JUN","JUNE"     ) ; AsciiNumber = "06"
  CASE("JUL","JULY"     ) ; AsciiNumber = "07"
  CASE("AUG","AUGUST"   ) ; AsciiNumber = "08"
  CASE("SEP","SEPTEMBER") ; AsciiNumber = "09"
  CASE("OCT","OCTOBER"  ) ; AsciiNumber = "10"
  CASE("NOV","NOVEMBER" ) ; AsciiNumber = "11"
  CASE("DEC","DECEMBER" ) ; AsciiNumber = "12"
  CASE DEFAULT            ; AsciiNumber = "00"
  END SELECT

END FUNCTION AsciiNumberOfMonth

SUBROUTINE CSVReplaceTrailingSpaces(asciicsv,commaAt)
!
! Description: This subroutine removes the spaces around commas in a line read from a CSV file.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(INOUT)   :: asciicsv
INTEGER(4)  , INTENT(IN)      :: commaAt
!
!                                Local
!-----------------------------+---------
INTEGER(4)                    :: comma
INTEGER(4)                    :: csv
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
comma = 0
DO csv = 1, LEN(asciicsv)-1
  IF(asciicsv(csv:csv)==",")THEN
    comma = comma + 1
  ELSE IF(comma==commaAt) THEN
    IF(asciicsv(csv-1:csv-1)/=" " .and. asciicsv(csv:csv)==" " .and. asciicsv(csv+1:csv+1)/=" ") THEN
      IF(asciicsv(csv+1:csv+1)/="," .and. asciicsv(csv-1:csv-1)/=",") THEN
        asciicsv(csv:csv) = ","
        EXIT
      END IF
    END IF
  END IF
END DO
RETURN
END SUBROUTINE CSVReplaceTrailingSpaces
!
SUBROUTINE CSVProcessExcessSpacesAndTabs(asciicsv)
!
! Description: This subroutine removes excess spaces and converts the tabs to commas in a line read from a CSV file.
!
! Audit Trail
!-----------------+-------+---------------------------------------------------------------------------------------------
!-----------------+-------+---------------------------------------------------------------------------------------------
!
USE Utilities
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(INOUT)   :: asciicsv
!
!                                Local
!-----------------------------+---------
INTEGER(4)                    :: csv
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
DO csv = 1,LEN(asciicsv)-2
  IF(asciicsv(csv:csv)==TAB) THEN
    DO
      IF(asciicsv(csv+1:csv+1)/=TAB) EXIT
      asciicsv(csv+1:)=asciicsv(csv+2:)
    END DO
    asciicsv(csv:csv) = ","
  END IF
  IF(asciicsv(csv:csv+1)==" ") THEN
    DO
      IF(asciicsv(csv+1:csv+1)/=" ".or.asciicsv(csv+2:)==" ") EXIT
      asciicsv(csv+1:)=asciicsv(csv+2:)
    END DO
  END IF
END DO
RETURN
END SUBROUTINE CSVProcessExcessSpacesAndTabs
!
END MODULE TimeDatePackage
