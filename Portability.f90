!  Source:  Portability.f90
!
!  Module:  Portability
!
!  Description: Fortran routines to facilitate portability between Windows and Linux / Intel Fortran and GNU Fortran
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 05-FEB-2025     | MA    | STR 150: Reuse Crescent Dunes Code for Generic Heliostat Field Control System
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
MODULE Portability
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
!                               Public Global Variables          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),PARAMETER         :: PORT_S_NORMAL   = 0             ! Normal return from routine
INTEGER(4),PARAMETER         :: PORT_E_FAILURE  = -1            ! Routine failed
CHARACTER(8)                 :: FortranCompiler = " "           ! Fortran Compiler (gfortran or ifort, ...)

INTERFACE
  FUNCTION GetFileTime(hFile, lpCreationTime, lpLastAccessTime, lpLastWriteTime) BIND(C, NAME="GetFileTime")
    IMPORT :: C_INT, C_PTR
    INTEGER(C_INT)     :: GetFileTime
    TYPE(C_PTR), VALUE :: hFile
    TYPE(C_PTR), VALUE :: lpCreationTime
    TYPE(C_PTR), VALUE :: lpLastAccessTime
    TYPE(C_PTR), VALUE :: lpLastWriteTime
  END FUNCTION GetFileTime

  FUNCTION SetFileTime(hFile, lpCreationTime, lpLastAccessTime, lpLastWriteTime) BIND(C, NAME="SetFileTime")
    IMPORT :: C_INT, C_PTR
    INTEGER(C_INT)     :: SetFileTime
    TYPE(C_PTR), VALUE :: hFile
    TYPE(C_PTR), VALUE :: lpCreationTime
    TYPE(C_PTR), VALUE :: lpLastAccessTime
    TYPE(C_PTR), VALUE :: lpLastWriteTime
  END FUNCTION SetFileTime

  FUNCTION CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile) BIND(C, NAME="CreateFileA")
    IMPORT :: C_INT, C_PTR, C_CHAR
    TYPE(C_PTR)                                      :: CreateFileA
    CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: lpFileName
    INTEGER(C_INT), VALUE                            :: dwDesiredAccess
    INTEGER(C_INT), VALUE                            :: dwShareMode
    TYPE(C_PTR),    VALUE                            :: lpSecurityAttributes
    INTEGER(C_INT), VALUE                            :: dwCreationDisposition
    INTEGER(C_INT), VALUE                            :: dwFlagsAndAttributes
    TYPE(C_PTR),    VALUE                            :: hTemplateFile
  END FUNCTION CreateFileA

  FUNCTION CloseHandle(hObject) BIND(C, NAME="CloseHandle")
    IMPORT :: C_INT, C_PTR
    INTEGER(C_INT)     :: CloseHandle
    TYPE(C_PTR), VALUE :: hObject
  END FUNCTION CloseHandle

  SUBROUTINE SleepTimeMsec(dwMilliseconds) BIND(C, NAME="Sleep")
    IMPORT :: C_INT
    INTEGER(C_INT), VALUE :: dwMilliseconds
  END SUBROUTINE SleepTimeMsec
END INTERFACE

! Windows API Constants
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_READONLY            = INT(z'00000001', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_HIDDEN              = INT(z'00000002', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_SYSTEM              = INT(z'00000004', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_DIRECTORY           = INT(z'00000010', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_ARCHIVE             = INT(z'00000020', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_DEVICE              = INT(z'00000040', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_NORMAL              = INT(z'00000080', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_TEMPORARY           = INT(z'00000100', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_SPARSE_FILE         = INT(z'00000200', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_REPARSE_POINT       = INT(z'00000400', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_COMPRESSED          = INT(z'00000800', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_OFFLINE             = INT(z'00001000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = INT(z'00002000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_SHARE_READ                    = INT(z'00000001', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_SHARE_WRITE                   = INT(z'00000002', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: FILE_SHARE_READ_AND_WRITE          = INT(z'00000003', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: GENERIC_READ                       = INT(z'80000000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: GENERIC_WRITE                      = INT(z'40000000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: GENERIC_READ_AND_WRITE             = INT(z'C0000000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: GENERIC_EXECUTE                    = INT(z'20000000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: GENERIC_ALL                        = INT(z'10000000', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: CREATE_NEW                         = INT(z'00000001', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: CREATE_ALWAYS                      = INT(z'00000002', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: OPEN_EXISTING                      = INT(z'00000003', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: OPEN_ALWAYS                        = INT(z'00000004', KIND=C_INT)
INTEGER(C_INT), PARAMETER    :: TRUNCATE_EXISTING                  = INT(z'00000005', KIND=C_INT)

TYPE, BIND(C) :: FILETIME
  INTEGER(C_INT) :: dwLowDateTime
  INTEGER(C_INT) :: dwHighDateTime
END TYPE FILETIME

CONTAINS
SUBROUTINE GetTimesForFile(PathFile, TmWindowsCreate, TmWindowsWrite, TmWinowsAccess, statusResult)
!+
!
! Description: This function gets the file creation, last write and last access time for a file using the Windows API
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 05-FEB-2025     | MA    | SCR 150: Remove dependence on Intel Fortran Extension, GetFileInfoQQ
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE ISO_C_BINDING
IMPLICIT NONE
!                                  Passed                           Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)         :: PathFile                       ! Fully qualified path of the file
INTEGER(8)  ,INTENT(OUT)        :: TmWindowsCreate                ! Windows Time of creation    (10^-7 seconds)
INTEGER(8)  ,INTENT(OUT)        :: TmWindowsWrite                 ! Windows Time of last write  (10^-7 seconds)
INTEGER(8)  ,INTENT(OUT)        :: TmWinowsAccess                 ! Windows Time of last access (10^-7 seconds)
INTEGER(4)  ,INTENT(OUT)        :: statusResult                   ! Return status

!                                  Local
!-------------------------------+---------------
TYPE(FILETIME),TARGET           :: ftCreate                       ! Windows API time of creation    (10^-7 seconds)
TYPE(FILETIME),TARGET           :: ftWrite                        ! Windows API time of last write  (10^-7 seconds)
TYPE(FILETIME),TARGET           :: ftAccess                       ! Windows API time of last access (10^-7 seconds)
TYPE(C_PTR)                     :: pCreate                        ! Pointer to the Windows API of creation
TYPE(C_PTR)                     :: pWrite                         ! Pointer to the Windows API of last write
TYPE(C_PTR)                     :: pAccess                        ! Pointer to the Windows API of last access
INTEGER(C_INT)                  :: statusCloseHandle              ! Return from CloseHandle
INTEGER(C_INT)                  :: statusGetFileTime              ! Return from GetFileTime
TYPE(C_PTR)                     :: hFile                          ! File handle
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Open file handle
  hFile = CreateFileA(TRIM(PathFile)//C_NULL_CHAR, 0, 3, C_NULL_PTR, 3, 0, C_NULL_PTR)
  IF (.NOT. C_ASSOCIATED(hFile)) THEN
    statusResult = PORT_E_FAILURE
    RETURN
  END IF

  ! Get the file times
  pCreate = C_LOC(ftCreate)
  pWrite  = C_LOC(ftWrite )
  pAccess = C_LOC(ftAccess)
  statusGetFileTime = GetFileTime(hFile, pCreate, pAccess, pWrite)
  IF (statusGetFileTime /= 0) THEN
    TmWindowsCreate = TimeWindowsFromTimeDateParts(ftCreate%dwHighDateTime,ftCreate%dwLowDateTime)
    TmWindowsWrite  = TimeWindowsFromTimeDateParts(ftAccess%dwHighDateTime,ftAccess%dwLowDateTime)
    TmWinowsAccess  = TimeWindowsFromTimeDateParts(ftWrite%dwHighDateTime ,ftWrite%dwLowDateTime )
    statusResult = PORT_S_NORMAL
  ELSE
    statusResult = PORT_E_FAILURE
  END IF

  ! Close the file handle
  statusCloseHandle = CloseHandle(hFile)
END SUBROUTINE GetTimesForFile

SUBROUTINE SetTimesForFile(PathFileChr, TmWindowsCreate, TmWindowsWrite, TmWindowsAccess, statusResult)
!+
!
! Description: This function sets the file creation, last write and last access time for a file using the Windows API
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 26-FEB-2025     | MA    | SCR 150: Remove dependence on Intel Fortran Extension, GetFileInfoQQ
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE ISO_C_BINDING
IMPLICIT NONE
!                                  Passed                           Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)         :: PathFileChr                    ! Fully qualified path of the file
INTEGER(8)  ,INTENT(IN)         :: TmWindowsCreate                ! Windows Time of creation    (10^-7 seconds)
INTEGER(8)  ,INTENT(IN)         :: TmWindowsWrite                 ! Windows Time of last write  (10^-7 seconds)
INTEGER(8)  ,INTENT(IN)         :: TmWindowsAccess                ! Windows Time of last access (10^-7 seconds)
INTEGER(4)  ,INTENT(OUT)        :: statusResult                   ! Return status

!                                  Local
!-------------------------------+---------------
LOGICAL(4)                      :: existsFile                     ! Set true if the input path is a valid file
INTEGER(4)                      :: ChrEnd                         ! Number of characters in the file path
CHARACTER(C_CHAR), ALLOCATABLE  :: lpFileNameChr(:)               !   File path to send to CreateFileA
INTEGER(C_INT)                  :: dwAccess                       ! Windows API desired access
INTEGER(C_INT)                  :: dwShare                        ! Windows API share mode
TYPE(C_PTR)                     :: lpSecurity                     ! Pointer to the File Security Attributes
INTEGER(C_INT)                  :: dwDisposition                  ! Action to take for existence or non existence
INTEGER(C_INT)                  :: dwAttributes                   ! File Atrributes
TYPE(C_PTR)                     :: hTemplate                      ! Optional handle to a file template
TYPE(C_PTR)                     :: hFile                          ! File handle
TYPE(FILETIME),    TARGET       :: ftCreate                       ! Windows API time of creation    (10^-7 seconds)
TYPE(FILETIME),    TARGET       :: ftWrite                        ! Windows API time of last write  (10^-7 seconds)
TYPE(FILETIME),    TARGET       :: ftAccess                       ! Windows API time of last access (10^-7 seconds)
TYPE(C_PTR)                     :: pCreate                        ! Pointer to the Windows Time of creation
TYPE(C_PTR)                     :: pWrite                         ! Pointer to the Windows Time of last write
TYPE(C_PTR)                     :: pAccess                        ! Pointer to the Windows Time of last access
INTEGER(C_INT)                  :: statusSetFileTime              ! Return from SetFileTime
INTEGER(C_INT)                  :: statusCloseHandle              ! Return from CloseHandle
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! Ensure the File is valid
  INQUIRE(FILE=PathFileChr, EXIST=existsFile)
  IF(.not.existsFile) THEN
    statusResult = PORT_E_FAILURE
    RETURN
  END IF
  !
  ! Set up the parameters for CreateFileA and obtain the File handle
  ChrEnd = LEN_TRIM(PathFileChr) + 1
  ALLOCATE(lpFileNameChr(ChrEnd))
  lpFileNameChr = PathFileChr(1:ChrEnd)//C_NULL_CHAR
  dwAccess      = GENERIC_READ_AND_WRITE
  dwShare       = FILE_SHARE_READ_AND_WRITE
  lpSecurity    = C_NULL_PTR
  dwDisposition = OPEN_EXISTING
  dwAttributes  = FILE_ATTRIBUTE_NORMAL
  hTemplate     = C_NULL_PTR
  hFile = CreateFileA(lpFileNameChr, dwAccess, dwShare, lpSecurity, dwDisposition, dwAttributes, hTemplate)
  DEALLOCATE(lpFileNameChr)

  ! Return if we don't have a valid File handle
  IF (.NOT. C_ASSOCIATED(hFile)) THEN
    statusResult = PORT_E_FAILURE
    RETURN
  END IF

  ! Set the desired write time
  ftCreate = TimeFileFromTimeWindows(TmWindowsCreate)
  ftWrite  = TimeFileFromTimeWindows(TmWindowsWrite)
  ftAccess = TimeFileFromTimeWindows(TmWindowsAccess)

  pCreate = C_NULL_PTR     !C_LOC(ftCreate)
  pWrite  = C_LOC(ftWrite)
  pAccess = C_NULL_PTR     !NC_LOC(ftAccess)
  statusSetFileTime = SetFileTime(hFile, pCreate, pAccess, pWrite)
  IF (statusSetFileTime == 0) THEN
    statusResult = PORT_E_FAILURE
  ELSE
    statusResult = PORT_S_NORMAL
  END IF

  ! Close the file handle
  statusCloseHandle = CloseHandle(hFile)
END SUBROUTINE SetTimesForFile

SUBROUTINE SleepMsec(dTimeMsec)
!+
!
! Description: This subroutine calls the Sleep routine in the Windows API
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Remove dependence on Intel Fortran Extension, SleepQQ
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE ISO_C_BINDING
IMPLICIT NONE
!                                  Passed                           Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)    , INTENT(IN)      :: dTimeMsec                      ! Time Delta to sleep   (milliseconds)

!                                  Local
!-------------------------------+---------------
INTEGER(C_INT)                  :: dwMillisecond                  ! Window API Time Delta to sleep (milliseconds)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  dwMillisecond = dTimeMsec
  CALL SleepTimeMsec(dwMillisecond)
  RETURN
END SUBROUTINE SleepMsec

FUNCTION TimeWindowsFromTimeDateParts(dwHighDateTime,dwLowDateTime) RESULT(TmWindows)
!+
!
! Description: This function packs the high and low parts of a Windows Time into a full Window Time
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 05-FEB-2025     | MA    | SCR 150: Remove dependence on Intel Fortran Extension, GetFileInfoQQ
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE ISO_C_BINDING
IMPLICIT NONE
!                                  Passed                           Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
  INTEGER(C_INT),INTENT(IN)     :: dwHighDateTime                 ! Windows Time 1st 32 bit part
  INTEGER(C_INT),INTENT(IN)     :: dwLowDateTime                  ! Windows Time 2nd 32 bit part
  INTEGER(8)                    :: TmWindows                      ! Windows Time reconstructed from parts (10^-7 seconds)

!                                  Local
!-------------------------------+---------------
  INTEGER(8)                    :: TmWindowsHigh                ! Windows Time 1st 32 bits shifted and cast as I8
  INTEGER(8)                    :: TmWindowsLow                 ! Windows Time 2nd 32 bits cast as I8
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  TmWindowsHigh = ISHFT(INT8(dwHighDateTime),32)
  TmWindowsLow  = IAND(z'00000000FFFFFFFF',INT8(dwLowDateTime))
  TmWindows     = IOR(TmWindowsHigh,TmWindowsLow)

  RETURN
END FUNCTION TimeWindowsFromTimeDateParts

FUNCTION TimeFileFromTimeWindows(TmWindows) RESULT(TmFile)
!+
!
! Description: This function unpacks the high and low parts of a Windows Time from a full Window Time
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 26-FEB-2025     | MA    | SCR 150: Remove dependence on Intel Fortran Extension, GetFileInfoQQ
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE ISO_C_BINDING
IMPLICIT NONE
!                                  Passed                           Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
  INTEGER(8), INTENT(IN)        :: TmWindows                      ! Windows Time (10^-7 seconds)
  TYPE(FILETIME)                :: TmFile                         ! Windows File Time
!
!                                  Local
!-------------------------------+---------------
  INTEGER(8)                    :: TmWindowsLocal                 ! Windows Time Local Copy (10^-7 seconds)
  INTEGER(4)                    :: TmWindowsLocal_w(2)            ! Word Equivalent to TmwindowsLocal
  EQUIVALENCE                     (TmWindowsLocal, TmWindowsLocal_w)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  TmWindowsLocal        = TmWindows
  TmFile%dwLowDateTime  = INT(TmWindowsLocal_w(1), KIND=C_INT)
  TmFile%dwHighDateTime = INT(TmWindowsLocal_w(2), KIND=C_INT)

  RETURN
END FUNCTION TimeFileFromTimeWindows

FUNCTION GetBytesForRecordLength(StatusResult) RESULT(BytesForRecordLength)
!+
!
! Description: This function returns the number of bytes per RECL for unformatted file I/O depending on the compiler
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 22-MAR-2025     | MA    | SCR 150: Allow operation with GNU Fortran or Intel Fortran
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                  Passed                           Description / (Bytes)
!-------------------------------+---------------------------------+-----------------------------------------------------
INTEGER(4)  ,INTENT(OUT)        :: statusResult                   ! Return status
INTEGER(4)                      :: BytesForRecordLength           ! Bytes assumed by the compiler for RECL       (bytes)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Ensure we have the name of the compiler
  IF(LEN_TRIM(FortranCompiler) == 0) THEN
    CALL GET_ENVIRONMENT_VARIABLE("FC",FortranCompiler)
  END IF

  ! Initialize a successful return
  StatusResult   = PORT_S_NORMAL

  ! Set the result based on the compiler
  SELECT CASE(TRIM(FortranCompiler))
  CASE("ifort","IFORT")
    BytesForRecordLength = 4
  CASE("gfortran","GNU")
    BytesForRecordLength = 1
  CASE DEFAULT
    BytesForRecordLength = 1
    StatusResult   = PORT_E_FAILURE
  END SELECT
  RETURN
END FUNCTION GetBytesForRecordLength

END MODULE Portability
