!  Source:  Utilities.f90
!
!  Module:  Utilities
!
!  Description: This Module provide basic function and subroutines that can be widely used.  All the routines in
!               Utilities are independent of the GE-Grid infrastructure.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
MODULE Utilities
IMPLICIT NONE
!                                Declarations                        Description / (units)
!-----------------------------+-------------------------------------+---------------------------------------------------
CHARACTER(1),PARAMETER,PUBLIC :: SPACE            = CHAR(32)        !z'20')
CHARACTER(1),PARAMETER,PUBLIC :: NULL             = CHAR(00)        !z'00')
CHARACTER(1),PARAMETER,PUBLIC :: TAB              = CHAR(09)        !z'09')
CHARACTER(1),PARAMETER,PUBLIC :: COMMA            = CHAR(44)        !z'2C')
CHARACTER(1),PARAMETER,PUBLIC :: SINGLEQUOTE      = CHAR(39)
CHARACTER(1),PARAMETER,PUBLIC :: SLASHBACKWARD    = "\"
CHARACTER(1),PARAMETER,PUBLIC :: DECIMALPOINT     = '.'
INTEGER(4)  ,PARAMETER,PRIVATE:: aLower           = ICHAR('a')
INTEGER(4)  ,PARAMETER,PRIVATE:: zLower           = ICHAR('z')
INTEGER(4)  ,PARAMETER,PRIVATE:: aUpper           = ICHAR('A')
INTEGER(4)  ,PARAMETER,PRIVATE:: zUpper           = ICHAR('Z')
CHARACTER(1),PARAMETER,PRIVATE:: zeroChar         = '0'
CHARACTER(1),PARAMETER,PRIVATE:: nineChar         = '9'
CHARACTER(1),PARAMETER,PRIVATE:: NullTerminator   = CHAR(00) !z'00')
INTEGER(4)  ,PARAMETER,PRIVATE:: NormalQuality    = 1
INTEGER(4)  ,PARAMETER,PUBLIC :: CHREND$MX        = 1024
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_S_NORMAL    =  0
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_E_FAILURE   = -1
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_E_FILEEXIST = -2           ! File does not exist.
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_E_FILEOPEN  = -3           ! Unable to open bit map file
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_E_FILEREAD  = -4           ! Error reading file
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_E_FILEWRITE = -5           ! Error writing file
INTEGER(4)  ,PARAMETER,PUBLIC :: UTIL_E_FILEEND   = -6           ! Premature end of file during read
!
!                              Interfaces
!-----------------------------------------------------------------------------------------------------------------------
INTERFACE TransferVectorToScalars
  MODULE PROCEDURE Transfer2x1VectorToScalars, Transfer3x1VectorToScalars
END INTERFACE
!
INTERFACE ValueEstimateMinMax
  MODULE PROCEDURE ValueEstimateMinMaxFromDataTypeAndMask
END INTERFACE
!
INTERFACE ValueEstimate
  MODULE PROCEDURE ValueEstimateFromData, ValueEstimateFromDataWithMask, ValueEstimateFromDataTypeAndMask,ValueWLSFromDataTypeAndMask,ValueMinMaxFromDataTypeAndMask
END INTERFACE
!
INTERFACE MinMax
  MODULE PROCEDURE MinMax_R8,MinMax_R4,MinMax_I8,MinMax_I4,MinMax_I2,MinMax_I1
END INTERFACE
!
INTERFACE Swap
  MODULE PROCEDURE Swap_R8,Swap_R4,Swap_I8,Swap_I4,Swap_I2,Swap_I1
END INTERFACE

INTERFACE IntersectionClosedSets
  MODULE PROCEDURE IntersectionClosedSets_R8,IntersectionClosedSets_R4,IntersectionClosedSets_I8,IntersectionClosedSets_I4,IntersectionClosedSets_I2
END INTERFACE

INTERFACE WithinClosedSet
  MODULE PROCEDURE WithinClosedSet_R8,WithinClosedSet_R4,WithinClosedSet_I8,WithinClosedSet_I4,WithinClosedSet_I2
END INTERFACE

INTERFACE WithinOpenSet
  MODULE PROCEDURE WithinOpenSet_R8,WithinOpenSet_R4,WithinOpenSet_I8,WithinOpenSet_I4,WithinOpenSet_I2
END INTERFACE

INTERFACE WithinLeftHalfOpenSet
  MODULE PROCEDURE WithinLeftHalfOpenSet_R8,WithinLeftHalfOpenSet_R4,WithinLeftHalfOpenSet_I8,WithinLeftHalfOpenSet_I4,WithinLeftHalfOpenSet_I2
END INTERFACE

INTERFACE WithinRightHalfOpenSet
  MODULE PROCEDURE WithinRightHalfOpenSet_R8,WithinRightHalfOpenSet_R4,WithinRightHalfOpenSet_I8,WithinRightHalfOpenSet_I4,WithinRightHalfOpenSet_I2
END INTERFACE

INTERFACE SortByAttribute
  MODULE PROCEDURE SortByAttributeR8,SortByAttributeR4,SortByAttributeI8,SortByAttributeI4,SortByAttributeI2,SortByAttributeAscii,SortByAttributeByte
END INTERFACE

INTERFACE SortReverseByAttribute
  MODULE PROCEDURE SortReverseByAttributeR8,SortReverseByAttributeR4,SortReverseByAttributeI8,SortReverseByAttributeI4,SortReverseByAttributeI2,SortReverseByAttributeAscii
END INTERFACE

INTERFACE SortID
  MODULE PROCEDURE SortID2,SortID4
END INTERFACE

INTERFACE AppendValueToArray
  MODULE PROCEDURE AppendValueToArrayI8, AppendValueToArrayI4, AppendValueToArrayI2 , AppendValueToArrayI1, &
                   AppendValueToArrayR8, AppendValueToArrayR4,                                              &
                   AppendValueToArrayChar
END INTERFACE
!

!                                Public Declarations              Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4),PUBLIC             :: BypassCheckpointLog  = .true.  ! Checkpoing logging  (ToDo - Update to Task array)
!
! ToDo SCR 150: Move these types and the associated routines to MatrixVectorProcessing Module
TYPE ValueAverage
  SEQUENCE
  REAL(8)                     :: Value    = 0d0                 ! Value
  INTEGER(4)                  :: Quality  = 0                   ! Status (1 Normal; 2 Faulted)
  INTEGER(4)                  :: NumGood  = 0                   ! Number of good values
END TYPE ValueAverage

TYPE ValueAvgMinMax
  SEQUENCE
  REAL(8)                     :: Value    = 0d0                 ! Value
  REAL(8)                     :: ValueMin = 0d0                 ! Minimum value
  REAL(8)                     :: ValueMax = 0d0                 ! Maximum value
  INTEGER(4)                  :: Quality  = 0                   ! Status (1 Normal; 2 Faulted)
  INTEGER(4)                  :: NumGood  = 0                   ! Number of good values
END TYPE ValueAvgMinMax
TYPE(ValueAvgMinMax),PARAMETER:: ValAvgMinMaxBad = ValueAvgMinMax(0,0,0,2,0)

TYPE Linear
  SEQUENCE
  REAL(8)                     :: Ho
  REAL(8)                     :: dHdW
  REAL(8)                     :: Hleft
  REAL(8)                     :: Wleft
  REAL(8)                     :: Hright
  REAL(8)                     :: Wright
  REAL(8)                     :: RatioStark
  LOGICAL(4)                  :: Degenerate
  INTEGER(4)                  :: Align
END TYPE Linear

TYPE LinearH
  SEQUENCE
  REAL(8)                     :: Ho
  REAL(8)                     :: dHdW
  LOGICAL(4)                  :: Degenerate
  INTEGER(4)                  :: Align
END TYPE LinearH

TYPE LinearW
  SEQUENCE
  REAL(8)                     :: Wo
  REAL(8)                     :: dWdH
  LOGICAL(4)                  :: Degenerate
  INTEGER(4)                  :: Align
END TYPE LinearW

TYPE LinearForm
  SEQUENCE
  REAL(8)                     :: Yo
  REAL(8)                     :: dYdX
  LOGICAL(4)                  :: Degenerate
  INTEGER(4)                  :: Align
END TYPE LinearForm

TYPE Quadratic
  SEQUENCE
  REAL(8)                     :: A
  REAL(8)                     :: B
  REAL(8)                     :: C
  REAL(8)                     :: D
  REAL(8)                     :: E
  REAL(8)                     :: F
  LOGICAL(4)                  :: Degenerate
  INTEGER(4)                  :: Align
END TYPE Quadratic

TYPE Elliptical
  SEQUENCE
  REAL(8)                     :: Xo
  REAL(8)                     :: Yo
  REAL(8)                     :: aAxis
  REAL(8)                     :: baxis
  REAL(8)                     :: Angle
  LOGICAL(4)                  :: Valid = .false.
  INTEGER(4)                  :: Align
END TYPE Elliptical

INTEGER(4),PARAMETER          :: nCurveEllipse = 64
TYPE EllipseCurve
  SEQUENCE
  REAL(8)                     :: Xmin
  REAL(8)                     :: Xmax
  REAL(8)                     :: X_n (nCurveEllipse)
  REAL(8)                     :: Y1_n(nCurveEllipse)
  REAL(8)                     :: Y2_n(nCurveEllipse)
END TYPE EllipseCurve

REAL   (8)     ,PRIVATE       :: WeightVerySmall   = 1d-3       ! Factor used to down-weight bad data

CONTAINS
!
FUNCTION SlashDir() RESULT (Slash)
!+
!
! Description: Returns the correct slash for directory indicator:
!              Windows: "\"
!              Linux:   "/"
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(1)                  :: Slash                          ! Directory Slash Indicator
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
slash = SLASHBACKWARD                                           ! Assume windows only for now
RETURN
END FUNCTION SlashDir

FUNCTION GetLengthRecordOfFileAtPath(Path, LengthRecord) RESULT(StatusRecord)
!+
!
! Description: This function determines the record length of an unformatted file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 24-MAR-2025     | MA    | SCR 150: Allow of image files to have different record lengths
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Portability
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: Path                           ! Image file path
INTEGER(4)  , INTENT(OUT)     :: LengthRecord                   ! Record length of the file                       (bytes)
INTEGER(4)                    :: StatusRecord                   ! Return status
!
!                                Local
!-----------------------------+-----------
INTEGER(4)                    :: UnitRead                       ! Fortran I/O unit for reading the file
LOGICAL(4)                    :: ExistsFile                     ! Set true if the bitmap file exists
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  ! See if the file exists and we can get the record length directly
  INQUIRE(FILE = Path, EXIST = ExistsFile, RECL = LengthRecord)
  IF(.not.ExistsFile) THEN
    LengthRecord = -1
    StatusRecord = UTIL_E_FILEEXIST
    RETURN
  END IF

  ! If the file exists but we were able to get the record length then return that.
  IF(LengthRecord > 1) THEN
    StatusRecord = UTIL_S_NORMAL
    RETURN
  END IF

  ! If Inquire did not work, then see if we can get the record length directly from the File
  OPEN(NewUnit = UnitRead      &
      ,FILE    = Path          &
      ,STATUS  = 'OLD'         &
      ,ACCESS  = 'DIRECT'      &
      ,RECL    =  10           &
      ,FORM    = 'UNFORMATTED' &
      ,ACTION  = 'READ'        &
      ,ERR=1                   &
      )
  READ(UnitRead, FMT='(I4)', ERR=2) LengthRecord
  CLOSE(UnitRead)

  IF(LengthRecord < 1) THEN
    StatusRecord = UTIL_E_FAILURE
  ELSE
    StatusRecord = UTIL_S_NORMAL
  END IF
  RETURN
!
!------------------------------------------------------------------------------------------------------------------------
!                         E R R O R   H A N D L I N G
!------------------------------------------------------------------------------------------------------------------------
!
  1 StatusRecord = UTIL_E_FILEOPEN
  CLOSE(UnitRead)
  RETURN

  2 StatusRecord = UTIL_E_FILEREAD
  CLOSE(UnitRead)
  RETURN
END FUNCTION GetLengthRecordOfFileAtPath

FUNCTION GetSizeOfDrive(NameShare, BytesFree) RESULT(StatusDrive)
!+
!
! Description: This function determines the number of bytes available on a disk drive.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Portability
IMPLICIT NONE
!                               Passed                           Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)      :: NameShare                       ! Disk drive share name id (e.g. \\EMS-1\C_Freespace)
REAL(8)     ,INTENT(OUT)     :: BytesFree                       ! Number of free bytes on share (-1 if unable to determine)
INTEGER(4)                   :: StatusDrive                     ! Return Status
!
!                               Local
!----------------------------+------------
LOGICAL(4)                   :: Error                           ! Error in reading in table
INTEGER(4),PARAMETER         :: FailureUnknown=1                ! Unknown error return from SystemCommand
CHARACTER(LEN=128)           :: Command                         ! Constructed Directory Command
CHARACTER(LEN=1024)          :: AsciiChr                        ! Input ascii read from file
CHARACTER(LEN=15),PARAMETER  :: Path = "C:\Temp\Dir.out"        ! Temporary directory file
INTEGER(4)                   :: Unit                            ! Unit for opening file
INTEGER(4)                   :: StatusCmd                       ! Return Status from SystemCommand
INTEGER(4)                   :: ChrStart                        ! Index in AsciiChr where free bytes number starts
INTEGER(4)                   :: ChrEnd                          ! Index in AsciiChr where free bytes number ends
LOGICAL(4)                   :: FileExists                      ! Set true if the alternate sizing file exists
CHARACTER(32)                :: NameLocal                       ! Local copy of share name
CHARACTER(32)                :: NameAlternateFile               ! Name of alternate file holding sizing information
CHARACTER(1)                 :: NameAlternateFile_Chr(32)       ! Equivalenced to NameFile
EQUIVALENCE                    (NameAlternateFile,NameAlternateFile_Chr)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  DO
    !
    ! Initialize the return status
    StatusDrive = -1
    BytesFree   = -1
    !
    ! Delete the old results file
    StatusCmd = SystemCommand("del C:\Temp\Dir.out")
    !
    ! Construct a directory command and issue it, piping the results to D:\CSP\Temp\Dir.out
    CALL SleepMsec(1000)

    Command    = "dir "//TRIM(NameShare)//" > "//Path
    StatusCmd  = SystemCommand(Command)
    !
    IF(StatusCmd==FailureUnknown) THEN
      NameAlternateFile = TRIM(NameShare(3:))//".dat" ; WHERE(NameAlternateFile_Chr=="-".or.NameAlternateFile_Chr=="\") NameAlternateFile_Chr = "_"
      INQUIRE(FILE="C:\CSP\Temp\"//TRIM(NameAlternateFile), EXIST=FileExists)
      IF(FileExists) THEN
        Command    = "copy /y C:\Temp\"//TRIM(NameAlternateFile)//Path
        statusCmd  = SystemCommand(Command)
      END IF
    END IF
    !
    ! Copy the file
    NameLocal  = TRIM(NameShare)
    Command    = "copy /y C:\Temp\Dir.out C:\Temp\"//NameLocal(3:7)//"_"//NameLocal(9:19)
    statusCmd  = SystemCommand(Command)
    !
    ! Open the file
    IF(.not.OpenFileAndGetUnit(Unit, Path, "old")) RETURN
    !
    ! Read the file until we find the size
    DO
      !
      ! Read a line from the file
      AsciiChr = " "
      !
      ! Skip Blank Lines
      READ(Unit, ERR=1, END=1, FMT='(A)') AsciiChr                                                   ; IF(LEN_TRIM(AsciiChr)==0) CYCLE
      !
      ! Skip if the line does not contain the available size
      ChrEnd = INDEX(TRIM(AsciiChr),' bytes free')-1                                                 ; IF(ChrEnd<1) CYCLE
      !
      ! Find the start of the size
      ChrStart = INDEX(TRIM(AsciiChr(1:ChrEnd)),' ',BACK=.true.)+1                                   ; IF(ChrStart<1) EXIT
      !
      ! Extract the free size
      BytesFree   = NumAsciiWithCommas(AsciiChr(ChrStart:ChrEnd),Error)                              ; IF(Error) EXIT
      StatusDrive = 0
      !PRINT *,TRIM(ID_Drive(Drive)),' bytes free: ',BytesFree
      EXIT

    END DO
1   EXIT

  END DO
  CLOSE(Unit)
  RETURN
END FUNCTION GetSizeOfDrive

SUBROUTINE GetSizeOfDriveAtUnit(UnitIO, BytesFree, StatusGet)
!+
!
! Description: This function determines the number of bytes available on a disk drive that has been mapped by a
!              a Fortran I/O unit number.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed                           Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: UnitIO                          ! Unit number for directory file I/O
REAL(8)   ,INTENT(OUT)       :: BytesFree                       ! Number of free bytes on share (-1 if unable to determine)
INTEGER(4),INTENT(OUT)       :: StatusGet                       ! Return Status
!
!                               Local
!----------------------------+------------
LOGICAL(4)                   :: Error                           ! Error in reading in table
CHARACTER(LEN=1024)          :: AsciiChr                        ! Input ascii read from file
INTEGER(4)                   :: ChrStart                        ! Index in AsciiChr where free bytes number starts
INTEGER(4)                   :: ChrEnd                          ! Index in AsciiChr where free bytes number ends
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  DO
    !
    ! Initialize the return status
    StatusGet = UTIL_E_FAILURE
    BytesFree = -1
    !
    !
    ! Read the file until we find the size
    DO
      !
      ! Read a line from the file
      AsciiChr = " "
      !
      ! Skip Blank Lines
      READ(UnitIO, ERR=1, END=1, FMT='(A)') AsciiChr                                                   ; IF(LEN_TRIM(AsciiChr)==0) CYCLE
      !
      ! Skip if the line does not contain the available size
      ChrEnd = INDEX(TRIM(AsciiChr),' bytes free')-1                                                 ; IF(ChrEnd<1) CYCLE
      !
      ! Find the start of the size
      ChrStart = INDEX(TRIM(AsciiChr(1:ChrEnd)),' ',BACK=.true.)+1                                   ; IF(ChrStart<1) EXIT
      !
      ! Extract the free size
      BytesFree = NumAsciiWithCommas(AsciiChr(ChrStart:ChrEnd),Error)                                ; IF(Error) EXIT
      StatusGet = UTIL_S_NORMAL
      EXIT

    END DO
1   EXIT

  END DO
  RETURN
END SUBROUTINE GetSizeOfDriveAtUnit

SUBROUTINE GetFoldersInPathBase(PathBase, FolderTemplate, Folder_m_n, nEnd)
!+
! Description: This function returns a list of Folders under the base path that match a Folder Template
!
! ---------------------------------------------------------------------------------------------------------------------
!
! ToDo - This may suffer the same issue we had with the reallocation null characters.  The address of the reallocated
!        array may move.  We will need to do this with Fortran Pointers
!
! ---------------------------------------------------------------------------------------------------------------------
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                         Passed Variables                 Description (Units)
!--------------------------------------+---------------------------------+----------------------------------------------
CHARACTER(*),INTENT(IN)                :: PathBase                       ! Path to search for the list of folders
CHARACTER(*),INTENT(IN)                :: FolderTemplate                 ! Folder Template
CHARACTER(1),INTENT(INOUT),ALLOCATABLE :: Folder_m_n(:,:)                ! List of Folders, must be previously allocated
INTEGER(4)  ,INTENT(INOUT)             :: nEnd                           ! Number of Folders in the List
!
!                                         Local Variables
!--------------------------------------+---------------
LOGICAL(4)                             :: ExistsDirectory                ! Set true if PathBase is an actual Folder
INTEGER(4)                             :: Shape_k(2)                     ! Shape of Folder_m_n
INTEGER(4)                             :: mSize                          ! 1st dimension of Folder_m_n
INTEGER(4)                             :: nSize                          ! 2nd dimension of Folder_m_n
EQUIVALENCE                              (mSize, Shape_k(1))
EQUIVALENCE                              (nSize, Shape_k(2))
INTEGER(4)                             :: nInc                           ! Increment in Folder List
CHARACTER(32)                          :: Folder                         ! Folder matching the template
CHARACTER(128)                         :: PathTemplate                   ! Fully qualified path of the folder template.
INTEGER(4)                             :: nPathTemplate                  ! Number of chartacters PathTemplate
CHARACTER(LEN=128)                     :: Command                        ! Constructed Directory Command
INTEGER(4)                             :: UnitPipe                       ! Fortran I/O Unit to pipe batch output
CHARACTER(96)                          :: AsciiChr                       ! Buffer to read piped output
INTEGER(4)                             :: StatusCommand                  ! Return from SystemCommand
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
  !
  ! Initialize the number of Folders found
  nEnd = 0
  !
  ! Ensure Folder_m_n is allocated
  IF(.not.ALLOCATED(Folder_m_n)) THEN
    WRITE(*,FMT='(A)') " GetFoldersInPathBase.Error Folder_m_n not initially allocated."
    RETURN
  END IF
  !
  ! Ensure the PathBase is valid
  CALL InquireDirectory(TRIM(PathBase), ExistsDirectory)
  IF(.not.ExistsDirectory) THEN
     WRITE(*,FMT='(A)') " GetFoldersInPathBase.Warning "//TRIM(PathBase)//" is not a directory"
    RETURN
  END IF
  !
  ! Get the dimensions of Folder_m_n
  Shape_k = SHAPE(Folder_m_n)
  !
  ! Set the increment to the initially allocated size
  nInc = nSize
  !
  ! Set the directory pattern path and size
  PathTemplate  = TRIM(ADJUSTL(PathBase))//TRIM(ADJUSTL(FolderTemplate))
  nPathTemplate = LEN_TRIM(PathTemplate)
  !
  ! Pipe the base directory contents
  Command       = "dir "//TRIM(PathBase)//" /O:-D > c:\temp\dso.txt"
  StatusCommand =  SystemCommand(TRIM(Command))

  IF(.not.(OpenFileAndGetUnit(UnitPipe, "c:\temp\dso.txt", "old").and.StatusCommand==UTIL_S_NORMAL)) THEN
    WRITE(*,FMT='(A)') " GetFoldersInPathBase.Warning "//TRIM(PathBase)//" could not pipe its contents"
  END IF

  DO
    AsciiChr = " "
    READ(UnitPipe,FMT='(A)',END=1,ERR=2) AsciiChr
    !
    ! Skip blank lines
    IF(AsciiChr(1:1)==" ") CYCLE
    !
    ! Skip if not a Folder
    IF(AsciiChr(25:29) /= "<DIR>") CYCLE
    !
    ! Check to see if the Folder matches our pattern
    Folder = TRIM(AsciiChr(40:))
    IF(.not.WildcardMatchAscii(TRIM(Folder),TRIM(FolderTemplate),MatchCase=.false.)) CYCLE
    !
    ! Add to our collection
    CALL AppendValueToArray(Folder_m_n, nEnd, TRIM(Folder), nInc)
  END DO
1 CLOSE(UnitPipe, Status="DELETE")
  RETURN

2 WRITE(*,FMT='(A)') " GetFoldersInPathBase.Error Read of piped file"
  CLOSE(UnitPipe, Status="DELETE")
  RETURN

END SUBROUTINE GetFoldersInPathBase

SUBROUTINE GetFilesInPathBase(PathBase, FileTemplate, File_m_n, nEnd)
!+
! Description: This function returns a list of Files under the base path that match a File Template
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                                         Passed Variables                 Description (Units)
!--------------------------------------+---------------------------------+----------------------------------------------
CHARACTER(*),INTENT(IN)                :: PathBase                       ! Path to search for the list of Files
CHARACTER(*),INTENT(IN)                :: FileTemplate                   ! File Template
CHARACTER(1),INTENT(INOUT),ALLOCATABLE :: File_m_n(:,:)                  ! List of Files, must be previously allocated
INTEGER(4)  ,INTENT(OUT)               :: nEnd                           ! Number of Files in the List
!
!                                         Local Variables
!--------------------------------------+---------------
LOGICAL(4)                             :: ExistsDirectory                ! Set true if PathBase is an actual Folder
INTEGER(4)                             :: Shape_k(2)                     ! Shape of File_m_n
INTEGER(4)                             :: mSize                          ! 1st dimension of File_m_n
INTEGER(4)                             :: nSize                          ! 2nd dimension of File_m_n
EQUIVALENCE                              (mSize, Shape_k(1))
EQUIVALENCE                              (nSize, Shape_k(2))
INTEGER(4)                             :: nInc                           ! Increment in Folder List
CHARACTER(64)                          :: File                           ! File matching the template
CHARACTER(LEN=128)                     :: Command                        ! Constructed Directory Command
INTEGER(4)                             :: UnitPipe                       ! Fortran I/O Unit to pipe batch output
CHARACTER(96)                          :: AsciiChr                       ! Buffer to read piped output
INTEGER(4)                             :: StatusCommand                  ! Return from SystemCommand
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
  !
  ! Initialize the number of Files found
  nEnd = 0
  !
  ! Ensure that File_m_n has been allocated
  IF(.not.ALLOCATED(File_m_n)) THEN
    WRITE(*,FMT='(A)') " GetFilesInPathBase.Error File_m_n not initially allocated."
    RETURN
  END IF
  !
  ! Ensure the PathBase is valid
  CALL InquireDirectory(TRIM(PathBase), ExistsDirectory)
  IF(.not.ExistsDirectory) THEN
     WRITE(*,FMT='(A)') " GetFoldersInPathBase.Warning "//TRIM(PathBase)//" is not a directory"
    RETURN
  END IF
  !
  ! Get the dimensions of File_m_n
  Shape_k = SHAPE(File_m_n)
  !
  ! Set the increment to the initially allocated size
  nInc = nSize
  !
  ! Pipe the directory contents
  Command       = "dir "//TRIM(PathBase)//" /O:-D > c:\temp\dso.txt"
  StatusCommand =  SystemCommand(TRIM(Command))

  IF(.not.(OpenFileAndGetUnit(UnitPipe, "c:\temp\dso.txt", "old").and.StatusCommand==UTIL_S_NORMAL)) THEN
    WRITE(*,FMT='(A)') " GetFilesInPathBase.Warning "//TRIM(PathBase)//" could not pipe its contents"
  END IF

  DO
    AsciiChr = " "
    READ(UnitPipe,FMT='(A)',END=1,ERR=2) AsciiChr
    !
    ! Skip blank lines
    IF(AsciiChr(1:1)==" ") CYCLE
    !
    ! Skip Folders
    IF(AsciiChr(25:29) == "<DIR>") CYCLE
    !
    ! Check to see if the File matches our pattern
    File = TRIM(AsciiChr(40:))
    IF(.not.WildcardMatchAscii(TRIM(File),TRIM(FileTemplate),MatchCase=.false.)) CYCLE
    !
    ! Add to our collection
    CALL AppendValueToArray(File_m_n, nEnd, TRIM(File), nInc)
  END DO
1 CLOSE(UnitPipe, Status="DELETE")
  RETURN

2 WRITE(*,FMT='(A)') " GetFilesInPathBase.Error Read of piped file"
  CLOSE(UnitPipe, Status="DELETE")
  RETURN

  RETURN

END SUBROUTINE GetFilesInPathBase

FUNCTION WriteCSVFile(Path, Value_x_y, xSize, ySize, Reverse) RESULT(ValuesWritten)
!+
! Description: This function writes the values in the input array, Value_x_y(x,y) to a csv file.  The values are written
!              as rows that are indexed by the x subscript.  The order in which the rows are written to the csv file
!              is specified by the Reverse input parameter.  If Reverse is true, then the rows are written in descending
!              order by starting the y subscript at its upper limit and decrementing it to 1.  If Reverse is false then
!              the rows are output in ascending order by starting the y subscript at 1 and incrementing it to its upper
!              limit. The ValuesWritten parameter is returned TRUE if the operation was successful; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                   Description (Units)
!------------------------------+----------------------------------+-----------------------------------------------------
INTEGER(4)      ,INTENT(IN)    :: xSize                           ! Size of x dimension
INTEGER(4)      ,INTENT(IN)    :: ySize                           ! Size of y dimension
REAL(8)         ,INTENT(IN)    :: Value_x_y(xSize,ySize)          ! Values to output
CHARACTER(LEN=*),INTENT(IN)    :: Path                            ! Path to file to be opened
LOGICAL(4)      ,INTENT(IN)    :: Reverse                         ! Reverse y output order
LOGICAL(4)                     :: ValuesWritten                   ! return status (TRUE = success)
!
!                               Local Variables
!------------------------------+-------------
INTEGER(4)                     :: UnitCSV                         ! Unit Number to be used to write the file
INTEGER(4)                     :: y                               ! Index through y dimension
INTEGER(4)                     :: yStart                          ! Starting y
INTEGER(4)                     :: yEnd                            ! Ending y
INTEGER(4)                     :: dy                              ! y increment
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(OpenFileAndGetUnit(UnitCSV, Path, "unknown")) THEN
    IF(Reverse) THEN
      yStart = ySize; yEnd = 1; dy = -1
    ELSE
      yStart = 1; yEnd = ySize; dy = +1
    END IF
    DO y = yStart, yEnd, dy
      WRITE(UnitCSV, FMT='(F5.1,*(",",F5.1))',ERR=1) Value_x_y(:,y)
    END DO
    CLOSE(UnitCSV)
    ValuesWritten = .true.
  ELSE
    ValuesWritten = .false.
  END IF
  RETURN

1 CLOSE(UnitCSV)
  ValuesWritten = .false.
  RETURN

END FUNCTION WriteCSVFile

FUNCTION EnsureDirectory(Path, StatusAge) RESULT(Exists)
!+
! Description: This function check the directory substring of the input character Path to determine if the corresponding
!              directory exists.  The Status input character string has should be in the set {'old', 'new', 'unknown'}.
!              This function uses 'old' if Status is not in that set.  For Status values of 'new' or 'unknown', then
!              the directory is created if it does not exist.  This function returns the ouptut Exists TRUE if the
!              directory exists; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: Path                            ! Path to a file in the directory or the directory itself
CHARACTER(LEN=*),INTENT(IN)  :: StatusAge                       ! Status of the directory ('new', 'old', 'unknown')
LOGICAL(4)                   :: Exists                          ! return status (TRUE = Directory Exists)
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: SlashPath                       ! Location of any directory component in Path
INTEGER(4)                   :: StatusDir                       ! Return from SystemCommand > 0 if directory created
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !
  ! Get the directory terminator in the  path
  SlashPath = INDEX(Path, SlashDir(), BACK=.TRUE.)
  Exists    = .false.
  !
  SELECT CASE(TRIM(LowerCaseAscii(StatusAge)))
  CASE('old')
    IF(SlashPath > 0) CALL InquireDirectory(Path(1:SlashPath), Exists)

  CASE('new','unknown')
    IF(SlashPath > 0) THEN
      CALL InquireDirectory(Path(1:SlashPath), Exists)
      IF(.not.Exists) THEN
        !
        ! Create the Directory
        StatusDir = SystemCommand('mkdir '//Path(1:SlashPath))   !;   PRINT *,' StatusDir = ',StatusDir
        Exists    = StatusDir > -1
      END IF
    END IF

  CASE DEFAULT
    !
    ! Default to 'old'
    IF(SlashPath > 0) CALL InquireDirectory(Path(1:SlashPath), Exists)

  END SELECT

  IF(.not.Exists) THEN
    WRITE(*,FMT='(A)') ' Directory does not exist '//TRIM(Path(1:SlashPath))
  END IF

  RETURN
END FUNCTION EnsureDirectory

FUNCTION OpenFileAndGetUnit(UnitIO, Path, StatusOpen ) RESULT(FileOpened)
!+
! Description: This function opens a file in default mode and obtains a Fortran I/O unit number for reading/writing to
!              the file.  The Status input string should be a member of the set {'old', 'new', 'unknown'}. This function
!              uses 'old' if Status is not in that set. If Status has values of 'new' or 'unknown', then the file is
!              created if it does not exist. This function returns the output, FileOpened, TRUE if the file was opened
!              successfully; FALSE otherwise. The input string Path specifies the location of the file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER (4)     ,INTENT(OUT) :: UnitIO                          ! Unit Number to be used for file open
CHARACTER(LEN=*),INTENT(IN)  :: Path                            ! Path to file to be opened
CHARACTER(LEN=*),INTENT(IN)  :: StatusOpen                      ! How the file is to be opened ('old','new' or 'unknown'
LOGICAL (4)                  :: FileOpened                      ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: SlashPath                       ! Location of any directory component in Path
LOGICAL(4)                   :: FileExists                      ! Set true if a file exists
LOGICAL(4)                   :: DirectoryExists                 ! Set true if a directory exists
INTEGER(4)                   :: StatusDir                       ! Return from SystemCommand.  > 0 if directory created
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Initialize Unit
  UnitIO = 0
  !
  SELECT CASE(TRIM(LowerCaseAscii(StatusOpen)))
  CASE('old')
    INQUIRE(FILE=Path,EXIST=FileExists)
    IF(FileExists) THEN
      OPEN(NewUnit=UnitIO,File=Path,Status='old',Err = 1)
      FileOpened = .TRUE.
    ELSE
      FileOpened = .FALSE.
    END IF
  CASE('new','unknown')
    !
    ! Get any directory information associated with the path
    SlashPath = INDEX(Path, SlashDir(), BACK=.TRUE.)
    IF(SlashPath > 0) THEN
      CALL InquireDirectory(Path(1:SlashPath), DirectoryExists)
      IF(.not.DirectoryExists) THEN
        !
        ! Create the Directory
        StatusDir       = SystemCommand('mkdir '//Path(1:SlashPath))   !;   PRINT *,' StatusDir = ',StatusDir
        DirectoryExists = StatusDir > -1
      END IF
    ELSE
      DirectoryExists = .TRUE.
    END IF

    IF(DirectoryExists) THEN
      !PRINT *,' Opening: ',TRIM(Path)
      OPEN(NewUnit=UnitIO,File=Path,Status=TRIM(StatusOpen),Err = 1)
      FileOpened = .TRUE.
    ELSE
      WRITE(*,FMT='(A)') ' Directory does not exist '//TRIM(Path)
      FileOpened = .FALSE.
    END IF
  CASE DEFAULT
    !
    ! Default to 'old'
    INQUIRE(FILE=Path,EXIST=FileExists)
    IF(FileExists) THEN
      OPEN(NewUnit=UnitIO,File=Path,Status='old',Err = 1)
      FileOpened = .TRUE.
    ELSE
      FileOpened = .FALSE.
    END IF

  END SELECT
  IF(.not.FileOpened) UnitIO = 0
  RETURN
  !
1 FileOpened = .FALSE. ; UnitIO = 0  ; WRITE(*,FMT='(A)') ' Error opening file '//TRIM(Path)
  RETURN
END FUNCTION OpenFileAndGetUnit

FUNCTION OpenFileForUpdateAndGetUnit(UnitIO, Path, StatusOpen) RESULT(FileOpened)
!+
! Description: This function opens a file in 'append' mode and obtains a Fortran I/O unit number for reading/writing to
!              the file. The Status input string should be a member of the set {'old', 'new', 'unknown'}. This function
!              uses 'old' if Status is not in that set.  For Status values of 'new' or 'unknown', then the file is
!              created if it does not exist. This function returns the output, FileOpened, TRUE if the file was opened
!              successfully; FALSE otherwise. The input string Path specifies the location of the file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER (4)     ,INTENT(OUT)  :: UnitIO                         ! Unit Number to be used for file open
CHARACTER(LEN=*),INTENT(IN)   :: Path                           ! Path to file to be opened
CHARACTER(LEN=*),INTENT(IN)   :: StatusOpen                     ! How the file is to be opened ('old','new' or 'unknown'
LOGICAL (4)                   :: FileOpened                     ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                    :: SlashPath                      ! Location of any directory component in Path
LOGICAL(4)                    :: FileExists                     ! Set true if a file exists
LOGICAL(4)                    :: DirectoryExists                ! Set true if a directory exists
INTEGER(4)                    :: StatusDir                      ! Return from SystemCommand > 0 if directory created
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! Initialize Unit
  UnitIO = 0
  !
  SELECT CASE(TRIM(LowerCaseAscii(StatusOpen)))
  CASE('old')
    INQUIRE(FILE=Path,EXIST=FileExists)
    IF(FileExists) THEN
      OPEN(Access='append',NewUnit=UnitIO,File=Path,Status='old',Err = 1)
      FileOpened = .TRUE.
    ELSE
      FileOpened = .FALSE.
    END IF
  CASE('new','unknown')
    !
    ! Get any directory information associated with the path
    SlashPath = INDEX(Path, SlashDir(), BACK=.TRUE.)
    IF(SlashPath > 0) THEN
      CALL InquireDirectory(Path(1:SlashPath), DirectoryExists)
      IF(.not.DirectoryExists) THEN
        !
        ! Create the Directory
        StatusDir       = SystemCommand('mkdir '//Path(1:SlashPath))   !;   PRINT *,' StatusDir = ',StatusDir
        DirectoryExists = StatusDir > -1
      END IF
    ELSE
      DirectoryExists = .TRUE.
    END IF

    IF(DirectoryExists) THEN
      !PRINT *,' Opening: ',TRIM(Path)
      OPEN(Access='append',NewUnit=UnitIO,File=Path,Status=TRIM(StatusOpen),Err = 1)
      FileOpened = .TRUE.
    ELSE
      WRITE(*,FMT='(A)') ' Directory does not exist '//TRIM(Path)
      FileOpened = .FALSE.
    END IF

  CASE DEFAULT
    !
    ! Default to 'old'
    INQUIRE(FILE=Path,EXIST=FileExists)
    IF(FileExists) THEN
      OPEN(Access='append',NewUnit=UnitIO,File=Path,Status='old',Err = 1)
      FileOpened = .TRUE.
    ELSE
      FileOpened = .FALSE.
    END IF

  END SELECT
  RETURN
  !
1 FileOpened = .FALSE.   ; WRITE(*,FMT='(A)') ' Error opening file '//TRIM(Path)
  RETURN
END FUNCTION OpenFileForUpdateAndGetUnit

FUNCTION RemoveDirectory(Path) RESULT(RemovedDirectory)
!+
! Description: This function removes the directory specified in the input string Path. The output parameter
!              RemovedDirectory is returned TRUE if successful; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed Variables                 Description (Units)
!-----------------------------+---------------------------------+--------------------------------------- ----------------
CHARACTER(LEN=*),INTENT(IN)   :: Path                           ! Pathof directory to be removed
LOGICAL (4)                   :: RemovedDirectory               ! return status (TRUE = success)
!
!                             Local Variables
!-----------------------------+--------------
INTEGER(4)                    :: StatusCommand                  ! Return status from SystemCommand
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  StatusCommand   = SystemCommand("rmdir /s "//TRIM(Path))
  RemovedDirectory = StatusCommand == UTIL_S_NORMAL
  RETURN
END FUNCTION RemoveDirectory

FUNCTION EnsureFileIsDeleted(Path) RESULT(FileIsDeleted)
!+
! Description: This function checks if the file specified in the input string Path was deleted.  The output parameter
!              FileIsDeleted is returned TRUE if successful; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+--------------------------------------- ----------------
CHARACTER(LEN=*),INTENT(IN)   :: Path                           ! Path to file to be opened deleted
LOGICAL (4)                   :: FileIsDeleted                  ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
LOGICAL(4)                    :: ExistsFile                     ! Set true if a file exists
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  INQUIRE(File=TRIM(Path), Exist = ExistsFile)
  IF(.not.ExistsFile) THEN
    FileIsDeleted = .true.
  ELSE IF(DeleteFile(Path)) THEN
    FileIsDeleted = .true.
  ELSE
    FileIsDeleted = .false.
  END IF
END FUNCTION EnsureFileIsDeleted

FUNCTION DeleteFile(Path) RESULT(DeletedFile)
!+
! Description: This function deletes the file specified in the input string Path.  The output parameter DeletedFile
!              is returned TRUE if successful; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+--------------------------------------- ----------------
CHARACTER(LEN=*),INTENT(IN)   :: Path                           ! Path to file to be opened deleted
LOGICAL (4)                   :: DeletedFile                    ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                    :: UnitDelete                     ! Fortran I/O unit number for deleted file
INTEGER(4)                    :: StatusOfOpen                   ! Return status from OPEN
INTEGER(4)                    :: StatusOfDelete                 ! Return status from CLOSE
EQUIVALENCE                     (StatusOfOpen, StatusOfDelete)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  OPEN(NewUnit = UnitDelete, FILE = Path, IOSTAT = StatusOfOpen, ERR=1, STATUS= 'OLD')
  IF(StatusOfOpen==0) THEN
    CLOSE(UnitDelete, IOSTAT = StatusOfDelete, ERR=2, Status = 'DELETE')
  END IF
  DeletedFile = StatusOfDelete==0
  RETURN
!
! ----------------------------------------------------------------------------------------------------------------------
!                         E R R O R   H A N D L I N G
! ----------------------------------------------------------------------------------------------------------------------
!
1 WRITE(*,FMT='(A)') "Error unable to access "//TRIM(Path); DeletedFile = .false.; RETURN
2 WRITE(*,FMT='(A)') "Error unable to delete "//TRIM(Path); DeletedFile = .false.; RETURN
END FUNCTION DeleteFile

FUNCTION ReadLineFromUnit(Unit, Ascii) RESULT(ReadAscii)
!+
! Description: This function reads a line from the files associated with the Fortran I/O Unit input parameter. If the
!              read is successful then the output parameter ReadAscii is returned TRUE; FALSE otherwise. The output
!              string Ascii is returned as follows:
!
!                    1) Blank if the read was successful
!                    2) "CSP_S_EOF" if and end of file was encountered
!                    3) "CSP_E_READERROR" for any error.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER (4)     ,INTENT(IN)   :: Unit                           ! Unit Number to be used for file open
CHARACTER(LEN=*),INTENT(OUT)  :: Ascii                          ! Ascii line read from the file
LOGICAL (4)                   :: ReadAscii                      ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
!                                None
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Ascii = " "
  READ(Unit,FMT='(A)',END=1,ERR=2) Ascii
  ReadAscii = .TRUE.
  RETURN

1 ReadAscii = .FALSE.
  Ascii = "CSP_S_EOF"
  RETURN

2 ReadAscii = .FALSE.
  Ascii = "CSP_E_FILEREAD"
  RETURN

END FUNCTION ReadLineFromUnit

FUNCTION OpenFile(Unit, Path, StatusOpen) RESULT(FileOpened)
!+
! Description: This function opens a file using the input Unit as the Fortran I/O unit number for reading/writing to
!              the file. The Status input string should be a member of the set {'old', 'new', 'unknown'}. This function
!              uses 'old' if Status is not in that set. If Status has values of 'new' or 'unknown', then the file is
!              created if it does not exist. This function returns the output, FileOpened, TRUE if the file was opened
!              successfully; FALSE otherwise. The input string Path specifies the location of the file.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)   :: Path                           ! Path to file to be opened
INTEGER (4)     ,INTENT(IN)   :: Unit                           ! Unit Number to be used for file open
CHARACTER(LEN=*),INTENT(IN)   :: StatusOpen                     ! How the file is to be opened ('old','new' or 'unknown'
LOGICAL (4)                   :: FileOpened                     ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                    :: SlashPath                      ! Location of any directory component in Patgh
LOGICAL(4)                    :: FileExists                     ! Set true if a file exists
LOGICAL(4)                    :: DirectoryExists                ! Set true if a directory exists
INTEGER(4)                    :: StatusDir                      ! Return from SystemCommand > 0 if directory created
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
SELECT CASE(TRIM(LowerCaseAscii(StatusOpen)))
CASE('old')
  INQUIRE(FILE=Path,EXIST=FileExists)
  IF(FileExists) THEN
    OPEN(Unit,File=Path,Status='old',Err = 1)
    FileOpened = .TRUE.
  ELSE
    FileOpened = .FALSE.
  END IF
CASE('new','unknown')
  !
  ! Get any directory information associated with the path
  SlashPath = INDEX(Path, SlashDir(), BACK=.TRUE.)
  IF(SlashPath > 0) THEN
    CALL InquireDirectory(Path(1:SlashPath), DirectoryExists)
    IF(.not.DirectoryExists) THEN
      !
      ! Create the Directory
      StatusDir       = SystemCommand('mkdir '//Path(1:SlashPath))   !;   PRINT *,' StatusDir = ',StatusDir
      DirectoryExists = StatusDir > -1
    END IF
  ELSE
    DirectoryExists = .TRUE.
  END IF

  IF(DirectoryExists) THEN
    !PRINT *,' Opening: ',TRIM(Path)
    OPEN(Unit,File=Path,Status=TRIM(StatusOpen),Err = 1)
    FileOpened = .TRUE.
  ELSE
    PRINT *,' Directory does not exist ',TRIM(Path)
    FileOpened = .FALSE.
  END IF

CASE DEFAULT
  !
  ! Default to 'old'
  INQUIRE(FILE=Path,EXIST=FileExists)
  IF(FileExists) THEN
    OPEN(Unit,File=Path,Status='old',Err = 1)
    FileOpened = .TRUE.
  ELSE
    FileOpened = .FALSE.
  END IF

END SELECT
RETURN
!
1 FileOpened = .FALSE.   ; PRINT *,' Error opening file '//TRIM(Path)
RETURN
END FUNCTION OpenFile

FUNCTION CanOpenFile(Unit, Path) RESULT(FileCanBeOpened)
!+
! Description: This function checks if file can be opened. If it can be opened then the output FileCanBeOpened is
!              returned TRUE; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)   :: Path                           ! Path to file to be opened
INTEGER (4)     ,INTENT(IN)   :: Unit                           ! Unit Number to be used for file open
LOGICAL (4)                   :: FileCanBeOpened                ! return status (TRUE = success)

!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!

OPEN(Unit,file=Path,status='old',err = 999)
FileCanBeOpened = .TRUE.
CLOSE(Unit)
RETURN
!
999 FileCanBeOpened = .FALSE.
CLOSE(Unit)
RETURN
END FUNCTION CanOpenFile

FUNCTION CopyFile(PathFrom, PathTo) RESULT(FileCopied)
!+
! Description: This function copies the file at the location specified in the input string PathFrom to a file at the
!              location specified in the input string PathTo.  If a file specified in PathTo already exists then it is
!              overwritten.  The FileCopied parameter is returned TRUE if the copy was successful; FALSE otherwise.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)   :: PathFrom                       ! Path of file to copy
CHARACTER(LEN=*),INTENT(IN)   :: PathTo                         ! Path of file to copy to
LOGICAL (4)                   :: FileCopied                     ! return status (TRUE = success)
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                    :: FromSlash                      ! End of the directory specification in PathFrom
INTEGER(4)                    :: ToSlash                        ! End of the directory specification in PathTo
CHARACTER(256)                :: PathCopied                     ! Path used for target of copy
INTEGER(4)                    :: UnitCopy                       ! I/O unit to open the file
INTEGER(4)                    :: StatusCmd                      ! Return status from command shell to copy file
CHARACTER(512)                :: Command                        ! Command string for comnand shell
LOGICAL(4)                    :: Exists                         ! Source file exists
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! Initialize the status returned.
  FileCopied = .false.

  DO
    INQUIRE(FILE=PathFrom,EXIST=Exists)                        ; IF(.not.Exists)                                   EXIT
    FromSlash  = INDEX(PathFrom, SlashDir(), BACK=.true.)      ; IF(LEN_TRIM(PathFrom)==FromSlash.or.FromSlash==0) EXIT
    ToSlash    = INDEX(PathTo  , SlashDir(), BACK=.true.)      ; IF(LEN_TRIM(PathTo  )==0        .or.ToSlash==0  ) EXIT
    IF(ToSlash==LEN_TRIM(PathTo)) THEN
      PathCopied = TRIM(PathTo)//PathFrom(FromSlash+1:)
    ELSE
      PathCopied = PathTo
    END IF
    UnitCopy = 0
    IF(.not.OpenFileAndGetUnit(UnitCopy,PathCopied,"unknown"))  EXIT
    CLOSE(UnitCopy)

    Command    = "copy /y "//TRIM(PathFrom)   &
                      //" "//TRIM(PathCopied)
    statusCmd  = SystemCommand(Command)                        ;                               IF(StatusCmd /= 0)  EXIT
    FileCopied = .true.
    EXIT
  END DO

END FUNCTION CopyFile
!
FUNCTION AddIncrementToValue(Val, dV, dVstart) RESULT (ValIncremented)
!+
!
! Description: Routine to adjust a Value by an increment.  It is intended for use with an increment buttom (up/down)
!              If the Value is zero then the increment is limited to dVStart.  If the incremented value crosses zero
!              then the value is set to zero.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)           :: Val                            ! Value to be incremented
REAL(8), INTENT(IN)           :: dV                             ! Increment to add to value
REAL(8), INTENT(IN)           :: dVStart                        ! Minimum increment if starting from zero
REAL(8)                       :: ValIncremented                 ! Result value
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
IF(dV == 0) THEN
  ValIncremented = Val
ELSE IF(Val == 0d0) THEN
  ValIncremented = dVstart * SIGN(1d0, dV)
ELSE IF(Val * (Val + dV) <= 0d0) THEN
  ValIncremented = 0d0
ELSE
  ValIncremented = Val + dV
END IF
!
END FUNCTION AddIncrementToValue

FUNCTION Numeric(achr) RESULT (IsNumber)
!+
!
! Description: Logical Test to see of an ascii character is a base 10 numeric character
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: achr
LOGICAL(4)                    :: IsNumber
!
!                                Local
!-----------------------------+---------------------------------
INTEGER(4)                    :: chr
INTEGER(4)                    :: chrEnd
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
chrEnd  = LEN(achr)
IsNumber = chrEnd > 0
DO chr = 1, chrEnd
  IF(achr(chr:chr) < zeroChar.or.achr(chr:chr) > nineChar) THEN
    IsNumber = .false.
    EXIT
  END IF
END DO
!
END FUNCTION Numeric

SUBROUTINE DecodeCSVFields(Acsv, A_Field, FieldMax, FieldEnd, Error)
!+
!
! Description: Decode a line read from a csv file.  The line is parsed into separate fields with "," as a separator.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: Acsv                           ! CSV line to be decoded
INTEGER(4)  , INTENT(IN)      :: FieldMax                       ! Number of fields
CHARACTER(*), INTENT(OUT)     :: A_Field(FieldMax)              ! Extracted fields
INTEGER(4)  , INTENT(OUT)     :: FieldEnd                       ! Number of fields extracted
LOGICAL(4)  , INTENT(OUT)     :: Error                          ! Error in extacting the fields
!
!                                Local
!-----------------------------+---------------------------------
INTEGER(4)                    :: LocationMax
INTEGER(4)                    :: comma$csv_location(FieldMax-1)
INTEGER(4)                    :: LocationEnd
INTEGER(4)                    :: Location
INTEGER(4)                    :: csv
INTEGER(4)                    :: csvComma
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  LocationMax = FieldMax - 1
  CALL LocateSeparators(Acsv,",",LocationMax, comma$csv_location, locationEnd)
  A_Field = " "
  IF(LocationEnd /= LocationMax) THEN
    Error = .true.
  ELSE
    csv      = 1
    FieldEnd = 1
    DO Location = 1, LocationEnd
      csvComma = Comma$csv_Location(Location)
      IF(csvComma > csv + 1) A_Field(FieldEnd) = Acsv(csv+1:csvComma-1)
      csv      = csvComma
      FieldEnd = FieldEnd + 1
    END DO
    IF(csv < LEN_TRIM(Acsv)) A_Field(FieldEnd) = Acsv(csv+1:)
    Error = .false.
  END IF
  RETURN

END SUBROUTINE DecodeCSVFields
!
!
SUBROUTINE LocateSeparators(achr,asep,locationMax,i$chr_location, locationEnd)
!+
!
! Description: Determine the location of all the separators in a string.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: achr                           ! String to be parsed
CHARACTER(1), INTENT(IN)      :: aSep                           ! Separator character
INTEGER(4),   INTENT(IN)      :: locationMax                    ! Maximum number of separator locations
INTEGER(4),   INTENT(OUT)     :: i$chr_location(locationMax)    ! Indices of all locations in aChr that are separators
INTEGER(4),   INTENT(OUT)     :: locationEnd                    ! Number of separator locations
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: chr                            ! Index through characters in arch
INTEGER(4)                    :: chrEnd                         ! Number of the characters in arch
LOGICAL(1)                    :: foundSeparator                 ! Set true if a separator character was found
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------

  chr         = 1
  chrEnd      = LEN_TRIM(achr)
  locationEnd = 0
  DO WHILE(chr<=chrEnd)
    foundSeparator = achr(chr:chr)==asep
    IF(foundSeparator) THEN
      IF(locationEnd==locationMax) EXIT
      locationEnd = locationEnd + 1
      i$chr_location(locationEnd) = chr
    END IF
    chr = chr + 1
  END DO
  RETURN
!
END SUBROUTINE LocateSeparators

SUBROUTINE ParseText(Text,asep,Text_parse,parseEnd)
!+
!
! Description: Easy to use parser that allows us to parse a string with one separator and return up to 16 tokens.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: Text                           ! String to be parsed
CHARACTER(1), INTENT(IN)      :: aSep                           ! Separator character
CHARACTER(*), INTENT(OUT)     :: Text_Parse(16)                 ! Parsed Characters (up to 16)
INTEGER(4)  , INTENT(IN OUT)  :: parseEnd
!
!                                Local Variables
!-----------------------------+---------------------------------
INTEGER(4)  ,PARAMETER        :: parseMax = 16                  ! Maximum number of tokens
CHARACTER(1)                  :: a_sep(1)                       ! List of valid separator characters
INTEGER(4)                    :: start$chr_parse(16)            ! Start character of the tokens
INTEGER(4)                    :: end$chr_parse(16)              ! End character of the tokens
INTEGER(4)                    :: parse                          ! Index through the tokens
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
a_sep(1) = aSep
CALL ParseString(Text,a_sep(1),1,16,start$chr_parse,end$chr_parse,parseEnd)
parseEnd = MIN(16,parseEnd)
DO parse = 1, parseEnd
  Text_parse(parse) = Text(start$chr_parse(parse):end$chr_parse(parse))
END DO
RETURN
END SUBROUTINE ParseText

SUBROUTINE ParseTextMax(Text,asep,Text_parse,parseEnd,parseMax)
!+
!
! Description: Easy to use parser that allows us to parse a string with one separator and return up to a user defined
!              number of tokens.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),   INTENT(IN)      :: parseMax                       ! Maximum number of tokens to parse
CHARACTER(*), INTENT(IN)      :: Text                           ! String to be parsed
CHARACTER(1), INTENT(IN)      :: aSep                           ! Separator character
CHARACTER(48),INTENT(OUT)     :: Text_Parse(parseMax)           ! Parsed Characters
INTEGER(4),   INTENT(OUT)     :: parseEnd                       ! Actual number of tokens parsed
!
!                                Local Variables
!-----------------------------+---------------------------------
CHARACTER(1)                  :: a_sep(1)                       ! Valid separator character cast as an array (of 1!)
INTEGER(4),ALLOCATABLE        :: start$chr_parse(:)             ! Start of each token
INTEGER(4),ALLOCATABLE        :: end$chr_parse  (:)             ! End of each token
INTEGER(4)                    :: parse                          ! Index through the tokens
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
a_sep(1) = aSep
ALLOCATE(start$chr_parse(parseMax),end$chr_parse(parseMax))
CALL ParseString(Text,a_sep,1,parseMax,start$chr_parse,end$chr_parse,parseEnd)
parseEnd = MIN(parseMax,parseEnd)
DO parse = 1, parseEnd
  Text_parse(parse) = Text(start$chr_parse(parse):end$chr_parse(parse))
END DO
DEALLOCATE(start$chr_parse,end$chr_parse)
RETURN
END SUBROUTINE ParseTextMax

SUBROUTINE ParseString(achr,a_sep,sepEnd,parseMax,start$chr_parse,end$chr_parse,parseEnd)
!+
!
! Description: Parse a string and separate it into components using separator charactors.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: achr                           ! String to be parsed
INTEGER(4),   INTENT(IN)      :: sepEnd                         ! Number of separators
INTEGER(4),   INTENT(IN)      :: parseMax                       ! Maximum number of tokens
CHARACTER(1), INTENT(IN)      :: a_sep(sepEnd)                  ! List of valid separator characters
INTEGER(4),   INTENT(OUT)     :: start$chr_parse(parseMax)      ! Start of each token
INTEGER(4),   INTENT(OUT)     :: end$chr_parse(parseMax)        ! End of each token
INTEGER(4),   INTENT(OUT)     :: parseEnd                       ! Number of tokens
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: sep                            ! Start of each token
INTEGER(4)                    :: parse                          ! End of each token
INTEGER(4)                    :: chr                            ! Index through the tokens
INTEGER(4)                    :: chrStart                       ! Start of a token
INTEGER(4)                    :: chrEnd                         ! End of a token
LOGICAL(4)                    :: foundSeparator                 ! Found separator character for a token
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------

chrStart = 1
chr      = 1
chrEnd   = LEN_TRIM(achr)
parse    = 0
DO WHILE(chr<=chrEnd)
  sep = 1
  foundSeparator = .false.
  DO WHILE(sep<=sepEnd .and. .not.foundSeparator)
    IF(achr(chr:chr)==a_sep(sep)) THEN
       foundSeparator = .true.
    ELSE
       sep = sep + 1
    END IF
  END DO
  IF(foundSeparator.or.chr==chrEnd) THEN
    IF(parse==parseMax) EXIT
    parse = parse + 1
    start$chr_parse(parse) = INT(chrStart, KIND=2)
    IF(foundSeparator) THEN
      end$chr_parse(parse) = INT(chr-1, KIND=2)
    ELSE
      end$chr_parse(parse) = INT(chr, KIND=2)
    END IF
    IF(chrStart>end$chr_parse(parse)) THEN
      parse = parse - 1
    END IF
    chrStart = chr + 1
  END IF
  chr = chr + 1
END DO
parseEnd = parse
RETURN
!
END SUBROUTINE ParseString

FUNCTION NumAsciiWithCommas(ascii_chr,error) RESULT(num)
!+
!
! Description: Convert a general alphanumeric character ascii into a REAL(8) using character manipulation
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: ascii_chr                      ! ascii to be converted
LOGICAL(4),   INTENT(OUT)     :: error                          ! error
REAL(8)                       :: num                            ! converted result
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: lengthMantissa                 ! no. of characters of the mantissa
INTEGER(4)                    :: chrEnd                         ! length of the char ascii
INTEGER(4)                    :: chr                            ! index in ascii
REAL(8)                       :: mantissa                       ! mantissa of the number
REAL(8)                       :: exponent                       ! exponent of the number
REAL(8)                       :: base                           ! base of the number
REAL(8)                       :: sign                           ! +/- 1 for explicitely signed number
CHARACTER(1)                  :: ascii                          ! Individual character of ascii_chr
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
error = .true.
num   = 0.0d0

!-- Find the non-blank ending character
chr = LEN_TRIM(ascii_chr)
DO
  IF(chr < 1) RETURN
  ascii = ascii_chr(chr:chr)
  IF(ascii/=SPACE.and.ascii/=NULL.and.ascii/=TAB) EXIT
  chr = chr - 1
END DO
chrEnd = chr

!-- Set the number base
IF (ascii_chr(chr:chr)=='X'.or.ascii_chr(chr:chr)=='x') THEN
  base = 16.0d0
ELSE IF (ascii_chr(chr:chr)=='O'.or.ascii_chr(chr:chr)=='o') THEN
  base = 8.0d0
ELSE IF (ascii_chr(chr:chr)=='B'.or.ascii_chr(chr:chr)=='b') THEN
  base = 2.0d0
ELSE
  base = 10.0d0
END IF
IF(base/=10) chrEnd = chrEnd - 1

IF(chrEnd<1) RETURN

IF(base/=10) THEN
  num = NumasciiBaseWithCommas(ascii_chr(1:chrEnd), base, error)
  RETURN
END IF

!--- Find the non-blank starting character
chr = 1
DO
  ascii = ascii_chr(chr:chr)
  IF(ascii/=SPACE.and.ascii/=NULL.and.ascii/=TAB) EXIT
  chr = chr + 1
END DO

IF(ascii_chr(chr:chr)=='+') THEN
  sign = +1.0d0
ElSE IF(ascii_chr(chr:chr)=='-') THEN
  sign = -1.0d0
ELSE
  sign = 1.0d0
END IF


lengthMantissa = MAX( INDEX(ascii_chr(chr:chrEnd),'E'),INDEX(ascii_chr(chr:chrEnd),'e') &
                     ,INDEX(ascii_chr(chr:chrEnd),'D'),INDEX(ascii_chr(chr:chrEnd),'d') )

IF (lengthMantissa==0) THEN

  !-- Convert non-exponential number
  num  = NumAsciiBaseWithCommas(ascii_chr(chr:chrEnd), base, error)

ELSE

  !-- Convert exponential number
  IF(lengthMantissa==1) THEN
    mantissa = 1.0d0
  ELSE IF(lengthMantissa==2 .and. sign/=0.0d0) THEN
    mantissa = sign
  ELSE
    mantissa = NumAsciiBaseWithCommas(ascii_chr(chr:lengthMantissa-1), base, error)
    IF (error) RETURN
  END IF

  exponent = NumAsciiBaseWithCommas(ascii_chr(lengthMantissa+chr:chrEnd), base, error)
  IF (error) RETURN

  num   = mantissa*base**exponent
  error = .false.
END IF
END FUNCTION NumAsciiWithCommas

FUNCTION NumAsciiBaseWithCommas(ascii_chr, base, error) RESULT(num)
!+
! Description: Convert a numerical character string expressed in a number base into a REAL(8). Commas can be present in
!              the string.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: ascii_chr                      ! ascii to be converted
REAL(8),      INTENT(IN)      :: base                           ! Base in which number is expressed
LOGICAL(4),   INTENT(OUT)     :: error                          ! error
REAL(8)                       :: num                            ! converted result
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: chr
INTEGER(4)                    :: chrEnd
CHARACTER(1)                  :: ascii                          ! Individual character of ascii_chr
INTEGER(4)                    :: digit
REAL(8)                       :: a,b,c
REAL(8)                       :: multiplier
INTEGER(4)                    :: chrStart
LOGICAL(1)                    :: foundDecimal
LOGICAL(1)                    :: foundDigit
REAL(8)                       :: numVal
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!-- Initialize returned values
  num   = 0
  error = .true.

  !-- Find the non-blank ending character
  chr = LEN(ascii_chr)
  DO
    IF(chr < 1) RETURN
    ascii = ascii_chr(chr:chr)
    IF(ascii/=SPACE.and.ascii/=NULL.and.ascii/=TAB) EXIT
    chr = chr - 1
  END DO
  chrEnd = chr

  !--- Find the non-blank starting character
  chr = 1
  DO
    ascii = ascii_chr(chr:chr)
    IF(ascii/=SPACE.and.ascii/=NULL.and.ascii/=TAB) EXIT
    chr = chr + 1
  END DO
  chrStart = chr

  !-- Set the sign multiplier
  multiplier = 1.0d0
  IF(ascii_chr(chrStart:chrStart)=='-') THEN
    chrStart = chrStart + 1
    multiplier = -1.0d0
  else IF(ascii_chr(chrStart:chrStart).eq.'+') THEN
    chrStart = chrStart + 1
  END IF

  IF(chrStart>chrEnd) RETURN

  a      = base
  b      = 1.0d0
  c      = 1.0d0
  numVal = 0.0d0
  foundDecimal = .false.
  foundDigit   = .false.
  DO chr = chrStart, chrEnd
    CALL UpperCase(ascii_chr(chr:chr),ascii)
    IF('0'<=ascii.and.ascii<='9') THEN
      digit = ICHAR(ascii) - ICHAR('0')
      IF(digit + 1 > base) RETURN
      numVal     = a*numVal + b*digit
      b          = b*c
      foundDigit = .true.

    ELSE IF('A'<=ascii.and.ascii<='F'.and.base==16) THEN
      digit = ICHAR(ascii) - ICHAR('A') + 10
      IF(digit + 1 > base) RETURN
      numVal     = a*numVal + b*digit
      b          = b*c
      foundDigit = .true.

    ELSE IF(ascii==DECIMALPOINT) THEN
      IF(foundDecimal) RETURN
      a = 1.0d0
      b = 1.0d0/base
      c = 1.0d0/base
      foundDecimal = .true.

    ELSE IF(ascii==COMMA) THEN
      CYCLE

    ELSE
      RETURN
    END IF
  END DO
  IF(.not.foundDigit) RETURN

  error = .false.
  num   = numVal*multiplier
END FUNCTION NumAsciiBaseWithCommas

FUNCTION NumAscii(ascii_chr,error) RESULT(num)
!+
!
! Description:  Convert a general alphanumeric character ascii into a REAL(8) using character manipulation.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: ascii_chr                      ! ascii to be converted
LOGICAL(4),   INTENT(OUT)     :: error                          ! error
REAL(8)                       :: num                            ! converted number
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: lengthMantissa                 ! no. of characters of the mantissa
INTEGER(4)                    :: chrEnd                         ! length of the char ascii
INTEGER(4)                    :: chr                            ! index through character in ascii_chr
REAL(8)                       :: mantissa                       ! mantissa of the number
REAL(8)                       :: exponent                       ! exponent of the number
REAL(8)                       :: base                           ! base of the number
REAL(8)                       :: sign                           ! +/- 1 for explicitely signed number
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  error = .true.
  num   = 0.0d0

  !-- Find the non-blank ending character
  chr = LEN_TRIM(ascii_chr)
  DO
    IF(chr < 1) RETURN
    IF(ascii_chr(chr:chr)/=SPACE.and.ascii_chr(chr:chr)/=NULL.and.ascii_chr(chr:chr)/=TAB) EXIT
    chr = chr - 1
  END DO
  chrEnd = chr

  !-- Set the number base
  IF (ascii_chr(chr:chr)=='X'.or.ascii_chr(chr:chr)=='x') THEN
    base = 16.0d0
  ELSE IF (ascii_chr(chr:chr)=='O'.or.ascii_chr(chr:chr)=='o') THEN
    base = 8.0d0
  ELSE IF (ascii_chr(chr:chr)=='B'.or.ascii_chr(chr:chr)=='b') THEN
    base = 2.0d0
  ELSE
    base = 10.0d0
  END IF
  IF(base/=10) chrEnd = chrEnd - 1

  IF(chrEnd<1) RETURN

  IF(base/=10) THEN
    num = NumasciiBase(ascii_chr(1:chrEnd), base, error)
    RETURN
  END IF

  !--- Find the non-blank starting character
  chr = 1
  DO
    IF(ascii_chr(chr:chr)/=SPACE.and.ascii_chr(chr:chr)/=NULL.and.ascii_chr(chr:chr)/=TAB) EXIT
    chr = chr + 1
  END DO

  IF(ascii_chr(chr:chr)=='+') THEN
    sign = +1.0d0
  ElSE IF(ascii_chr(chr:chr)=='-') THEN
    sign = -1.0d0
  ELSE
    sign = 0.0d0
  END IF

  lengthMantissa = MAX( INDEX(ascii_chr(chr:chrEnd),'E'),INDEX(ascii_chr(chr:chrEnd),'e') &
                       ,INDEX(ascii_chr(chr:chrEnd),'D'),INDEX(ascii_chr(chr:chrEnd),'d') )

  IF (lengthMantissa==0) THEN

    !-- Convert non-exponential number
    num  = NumAsciiBase(ascii_chr(chr:chrEnd), base, error)

  ELSE

    !-- Convert exponential number
    IF(lengthMantissa==1) THEN
      mantissa = 1.0d0
    ELSE IF(lengthMantissa==2 .and. sign/=0.0d0) THEN
      mantissa = sign
    ELSE
      mantissa = NumAsciiBase(ascii_chr(chr:lengthMantissa-1), base, error)
      IF (error) RETURN
    END IF

    exponent = NumAsciiBase(ascii_chr(lengthMantissa+chr:chrEnd), base, error)
    IF (error) RETURN

    num   = mantissa*base**exponent
    error = .false.
  END IF
END FUNCTION NumAscii

FUNCTION NullifyString(ascii_chr) RESULT(sReturn)
!+
!
! Description: Converts all NULL ('00'x) or TAB ('09'x) characters in the input string ascii_chr to SPACES (z'20') and
!              returns the converted string in the output sReturn.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: ascii_chr                      !ascii to be converted
CHARACTER(256)                :: sReturn                        !return character string

!
!                                Local
!-------------------------------------
INTEGER(4)                    :: chr                            ! index through characters in ascii_chr
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------

  !-- Find the non-blank ending character
  sReturn = ascii_chr
  chr = LEN_TRIM(sReturn)
  DO
    IF(chr < 1) RETURN
    IF(sReturn(chr:chr) == NULL .or. sReturn(chr:chr) == TAB) sReturn(chr:chr) = SPACE
    chr = chr - 1
  END DO

END FUNCTION NullifyString

SUBROUTINE NullTerminateString(CvalChr, Bval_Chr, chrEnd)
!+
! Description: Thie subroutine appends a null (0) to the input string CvalChr and returns the null terminated string in
!              the output Bval_chr.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: CvalChr                        ! Input string
BYTE        , INTENT(IN OUT)  :: Bval_Chr(CHREND$MX)            ! Byte copy of CvalChr, padded with a null terminator
INTEGER(4)  , INTENT(OUT)     :: ChrEnd                         ! Size of Bval_Chr including the null terminator
!
!                                Local
!-----------------------------+---------------------------------
INTEGER(4)                    :: Chr                            ! Index through characters in StringChr
CHARACTER(1)                  :: Cval                           ! Single character of CvalChr
BYTE                          :: Bval                           ! Byte equialence of Cval
EQUIVALENCE                     (Bval,Cval)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
 ! See if the string is already NULL Terminated
  ChrEnd = MIN(LEN_TRIM(CvalChr) + 1, CHREND$MX)
  IF(chrEnd > 1) THEN
    DO Chr = 1, ChrEnd-1
      Cval = CvalChr(Chr:Chr)
      Bval_Chr(Chr) = Bval
    END DO
  END IF
  Bval_Chr(ChrEnd) = 0
  RETURN
END SUBROUTINE NullTerminateString

FUNCTION NullTerminatedString (CvalChr, ChrEnd) RESULT(Bval_Chr)
!+
! Description: This function copies individual characters from the input string CvalChr to the output string Bval_Chr
!              until a null (0) is encountered or the next to last location of Bval_Chr is reached.  The last location
!              of Bval_Chr is populated with a null.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: CvalChr                        ! Input string
INTEGER(4)  , INTENT(IN)      :: ChrEnd                         ! Size of the result string
BYTE                          :: Bval_Chr(ChrEnd)               ! Byte copy of CvalChr, padded with a null terminator
!
!                                Local
!-----------------------------+---------------------------------
INTEGER(4)                    :: ChrLast                        ! Maximum number of characters to transfer
INTEGER(4)                    :: Chr                            ! Index through characters in StringChr
CHARACTER(1)                  :: Cval                           ! Single character of CvalChr
BYTE                          :: Bval                           ! Byte equialence of Cval
EQUIVALENCE                     (Bval,Cval)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Bval_Chr = 0
  chrLast = MIN(LEN_TRIM(CvalChr), ChrEnd-1)
  IF(chrLast > 0) THEN
    DO Chr = 1, ChrLast
      Cval = CvalChr(Chr:Chr)
      IF(Cval==NullTerminator) EXIT
      Bval_Chr(Chr) = Bval
    END DO
  END IF
  RETURN
END FUNCTION NullTerminatedString

SUBROUTINE StringFromNullTerminate(CvalChr, Bval_Chr, chrEnd)
!+
! Description: This subroutine constructs a string from a null terminated char array.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: CvalChr                        ! Output string
INTEGER(4)  , INTENT(IN)      :: ChrEnd                         ! Size of the input null terminated byte array
BYTE        , INTENT(IN)      :: Bval_Chr(ChrEnd)               ! Byte array of characters padded with a null terminator
!
!                                Local
!-----------------------------+---------------------------------
INTEGER(4)                    :: ChrLast                        ! Maximum number of characters to transfer
INTEGER(4)                    :: Chr                            ! Index through characters in StringChr
CHARACTER(1)                  :: Cval                           ! Single character of CvalChr
BYTE                          :: Bval                           ! Byte equialence of Cval
EQUIVALENCE                     (Bval,Cval)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CvalChr = " "
  chrLast = MIN(LEN(CvalChr),ChrEnd-1)
  IF(chrLast > 1) THEN
    DO Chr = 1, ChrLast
      Bval = Bval_Chr(Chr)
      IF(Bval==0) EXIT
      CvalChr(Chr:Chr) = Cval
    END DO
  END IF
  RETURN
END SUBROUTINE StringFromNullTerminate

SUBROUTINE StringFromNullTerminated(CvalChr, Bval_Chr)
!+
! Description: This subroutine constructs a string from a null terminated char array.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(OUT)     :: CvalChr                        ! Output string
BYTE        , INTENT(IN)      :: Bval_Chr(*)                    ! Byte array of characters ended with a null terminator
!
!                                Local
!-----------------------------+---------------------------------
INTEGER(4)                    :: ChrLast                        ! Maximum number of characters to transfer
INTEGER(4)                    :: Chr                            ! Index through characters in StringChr
CHARACTER(1)                  :: Cval                           ! Single character of CvalChr
BYTE                          :: Bval                           ! Byte equialence of Cval
EQUIVALENCE                     (Bval,Cval)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CvalChr = " "
  ChrLast = LEN(CvalChr)
  IF(chrLast > 1) THEN
    DO Chr = 1, ChrLast
      Bval = Bval_Chr(Chr)
      IF(Bval==0) EXIT
      CvalChr(Chr:Chr) = Cval
    END DO
  END IF
  RETURN
END SUBROUTINE StringFromNullTerminated

FUNCTION StringsEqual(string1, string2) RESULT(bEqual)
!+
!
! Description:  This function compares 2 strings to check if they are essentially equal.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: string1                        !ascii string1 to be compared
CHARACTER(*), INTENT(IN)      :: string2                        !ascii string2 to be compared
LOGICAL(4)                    :: bEqual                         !return true if strings are equal
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------

  bEqual = .false.

  IF (LEN_TRIM(string1) == 0) THEN          ! Test for empty string (LEN = 0)
    IF (LEN_TRIM(string2) == 0) THEN
      bEqual = .true.
    ELSE
      bEqual = .false.
    END IF
  ELSE IF (LEN_TRIM(string2) == 0) THEN
      bEqual = .false.
  ELSE
    IF (ADJUSTL(TRIM(NullifyString(string1))) == ADJUSTL(TRIM(NullifyString(string2)))) bEqual = .true.
  END IF

END FUNCTION StringsEqual

FUNCTION NumAsciiBase(asciiChr, base, error) RESULT(num)
!+
! Description: This function convert a numerical character string expressed in a number base into a REAL(8).
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: asciiChr                      !ascii to be converted
REAL(8),      INTENT(IN)      :: base                           !Base in which number is expressed
LOGICAL(4),   INTENT(OUT)     :: error                          !error
REAL(8)                       :: num                            !converted result
!
!                                Local
!-------------------------------------
INTEGER(4)                    :: Chr                            ! Index through characters in asciiChr
INTEGER(4)                    :: chrEnd
CHARACTER(1)                  :: ascii
INTEGER(4)                    :: digit
REAL(8)                       :: a,b,c
REAL(8)                       :: multiplier
INTEGER(4)                    :: chrStart
LOGICAL(1)                    :: foundDecimal
REAL(8)                       :: number
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!-- Initialize returned values
  num   = 0
  error = .true.

  !-- Find the non-blank ending character
  chr = LEN(asciiChr)
  DO
    IF(chr < 1) RETURN
    IF(asciiChr(chr:chr)/=SPACE.and.asciiChr(chr:chr)/=NULL.and.asciiChr(chr:chr)/=TAB) EXIT
    chr = chr - 1
  END DO
  chrEnd = chr

  !--- Find the non-blank starting character
  chr = 1
  DO
    IF(asciiChr(chr:chr)/=SPACE.and.asciiChr(chr:chr)/=NULL.and.asciiChr(chr:chr)/=TAB) EXIT
    chr = chr + 1
  END DO
  chrStart = chr

  !-- Set the sign multiplier
  multiplier = 1.0d0
  IF(asciiChr(chrStart:chrStart)=='-') THEN
    chrStart = chrStart + 1
    multiplier = -1.0d0
  else IF(asciiChr(chrStart:chrStart).eq.'+') THEN
    chrStart = chrStart + 1
  END IF

  IF(chrStart>chrEnd) RETURN

  a      = base
  b      = 1.0d0
  c      = 1.0d0
  number = 0.0d0
  foundDecimal = .false.
  DO chr = chrStart, chrEnd
    CALL UpperCase(asciiChr(chr:chr),ascii)
    IF('0'<=ascii.and.ascii<='9') THEN
      digit = ICHAR(ascii) - ICHAR('0')
      IF(digit + 1 > base) RETURN
      number = a*number + b*digit
      b   = b*c

    ELSE IF('A'<=ascii.and.ascii<='F'.and.base==16) THEN
      digit = ICHAR(ascii) - ICHAR('A') + 10
      IF(digit + 1 > base) RETURN
      number = a*number + b*digit
      b   = b*c

    ELSE IF(ascii==DECIMALPOINT) THEN
      IF(foundDecimal) RETURN
      a = 1.0d0
      b = 1.0d0/base
      c = 1.0d0/base
      foundDecimal = .true.

    ELSE
      RETURN
    END IF
  END DO
  error = .false.
  num   = number*multiplier
END FUNCTION NumAsciiBase

FUNCTION UpperCaseAscii(AsciiChr)  RESULT(AsciiOutChr)
!+
!
! Description:  This function converts all letters in the input string AsciiChr to uppercase and returns the result in
!               the output string AsciiOutChr.  It is a function shell around the subroutine UpperCase.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: asciiChr                       ! Input string
CHARACTER(256)                :: asciiOutChr                    ! Output string
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CALL UpperCase(AsciiChr, asciiOutChr)
END FUNCTION UpperCaseAscii

FUNCTION LowerCaseAscii(AsciiChr)  RESULT(AsciiOutChr)
!+
!
! Description:  This function converts all letters in the input string AsciiChr to lowercase and returns the result in
!               the output string AsciiOutChr.  It is a function shell around the subroutine LowerCase.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: asciiChr                       ! Input string
CHARACTER(256)                :: asciiOutChr                    ! Output string
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CALL LowerCase(AsciiChr, asciiOutChr)
END FUNCTION LowerCaseAscii

SUBROUTINE UpperCase(asciiChr, asciiOutChr)
!+
!
! Description:  This subroutine converts all letters in the input string AsciiChr to uppercase and returns the result in
!               the output string AsciiOutChr.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: asciiChr                       ! Input string
CHARACTER(*), INTENT(OUT)     :: asciiOutChr                    ! Output string
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: Chr                            ! Index through characters in asciiChr
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  asciiOutChr = asciiChr
  DO chr = 1, MIN(LEN(asciiOutChr),LEN(asciiChr))
    IF('a'<=asciiOutChr(chr:chr) .and. asciiOutChr(chr:chr)<='z') THEN
      asciiOutChr(chr:chr) = CHAR(ICHAR(asciiOutChr(chr:chr)) - aLower + aUpper)
    END IF
  END DO
END SUBROUTINE UpperCase
!
SUBROUTINE LowerCase(asciiChr, asciiOutChr)
!+
!
! Description:  This subroutine converts all letters in the input string AsciiChr to lowercase and returns the result in
!               the output string AsciiOutChr.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)      :: asciiChr                       ! Input string
CHARACTER(*), INTENT(OUT)     :: asciiOutChr                    ! Output string
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: Chr                            ! Index through characters in asciiChr
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  asciiOutChr = asciiChr
  DO chr = 1, MIN(LEN(asciiOutChr),LEN(asciiChr))
    IF('A'<=asciiOutChr(chr:chr) .and. asciiOutChr(chr:chr)<='Z') THEN
      asciiOutChr(chr:chr) = CHAR(ICHAR(asciiOutChr(chr:chr)) - aUpper + aLower)
    END IF
  END DO
END SUBROUTINE LowerCase

FUNCTION AngleLatLongAscii(Ascii,Error) RESULT(Angle)
!+
!
! Description: This function converts a Latitude/Longitude Ascii String to an angle
!              Latitude : Positive North; Negative South
!              Longitude: Positive East ; Negative West
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(32),INTENT(IN)      :: Ascii                          !Input String
LOGICAL(4)   ,INTENT(INOUT)   :: Error                          !Error in Value
REAL(8)                       :: Angle                          !Output Angle (degrees)
!
!                                Local
!-----------------------------+-------
CHARACTER(32)                 :: AsciiChr                       !Local copy of input String
CHARACTER(1)                  :: Ascii_Chr(32)                  !Local copy of input String
EQUIVALENCE                     (AsciiChr,Ascii_Chr)
CHARACTER(32)                 :: AsciiDegrees                   !Degrees String
CHARACTER(32)                 :: AsciiArcMinutes                !Arc Minutes String
CHARACTER(32)                 :: AsciiArcSeconds                !Arc Seconds String
INTEGER(4)  ,PARAMETER        :: sepEnd             = 3         !Number of separators
INTEGER(4)  ,PARAMETER        :: parseMax           = 64        !Maximum number of tokens
CHARACTER(1),PARAMETER        :: a_sep(3)= (/"ø","'",""""/)     !List of valid separator characters
INTEGER(4)                    :: start$chr_parse(parseMax)
INTEGER(4)                    :: end$chr_parse  (parseMax)
INTEGER(4)                    :: parseEnd
INTEGER(4)                    :: sign
LOGICAL(4)                    :: ErrorDegrees
LOGICAL(4)                    :: ErrorArcMinutes
LOGICAL(4)                    :: ErrorArcSeconds
REAL(8)                       :: Degrees
REAL(8)                       :: ArcMinutes
REAL(8)                       :: ArcSeconds
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  AsciiChr = Ascii
  sign = 0
  IF(INDEX(AsciiChr,"W") > 0) THEN
    sign = -1
    Ascii_Chr(INDEX(Asciichr,"W")) = " "
  ELSE IF(INDEX(AsciiChr,"E") > 0) THEN
    sign = +1
    Ascii_Chr(INDEX(Asciichr,"E")) = " "
  ELSE IF(INDEX(AsciiChr,"S") > 0) THEN
    sign = -1
    Ascii_Chr(INDEX(Asciichr,"S")) = " "
  ELSE IF(INDEX(AsciiChr,"N") > 0) THEN
    sign = +1
    Ascii_Chr(INDEX(Asciichr,"N")) = " "
  END IF

  CALL ParseString(AsciiChr,a_sep,sepEnd,parseMax,start$chr_parse,end$chr_parse,parseEnd)
  AsciiDegrees    = "0"
  AsciiArcMinutes = "0"
  AsciiArcSeconds = "0"
  IF(parseEnd>=1) AsciiDegrees    = AsciiChr(start$chr_parse(1):end$chr_parse(1))
  IF(parseEnd>=2) AsciiArcMinutes = AsciiChr(start$chr_parse(2):end$chr_parse(2))
  IF(parseEnd>=3) AsciiArcSeconds = AsciiChr(start$chr_parse(3):end$chr_parse(3))
  Degrees    = NumAscii(AsciiDegrees,ErrorDegrees)
  ArcMinutes = NumAscii(AsciiArcMinutes,ErrorArcMinutes)
  ArcSeconds = NumAscii(AsciiArcSeconds,ErrorArcSeconds)
  Error = .TRUE.
  IF(.not.ErrorDegrees .and. .not.ErrorArcMinutes .and..not.ErrorArcSeconds) THEN
    IF(ArcMinutes >= 0.0d0 .and. ArcMinutes <= 60.0d0) THEN
      Degrees = Degrees + ArcMinutes/60.0d0
      IF(ArcSeconds >= 0.0d0 .and. ArcSeconds <= 60.0d0) THEN
        Degrees = Degrees + ArcSeconds/3600.0d0
        IF(ABS(Degrees) <= 360.0) THEN
          Error = .FALSE.
        END IF
      END IF
    END IF
  END IF
  IF(.not.Error) THEN
    IF(sign < 0) THEN
      Angle = -Degrees
    ELSE
      Angle = +Degrees
    END IF
  ELSE
    Angle = 0
  END IF
  RETURN
END FUNCTION AngleLatLongAscii

SUBROUTINE SortByAttributeR8(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for REAL(8) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
REAL(8)   ,INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CALL SortByAttributeGeneric(AttributeR8_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN

END SUBROUTINE SortByAttributeR8
SUBROUTINE SortByAttributeR4(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for REAL(4) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
REAL(4)   ,INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortByAttributeGeneric(AttributeR4_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortByAttributeR4
SUBROUTINE SortByAttributeI8(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for INTEGER(8) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
INTEGER(8),INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortByAttributeGeneric(AttributeI8_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortByAttributeI8
SUBROUTINE SortByAttributeI4(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for INTEGER(4) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
INTEGER(4),INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortByAttributeGeneric(AttributeI4_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortByAttributeI4
SUBROUTINE SortByAttributeI2(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for INTEGER(2) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
INTEGER(2),INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortByAttributeGeneric(AttributeI2_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortByAttributeI2
SUBROUTINE SortByAttributeAscii(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in alphabetical order. This subroutine is called for CHARACTER Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4)  ,INTENT(IN)       :: lv$ndx                         !Number of devices
CHARACTER(*),INTENT(IN)       :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4)  ,INTENT(OUT)      :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
  CALL SortByAttributeGeneric(AttributeAscii_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortByAttributeAscii

SUBROUTINE SortByAttributeGeneric(AttributeR8_ndx     &
                                 ,AttributeR4_ndx     &
                                 ,AttributeI8_ndx     &
                                 ,AttributeI4_ndx     &
                                 ,AttributeI2_ndx     &
                                 ,AttributeAscii_ndx  &
                                 ,ndxSort_ndx, lv$ndx)
!+
!
! Description: Prepares an array, ndxSort_ndx, that can access devices as ordered by any of the optional arguments:
!
!                 1) AttributeR8_ndx
!                 1) AttributeR4_ndx
!                 1) AttributeI8_ndx
!                 1) AttributeI4_ndx
!                 1) AttributeI2_ndx
!                 1) AttributeAscii_ndx
!
!              This routine encapsulates the shell short that is needed for other Sorting Routines.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  ,INTENT(IN)       :: lv$ndx                         ! Number of devices
REAL(8)     ,OPTIONAL         :: AttributeR8_ndx(lv$ndx)        ! Values to be sorted of type REAL(8)
REAL(4)     ,OPTIONAL         :: AttributeR4_ndx(lv$ndx)        ! Values to be sorted of type REAL(4)
INTEGER(8)  ,OPTIONAL         :: AttributeI8_ndx(lv$ndx)        ! Values to be sorted of type INTEGER(8)
INTEGER(4)  ,OPTIONAL         :: AttributeI4_ndx(lv$ndx)        ! Values to be sorted of type INTEGER(4)
INTEGER(2)  ,OPTIONAL         :: AttributeI2_ndx(lv$ndx)        ! Values to be sorted of type INTEGER(2)
CHARACTER(*),OPTIONAL         :: AttributeAscii_ndx(lv$ndx)     ! Values to be sorted of type CHARACTER
INTEGER(4)  ,INTENT(OUT)      :: ndxSort_ndx(lv$ndx)            ! indices sorted by attribute
!
!                                Local
!-----------------------------+-------
LOGICAL(1)                    :: done                           ! Done flag
INTEGER(4)                    :: m                              ! log base 2 of sub table size to sort
INTEGER(4)                    :: k                              ! counter
INTEGER(4)                    :: itop                           ! loop boundary
INTEGER(4)                    :: ndxsortj                       ! ptr to free lowest entry
INTEGER(4)                    :: ndxsortl                       ! ptr to contender for lowest entry
INTEGER(4)                    :: ndxJ                           ! actual free lowest entry in table
INTEGER(4)                    :: ndxL                           ! actual contender for lowest entry
INTEGER(4)                    :: ndx                            ! index thru devices
INTEGER(4),PARAMETER          :: Undefined = 0                  ! No attribute was passed
INTEGER(4),PARAMETER          :: Ascii     = 1                  ! The type of the passed attribute is CHARACTER
INTEGER(4),PARAMETER          :: I2        = 2                  ! The type of the passed attribute is INTEGER(2)
INTEGER(4),PARAMETER          :: I4        = 3                  ! The type of the passed attribute is INTEGER(4)
INTEGER(4),PARAMETER          :: I8        = 4                  ! The type of the passed attribute is INTEGER(8)
INTEGER(4),PARAMETER          :: R4        = 5                  ! The type of the passed attribute is REAL(4)
INTEGER(4),PARAMETER          :: R8        = 6                  ! The type of the passed attribute is REAL(8)
INTEGER(4)                    :: TypeAttribute                  ! Type of the passed attribute
LOGICAL(4)                    :: LessThan                       ! Attrbute L is less than Attribute J
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! set up initial ordering
  DO ndx = 1, lv$Ndx
    ndxSort_ndx(ndx) = ndx
  END DO
  IF(lv$ndx<2) RETURN

  TypeAttribute = Undefined
  IF(PRESENT(AttributeAscii_ndx)) TypeAttribute = Ascii
  IF(PRESENT(AttributeI2_ndx)   ) TypeAttribute = I2
  IF(PRESENT(AttributeI4_ndx)   ) TypeAttribute = I4
  IF(PRESENT(AttributeI8_ndx)   ) TypeAttribute = I8
  IF(PRESENT(AttributeR4_ndx)   ) TypeAttribute = R4
  IF(PRESENT(AttributeR8_ndx)   ) TypeAttribute = R8
  IF(TypeAttribute == Undefined) RETURN

  m = 2
  DO  WHILE(m <= lv$Ndx)
    m = 2*m
  END DO

  done = .false.

  DO WHILE( .not. done)
    m = (m-1)/2
    IF(m == 0) THEN
      done = .true.
    ELSE
      itop = lv$Ndx - m
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxsortl  = ndxsortj + m
          ndxj      = ndxsort_ndx(ndxsortj)
          ndxl      = ndxsort_ndx(ndxsortl)
          SELECT CASE(TypeAttribute)
          CASE(Ascii) ; LessThan = AttributeAscii_ndx(ndxl) < AttributeAscii_ndx(ndxj)
          CASE(R8)    ; LessThan = AttributeR8_ndx(ndxl)    < AttributeR8_ndx(ndxj)
          CASE(R4)    ; LessThan = AttributeR4_ndx(ndxl)    < AttributeR4_ndx(ndxj)
          CASE(I8)    ; LessThan = AttributeI8_ndx(ndxl)    < AttributeI8_ndx(ndxj)
          CASE(I4)    ; LessThan = AttributeI4_ndx(ndxl)    < AttributeI4_ndx(ndxj)
          CASE(I2)    ; LessThan = AttributeI2_ndx(ndxl)    < AttributeI2_ndx(ndxj)
          END SELECT
          IF(LessThan) THEN
            ndxsort_ndx(ndxsortj) = ndxl
            ndxsort_ndx(ndxsortl) = ndxj
            ndxsortj              = ndxsortj - m
          ELSE
            ndxsortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  !
  RETURN
END SUBROUTINE SortByAttributeGeneric

SUBROUTINE SortByAttributeDouble(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for REAL(8) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: lv$ndx                         ! Number of devices
REAL(8)   ,INTENT(IN)         :: Attribute_ndx(lv$ndx)          ! Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            ! indices sorted by attribute
!
!                                Local
!-----------------------------+-------
LOGICAL(1)                    :: done                           ! Done flag
INTEGER(4)                    :: m                              ! log base 2 of sub table size to sort
INTEGER(4)                    :: k                              ! counter
INTEGER(4)                    :: itop                           ! loop boundary
INTEGER(4)                    :: ndxsortj                       ! ptr to free lowest entry
INTEGER(4)                    :: ndxsortl                       ! ptr to contender for lowest entry
INTEGER(4)                    :: ndxJ                           ! actual free lowest entry in table
INTEGER(4)                    :: ndxL                           ! actual contender for lowest entry
INTEGER(4)                    :: ndx                            ! index thru devices
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !  set up initial ordering
  DO ndx = 1, lv$Ndx
    ndxSort_ndx(ndx) = ndx
  END DO

  m = 2
  DO  WHILE(m <= lv$Ndx)
    m = 2*m
  END DO

  done = .false.

  DO WHILE( .not. done)
    m = (m-1)/2
    IF(m == 0) THEN
      done = .true.
    ELSE
      itop = lv$Ndx - m
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxsortl  = ndxsortj + m
          ndxj      = ndxsort_ndx(ndxsortj)
          ndxl      = ndxsort_ndx(ndxsortl)

          IF(Attribute_ndx(ndxl) < Attribute_ndx(ndxj)) THEN
            ndxsort_ndx(ndxsortj) = ndxl
            ndxsort_ndx(ndxsortl) = ndxj
            ndxsortj              = ndxsortj - m
          ELSE
            ndxsortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  !
  RETURN
END SUBROUTINE SortByAttributeDouble

SUBROUTINE SortByAttributeCharacter(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in alphabetical order. This subroutine is called for CHARACTER Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   ,INTENT(IN)      :: lv$ndx                         ! Number of devices
CHARACTER(*) ,INTENT(IN)      :: Attribute_ndx(lv$ndx)          ! Values to be sorted
INTEGER(4)   ,INTENT(OUT)     :: ndxSort_ndx(lv$ndx)            ! indices sorted by attribute
!
!                                Local
!-----------------------------+-------
LOGICAL(1)                    :: done                           ! Done flag
INTEGER(4)                    :: m                              ! log base 2 of sub table size to sort
INTEGER(4)                    :: k                              ! counter
INTEGER(4)                    :: itop                           ! loop boundary
INTEGER(4)                    :: ndxsortj                       ! ptr to free lowest entry
INTEGER(4)                    :: ndxsortl                       ! ptr to contender for lowest entry
INTEGER(4)                    :: ndxJ                           ! actual free lowest entry in table
INTEGER(4)                    :: ndxL                           ! actual contender for lowest entry
INTEGER(4)                    :: ndx                            ! index thru devices
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !  set up initial ordering
  DO ndx = 1, lv$Ndx
    ndxSort_ndx(ndx) = ndx
  END DO

  m = 2
  DO  WHILE(m <= lv$Ndx)
    m = 2*m
  END DO

  done = .false.

  DO WHILE( .not. done)
    m = (m-1)/2
    IF(m == 0) THEN
      done = .true.
    ELSE
      itop = lv$Ndx - m
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxsortl  = ndxsortj + m
          ndxj      = ndxsort_ndx(ndxsortj)
          ndxl      = ndxsort_ndx(ndxsortl)

          IF(Attribute_ndx(ndxl) < Attribute_ndx(ndxj)) THEN
            ndxsort_ndx(ndxsortj) = ndxl
            ndxsort_ndx(ndxsortl) = ndxj
            ndxsortj              = ndxsortj - m
          ELSE
            ndxsortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  !
  RETURN
END SUBROUTINE SortByAttributeCharacter

SUBROUTINE SortByAttributeByte(Attribute_n_ndx, ndxSort_ndx, nEnd, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in ascending order. This subroutine is called for CHARACTER(1) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)   ,INTENT(IN)      :: lv$ndx                         ! Number of devices
INTEGER(4)   ,INTENT(IN)      :: nEnd                           ! Number of characters in each attribute
CHARACTER(1) ,INTENT(IN)      :: Attribute_n_ndx(nEnd, lv$ndx)  ! Values to be sorted
INTEGER(4)   ,INTENT(OUT)     :: ndxSort_ndx(lv$ndx)            ! indices sorted by attribute
!
!                                Local
!-----------------------------+-------
LOGICAL(1)                    :: done                           ! Done flag
INTEGER(4)                    :: m                              ! log base 2 of sub table size to sort
INTEGER(4)                    :: k                              ! counter
INTEGER(4)                    :: itop                           ! loop boundary
INTEGER(4)                    :: ndxsortj                       ! ptr to free lowest entry
INTEGER(4)                    :: ndxsortl                       ! ptr to contender for lowest entry
INTEGER(4)                    :: ndxJ                           ! actual free lowest entry in table
INTEGER(4)                    :: ndxL                           ! actual contender for lowest entry
INTEGER(4)                    :: ndx                            ! index thru devices
CHARACTER(32)                 :: Attributej                     ! 1st Attribute for comparison
CHARACTER(32)                 :: Attributel                     ! 2nd Attribute for comparison
CHARACTER(1)                  :: Attributej_n(32)               ! 1st Attribute Byte equivalence
CHARACTER(1)                  :: Attributel_n(32)               ! 2nd Attribute Byte equivalence
EQUIVALENCE                     (Attributej_n, Attributej)
EQUIVALENCE                     (Attributel_n, Attributel)
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  !  set up initial ordering
  DO ndx = 1, lv$Ndx
    ndxSort_ndx(ndx) = ndx
  END DO

  m = 2
  DO  WHILE(m <= lv$Ndx)
    m = 2*m
  END DO

  done = .false.

  Attributej = " "
  Attributel = " "
  DO WHILE( .not. done)
    m = (m-1)/2
    IF(m == 0) THEN
      done = .true.
    ELSE
      itop = lv$Ndx - m
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxsortl  = ndxsortj + m
          ndxj      = ndxsort_ndx(ndxsortj)
          ndxl      = ndxsort_ndx(ndxsortl)

          Attributej_n(1:nEnd) = Attribute_n_ndx(:,ndxj)
          Attributel_n(1:nEnd) = Attribute_n_ndx(:,ndxl)
          IF(Attributel < Attributej) THEN
            ndxsort_ndx(ndxsortj) = ndxl
            ndxsort_ndx(ndxsortl) = ndxj
            ndxsortj              = ndxsortj - m
          ELSE
            ndxsortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  !
  RETURN
END SUBROUTINE SortByAttributeByte

SUBROUTINE SortReverseByAttributeR8(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in descending order. This subroutine is called for REAL(8) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
REAL(8)   ,INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CALL SortReverseByAttributeGeneric(AttributeR8_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN

END SUBROUTINE SortReverseByAttributeR8
SUBROUTINE SortReverseByAttributeR4(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in descending order. This subroutine is called for REAL(4) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
REAL(4)   ,INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortReverseByAttributeGeneric(AttributeR4_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortReverseByAttributeR4
SUBROUTINE SortReverseByAttributeI8(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in descending order. This subroutine is called for INTEGER(8) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
INTEGER(8),INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortReverseByAttributeGeneric(AttributeI8_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortReverseByAttributeI8
SUBROUTINE SortReverseByAttributeI4(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in descending order. This subroutine is called for INTEGER(4) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
INTEGER(4),INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortReverseByAttributeGeneric(AttributeI4_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortReverseByAttributeI4
SUBROUTINE SortReverseByAttributeI2(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in descending order. This subroutine is called for INTEGER(2) Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4),INTENT(IN)         :: lv$ndx                         !Number of devices
INTEGER(2),INTENT(IN)         :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4),INTENT(OUT)        :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL SortReverseByAttributeGeneric(AttributeI2_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortReverseByAttributeI2
SUBROUTINE SortReverseByAttributeAscii(Attribute_ndx, ndxSort_ndx, lv$ndx)
!+
!
! Description: This subroutine prepares an array, ndxSort_ndx, that can be used to access the elements of Attribute_ndx
!              in alphabetical order. This subroutine is called for CHARACTER Atrribute_ndx.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
INTEGER(4)  ,INTENT(IN)       :: lv$ndx                         !Number of devices
CHARACTER(*),INTENT(IN)       :: Attribute_ndx(lv$ndx)          !Values to be sorted
INTEGER(4)  ,INTENT(OUT)      :: ndxSort_ndx(lv$ndx)            !indices sorted by attribute
  CALL SortReverseByAttributeGeneric(AttributeAscii_ndx = Attribute_ndx, ndxSort_ndx = ndxSort_ndx, lv$ndx = lv$ndx)
  RETURN
END SUBROUTINE SortReverseByAttributeAscii

SUBROUTINE SortReverseByAttributeGeneric(AttributeR8_ndx     &
                                 ,AttributeR4_ndx     &
                                 ,AttributeI8_ndx     &
                                 ,AttributeI4_ndx     &
                                 ,AttributeI2_ndx     &
                                 ,AttributeAscii_ndx  &
                                 ,ndxSort_ndx, lv$ndx)
!+
!
! Description: Prepares an array, ndxSort_ndx, that can access devices as ordered by any of the optional arguments:
!
!                 1) AttributeR8_ndx
!                 1) AttributeR4_ndx
!                 1) AttributeI8_ndx
!                 1) AttributeI4_ndx
!                 1) AttributeI2_ndx
!                 1) AttributeAscii_ndx
!
!              This routine encapsulates the shell short that is needed for other Sorting Routines.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 25-FEB-2025     | MA    | SCR 150: Added to help process the Tz Info files
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)  ,INTENT(IN)       :: lv$ndx                         ! Number of devices
REAL(8)     ,OPTIONAL         :: AttributeR8_ndx(lv$ndx)        ! Values to be sorted of type REAL(8)
REAL(4)     ,OPTIONAL         :: AttributeR4_ndx(lv$ndx)        ! Values to be sorted of type REAL(4)
INTEGER(8)  ,OPTIONAL         :: AttributeI8_ndx(lv$ndx)        ! Values to be sorted of type INTEGER(8)
INTEGER(4)  ,OPTIONAL         :: AttributeI4_ndx(lv$ndx)        ! Values to be sorted of type INTEGER(4)
INTEGER(2)  ,OPTIONAL         :: AttributeI2_ndx(lv$ndx)        ! Values to be sorted of type INTEGER(2)
CHARACTER(*),OPTIONAL         :: AttributeAscii_ndx(lv$ndx)     ! Values to be sorted of type CHARACTER
INTEGER(4)  ,INTENT(OUT)      :: ndxSort_ndx(lv$ndx)            ! indices sorted by attribute
!
!                                Local
!-----------------------------+-------
LOGICAL(1)                    :: done                           ! Done flag
INTEGER(4)                    :: m                              ! log base 2 of sub table size to sort
INTEGER(4)                    :: k                              ! counter
INTEGER(4)                    :: itop                           ! loop boundary
INTEGER(4)                    :: ndxsortj                       ! ptr to free lowest entry
INTEGER(4)                    :: ndxsortl                       ! ptr to contender for lowest entry
INTEGER(4)                    :: ndxJ                           ! actual free lowest entry in table
INTEGER(4)                    :: ndxL                           ! actual contender for lowest entry
INTEGER(4)                    :: ndx                            ! index thru devices
INTEGER(4),PARAMETER          :: Undefined = 0                  ! No attribute was passed
INTEGER(4),PARAMETER          :: Ascii     = 1                  ! The type of the passed attribute is CHARACTER
INTEGER(4),PARAMETER          :: I2        = 2                  ! The type of the passed attribute is INTEGER(2)
INTEGER(4),PARAMETER          :: I4        = 3                  ! The type of the passed attribute is INTEGER(4)
INTEGER(4),PARAMETER          :: I8        = 4                  ! The type of the passed attribute is INTEGER(8)
INTEGER(4),PARAMETER          :: R4        = 5                  ! The type of the passed attribute is REAL(4)
INTEGER(4),PARAMETER          :: R8        = 6                  ! The type of the passed attribute is REAL(8)
INTEGER(4)                    :: TypeAttribute                  ! Type of the passed attribute
LOGICAL(4)                    :: GreaterThan                       ! Attrbute L is greater than Attribute J
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! set up initial ordering
  DO ndx = 1, lv$Ndx
    ndxSort_ndx(ndx) = ndx
  END DO
  IF(lv$ndx<2) RETURN

  TypeAttribute = Undefined
  IF(PRESENT(AttributeAscii_ndx)) TypeAttribute = Ascii
  IF(PRESENT(AttributeI2_ndx)   ) TypeAttribute = I2
  IF(PRESENT(AttributeI4_ndx)   ) TypeAttribute = I4
  IF(PRESENT(AttributeI8_ndx)   ) TypeAttribute = I8
  IF(PRESENT(AttributeR4_ndx)   ) TypeAttribute = R4
  IF(PRESENT(AttributeR8_ndx)   ) TypeAttribute = R8
  IF(TypeAttribute == Undefined) RETURN

  m = 2
  DO  WHILE(m <= lv$Ndx)
    m = 2*m
  END DO

  done = .false.

  DO WHILE( .not. done)
    m = (m-1)/2
    IF(m == 0) THEN
      done = .true.
    ELSE
      itop = lv$Ndx - m
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxsortl  = ndxsortj + m
          ndxj      = ndxsort_ndx(ndxsortj)
          ndxl      = ndxsort_ndx(ndxsortl)
          SELECT CASE(TypeAttribute)
          CASE(Ascii) ; GreaterThan = AttributeAscii_ndx(ndxl) > AttributeAscii_ndx(ndxj)
          CASE(R8)    ; GreaterThan = AttributeR8_ndx(ndxl)    > AttributeR8_ndx(ndxj)
          CASE(R4)    ; GreaterThan = AttributeR4_ndx(ndxl)    > AttributeR4_ndx(ndxj)
          CASE(I8)    ; GreaterThan = AttributeI8_ndx(ndxl)    > AttributeI8_ndx(ndxj)
          CASE(I4)    ; GreaterThan = AttributeI4_ndx(ndxl)    > AttributeI4_ndx(ndxj)
          CASE(I2)    ; GreaterThan = AttributeI2_ndx(ndxl)    > AttributeI2_ndx(ndxj)
          END SELECT
          IF(GreaterThan) THEN
            ndxsort_ndx(ndxsortj) = ndxl
            ndxsort_ndx(ndxsortl) = ndxj
            ndxsortj              = ndxsortj - m
          ELSE
            ndxsortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  !
  RETURN
END SUBROUTINE SortReverseByAttributeGeneric

FUNCTION CleanString(AsciiChr) RESULT(CleanedChr)
!+
!
! Description: This function replaces all control characters from the input string with SPACE's (z'20') and returns the
!              result in the output CleanedChr.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)       :: AsciiChr                       !input char string
CHARACTER(512)                :: CleanedChr                     !output char string
!
!                                Local
!-----------------------------+-------
CHARACTER(512)                :: CleanChr                       !local copy of "cleaned" string
INTEGER(4)                    :: chr                            !string chr index
INTEGER(4)                    :: chrS                           !squish chr index
INTEGER(4),PARAMETER          :: iSPACE = 32                    !Space Character " "
INTEGER(4),PARAMETER          :: iESCAPE = 126                  !Escape Character "~"
INTEGER(4)                    :: iAscii_w                       !Word Buffer
CHARACTER(1)                  :: Ascii_chr(4)                   !Character buffer
INTEGER(4)                    :: iAscii_hw(2)                   !Halfword Buffer
INTEGER(4)                    :: iAscii_b(4)                    !Byte buffer
EQUIVALENCE                     (iAscii_w,iAscii_hw,iAscii_b,Ascii_chr)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CleanChr = SPACE
  iAscii_w = 0

  chrS     = 0
  DO chr = 1, LEN_TRIM(AsciiChr)
    IF(AsciiChr(chr:chr)==TAB) THEN
      ChrS = ChrS + 1
      CleanChr(ChrS:ChrS) = SPACE
    ELSE
      Ascii_chr(1) = AsciiChr(Chr:Chr)
      IF(WithinClosedSet(iSPACE, iAscii_hw(1), iESCAPE)) THEN
        chrS = chrS + 1
        CleanChr(chrS:chrS) = AsciiChr(chr:chr)
      ElSE
        chrS = chrS + 1
        CleanChr(ChrS:ChrS) = SPACE
      END IF
    END IF
    IF(chrS==256) EXIT
  END DO

  CleanedChr = CleanChr
  RETURN
END FUNCTION CleanString

FUNCTION RemoveMultipleSpaces(AsciiChr) RESULT(SingleSpacesChr)
!+
!
! Description: This function replaces all multiple spaces in an input string with single spaces
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)       :: AsciiChr                       !input char string
CHARACTER(256)                :: SingleSpacesChr                !output char string
!
!                                Local
!-----------------------------+-------
CHARACTER(256)                :: CleanChr                       !local copy of "cleaned" string
INTEGER(4)                    :: chr                            !string chr index
INTEGER(4)                    :: chrS                           !squish chr index
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  CleanChr = SPACE

  chrS     = 0
  DO chr = 1, LEN_TRIM(AsciiChr)
    IF(AsciiChr(chr:chr) /= SPACE .or. chr == 1) THEN
      chrS = chrS + 1
      CleanChr(chrS:chrS) = AsciiChr(chr:chr)
    ElSE
      IF(CleanChr(chrS:chrS) == SPACE) CYCLE
      chrS = chrS + 1
      CleanChr(chrS:chrS) = AsciiChr(chr:chr)
    END IF
  END DO
  SingleSpacesChr = CleanChr
  RETURN
END FUNCTION RemoveMultipleSpaces

FUNCTION Squish(AsciiChr)
!+
!
! Description: This function removes all white space from a character string.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)       :: AsciiChr                       !input character string
CHARACTER(128)                :: Squish                         !output character string
!
!                                Local
!-----------------------------+-------
CHARACTER(128)                :: SquishChr                      !local copy of "squished" string
INTEGER(4)                    :: chr                            !string chr index
INTEGER(4)                    :: chrS                           !squish chr index
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  chrS = 0
  SquishChr = SPACE
  DO chr = 1, LEN_TRIM(AsciiChr)
    IF(AsciiChr(chr:chr)==NULL .or. AsciiChr(chr:chr)==SPACE .or. AsciiChr(chr:chr)==TAB) CYCLE
    chrS = chrS + 1
    SquishChr(chrS:chrS) = AsciiChr(chr:chr)
    IF(chrS==128) EXIT
  END DO

  Squish = SquishChr
  RETURN
END FUNCTION Squish

SUBROUTINE ReplaceCharInString(CharChr, Char, CharNew)
!+
!
! Description: This subroutine replaces all occurences of the character Char with CharNew in the string CharChr.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN OUT)   :: CharChr                        ! input char string
CHARACTER(1),INTENT(IN)       :: Char                           ! Character to be replaced
CHARACTER(1),INTENT(IN)       :: CharNew                        ! Character to replace
!
!                                Local
!-----------------------------+-------
INTEGER(4)                    :: ChrEnd                         ! Trimmed length of CharChr
INTEGER(4)                    :: Chr                            ! Index through the individual characters of CharChr
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
! Get the trimmed length of the input string, but return a blank string if the length is zero
  ChrEnd = LEN_TRIM(CharChr)         ; IF(ChrEnd==0) RETURN
!
! Replace Char with CharNew
  DO CONCURRENT (Chr=1:ChrEnd)
    IF(CharChr(Chr:Chr) == Char) CharChr(Chr:Chr) = CharNew
  END DO
END SUBROUTINE ReplaceCharInString

FUNCTION AngleAtEncoder(Encoder, Gain, Offset) RESULT(Angle)
!+
!
! Description: This function converts encoder counts to an angle for a linear encoder
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: Encoder                         ! Encoder (counts)
REAL(8),   INTENT(IN)        :: Gain                            ! Conversion Gain (counts/degree)
REAL(8),   INTENT(IN)        :: Offset                          ! Conversion Offset (counts)
REAL(8)                      :: Angle                           ! Angle to convert to counts (deg)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
 Angle = (DBLE(Encoder)-Offset)/Gain
 RETURN
END FUNCTION AngleAtEncoder

FUNCTION EncoderAtAngle(Angle, Gain, Offset) RESULT(Encoder)
!+
!
! Description: This function convert an angle to encoder counts for a linear encoder
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: Angle                           ! Angle to convert to counts (deg)
REAL(8), INTENT(IN)          :: Gain                            ! Conversion Gain (counts/degree)
REAL(8), INTENT(IN)          :: Offset                          ! Conversion Offset (counts)
INTEGER(4)                   :: Encoder                         ! Encoder (counts)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
 Encoder = NINT(Angle*Gain + Offset)
RETURN
END FUNCTION EncoderAtAngle

FUNCTION dAngle180(Angle1, Angle2) RESULT(dAngle)
!+
!
! Description: This function calculates the delta angle detween two angles and ensures that the difference is in the
!              the left half open set (-180, +180}.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: Angle1                          ! 1st Angle (deg)
REAL(8), INTENT(IN)          :: Angle2                          ! 2nd Angle (deg)
REAL(8)                      :: dAngle                          ! Difference between the two angles
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  dAngle = MOD(Angle1 - Angle2, 360.0d0)
  IF(dAngle <= -180d0) THEN
    dAngle = dAngle + 360.0d0
  ELSE IF(dAngle > 180.0d0) THEN
    dAngle = dAngle - 360.0d0
  END IF

  RETURN
END FUNCTION dAngle180

FUNCTION Angle360(Angle)
!+
!
! Description: This function converts an angle to its equivalent in the half open set [0,360)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: Angle                           ! Angle (deg)
REAL(8)                      :: Angle360                        ! Angle in [0,360)
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Angle360 = MOD(Angle, 360.0d0)
  IF(Angle360 < 0.0d0) THEN
    Angle360 = Angle + 360.0d0
  END IF
  !
  RETURN
END FUNCTION Angle360


FUNCTION AngleLim(AngleMin, Angle, AngleMax) RESULT (AngleLimited)
!+
!
! Description: This function converts an angle to its equivalent in the half open set [AngleMin, AngleMax)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: Angle                           ! Angle (deg)
REAL(8), INTENT(IN)          :: AngleMin                        ! Min Angle (deg)
REAL(8), INTENT(IN)          :: AngleMax                        ! Max Angle (deg)
REAL(8)                      :: AngleLimited                    ! AngleLimited in the half open set [AngleMin, AngleMax)
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  AngleLimited = Angle

  DO WHILE (AngleLimited <  AngleMin ) ; AngleLimited = AngleLimited + 360.0d0 ; END DO
  DO WHILE (AngleLimited >= AngleMax ) ; AngleLimited = AngleLimited - 360.0d0 ; END DO
!
RETURN
END FUNCTION AngleLim

FUNCTION AngleInRange(AngleLo, AngleAt, AngleHi) RESULT(Angle)
!+
!
! Description: This function convert an angle to its equivalent in the closed set [AngleLo, AngleHi]
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: AngleAt                         ! Heliostat angle            (deg)
REAL(8)   , INTENT(IN)       :: AngleHi                         ! Heliostat angle High Limit (deg)
REAL(8)   , INTENT(IN)       :: AngleLo                         ! Heliostat angle Low Limit  (deg)
REAL(8)                      :: Angle                           ! Limited angle              (deg)
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: dAngleHi                        ! Angle hi limit violation  (deg)
REAL(8)                      :: dAngleLo                        ! Angle lo limit violation  (deg)
REAL(8)                      :: Rotations                       ! Rotations required
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(AngleAt < AngleLo) THEN
    !
    ! Below the low limit, rotate in a positive sense to get at or above the low limit
    dAngleLo  = AngleLo - AngleAt
    Rotations = AINT(dAngleLo/360d0) + 1d0
    Angle     = AngleAt + 360d0*Rotations
    Angle     = MinMax(AngleLo, AngleAt - 360d0*Rotations, AngleHi)

  ELSE IF(AngleAt > AngleHi) THEN
    !
    ! Above the high limit, rotate in a negative sense to get below or at the high limit
    dAngleHi  = AngleAt - AngleHi
    Rotations = AINT(dAngleHi/360d0) + 1d0
    Angle     = MinMax(AngleLo, AngleAt - 360d0*Rotations, AngleHi)

  ELSE
    !
    !within limit, return the angle
    Angle = AngleAt

  END IF

  RETURN
END FUNCTION AngleInRange
!
FUNCTION Angle180(Angle)
!+
!
! Description: This function converts an angle to its equivalent in the half open set (-180,180]
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: Angle                           ! Angle (deg)
REAL(8)                      :: Angle180                        ! Angle in (-180,180]
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Angle180 = MOD(Angle, 360d0)
  IF(Angle180 > 180d0) THEN
    Angle180 = Angle - 360d0
  ELSE IF(Angle180 < -180d0) THEN
    Angle180 = Angle + 360d0
  END IF
  !
  RETURN
END FUNCTION Angle180

SUBROUTINE ShiftValueAverage(ValueAverage, Value_dat, datEnd)
!+
!
! Description: This subroutine shifts the values of an array to have a specified average value.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: ValueAverage                    ! Desired average value
INTEGER(4), INTENT(IN)       :: datEnd                          ! Size of the input array
REAL(8)   , INTENT(IN OUT)   :: Value_dat(datEnd)               ! Array to shift
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Value_dat = ValueAverage + (Value_dat - SUM(Value_dat)/DBLE(datEnd))

END SUBROUTINE ShiftValueAverage

SUBROUTINE Transfer2x1VectorToScalars(R_n, X, Y)
!+
!
! Description: This subroutine transfers a 2x1 vector to X and Y components.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: R_n(2)                          ! 2x1 Vector
REAL(8), INTENT(OUT)         :: X                               ! X component of R_n
REAL(8), INTENT(OUT)         :: Y                               ! Y component of R_n
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  X = R_n(1)
  Y = R_n(2)
  RETURN
END SUBROUTINE Transfer2x1VectorToScalars
!
SUBROUTINE Transfer3x1VectorToScalars(R_n, X, Y, Z)
!+
!
! Description: This subroutine transfers a 3x1 vector to X, Y and Z components
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: R_n(3)                          ! 3x1 Vector
REAL(8), INTENT(OUT)         :: X                               ! X component of R_n
REAL(8), INTENT(OUT)         :: Y                               ! Y component of R_n
REAL(8), INTENT(OUT)         :: Z                               ! Z component of R_n
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  X = R_n(1)
  Y = R_n(2)
  Z = R_n(3)
  RETURN
END SUBROUTINE Transfer3x1VectorToScalars

SUBROUTINE SolveQuartic41Polynomial(c, d, Temp)
!+
!
! Description: This subroutine solves a special quartic equation for Temperature:
!
!                 Temperature^4 + c Temperature + d = 0
!
!              where c and d are constants and Temperature is on an absolute scale (Kelvin or Rankine).
!              It is used to calculate temperature for mixed radiation and conduction thermal problems.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: c                               ! Linear coefficient
REAL(8), INTENT(IN)          :: d                               ! quartic coefficient
REAL(8), INTENT(OUT)         :: Temp                            ! output temperature (Kelvin or Rankine)
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: t0
REAL(8)                      :: t1
REAL(8)                      :: t
REAL(8)                      :: v
REAL(8)                      :: W
REAL(8)                      :: v2
REAL(8)                      :: w2
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  t0 = 12d0*(-ABS(d))
  t1 = 27d0*c**2
  t  = ( (t1 + DSQRT(t1**2 - 4d0*t0**3) )/2d0 )**(1d0/3d0)
  v  = (t+t0/t)/3d0
  w  = 8d0*ABS(c)/(4d0*DSQRT(v))

  Temp = 0d0
  DO
    IF(v < 0) EXIT
    IF(w < v) EXIT

    v2 = DSQRT(v)/2d0
    w2 = DSQRT(w-v)/2d0

    Temp = w2-v2
    EXIT
  END DO
  RETURN
END SUBROUTINE SolveQuartic41Polynomial

SUBROUTINE LDLFactorBandOneMatrix(Gii_n, Gij_n, D_n, L_n, nEnd)
!+
!
! Description: This subroutine calculates the LDL' factors of a special symmetric banded matrix with only 2 off-diagonal
!              entries per row as follows:
!
!                        ...
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: nEnd                            ! Rank of matrix
REAL(8), INTENT(IN)          :: Gii_n(nEnd)                     ! Diagonals of G matrix
REAL(8), INTENT(IN)          :: Gij_n(nEnd)                     ! Off-diagonals of G matrix
REAL(8), INTENT(OUT)         :: D_n  (nEnd)                     ! Diagonal factor
REAL(8), INTENT(OUT)         :: L_n  (nEnd)                     ! Lower triangular factor
!
!                               Local variables
!----------------------------+---------------
INTEGER(4)                   :: n                               ! Index through rows
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  D_n(1) = Gii_n(1)
  L_n(1) = Gij_n(1)/D_n(1)
  DO n = 2, nEnd
    D_n(n) = Gii_n(n) - D_n(n-1)*L_n(n-1)**2
    L_n(n) = Gij_n(n)/D_n(n)
  END DO
  RETURN
END SUBROUTINE LDLFactorBandOneMatrix

SUBROUTINE SolveBandOneMatrixEquation(x_n, b_n, D_n, L_n, nEnd)
!+
!
! Description: This subroutine solves the equation [L][D][L]' [x] = [b]  where the [L] matrix has only one-offdiagonal term
!              term just below each diagonal.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: nEnd                            ! Rank of matrix
REAL(8), INTENT(OUT)         :: x_n  (nEnd)                     ! Solution
REAL(8), INTENT(IN)          :: b_n  (nEnd)                     ! Right side vector
REAL(8), INTENT(IN)          :: D_n  (nEnd)                     ! Diagonal factor
REAL(8), INTENT(IN)          :: L_n  (nEnd)                     ! Lower triangular factor
!
!                               Local variables
!----------------------------+---------------
INTEGER(4)                   :: n                               ! Index through rows
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
! Forward substitution
x_n = b_n
DO n = 2, nEnd
  x_n(n) = x_n(n) - L_n(n-1)*x_n(n-1)
END DO
!
! Diagonal inverse multiplication
x_n = x_n / D_n
!
! Back substitution
DO n = nEnd-1, 1, -1
  x_n(n) = x_n(n) - L_n(n)*x_n(n+1)
END DO
RETURN

END SUBROUTINE SolveBandOneMatrixEquation

FUNCTION IntersectionClosedSets_R8(VminSet1, VmaxSet1, VminSet2, VmaxSet2) RESULT(Intersection)
!+
!
! Description: This function returns TRUE if two closed sets in the Real(8) domain have an non-empty intersection.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: VminSet1                        ! Minimum value for Set 1
REAL(8)   , INTENT(IN)       :: VmaxSet1                        ! Maximum value for Set 1
REAL(8)   , INTENT(IN)       :: VminSet2                        ! Minimum value for Set 2
REAL(8)   , INTENT(IN)       :: VmaxSet2                        ! Maximum value for Set 2
LOGICAL(4)                   :: Intersection                    ! The two sets of a non-empty intersection
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  Intersection = WithinClosedSet(VminSet1, VminSet2, VmaxSet1) .or. &
                 WithinClosedSet(VminSet1, VmaxSet2, VmaxSet1) .or. &
                 WithinClosedSet(VminSet2, VminSet1, VmaxSet2) .or. &
                 WithinClosedSet(VminSet2, VmaxSet1, VmaxSet2)
END FUNCTION IntersectionClosedSets_R8
!
FUNCTION IntersectionClosedSets_R4(VminSet1, VmaxSet1, VminSet2, VmaxSet2) RESULT(Intersection)
!+
!
! Description: This function returns TRUE if two closed sets in the Real(4) domain have an intersection.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4)   , INTENT(IN)       :: VminSet1                        ! Minimum value for Set 1
REAL(4)   , INTENT(IN)       :: VmaxSet1                        ! Maximum value for Set 1
REAL(4)   , INTENT(IN)       :: VminSet2                        ! Minimum value for Set 2
REAL(4)   , INTENT(IN)       :: VmaxSet2                        ! Maximum value for Set 2
LOGICAL(4)                   :: Intersection                    ! The two sets of a non-empty intersection
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  Intersection = WithinClosedSet(VminSet1, VminSet2, VmaxSet1) .or. &
                 WithinClosedSet(VminSet1, VmaxSet2, VmaxSet1) .or. &
                 WithinClosedSet(VminSet2, VminSet1, VmaxSet2) .or. &
                 WithinClosedSet(VminSet2, VmaxSet1, VmaxSet2)
END FUNCTION IntersectionClosedSets_R4
!
FUNCTION IntersectionClosedSets_I8(IminSet1, ImaxSet1, IminSet2, ImaxSet2) RESULT(Intersection)
!+
!
! Description: This function returns TRUE if two closed sets in the INTEGER(8) domain have an intersection.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8), INTENT(IN)       :: IminSet1                        ! Minimum value for Set 1
INTEGER(8), INTENT(IN)       :: ImaxSet1                        ! Maximum value for Set 1
INTEGER(8), INTENT(IN)       :: IminSet2                        ! Minimum value for Set 2
INTEGER(8), INTENT(IN)       :: ImaxSet2                        ! Maximum value for Set 2
LOGICAL(4)                   :: Intersection                    ! The two sets of a non-empty intersection
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  Intersection = WithinClosedSet(IminSet1, IminSet2, ImaxSet1) .or. &
                 WithinClosedSet(IminSet1, ImaxSet2, ImaxSet1) .or. &
                 WithinClosedSet(IminSet2, IminSet1, ImaxSet2) .or. &
                 WithinClosedSet(IminSet2, ImaxSet1, ImaxSet2)
END FUNCTION IntersectionClosedSets_I8
!
FUNCTION IntersectionClosedSets_I4(IminSet1, ImaxSet1, IminSet2, ImaxSet2) RESULT(Intersection)
!+
!
! Description: This function returns TRUE if two closed sets in the INTEGER(4) domain have an intersection.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: IminSet1                        ! Minimum value for Set 1
INTEGER(4), INTENT(IN)       :: ImaxSet1                        ! Maximum value for Set 1
INTEGER(4), INTENT(IN)       :: IminSet2                        ! Minimum value for Set 2
INTEGER(4), INTENT(IN)       :: ImaxSet2                        ! Maximum value for Set 2
LOGICAL(4)                   :: Intersection                    ! The two sets of a non-empty intersection
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  Intersection = WithinClosedSet(IminSet1, IminSet2, ImaxSet1) .or. &
                 WithinClosedSet(IminSet1, ImaxSet2, ImaxSet1) .or. &
                 WithinClosedSet(IminSet2, IminSet1, ImaxSet2) .or. &
                 WithinClosedSet(IminSet2, ImaxSet1, ImaxSet2)
END FUNCTION IntersectionClosedSets_I4
!
FUNCTION IntersectionClosedSets_I2(IminSet1, ImaxSet1, IminSet2, ImaxSet2) RESULT(Intersection)
!+
!
! Description: This function returns TRUE if two closed sets in the INTEGER(2) domain have an intersection.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)       :: IminSet1                        ! Minimum value for Set 1
INTEGER(2), INTENT(IN)       :: ImaxSet1                        ! Maximum value for Set 1
INTEGER(2), INTENT(IN)       :: IminSet2                        ! Minimum value for Set 2
INTEGER(2), INTENT(IN)       :: ImaxSet2                        ! Maximum value for Set 2
LOGICAL(4)                   :: Intersection                    ! The two sets of a non-empty intersection
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  Intersection = WithinClosedSet(IminSet1, IminSet2, ImaxSet1) .or. &
                 WithinClosedSet(IminSet1, ImaxSet2, ImaxSet1) .or. &
                 WithinClosedSet(IminSet2, IminSet1, ImaxSet2) .or. &
                 WithinClosedSet(IminSet2, ImaxSet1, ImaxSet2)
END FUNCTION IntersectionClosedSets_I2

FUNCTION WithinClosedSet_R8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a closed set in the Real(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(8)   , INTENT(IN)       :: V                               ! Value
REAL(8)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V <= Vmax
END FUNCTION WithinClosedSet_R8

FUNCTION WithinClosedSet_R4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a closed set in the Real(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(4)   , INTENT(IN)       :: V                               ! Value
REAL(4)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V <= Vmax
END FUNCTION WithinClosedSet_R4

FUNCTION WithinClosedSet_I8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a closed set in the Integer(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(8), INTENT(IN)       :: V                               ! Value
INTEGER(8)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V <= Vmax
END FUNCTION WithinClosedSet_I8

FUNCTION WithinClosedSet_I4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a closed set in the Integer(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(4), INTENT(IN)       :: V                               ! Value
INTEGER(4)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V <= Vmax
END FUNCTION WithinClosedSet_I4

FUNCTION WithinClosedSet_I2(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a closed set in the Integer(2) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(2), INTENT(IN)       :: V                               ! Value
INTEGER(2)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V <= Vmax
END FUNCTION WithinClosedSet_I2

FUNCTION WithinOpenSet_R8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of an open set in the Real(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(8)   , INTENT(IN)       :: V                               ! Value
REAL(8)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V < Vmax
END FUNCTION WithinOpenSet_R8

FUNCTION WithinOpenSet_R4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of an open set in the Real(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(4)   , INTENT(IN)       :: V                               ! Value
REAL(4)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V < Vmax
END FUNCTION WithinOpenSet_R4

FUNCTION WithinOpenSet_I8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of an open set in the Integer(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(8), INTENT(IN)       :: V                               ! Value
INTEGER(8)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V < Vmax
END FUNCTION WithinOpenSet_I8

FUNCTION WithinOpenSet_I4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of an open set in the Integer(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(4), INTENT(IN)       :: V                               ! Value
INTEGER(4)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V < Vmax
END FUNCTION WithinOpenSet_I4

FUNCTION WithinOpenSet_I2(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of an open set in the Integer(2) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(2), INTENT(IN)       :: V                               ! Value
INTEGER(2)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V < Vmax
END FUNCTION WithinOpenSet_I2

FUNCTION WithinLeftHalfOpenSet_R8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a left half open set in the Real(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(8)   , INTENT(IN)       :: V                               ! Value
REAL(8)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V <= Vmax
END FUNCTION WithinLeftHalfOpenSet_R8

FUNCTION WithinLeftHalfOpenSet_R4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a left half open set in the Real(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(4)   , INTENT(IN)       :: V                               ! Value
REAL(4)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V <= Vmax
END FUNCTION WithinLeftHalfOpenSet_R4

FUNCTION WithinLeftHalfOpenSet_I8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a left half open set in the Integer(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(8), INTENT(IN)       :: V                               ! Value
INTEGER(8)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V <= Vmax
END FUNCTION WithinLeftHalfOpenSet_I8

FUNCTION WithinLeftHalfOpenSet_I4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a left half open set in the Integer(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(4), INTENT(IN)       :: V                               ! Value
INTEGER(4)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V <= Vmax
END FUNCTION WithinLeftHalfOpenSet_I4

FUNCTION WithinLeftHalfOpenSet_I2(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a left half open set in the Integer(2) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(2), INTENT(IN)       :: V                               ! Value
INTEGER(2)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin < V .and. V <= Vmax
END FUNCTION WithinLeftHalfOpenSet_I2

FUNCTION WithinRightHalfOpenSet_R8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a right half open set in the Real(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(8)   , INTENT(IN)       :: V                               ! Value
REAL(8)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V < Vmax
END FUNCTION WithinRightHalfOpenSet_R8

FUNCTION WithinRightHalfOpenSet_R4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a right half open set in the Real(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4)   , INTENT(IN)       :: Vmin                            ! Minimum value
REAL(4)   , INTENT(IN)       :: V                               ! Value
REAL(4)                      :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V < Vmax
END FUNCTION WithinRightHalfOpenSet_R4

FUNCTION WithinRightHalfOpenSet_I8(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a right half open set in the Integer(8) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(8), INTENT(IN)       :: V                               ! Value
INTEGER(8)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V < Vmax
END FUNCTION WithinRightHalfOpenSet_I8

FUNCTION WithinRightHalfOpenSet_I4(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a right half open set in the Integer(4) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(4), INTENT(IN)       :: V                               ! Value
INTEGER(4)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V < Vmax
END FUNCTION WithinRightHalfOpenSet_I4

FUNCTION WithinRightHalfOpenSet_I2(Vmin, V, Vmax) RESULT(Within)
!+
!
! Description: This function returns TRUE if a value is a member of a right half open set in the Integer(2) domain.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE
!                             Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)       :: Vmin                            ! Minimum value
INTEGER(2), INTENT(IN)       :: V                               ! Value
INTEGER(2)                   :: Vmax                            ! Maximum value
LOGICAL(4)                   :: Within
!
!                             Local
!----------------------------+------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Within = Vmin <= V .and. V < Vmax
END FUNCTION WithinRightHalfOpenSet_I2

ELEMENTAL FUNCTION MinMax_R8(MinLimit,Val,MaxLimit) RESULT(ValLimited)
!+
!
! Description: This function limits a values to lie in the closed set [Min,Max] in the Real(8) domain.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: MinLimit                        ! Lower limit
REAL(8), INTENT(IN)          :: MaxLimit                        ! Upper limit
REAL(8), INTENT(IN)          :: Val                             ! Input Value
REAL(8)                      :: ValLimited                      ! Limited value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValLimited = MIN(MaxLimit,MAX(Val,MinLimit))
RETURN
END FUNCTION MinMax_R8
ELEMENTAL FUNCTION MinMax_R4(MinLimit,Val,MaxLimit) RESULT(ValLimited)
!+
!
! Description: This function limits a values to lie in the closed set [Min,Max] in the Real(4) domain.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(4), INTENT(IN)          :: MinLimit                        ! Lower limit
REAL(4), INTENT(IN)          :: MaxLimit                        ! Upper limit
REAL(4), INTENT(IN)          :: Val                             ! Input Value
REAL(4)                      :: ValLimited                      ! Limited value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValLimited = MIN(MaxLimit,MAX(Val,MinLimit))
RETURN
END FUNCTION MinMax_R4
ELEMENTAL FUNCTION MinMax_I8(MinLimit,Val,MaxLimit) RESULT(ValLimited)
!+
!
! Description: This function limits a values to lie in the closed set [Min,Max] in the Integer(8) domain.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8), INTENT(IN)       :: MinLimit                        ! Lower limit
INTEGER(8), INTENT(IN)       :: MaxLimit                        ! Upper limit
INTEGER(8), INTENT(IN)       :: Val                             ! Input Value
INTEGER(8)                   :: ValLimited                      ! Limited value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValLimited = MIN(MaxLimit,MAX(Val,MinLimit))
RETURN
END FUNCTION MinMax_I8
ELEMENTAL FUNCTION MinMax_I4(MinLimit,Val,MaxLimit) RESULT(ValLimited)
!+
!
! Description: This function limits a values to lie in the closed set [Min,Max] in the Integer(4) domain.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: MinLimit                        ! Lower limit
INTEGER(4), INTENT(IN)       :: MaxLimit                        ! Upper limit
INTEGER(4), INTENT(IN)       :: Val                             ! Input Value
INTEGER(4)                   :: ValLimited                      ! Limited value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValLimited = MIN(MaxLimit,MAX(Val,MinLimit))
RETURN
END FUNCTION MinMax_I4
ELEMENTAL FUNCTION MinMax_I2(MinLimit,Val,MaxLimit) RESULT(ValLimited)
!+
!
! Description: This function limits a values to lie in the closed set [Min,Max] in the Integer(2) domain.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2), INTENT(IN)       :: MinLimit                        ! Lower limit
INTEGER(2), INTENT(IN)       :: MaxLimit                        ! Upper limit
INTEGER(2), INTENT(IN)       :: Val                             ! Input Value
INTEGER(2)                   :: ValLimited                      ! Limited value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValLimited = MIN(MaxLimit,MAX(Val,MinLimit))
RETURN
END FUNCTION MinMax_I2
ELEMENTAL FUNCTION MinMax_I1(MinLimit,Val,MaxLimit) RESULT(ValLimited)
!+
!
! Description: This function limits a values to lie in the closed set [Min,Max] in the Integer(1) domain.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(1), INTENT(IN)       :: MinLimit                        ! Lower limit
INTEGER(1), INTENT(IN)       :: MaxLimit                        ! Upper limit
INTEGER(1), INTENT(IN)       :: Val                             ! Input Value
INTEGER(1)                   :: ValLimited                      ! Limited value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValLimited = MIN(MaxLimit,MAX(Val,MinLimit))
RETURN
END FUNCTION MinMax_I1

SUBROUTINE Swap_I1(Val1, Val2)
!+
!
! Description: This subroutine swaps two Integer(1) values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(1)                   :: Val1                            ! 1st input value to swap
INTEGER(1)                   :: Val2                            ! 2nd input value to swap
!
!                             Local Variables
!----------------------------+---------------
INTEGER(1)                   :: ValSave                         ! Temporary
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValSave = Val1
Val1    = Val2
Val2    = ValSave
RETURN
END SUBROUTINE Swap_I1
SUBROUTINE Swap_I2(Val1, Val2)
!+
!
! Description: This subroutine swaps two Integer(2) values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(2)                   :: Val1                            ! 1st input value to swap
INTEGER(2)                   :: Val2                            ! 2nd input value to swap
!
!                             Local Variables
!----------------------------+---------------
INTEGER(2)                   :: ValSave                         ! Temporary
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValSave = Val1
Val1    = Val2
Val2    = ValSave
RETURN
END SUBROUTINE Swap_I2
SUBROUTINE Swap_I4(Val1, Val2)
!+
!
! Description: This subroutine swaps two Integer(4) values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)                   :: Val1                            ! 1st input value to swap
INTEGER(4)                   :: Val2                            ! 2nd input value to swap
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: ValSave                         ! Temporary
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValSave = Val1
Val1    = Val2
Val2    = ValSave
RETURN
END SUBROUTINE Swap_I4
SUBROUTINE Swap_I8(Val1, Val2)
!+
!
! Description: This subroutine swaps two Integer(8) values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(8)                   :: Val1                            ! 1st input value to swap
INTEGER(8)                   :: Val2                            ! 2nd input value to swap
!
!                             Local Variables
!----------------------------+---------------
INTEGER(8)                   :: ValSave                         ! Temporary
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValSave = Val1
Val1    = Val2
Val2    = ValSave
RETURN
END SUBROUTINE Swap_I8
SUBROUTINE Swap_R4(Val1, Val2)
!+
!
! Description: This subroutine swaps two Real(4) values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL   (4)                   :: Val1                            ! 1st input value to swap
REAL   (4)                   :: Val2                            ! 2nd input value to swap
!
!                             Local Variables
!----------------------------+---------------
REAL   (4)                   :: ValSave                         ! Temporary
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValSave = Val1
Val1    = Val2
Val2    = ValSave
RETURN
END SUBROUTINE Swap_R4
SUBROUTINE Swap_R8(Val1, Val2)
!+
!
! Description: This subroutine swaps two Real(8) values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL   (8)                   :: Val1                            ! 1st input value to swap
REAL   (8)                   :: Val2                            ! 2nd input value to swap
!
!                             Local Variables
!----------------------------+---------------
REAL   (8)                   :: ValSave                         ! Temporary
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ValSave = Val1
Val1    = Val2
Val2    = ValSave
RETURN
END SUBROUTINE Swap_R8

FUNCTION ValueAverageFromData(Value_dat, Status_dat, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value of a set of data. Only elements in the data set that have
!              Normal Quality are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat(datEnd)               ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT(IAND(Status_dat,NormalQuality) == 1)
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = SUM(Value_dat, Status_dat == 1)/DBLE(ValueResult%NumGood)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueAverageFromData

FUNCTION ValueAvgMinMaxFromData(Value_dat, Status_dat, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value, minimum value and maximum values of a set of data. Only
!              elements of the data set that are have NormalQuality are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat(datEnd)               ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
TYPE(ValueAvgMinMax)         :: ValueResult                     ! Average, Min and Max value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT(IAND(Status_dat,NormalQuality) == 1)
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = SUM   (Value_dat, Status_dat == 1)/DBLE(ValueResult%NumGood)
  ValueResult%ValueMin = MINVAL(Value_dat, Status_dat == 1)
  ValueResult%ValueMax = MAXVAL(Value_dat, Status_dat == 1)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%ValueMin = 0
  ValueResult%ValueMax = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueAvgMinMaxFromData

FUNCTION ValueWLSMinMaxFromData(Value_dat, Variance_dat, Status_dat, ValueBias, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value, minimum value and maximum values of a set of data. Only
!              elements of the data set that are have NormalQuality are included in the determination. The average
!              values is calculated as the Weighted Least Squares estimate using all the data with NormalQuality.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat(datEnd)               ! Table of data values
REAL(8)        , INTENT(IN)  :: Variance_dat(datEnd)            ! Table of data standard variancess
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
REAL(8)        , INTENT(IN)  :: ValueBias                       ! Bias in data for estimation stability
TYPE(ValueAvgMinMax)         :: ValueResult                     ! Average, Min and Max value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: dat                             ! Index through values
REAL(8)        ,ALLOCATABLE  :: V_dat(:)                        ! Local copy of values
INTEGER(4)                   :: datSave = 0                     ! Sizing for Weight_dat
INTEGER(4)                   :: datMin = 0                      ! Minimum number of good measurements
REAL(8)                      :: Vavg                            ! WLS estimated value
REAL(8)                      :: Vsum                            ! Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           ! Sum of elements of V_Vec squared
REAL(8),PARAMETER            :: dVTolerance = 3d0               ! Tolerance for bad data detection (num STDV)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult = ValAvgMinMaxBad   ; IF(datEnd < 2) RETURN

IF(datEnd /= datSave) THEN
  IF(ALLOCATED(V_dat)) DEALLOCATE(V_dat)
  ALLOCATE(V_dat(datEnd))
  datSave = datEnd
  datMin  = (datEnd + 1)/2
END IF
V_dat = Value_dat + ValueBias
WHERE(IAND(Status_dat,NormalQuality)/=1) V_dat = 0d0

Vavg = 0
DO
  IF(COUNT(V_dat>0d0) <= datMin) EXIT
  Vsum  = SUM(V_dat/Variance_dat, V_dat > 0d0)
  V2sum = SUM(1d0/Variance_dat  , V_dat > 0d0)
  Vavg  = Vsum/V2Sum
  PRINT *,' Vavg: ',Vavg
  DO dat = 1, datEnd
    IF(V_dat(dat) > 0d0) THEN
      PRINT *,' Normalized Residual ', ((V_dat(dat) - Vavg)**2)/(Variance_dat(dat) - 1d0/V2Sum) , Value_dat(dat)
    ELSE
      PRINT *,' Bad Data Detected   '
    END IF
  END DO
  PRINT *,'--------'
  dat   = MAXLOC(  ((V_dat - Vavg)**2) / (Variance_dat - 1d0/V2sum), 1, V_dat > 0d0)
  IF((V_dat(dat) - Vavg)**2 > (dVTolerance**2)*(Variance_dat(dat) - 1d0/V2sum)) THEN
    V_dat(dat) = 0d0
  ELSE
    EXIT
  END IF
END DO

ValueResult%NumGood = COUNT(V_dat > 0d0)

IF(ValueResult%NumGood > 1 .and. datEnd > 3) THEN
  ValueResult%Value    = Vavg - ValueBias
  ValueResult%ValueMin = MINVAL(Value_dat, V_dat > 0d0)
  ValueResult%ValueMax = MAXVAL(Value_dat, V_dat > 0d0)
  ValueResult%Quality  = 1
END IF
RETURN
END FUNCTION ValueWLSMinMaxFromData

FUNCTION ValueEstimateFromData(Value_dat, Status_dat, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value, minimum value and maximum values of a set of data. Only
!              elements of the data set that are have NormalQuality are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat(datEnd)               ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood =COUNT(IAND(Status_dat,NormalQuality) == 1)
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = SUM(Value_dat, Status_dat == 1)/DBLE(ValueResult%NumGood)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueEstimateFromData
!
FUNCTION ValueEstimateFromDataWithMask(Value_dat, Status_dat, MaskGood, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value, minimum value and maximum values of a set of data. Only
!              elements of the data set with Status data that matches a Mask are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat (datEnd)              ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
INTEGER(4)     , INTENT(IN)  :: MaskGood                        ! Mask for good data quality
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT( IAND(Status_dat, MaskGood)/=0 )
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = SUM(Value_dat, IAND(Status_dat,MaskGood)/=0)/DBLE(ValueResult%NumGood)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueEstimateFromDataWithMask
!
FUNCTION ValueEstimateFromDataTypeAndMask(Value_dat, Status_dat, Type_dat, MaskType, MaskGood, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value, minimum value and maximum values of a set of data. Only the
!              elements of the data set with Status data that match MaskGood and with Type data that match a MaskType
!              are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat (datEnd)              ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
INTEGER(4)     , INTENT(IN)  :: Type_dat  (datEnd)              ! Table of data type
INTEGER(4)     , INTENT(IN)  :: MaskType                        ! Mask for data usage
INTEGER(4)     , INTENT(IN)  :: MaskGood                        ! Mask for good data quality
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT( IAND(Type_dat,MaskType)/=0 .and. IAND(Status_dat, MaskGood)/=0 )
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = SUM(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)/DBLE(ValueResult%NumGood)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueEstimateFromDataTypeAndMask
!
FUNCTION ValueMinMaxFromDataTypeAndMask(MinOrMax, Value_dat, Status_dat, Type_dat, MaskType, MaskGood, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the minimum value or maximum values of a set of data. The character input
!              MinOrMax specifies whether the minimum value of the maximum value is to be returned.  Only the elements
!              of the data set with Status data that match MaskGood and with Type data that match a MaskType
!              included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(3)   , INTENT(IN)  :: MinOrMax                        ! Min or Max
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat (datEnd)              ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
INTEGER(4)     , INTENT(IN)  :: Type_dat  (datEnd)              ! Table of data type
INTEGER(4)     , INTENT(IN)  :: MaskType                        ! Mask for data usage
INTEGER(4)     , INTENT(IN)  :: MaskGood                        ! Mask for good data quality
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT( IAND(Type_dat,MaskType)/=0 .and. IAND(Status_dat, MaskGood)/=0 )
IF(ValueResult%NumGood > 0) THEN
  IF    (MinOrMax.eq."min" .or. MinOrMax.eq."Min" .or. MinOrMax.eq."MIN") THEN
    ValueResult%Value    = MAXVAL(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)
    ValueResult%Quality  = 1
  ELSEIF(MinOrMax.eq."max" .or. MinOrMax.eq."Max" .or. MinOrMax.eq."MAX") THEN
    ValueResult%Value    = MINVAL(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)
    ValueResult%Quality  = 1
  ELSE
    ValueResult%Value    = 0
    ValueResult%Quality  = 3
  END IF
ELSE
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueMinMaxFromDataTypeAndMask
!
FUNCTION ValueEstimateMinMaxFromDataTypeAndMask(Value_dat, Status_dat, Type_dat, MaskType, MaskGood, datEnd) RESULT(ValueResult)
!+
!
! Description: This function determines the average value, minimum value and maximum values of a set of data. Only the
!              elements of the data set with Status data that match MaskGood and with Type data that match a MaskType
!              are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat (datEnd)              ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
INTEGER(4)     , INTENT(IN)  :: Type_dat  (datEnd)              ! Table of data type
INTEGER(4)     , INTENT(IN)  :: MaskType                        ! Mask for data usage
INTEGER(4)     , INTENT(IN)  :: MaskGood                        ! Mask for good data quality
TYPE(ValueAvgMinMax)         :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT( IAND(Type_dat,MaskType)/=0 .and. IAND(Status_dat, MaskGood)/=0 )
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = SUM(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)/DBLE(ValueResult%NumGood)
  ValueResult%ValueMin = MINVAL(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)
  ValueResult%ValueMax = MAXVAL(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%ValueMin = 0
  ValueResult%ValueMax = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueEstimateMinMaxFromDataTypeAndMask

FUNCTION ValueWLSFromDataTypeAndMask(Value_dat, Status_dat, Type_dat, Variance_dat, MaskType, MaskGood, dVTolerance, DatEnd) RESULT(ValueResult)
!+
! Description:
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: datEnd                          ! Size of tables
REAL(8)   ,INTENT(IN)        :: Value_dat   (datEnd)            ! Table of data values
INTEGER(4),INTENT(IN)        :: Status_dat  (datEnd)            ! Table of data quality
INTEGER(4),INTENT(IN)        :: Type_dat    (datEnd)            ! Table of data type
REAL(8)   ,INTENT(IN)        :: Variance_dat(datEnd)            ! Table of data variance
INTEGER(4),INTENT(IN)        :: MaskType                        ! Mask for data usage
INTEGER(4),INTENT(IN)        :: MaskGood                        ! Mask for good data quality
REAL(8)   ,INTENT(IN)        :: dVTolerance                     ! Bad data detection tolerance
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: dat                             ! Index through values
INTEGER(4)                   :: Num                             ! Index through matching and good values
REAL(8)                      :: Val_Num(100)                    ! Matching and good values for WLS
REAL(8)                      :: Variance_Num(100)               ! Variance of WLS values
LOGICAL(1)                   :: BadData_Num(100)                ! Bad Data Indicator
INTEGER(4), PARAMETER        :: NumMinForWlsEstimate = 2        ! Number needed for a valid estimate
REAL(8)                      :: Vestimate                       ! Weighted Least Squares estimate from Val_Num
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT( IAND(Type_dat,MaskType)/=0 .and. IAND(Status_dat, MaskGood)/=0 )
SELECT CASE (ValueResult%NumGood)
CASE (:0)
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
CASE (1:2)
  ValueResult%Value    = SUM(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)/DBLE(ValueResult%NumGood)
  ValueResult%Quality  = 1
CASE (3:)
  Num = 0
  DO dat = 1, datEnd
    IF(IAND(Type_dat(dat),MaskType)/=0 .and. IAND(Status_dat(dat),MaskGood)/=0) THEN
      Num = Num + 1; Val_Num(Num) = Value_dat(dat) ; Variance_Num(Num) = Variance_dat(dat)
    END IF
  END DO
  CALL WLSEstimateAndBadDataDetection(Val_Num(1:Num), Variance_Num(1:Num), Num, NumMinForWLSEstimate, dVTolerance, BadData_Num(1:Num), Vestimate)
  ValueResult%Value   = Vestimate
  ValueResult%NumGood = COUNT(.not.BadData_Num(1:Num))
  ValueResult%Quality = 1
END SELECT
RETURN
END FUNCTION ValueWLSFromDataTypeAndMask

FUNCTION QuadraticFormEstimateOld(X_n, Y_n, W_n, X0, Y0, nEnd) RESULT(QuadraticForm)
!+
! Description: Old and unused FUNCTION QuadraticFormEstimate
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: nEnd                          ! Size of tables
REAL(8)                      :: X_n(nEnd)                     ! i values
REAL(8)                      :: Y_n(nEnd)                     ! j values
REAL(8)                      :: W_n(nEnd)
REAL(8)        , INTENT(IN)  :: X0
REAL(8)        , INTENT(IN)  :: Y0
TYPE(Quadratic)              :: QuadraticForm                 ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                             ! Index through values
REAL(8)                      :: T_m_m(5,5)
REAL(8)                      :: U_m_m(5,5)
REAL(8)                      :: p_m(5)
REAL(8)                      :: q_m(5)
LOGICAL(4)                   :: FullRank = TRUE
INTEGER(4)                   :: nFrom
INTEGER(4)                   :: nTo
REAL(8)                      :: Xfrom
REAL(8)                      :: Yfrom
REAL(8)                      :: Xto
REAL(8)                      :: Yto
REAL(8)                      :: AngleFrom
REAL(8)                      :: AngleTo
LOGICAL(4)                   :: FoundConcave
REAL(8)                      :: dNTotal
REAL(8)                      :: dNAt
INTEGER(4)                   :: nAt
REAL(8)                      :: R_m(2)
REAL(8)                      :: Rfrom_m(2)
REAL(8)                      :: Rto_m(2)
LOGICAL(4)                   :: SubstituteForConcavePoints = .FALSE.
INTEGER(4)                   :: nBad
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
W_n          = 1d0
nBad         = 0
DO
  FoundConcave = .false.
  DO n = 1, nEnd
    IF(W_n(n) /= 1d0) CYCLE
    IF (n == 1) THEN
      nFrom = nEnd
    ELSE
      nFrom = n-1
    END IF

    DO WHILE(W_n(nFrom) /= 1d0)
      nFrom = nFrom - 1
      IF(nFrom==0) nFrom = nEnd
      IF(nFrom==n) EXIT
    END DO
    Xfrom = X_n(nFrom); Yfrom = Y_n(nFrom)

    IF (n == nEnd) THEN
      nTo = 1
    ELSE
      nTo = n+1
    END IF
    DO WHILE(W_n(nTo) /= 1d0 .and. SubstituteForConcavePoints)
      nTo = nTo + 1
      IF(nTo > nEnd) nTo = 1
      IF(nTo==n) EXIT
    END DO
    Xto = X_n(nTo); Yto = Y_n(nTo)

    ! Calculate interior angle to see if convex or not
    IF (MAGNITUDE((/Xfrom, Yfrom/) - (/X_n(n), Y_n(n)/) ,2) < 3*EPSILON .or. &
        MAGNITUDE(  (/X0,  Y0/)    - (/X_n(n), Y_n(n)/) ,2) < 3*EPSILON .or. &
        MAGNITUDE(  (/Xto, Yto/)   - (/X_n(n), Y_n(n)/) ,2) < 3*EPSILON .or. &
        MAGNITUDE(  (/X0,  Y0/)    - (/X_n(n), Y_n(n)/) ,2) < 3*EPSILON) THEN
      W_n(n) = EPSILON
      nBad   = nBad + 1
      CYCLE
    END IF

    AngleFrom = ACOSD( DOT_PRODUCT((/Xfrom, Yfrom/) -(/X_n(n), Y_n(n)/), (/X0, Y0/) -(/X_n(n), Y_n(n)/)) &
                            /(MAGNITUDE( (/Xfrom, Yfrom/) -(/X_n(n), Y_n(n)/) ,2) + EPSILON)              &
                            /(MAGNITUDE( (/X0, Y0/) -(/X_n(n), Y_n(n)/)       ,2) + EPSILON))
    AngleTo   = ACOSD( DOT_PRODUCT((/Xto, Yto/) -(/X_n(n), Y_n(n)/), (/X0, Y0/) -(/X_n(n), Y_n(n)/))  &
                            /(MAGNITUDE( (/Xto, Yto/) -(/X_n(n), Y_n(n)/)     ,2) + EPSILON)           &
                            /(MAGNITUDE( (/X0, Y0/) -(/X_n(n), Y_n(n)/)       ,2) + EPSILON))
    IF (AngleFrom + AngleTo > 180) THEN
      FoundConcave = .true.
      W_n(n)      = EPSILON
      nBad        = nBad + 1
      EXIT
    END IF
  END DO
  IF(.not.FoundConcave) EXIT
END DO

IF(nEnd-nBad < 5) THEN
  QuadraticForm%A = 0
  QuadraticForm%B = 0
  QuadraticForm%C = 0
  QuadraticForm%D = 0
  QuadraticForm%E = 0
  QuadraticForm%degenerate = TRUE
  RETURN
END IF

! Fill in linear substitute points for concave points
! This will help the ellipse fit
DO n = 1, nEnd
  ! Check if point is concave
  IF(W_n(n) < 1d0) THEN
    !
    ! Find the 2 bounding convex points
    IF (n == 1) THEN
      nFrom = nEnd
    ELSE
      nFrom = n-1
    END IF
    DO WHILE(W_n(nFrom) /= 1d0)
      nFrom = nFrom - 1
      IF(nFrom==0) nFrom = nEnd
    END DO
    Rfrom_m = (/X_n(nFrom), Y_n(nFrom)/)

    IF (n == nEnd) THEN
      nTo = 1
    ELSE
      nTo = n+1
    END IF
    DO WHILE(W_n(nTo) /= 1d0)
      nTo = nTo + 1
      IF(nTo > nEnd) nTo = 1
    END DO
    Rto_m = (/X_n(nTo), Y_n(nTo)/)
    !
    ! Generate linear estimate in between
    nAt = nFrom
    dNTotal  = 0d0
    dNAt     = 0d0
    DO WHILE(nAt /= nTo)
     dNTotal = dNTotal + 1d0
     IF(nAt==n) dNAt = dNtotal
      nAt = nAt + 1
      IF(nAt > nEnd) nAt = 1
    END DO
    R_m    = Rfrom_m + (Rto_m - Rfrom_m) * dNAt/dNtotal
    X_n(n) = R_m(1); Y_n(n) = R_m(2)
    !
    ! Down-weight linear substitute points
    W_n(n) = 0.3
  END IF
END DO

T_m_m(1,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n**2), DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n**2,X_n), DOT_PRODUCT(W_n*X_n**2,Y_n)/)
T_m_m(2,:) = (/DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*X_n**2,Y_n), DOT_PRODUCT(W_n*X_n,Y_n**2)/)
T_m_m(3,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*Y_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**2), DOT_PRODUCT(W_n*Y_n,Y_n**2)/)
T_m_m(4,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*X_n,X_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   /)
T_m_m(5,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*Y_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   , DOT_PRODUCT(W_n*Y_n,Y_n)   /)

p_m        = (/DOT_PRODUCT(W_n*X_n,X_n)      , DOT_PRODUCT(W_n*X_n,Y_n)      , DOT_PRODUCT(W_n*Y_n,Y_n)      , SUM(W_n*X_n)               , SUM(W_n*Y_n)               /)

U_m_m = InverseCholeskyNxN(T_m_m, 5, FullRank) ! I_m_m = MATMUL(U_m_m, T_m_m)

IF(FullRank) THEN
  q_m = MATMUL(U_m_m,p_m)
  QuadraticForm%A = q_m(1)
  QuadraticForm%B = q_m(2)
  QuadraticForm%C = q_m(3)
  QuadraticForm%D = q_m(4)
  QuadraticForm%E = q_m(5)
  QuadraticForm%degenerate = FALSE
ELSE
  QuadraticForm%A = 0
  QuadraticForm%B = 0
  QuadraticForm%C = 0
  QuadraticForm%D = 0
  QuadraticForm%E = 0
  QuadraticForm%degenerate = TRUE
ENDIF

RETURN
END FUNCTION QuadraticFormEstimateOld

FUNCTION QuadraticFormEstimate(X_n, Y_n, W_n, X0, Y0, nEnd) RESULT(QuadraticForm)
!+
! Description: This function calculates 5 coefficients (A,B,C,D, and E) of the following equation in x and y:
!
!              Ax^2 + Bxy + Cy^2 + Dx + Ey = 1
!
!              The coefficients are calculated by using a Weight Least Squares estimate of the coefficients that best
!              fit the boundary points, X_n and Y_n with associated weighting W_n, passed as arguments to this Function%
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: nEnd                          ! Size of tables
REAL(8)                      :: X_n(nEnd)                     ! i values
REAL(8)                      :: Y_n(nEnd)                     ! j values
REAL(8)                      :: W_n(nEnd)
REAL(8)        , INTENT(IN)  :: X0
REAL(8)        , INTENT(IN)  :: Y0
TYPE(Quadratic)              :: QuadraticForm                 ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                             ! Index through values
REAL(8)                      :: T_m_m(5,5)
REAL(8)                      :: U_m_m(5,5)
REAL(8)                      :: p_m(5)
REAL(8)                      :: q_m(5)
LOGICAL(4)                   :: FullRank = TRUE
INTEGER(4)                   :: nFrom
INTEGER(4)                   :: nTo
REAL(8)                      :: Xfrom
REAL(8)                      :: Yfrom
REAL(8)                      :: Xto
REAL(8)                      :: Yto
REAL(8)                      :: AngleFrom
REAL(8)                      :: AngleTo
INTEGER(4)                   :: nWorst
REAL(8)                      :: dAngleWorst
REAL(8),ALLOCATABLE          :: Angle_n(:)
INTEGER(4)                   :: nLast = 0
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(nLast < MAX(1,nEnd)) THEN
  IF(ALLOCATED(Angle_n)) DEALLOCATE(Angle_n)
  nLast = MAX(1, nEnd)
  ALLOCATE(Angle_n(nLast))
END IF
W_n = 1d0
DO
  DO n = 1, nEnd
    IF (n == 1) THEN
      nFrom = nEnd
    ELSE
      nFrom = n-1
    END IF

    Xfrom = X_n(nFrom); Yfrom = Y_n(nFrom)

    IF (n == nEnd) THEN
      nTo = 1
    ELSE
      nTo = n+1
    END IF
    Xto = X_n(nTo); Yto = Y_n(nTo)

    ! Calculate interior angle to see if convex or not

    AngleFrom = ACOSD( DOT_PRODUCT((/Xfrom, Yfrom/) -(/X_n(n), Y_n(n)/), (/X0, Y0/) -(/X_n(n), Y_n(n)/)) &
                            /(MAGNITUDE( (/Xfrom, Yfrom/) -(/X_n(n), Y_n(n)/) ,2) + EPSILON)              &
                            /(MAGNITUDE( (/X0, Y0/) -(/X_n(n), Y_n(n)/)       ,2) + EPSILON))
    AngleTo   = ACOSD( DOT_PRODUCT((/Xto, Yto/) -(/X_n(n), Y_n(n)/), (/X0, Y0/) -(/X_n(n), Y_n(n)/))  &
                            /(MAGNITUDE( (/Xto, Yto/) -(/X_n(n), Y_n(n)/)     ,2) + EPSILON)           &
                            /(MAGNITUDE( (/X0, Y0/) -(/X_n(n), Y_n(n)/)       ,2) + EPSILON))
    Angle_n(n) = AngleFrom + AngleTo
  END DO

  nWorst    = 0
  DO n = 1, nEnd
    IF (n == 1) THEN
      nFrom = nEnd
    ELSE
      nFrom = n-1
    END IF
    IF (n == nEnd) THEN
      nTo = 1
    ELSE
      nTo = n+1
    END IF
    IF(Angle_n(n) < 180d0 .and. Angle_n(nFrom) > 180d0 .and. Angle_n(nTo) > 180d0) THEN
      IF(nWorst == 0) THEN
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      ELSE IF(ABS(Angle_n(n) - 180d0) < dAngleWorst) THEN
        nWorst = n
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      END IF
    ELSE IF(Angle_n(n) > 180d0 .and. Angle_n(nFrom) < 180d0 .and. Angle_n(nTo) < 180d0) THEN
      IF(nWorst == 0) THEN
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      ELSE IF(ABS(Angle_n(n) - 180d0) < dAngleWorst) THEN
        nWorst = n
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      END IF
    END IF
  END DO

  IF (nWorst == 0) EXIT

  n = nWorst
  IF (n == 1) THEN
    nFrom = nEnd
  ELSE
    nFrom = n-1
  END IF
  IF (n == nEnd) THEN
    nTo = 1
  ELSE
    nTo = n+1
  END IF

  X_n(n) = (X_n(nFrom) + X_n(nTo))/2.0d0
  Y_n(n) = (Y_n(nFrom) + Y_n(nTo))/2.0d0

END DO

T_m_m(1,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n**2), DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n**2,X_n), DOT_PRODUCT(W_n*X_n**2,Y_n)/)
T_m_m(2,:) = (/DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*X_n**2,Y_n), DOT_PRODUCT(W_n*X_n,Y_n**2)/)
T_m_m(3,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*Y_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**2), DOT_PRODUCT(W_n*Y_n,Y_n**2)/)
T_m_m(4,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*X_n,X_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   /)
T_m_m(5,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*Y_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   , DOT_PRODUCT(W_n*Y_n,Y_n)   /)

p_m        = (/DOT_PRODUCT(W_n*X_n,X_n)      , DOT_PRODUCT(W_n*X_n,Y_n)      , DOT_PRODUCT(W_n*Y_n,Y_n)      , SUM(W_n*X_n)               , SUM(W_n*Y_n)               /)

U_m_m = InverseCholeskyNxN(T_m_m, 5, FullRank) ! I_m_m = MATMUL(U_m_m, T_m_m)

IF(FullRank) THEN
  q_m = MATMUL(U_m_m,p_m)
  QuadraticForm%A = q_m(1)
  QuadraticForm%B = q_m(2)
  QuadraticForm%C = q_m(3)
  QuadraticForm%D = q_m(4)
  QuadraticForm%E = q_m(5)
  QuadraticForm%degenerate = FALSE
ELSE
  QuadraticForm%A = 0
  QuadraticForm%B = 0
  QuadraticForm%C = 0
  QuadraticForm%D = 0
  QuadraticForm%E = 0
  QuadraticForm%degenerate = TRUE
ENDIF

RETURN
END FUNCTION QuadraticFormEstimate

FUNCTION EllipseEstimateFromData(X_n, Y_n, W_n, nEnd) RESULT(Ellipse)
USE NumericalConstants
IMPLICIT NONE
!+
! Description: This function calculates 5 coefficients a, b, xo, yo and angle that satisfiy the general ellipse equation
!
!               (x*cos(angle) + y*sin(angle) - xo)^2      (x*sin(angle) - y*cos(angle) - yo)^2
!               ------------------------------------   +   -----------------------------------   =  1
!                            a^2                                       b^2
!
!              The coefficients are calculated by using a Weight Least Squares estimate of the coefficients that best
!              fit the boundary points, X_n and Y_n with associated weighting W_n, passed as arguments to this Function%
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: nEnd                            ! Size of tables
REAL(8)                      :: X_n(nEnd)                       ! x-coordinate values (distance)
REAL(8)                      :: Y_n(nEnd)                       ! y-coordinate values (distance)
REAL(8)                      :: W_n(nEnd)                       ! Weight of values    (1/distance^2)
TYPE(Elliptical)             :: Ellipse                         ! Ellipse result
!
!                             Local Variables
!----------------------------+---------------
TYPE(Quadratic)              :: QuadraticForm                   ! Quadratic Form fit to the input data
REAL(8)                      :: A                               ! Quadratic Form A parameter
REAL(8)                      :: B                               ! Quadratic Form B parameter
REAL(8)                      :: C                               ! Quadratic Form C parameter
REAL(8)                      :: D                               ! Quadratic Form D parameter
REAL(8)                      :: E                               ! Quadratic Form E parameter
REAL(8)                      :: F                               ! Quadratic Form F parameter
REAL(8)                      :: Discriminant                    ! Quadratic Form discriminant B^2 - 4*A*C
REAL(8)                      :: Xo                              ! X coordinate of Ellipse center
REAL(8)                      :: Yo                              ! Y coordinate of Ellipse center
REAL(8)                      :: Angle                           ! Rotation angle of ellipse (degrees)
REAL(8)                      :: Aaxis                           ! Major axis of the ellipse ("radius", i.e. measured from the center)
REAL(8)                      :: Baxis                           ! Minor axis of the ellipse ("radius", i.e. not the diameter)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
! Get the Quadratic Form
  QuadraticForm = QuadraticFormEstimateFromData(X_n, Y_n, W_n, nEnd)

  IF(QuadraticForm%Degenerate) THEN
    Ellipse%Valid = .false.
    RETURN
  END IF

  A            = QuadraticForm%A
  B            = QuadraticForm%B
  C            = QuadraticForm%C
  D            = QuadraticForm%D
  E            = QuadraticForm%E
  F            = QuadraticForm%F

  Discriminant = B**2 - 4d0*A*C

  ! Test for non-ellipse quadratic form (hyperbola, parabola, point-ellipse or linear form)
  IF(Discriminant>=-Epsilon) THEN
    Ellipse%Valid = .false.
    RETURN
  END IF
  !
  ! Calculate the Center
  Xo = (2d0*C*D - B*E)/Discriminant
  Yo = (2d0*A*E - B*D)/Discriminant

  ! Calculate the Rotation Angle
  IF(ABS(B)>Epsilon) THEN
    Angle = ATAN2D(B,A-C)/2d0
  ELSE
    Angle = 0d0
  END IF

  ! Calulate the semi-major and semi-minor axes:
  Aaxis = (A*COSD(Angle)**2 + B*COSD(Angle)*SIND(Angle) + C*SIND(Angle)**2)**(-0.5d0)
  Baxis = (A*SIND(Angle)**2 - B*COSD(Angle)*SIND(Angle) + C*COSD(Angle)**2)**(-0.5d0)


  !Put the ellipse in standard form (minimum rotation)
  IF(Angle < 0d0) THEN
    IF(ABS(Angle + 90d0) < ABS(Angle)) THEN
      Angle = Angle + 90d0
      CALL Swap(Aaxis, Baxis)
    END IF
  ELSE
    IF(ABS(Angle - 90d0) < ABS(Angle)) THEN
      Angle = Angle - 90d0
      CALL Swap(Aaxis, Baxis)
    END IF
  END IF

  ! Return a valid ellipse
  Ellipse%Xo    = Xo
  Ellipse%Yo    = Yo
  Ellipse%Angle = Angle
  Ellipse%Aaxis = Aaxis
  Ellipse%Baxis = Baxis
  Ellipse%Valid = .true.
  RETURN
END FUNCTION EllipseEstimateFromData

FUNCTION OutsideEllipse(Ellipse, w, h) RESULT(Outside)
!+
! Description: This function returns a true value if the inputs, w and h, are not in the interior or on the boundary of
!              an ellipse whose parameters are specified in the Ellipse input parameter.
!
! Audit Trail
! ----------------+-------+----------------------------------------------------------------------------------------------
! ----------------+-------+----------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-----------------------------------
TYPE(Elliptical),INTENT(IN)  :: Ellipse                        ! Ellipse
INTEGER(4)      ,INTENT(IN)  :: w                              ! Current pixel position in the horizontal plane (pixels)
INTEGER(4)      ,INTENT(IN)  :: h                              ! Current pixel position in the vertical plane (pixels)
LOGICAL(4)                   :: Outside

! Local Variables
!----------------------------
REAL(8)                       :: w0                             ! Ellipse center pixel width
REAL(8)                       :: h0                             ! Ellipse center pixel height
REAL(8)                       :: a                              ! Ellipse major axis
REAL(8)                       :: b                              ! Ellipse minor axis
REAL(8)                       :: cA                             ! Ellipse angle of rotation cosine
REAL(8)                       :: sA                             ! Ellipse angle of rotation sine
REAL(8)                       :: dW                             ! Distance from center in horizontal axis (pixels)
REAL(8)                       :: dH                             ! Distance from center in vertical axis (pixels)
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
  !
  ! Set the ellipse center and axes:
  w0 = Ellipse%Xo
  h0 = Ellipse%Yo
  a  = Ellipse%aAxis
  b  = Ellipse%bAxis
  cA = COSD(Ellipse%Angle)
  sA = SIND(Ellipse%Angle)
  !
  ! Get the distance from the center
  dW = DBLE(w) - w0
  dH = DBLE(h) - h0
  !
  ! Perform logical test for point outside the ellipse and return result in Outside
  Outside = ((cA*dW+sA*dH)/a)**2 + ((cA*dH-sA*dW)/b)**2 > 1d0
  RETURN
END FUNCTION OutsideEllipse

FUNCTION QuadraticFormEstimateFromData(X_n, Y_n, W_n, nEnd) RESULT(QuadraticForm)
!+
! Description: This function calculates 6 coefficients (A,B,C,D,E and F) of the following equation in x and y:
!
!              Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0
!
!              The coefficients are calculated by using a Weight Least Squares estimate of the coefficients that best
!              fit the boundary points, X_n and Y_n with associated weighting W_n, passed as arguments to this Function%
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: nEnd                            ! Size of tables
REAL(8)                      :: X_n(nEnd)                       ! i values
REAL(8)                      :: Y_n(nEnd)                       ! j values
REAL(8)                      :: W_n(nEnd)
TYPE(Quadratic)              :: QuadraticForm                   ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                               ! Index through values
REAL(8)                      :: Xo
REAL(8)                      :: Yo
REAL(8)                      :: T_m_m(5,5)
REAL(8)                      :: U_m_m(5,5)
REAL(8)                      :: p_m(5)
REAL(8)                      :: q_m(5)
REAL(8)                      :: A                               ! Quadratic Form A parameter
REAL(8)                      :: B                               ! Quadratic Form B parameter
REAL(8)                      :: C                               ! Quadratic Form C parameter
REAL(8)                      :: D                               ! Quadratic Form D parameter
REAL(8)                      :: E                               ! Quadratic Form E parameter
EQUIVALENCE                    (A,q_m(1)),(B,q_m(2)),(C,q_m(3)),(D,q_m(4)),(E,q_m(5))
LOGICAL(4)                   :: FullRank = TRUE
INTEGER(4)                   :: nFrom
INTEGER(4)                   :: nTo
REAL(8)                      :: Xfrom
REAL(8)                      :: Yfrom
REAL(8)                      :: Xto
REAL(8)                      :: Yto
REAL(8)                      :: AngleFrom
REAL(8)                      :: AngleTo
INTEGER(4)                   :: nWorst
REAL(8)                      :: dAngleWorst
REAL(8),ALLOCATABLE          :: Angle_n(:)
INTEGER(4)                   :: nLast = 0
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(nLast < MAX(1,nEnd)) THEN
  IF(ALLOCATED(Angle_n)) DEALLOCATE(Angle_n)
  nLast = MAX(1, nEnd)
  ALLOCATE(Angle_n(nLast))
END IF
Xo  = SUM(X_n)/DBLE(nEnd)
Yo  = SUM(Y_n)/DBLE(nEnd)
W_n = 1d0
DO
  DO n = 1, nEnd
    IF (n == 1) THEN
      nFrom = nEnd
    ELSE
      nFrom = n-1
    END IF

    Xfrom = X_n(nFrom); Yfrom = Y_n(nFrom)

    IF (n == nEnd) THEN
      nTo = 1
    ELSE
      nTo = n+1
    END IF
    Xto = X_n(nTo); Yto = Y_n(nTo)

    ! Calculate interior angle to see if convex or not

    AngleFrom = ACOSD( DOT_PRODUCT((/Xfrom, Yfrom/) -(/X_n(n), Y_n(n)/), (/Xo, Yo/) -(/X_n(n), Y_n(n)/)) &
                            /(MAGNITUDE( (/Xfrom, Yfrom/) -(/X_n(n), Y_n(n)/) ,2) + EPSILON)              &
                            /(MAGNITUDE( (/Xo, Yo/) -(/X_n(n), Y_n(n)/)       ,2) + EPSILON))
    AngleTo   = ACOSD( DOT_PRODUCT((/Xto, Yto/) -(/X_n(n), Y_n(n)/), (/Xo, Yo/) -(/X_n(n), Y_n(n)/))  &
                            /(MAGNITUDE( (/Xto, Yto/) -(/X_n(n), Y_n(n)/)     ,2) + EPSILON)           &
                            /(MAGNITUDE( (/Xo, Yo/) -(/X_n(n), Y_n(n)/)       ,2) + EPSILON))
    Angle_n(n) = AngleFrom + AngleTo
  END DO

  nWorst    = 0
  DO n = 1, nEnd
    IF (n == 1) THEN
      nFrom = nEnd
    ELSE
      nFrom = n-1
    END IF
    IF (n == nEnd) THEN
      nTo = 1
    ELSE
      nTo = n+1
    END IF
    IF(Angle_n(n) < 180d0 .and. Angle_n(nFrom) > 180d0 .and. Angle_n(nTo) > 180d0) THEN
      IF(nWorst == 0) THEN
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      ELSE IF(ABS(Angle_n(n) - 180d0) < dAngleWorst) THEN
        nWorst = n
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      END IF
    ELSE IF(Angle_n(n) > 180d0 .and. Angle_n(nFrom) < 180d0 .and. Angle_n(nTo) < 180d0) THEN
      IF(nWorst == 0) THEN
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      ELSE IF(ABS(Angle_n(n) - 180d0) < dAngleWorst) THEN
        nWorst = n
        dAngleWorst = ABS(Angle_n(n) - 180d0)
      END IF
    END IF
  END DO

  IF (nWorst == 0) EXIT

  n = nWorst
  IF (n == 1) THEN
    nFrom = nEnd
  ELSE
    nFrom = n-1
  END IF
  IF (n == nEnd) THEN
    nTo = 1
  ELSE
    nTo = n+1
  END IF

  X_n(n) = (X_n(nFrom) + X_n(nTo))/2.0d0
  Y_n(n) = (Y_n(nFrom) + Y_n(nTo))/2.0d0

END DO

! Translate to a coordinate system around the center:
X_n = X_n - Xo
Y_n = Y_n - Yo

T_m_m(1,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n**2), DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n**2,X_n), DOT_PRODUCT(W_n*X_n**2,Y_n)/)
T_m_m(2,:) = (/DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*X_n**2,Y_n), DOT_PRODUCT(W_n*X_n,Y_n**2)/)
T_m_m(3,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*Y_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**2), DOT_PRODUCT(W_n*Y_n,Y_n**2)/)
T_m_m(4,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*X_n,X_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   /)
T_m_m(5,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*Y_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   , DOT_PRODUCT(W_n*Y_n,Y_n)   /)

p_m        = (/DOT_PRODUCT(W_n*X_n,X_n)      , DOT_PRODUCT(W_n*X_n,Y_n)      , DOT_PRODUCT(W_n*Y_n,Y_n)      , SUM(W_n*X_n)               , SUM(W_n*Y_n)               /)

U_m_m = InverseCholeskyNxN(T_m_m, 5, FullRank) ! I_m_m = MATMUL(U_m_m, T_m_m)

! Translate back to the original coordinate system
X_n = X_n + Xo
Y_n = Y_n + Yo

IF(FullRank) THEN
  q_m = MATMUL(U_m_m,p_m)
  QuadraticForm%A = A
  QuadraticForm%B = B
  QuadraticForm%C = C
  QuadraticForm%D = D - 2d0*A*Xo - B*Yo
  QuadraticForm%E = E - 2d0*C*Yo - B*Xo
  QuadraticForm%F = A*Xo**2 + B*Xo*Yo + C*Yo**2 - D*Xo - E*Yo - 1d0
  QuadraticForm%degenerate = FALSE
ELSE
  QuadraticForm%A = 0
  QuadraticForm%B = 0
  QuadraticForm%C = 0
  QuadraticForm%D = 0
  QuadraticForm%E = 0
  QuadraticForm%F = 0
  QuadraticForm%degenerate = TRUE
ENDIF

RETURN
END FUNCTION QuadraticFormEstimateFromData

FUNCTION LinearFunctionEstimation(X_n, Y_n, nEnd) RESULT(LinearFunction)
USE MatrixVectorPackage
IMPLICIT NONE
!+
! Description: This function calculates 2 coefficients (Yo and dYdX) of the following equation in x and y:
!
!                               y = x*dYdX + Yo
!
!              The coefficients are calculated by using a Least Squares estimate of the coefficients that best fit the
!              data points, X_n and Y_n passed as arguments to this Function%
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: nEnd                            ! Number of Points
REAL(8)   , INTENT(IN)       :: X_n(nEnd)                       ! X-axis values
REAL(8)   , INTENT(IN)       :: Y_n(nEnd)                       ! Y-axis values
TYPE(LinearForm)             :: LinearFunction                  ! Ellipse result
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: A_m_m(2,2)                      ! Least Squares Fit Matrix
REAL(8)                      :: Ainv_m_m(2,2)                   ! Least Squares Fit Matrix Inverse
REAL(8)                      :: B_m(2)                          ! Least Squares Fit Right Hand Side Vector
REAL(8)                      :: C_m(2)                          ! Least Squares Fit Solution Vector
REAL(8)                      :: Yo                              ! Least Squares Fit Constant Term
REAL(8)                      :: dYdX                            ! Least Squares Fit Multiplier Term
EQUIVALENCE                    (C_m(1),Yo),(C_m(2),dYdX)
LOGICAL(4)                   :: FullRank                        ! Matrix inverse is non-singular
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  SELECT CASE(nEnd)
  CASE(:0)
    LinearFunction%Yo         = 0d0
    LinearFunction%dYdX       = 0d0
    LinearFunction%Degenerate = .true.
  CASE(1)
    LinearFunction%Yo         = Y_n(nEnd)
    LinearFunction%dYdX       = 0d0
    LinearFunction%Degenerate = .false.
  CASE(2:)
    A_m_m    = RESHAPE((/ DBLE(nEnd), SUM(X_n), SUM(X_n), SUM(X_n**2) /), (/2,2/))
    B_m      =         (/ SUM(Y_n), SUM(Y_n*Y_n) /)

    Ainv_m_m = Inverse2X2(A_m_m, FullRank)
    C_m      = MATMUL(Ainv_m_m,B_m)
    IF(.not.FullRank) THEN
      LinearFunction%Yo       = SUM(Y_n)/DBLE(nEnd)
      LinearFunction%dYdX     = 0d0
    ELSE
      LinearFunction%Yo       = Yo
      LinearFunction%dYdX     = dYdX
    END IF
    LinearFunction%Degenerate = .false.
  END SELECT
  RETURN

END FUNCTION LinearFunctionEstimation

FUNCTION EllipseFromQuadraticForm(QuadraticForm, R_m_n) RESULT(Ellipse)
USE NumericalConstants
IMPLICIT NONE
!+
!+
! Description: This function calculates 5 coefficients a, b, xo, yo and angle that satisfiy the general ellipse equation:
!
!               (x*cos(angle) + y*sin(angle) - xo)^2      (x*sin(angle) - y*cos(angle) - yo)^2
!               ------------------------------------   +   -----------------------------------   =  1
!                            a^2                                       b^2
!
!              From the 5 (A,B,C,D and E)coefficients of the Quadratic Form
!
!                            Ax^2 + Bxy + Cy^2 + Dx + Ey = 1
!
!              A bounding quadrilateral is passed to properly orient the roation angle of the Ellipse%

! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
TYPE(Quadratic), INTENT(IN)  :: QuadraticForm                   ! Size of tables
REAL(8)        , INTENT(IN)  :: R_m_n(2,4)                      ! Coordinates of bounding quadralateral corners (pixels)
TYPE(Elliptical)             :: Ellipse                         ! Ellipse result
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: A                               ! Quadratic Form A parameter
REAL(8)                      :: B                               ! Quadratic Form B parameter
REAL(8)                      :: C                               ! Quadratic Form C parameter
REAL(8)                      :: D                               ! Quadratic Form D parameter
REAL(8)                      :: E                               ! Quadratic Form E parameter
REAL(8)                      :: Ay                              ! Term used to calculate (Xo,Yo)
REAL(8)                      :: By                              ! Term used to calculate (Xo,Yo)
REAL(8)                      :: Cy                              ! Term used to calculate (Xo,Yo)
REAL(8)                      :: Y1                              ! Term used to calculate Yo
REAL(8)                      :: Y2                              ! Term used to calculate Yo
REAL(8)                      :: X1                              ! Term used to calculate Xo
REAL(8)                      :: X2                              ! Term used to calculate Xo
REAL(8)                      :: Xo                              ! X coordinate of Ellipse center
REAL(8)                      :: Yo                              ! Y coordinate of Ellipse center
REAL(8)                      :: A_m(2)                          ! Vector aligned with major axis
REAL(8)                      :: B_m(2)                          ! Vector aligned with minor axis
REAL(8)                      :: signAxis                        ! sign of (1/a^2 - 1/b^2)
REAL(8)                      :: signFn                          ! sign of Fn for unnormalized quadratic
REAL(8)                      :: Xn                              ! Term used to calculate rotation angle
REAL(8)                      :: Yn                              ! Term used to calculate rotation angle
REAL(8)                      :: Angle                           ! Rotation angle of ellipse (degrees)
REAL(8)                      :: cA                              ! Cosine of Angle
REAL(8)                      :: sA                              ! Sine   of Angle
REAL(8)                      :: Af                              ! Term used to calculate the major axis
REAL(8)                      :: Bf                              ! Term used to calculate the minor axis
REAL(8)                      :: Finv                            ! Term used to calculate the major and minor axis
REAL(8)                      :: Aaxis                           ! Major axis of the ellipse ("radius", i.e. measured from the center)
REAL(8)                      :: Baxis                           ! Minor axis of the ellipse ("radius", i.e. not the diameter)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
!
A  = QuadraticForm%A
B  = QuadraticForm%B
C  = QuadraticForm%C
D  = QuadraticForm%D
E  = QuadraticForm%E

IF(B**2 - 4d0*A*C > 0) THEN
  Ellipse%Valid = .false.
  RETURN
END IF

! Calculate the center
Ay = C - (B**2)/(4d0*A)
By = E - (B*D )/(2d0*A)
Cy = -(D**2/(4*A) + 1d0)
Y1 = (-By + SQRT(By**2 - 4d0*Ay*Cy) )/(2d0*Ay)
Y2 = (-By - SQRT(By**2 - 4d0*Ay*Cy) )/(2d0*Ay)
X1 = (-B/(2d0*A))*Y1 - D/(2d0*A)
X2 = (-B/(2d0*A))*Y2 - D/(2d0*A)
Yo  = (Y1+Y2)/2d0
Xo  = (X1+X2)/2d0
!
! Calculate the rotation angle
A_m = R_m_n(:,2) - R_m_n(:,1)
B_m = R_m_n(:,3) - R_m_n(:,2)
IF(DOT_PRODUCT(A_m,A_m) < DOT_PRODUCT(B_m,B_m)) THEN
  signAxis = +1d0
ELSE
  signAxis = -1d0
END IF
IF(A < 0d0) THEN
  signFn   = +1d0
ELSE
  signFn   = -1d0
END IF
Yn = (   B /(SQRT(B**2 + (A-C)**2 + Epsilon)))*(-SignFn*signAxis)
Xn = ((A-C)/(SQRT(B**2 + (A-C)**2 + Epsilon)))*(-SignFn*signAxis)
IF(ABS(A-C)>Epsilon .or. ABS(B)>Epsilon) THEN
  Angle = ATAN2D(Yn,Xn)/2d0
ELSE
  Angle = 0d0
END IF
cA = COSD(Angle)
sA = SIND(Angle)
!
! Calculate the major and minor axis ("radii", i.e. not the diameters)
IF(ABS(1d0 - ABS(Angle)/45d0) < 1d-1) THEN
  Af  = (+(cA**2)*B + 2d0*cA*sA*C) / (2d0*((cA**3)*sA + cA*(sA**3)))
  Bf  = (-(sA**2)*B + 2d0*cA*sA*C) / (2d0*((cA**3)*sA + cA*(sA**3)))
ELSE
  Af  = ( (cA**2)*A - (sA**2)*C)/(cA**2 - (sA**2))
  Bf  = (-(sA**2)*A + (cA**2)*C)/(cA**2 - (sA**2))
END IF
Finv  = -(1d0 + ((cA*Xo  + sA*Yo)**2)*Af + ((sA*Xo - cA*Yo)**2)*Bf)
aAxis = SQRT(-(1d0/Af)*Finv)
bAxis = SQRT(-(1d0/Bf)*Finv)

Ellipse%Xo    = Xo
Ellipse%Yo    = Yo
Ellipse%Angle = Angle
Ellipse%Aaxis = Aaxis
Ellipse%Baxis = Baxis
Ellipse%Valid = .true.
RETURN
END FUNCTION EllipseFromQuadraticForm

FUNCTION CurveOfEllipse(Ellipse) RESULT(Curve)
USE NumericalConstants
IMPLICIT NONE
!+
! Description: This function calculates a set of points (x,y1,y2) that lie on the ellipse curve.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
TYPE(Elliptical),INTENT(IN)  :: Ellipse                         ! Ellipse input
TYPE(EllipseCurve)           :: Curve                           ! Ellipse outline points
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: An                              ! Quadratic Form An parameter
REAL(8)                      :: Bn                              ! Quadratic Form Bn parameter
REAL(8)                      :: Cn                              ! Quadratic Form Cn parameter
REAL(8)                      :: cA                              ! Cosine of Angle
REAL(8)                      :: sA                              ! Sine   of Angle
REAL(8)                      :: aRad                            ! Axis in the rotated x direction
REAL(8)                      :: bRad                            ! Axis in the rotated y direction
REAL(8)                      :: discriminant
REAL(8)                      :: Xo                              ! Calculation terms from Excel sheet
REAL(8)                      :: Yo
REAL(8)                      :: a0
REAL(8)                      :: b0
REAL(8)                      :: b1
REAL(8)                      :: c0
REAL(8)                      :: c1
REAL(8)                      :: c2
REAL(8)                      :: aX
REAL(8)                      :: bX
REAL(8)                      :: cX
REAL(8)                      :: Xmin
REAL(8)                      :: Xmax
REAL(8)                      :: Xstart
REAL(8)                      :: Xend
REAL(8)                      :: Ay
INTEGER(4)                   :: n
REAL(8)                      :: X
REAL(8)                      :: By
REAL(8)                      :: Cy
REAL(8)                      :: Y1
REAL(8)                      :: Y2
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
cA    = COSD(Ellipse%Angle)
sA    = SIND(Ellipse%Angle)
aRad  = Ellipse%Aaxis
bRad  = Ellipse%Baxis
Xo    = Ellipse%Xo
Yo    = Ellipse%Yo

An = (cA**2)/(aRad**2) + (sA**2)/(bRad**2)
Bn = (2d0*cA*sA)/(aRad**2)  - (2d0*cA*sA)/(bRad**2)
Cn = (sA**2)/(aRad**2) + (cA**2)/(bRad**2)
!
a0   = Cn
b0   = -2d0*Yo*Cn
b1   = Bn
c0   = Cn*Yo**2 - 1d0
c1   = -Bn*Yo
c2   = An
Ax   = b1**2 - 4d0*a0*c2
Bx   = 2d0*b1*b0 - 4d0*a0*c1
Cx   = b0**2 - 4d0*a0*c0
Xmin = Xo + MIN( (-Bx - SQRT(Bx**2 - 4d0*Ax*Cx))/(2d0*Ax), (-Bx + SQRT(Bx**2 - 4d0*Ax*Cx))/(2d0*Ax))
Xmax = Xo + MAX( (-Bx - SQRT(Bx**2 - 4d0*Ax*Cx))/(2d0*Ax), (-Bx + SQRT(Bx**2 - 4d0*Ax*Cx))/(2d0*Ax))

Xstart = Xmin + Epsilon
Xend   = Xmax - Epsilon
Ay     = Cn

DO n = 1,nCurveEllipse
  X = Xstart + (DBLE(n-1)/DBLE(nCurveEllipse-1)) * (Xend - Xstart)
  By = (2d0*cA*sA*(X-Xo) -2d0*(sA**2)*Yo)/aRad**2 + (-2d0*cA*sA*(X-Xo) -2d0*(cA**2)*Yo)/bRad**2
  Cy = (cA**2 * (X-Xo)**2 - 2d0*cA*sA*(X-Xo)*Yo + sA**2 * Yo**2)/aRad**2  + (sA**2 * (X-Xo)**2  + 2d0*cA*sA*(X-Xo)*Yo + cA**2 * Yo**2)/bRad**2 - 1d0

  discriminant = By**2 - 4d0*Ay*Cy

  IF (discriminant < 0d0) THEN
    Y1 = 0d0
    Y2 = 0d0
  ELSE
    Y1 = (-By + SQRT(discriminant))/(2d0*Ay)
    Y2 = (-By - SQRT(discriminant))/(2d0*Ay)
  END IF

  Curve%X_n(n) = X
  Curve%Y1_n(n) = Y1
  Curve%Y2_n(n) = Y2

END DO

Curve%Xmin = Xmin
Curve%Xmax = Xmax

RETURN
END FUNCTION CurveOfEllipse

FUNCTION QuadraticFunctionEstimate(X_n, Y_n, W_n, nEnd) RESULT(QuadraticForm)
!+
! Description: This function calculates 5 coefficients (A,B,C,D and E) of the following equation in x and y:
!
!                     y = A*x^2 + B*xy + C*y^2 + D*x + E
!
!              The coefficients are calculated by using a Weight Least Squares estimate of the coefficients that best
!              fit the boundary points, X_n and Y_n with associated weighting W_n, passed as arguments to this Function%
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)    ,INTENT(IN)    :: nEnd                          ! Size of tables
REAL(8)       ,INTENT(IN)    :: X_n(nEnd)                     ! x values
REAL(8)       ,INTENT(IN)    :: Y_n(nEnd)                     ! y values
REAL(8)       ,INTENT(INOUT) :: W_n(nEnd)                     ! Weight of the values
TYPE(Quadratic)              :: QuadraticForm                 ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                             ! Index through values
REAL(8)                      :: T_m_m(5,5)                    ! Jacobian
REAL(8)                      :: U_m_m(5,5)                    ! Inverse of the Jacobian
REAL(8)                      :: p_m(5)                        ! Right hand side vector
REAL(8)                      :: q_m(5)                        ! Solution of the equation
LOGICAL(4)                   :: FullRank = TRUE               ! Jacobian is full rank
INTEGER(4)                   :: nWorst                        ! The worst data point index
REAL(8)                      :: dFworst                       ! The worst function value
REAL(8)                      :: dF                            ! Function value
REAL(8)                      :: dFavg                         ! Average residual
INTEGER(4)                   :: nAvg                          ! Number of samples in average residual
REAL(8)   ,ALLOCATABLE       :: dF_n(:)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IF(nEnd < 1) THEN
  QuadraticForm%A = 0
  QuadraticForm%B = 0
  QuadraticForm%C = 0
  QuadraticForm%D = 0
  QuadraticForm%E = 0
  QuadraticForm%degenerate = TRUE
  RETURN
END IF

ALLOCATE(dF_n(nEnd))

DO

  T_m_m(1,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n**2), DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n**2,X_n), DOT_PRODUCT(W_n*X_n,X_n)/)
  T_m_m(2,:) = (/DOT_PRODUCT(W_n*X_n**3,Y_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*X_n**2,Y_n), DOT_PRODUCT(W_n*X_n,Y_n)/)
  T_m_m(3,:) = (/DOT_PRODUCT(W_n*X_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**3)   , DOT_PRODUCT(W_n*Y_n**2,Y_n**2), DOT_PRODUCT(W_n*X_n,Y_n**2), DOT_PRODUCT(W_n*Y_n,Y_n)/)
  T_m_m(4,:) = (/DOT_PRODUCT(W_n*X_n**2,X_n)   , DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*X_n,X_n)   , DOT_PRODUCT(W_n,X_n   )/)
  T_m_m(5,:) = (/DOT_PRODUCT(W_n*X_n   ,X_n)   , DOT_PRODUCT(W_n*X_n,Y_n   )   , DOT_PRODUCT(W_n*Y_n,Y_n   )   , DOT_PRODUCT(W_n,X_n)       , SUM(W_n)                /)

  p_m        = (/DOT_PRODUCT(W_n*X_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n**2)   , DOT_PRODUCT(W_n*Y_n**2,Y_n)   , DOT_PRODUCT(W_n*X_n,Y_n)   , SUM(W_n*Y_n)            /)

  U_m_m = InverseCholeskyNxN(T_m_m, 5, FullRank) ! I_m_m = MATMUL(U_m_m, T_m_m)

  IF(FullRank) THEN
    q_m = MATMUL(U_m_m,p_m)
    QuadraticForm%A = q_m(1)
    QuadraticForm%B = q_m(2)
    QuadraticForm%C = q_m(3)
    QuadraticForm%D = q_m(4)
    QuadraticForm%E = q_m(5)
    QuadraticForm%degenerate = FALSE
  ELSE
    QuadraticForm%A = 0
    QuadraticForm%B = 0
    QuadraticForm%C = 0
    QuadraticForm%D = 0
    QuadraticForm%E = 0
    QuadraticForm%degenerate = TRUE
    EXIT
  ENDIF

  dFavg = 0d0
  nAvg  = 0
  nWorst = 0
  dFworst = 0
  DO n = 1, nEnd
    dF_n(n) = 0d0
    IF(W_n(n) == 0d0) CYCLE
    dF = QuadraticForm%A * x_n(n)**2        &
       + QuadraticForm%B * x_n(n)*y_n(n)    &
       + QuadraticForm%C * y_n(n)**2        &
       + QuadraticForm%D * x_n(n)           &
       + QuadraticForm%E                    &
       - y_n(n)
    dF_n(n) = dF
    dFavg = dFavg + ABS(dF)
    nAvg  = nAvg + 1
    IF(ABS(dF) > dFworst) THEN
      dFworst = ABS(dF)
      nWorst = n
    END IF
  END DO
  IF (nWorst == 0) EXIT
  IF(dFworst < 3d0*(dFavg/(DBLE(nAvg) + 1d-12))) EXIT
  W_n(nWorst) = 0d0
END DO
DEALLOCATE(dF_n)
RETURN
END FUNCTION QuadraticFunctionEstimate

FUNCTION SolveQuadraticEquation(A,B,C,Error) RESULT(X_n)
!+
! Description:  This function calculates the 2 real solutions to the quadratic equation:
!
!                     0 = A*x^2 + B*x + C
!
!               If the solution is complex then the Error flag is set and 0 is returned
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)       ,INTENT(IN)    :: A                               ! A coefficient for the quadratic equation
REAL(8)       ,INTENT(IN)    :: B                               ! B coefficient for the quadratic equation
REAL(8)       ,INTENT(IN)    :: C                               ! C coefficient for the quadratic equation
LOGICAL(4)    ,INTENT(OUT)   :: Error                           ! Error Flag
REAL(8)                      :: X_n(2)                          ! x solution values
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: Discriminant                    ! Discriminant B^2 - 4*A*C
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(A == 0d0) THEN
    IF(B == 0d0) THEN
      X_n = 0d0  ; Error = .true.
    ELSE
      X_n = -C/B ; Error = .false.
    END IF
  ELSE
    Discriminant = B**2 - 4d0*A*C
    IF(Discriminant < 0d0) THEN
      X_n = 0d0 ; Error = .true.
    ELSE
      X_n = ( -B + (/1d0, -1d0/)*SQRT(Discriminant) )/(2d0*A) ; Error = .false.
    END IF
  END IF
  RETURN

END FUNCTION SolveQuadraticEquation

FUNCTION QuadraticConstrainedEstimate(X_n, Y_n, W_n, nEnd, Ystart, Ystop, Wo, Ao, Bo, Co) RESULT(QuadraticForm)
!+
! Description:  Finds constants A, B, C which are a constrained least squares fit to the equation over the set {X_n, Y_n}
!
!                     x = A(y - yMid)^2 + B(y - yStart) + C
!
!               with soft constraints Ao, Bo and Co.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)    ,INTENT(IN)    :: nEnd                          ! Size of tables
REAL(8)       ,INTENT(IN)    :: X_n(nEnd)                     ! x values (pixels)
REAL(8)       ,INTENT(IN)    :: Y_n(nEnd)                     ! y values (pixels)
REAL(8)       ,INTENT(IN)    :: Ystart                        ! Origin for Y (pixels)
REAL(8)       ,INTENT(IN)    :: Ystop                         ! Endpoint for Y (pixels)
REAL(8)       ,INTENT(INOUT) :: W_n(nEnd)                     ! Weight of the values
REAL(8)       ,INTENT(IN)    :: Wo                            ! Weight of the soft constraints
REAL(8)       ,INTENT(IN)    :: Ao                            ! Soft constraint for A
REAL(8)       ,INTENT(IN)    :: Bo                            ! Soft constraint for B
REAL(8)       ,INTENT(IN)    :: Co                            ! Soft constraint for C
TYPE(Quadratic)              :: QuadraticForm                 ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                             ! Index through values
REAL(8),ALLOCATABLE          :: Ym_n(:)                       ! Distance Mid point of Ystart and Yend (pixels)
REAL(8),ALLOCATABLE          :: Ys_n(:)                       ! Distance from Ystart of (pixels)
REAL(8)                      :: T_m_m(3,3)                    ! Jacobian
REAL(8)                      :: U_m_m(3,3)                    ! Inverse of the Jacobian
REAL(8)                      :: p_m(3)                        ! Right hand side vector
REAL(8)                      :: q_m(3)                        ! Solution of the equation
LOGICAL(4)                   :: FullRank = TRUE               ! Jacobian is full rank
INTEGER(4)                   :: nWorst                        ! The worst data point index
REAL(8)                      :: fWorst                        ! The worst function value
REAL(8)                      :: f                             ! Function value
REAL(8)                      :: dFavg                         ! Average residual
INTEGER(4)                   :: nAvg                          ! Number of samples in average residual
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ALLOCATE(Ym_n(nEnd),Ys_n(nEnd))
Ym_n = Y_n - (Ystart + Ystop)/2d0
Ys_n = Y_n - Ystart
W_n = 1d0
DO

  T_m_m(1,:) = (/DOT_PRODUCT(W_n*Ym_n**2,Ym_n**2), DOT_PRODUCT(W_n*Ym_n**2,Ys_n), DOT_PRODUCT(W_n,Ym_n**2)/) + (/ Wo, 0d0, 0d0/)
  T_m_m(2,:) = (/DOT_PRODUCT(W_n*Ym_n**2,Ys_n   ), DOT_PRODUCT(W_n*Ys_n,Ys_n   ), DOT_PRODUCT(W_n,Ys_n)   /) + (/0d0,  Wo, 0d0/)
  T_m_m(3,:) = (/DOT_PRODUCT(W_n,Ym_n**2        ), DOT_PRODUCT(W_n,Ys_n        ), SUM(W_n)                /) + (/0d0, 0d0, Wo /)

  p_m        = (/DOT_PRODUCT(W_n*X_n,Ym_n**2)    , DOT_PRODUCT(W_n*X_n,Ys_n)    , DOT_PRODUCT(W_n,X_n)    /) + (/ Ao,  Bo,  Co/) * Wo

  U_m_m = InverseCholeskyNxN(T_m_m, 3, FullRank) ! I_m_m = MATMUL(U_m_m, T_m_m)

  IF(FullRank) THEN
    q_m = MATMUL(U_m_m,p_m)
    QuadraticForm%A = q_m(1)
    QuadraticForm%B = q_m(2)
    QuadraticForm%C = q_m(3)
    QuadraticForm%D = 0d0
    QuadraticForm%E = 0d0
    QuadraticForm%degenerate = FALSE
    EXIT
  ELSE
    QuadraticForm%A = 0
    QuadraticForm%B = 0
    QuadraticForm%C = 0
    QuadraticForm%D = 0
    QuadraticForm%E = 0
    QuadraticForm%degenerate = TRUE
  ENDIF

  dFavg = 0d0
  nAvg  = 0
  nWorst = 0
  fWorst = 0
  DO n = 1, nEnd
    IF(W_n(n) == 0d0) CYCLE
    f = QuadraticForm%A * Ym_n(n)**2 &
      + QuadraticForm%B * Ys_n(n)    &
      + QuadraticForm%C              &
      - x_n(n)
    IF(ABS(f) > fWorst) THEN
      fWorst = ABS(f)
      IF(fWorst > 5) THEN
        nWorst = n
      END IF
    END IF
    nAvg = nAvg + 1
    dFavg = dFavg + ABS(f)
  END DO
  IF (nWorst == 0) EXIT
  IF(fWorst < 3d0*(dFavg/(DBLE(nAvg) + 1d-12))) EXIT
  W_n(nWorst) = 0d0
END DO

RETURN
END FUNCTION QuadraticConstrainedEstimate

FUNCTION LinearFunctionEstimate(X_n, Y_n, W_n, nEnd) RESULT(LinearForm)
!+
! Description:
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)    ,INTENT(IN)    :: nEnd                          ! Size of tables
REAL(8)       ,INTENT(IN)    :: X_n(nEnd)                     ! x values
REAL(8)       ,INTENT(IN)    :: Y_n(nEnd)                     ! y values
REAL(8)       ,INTENT(INOUT) :: W_n(nEnd)                     ! Weight of the values
TYPE(Quadratic)              :: LinearForm                    ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                             ! Index through values
REAL(8)                      :: T_m_m(2,2)                    ! Jacobian
REAL(8)                      :: U_m_m(2,2)                    ! Inverse of the Jacobian
REAL(8)                      :: p_m(2)                        ! Right hand side vector
REAL(8)                      :: q_m(2)                        ! Solution of the equation
LOGICAL(4)                   :: FullRank = TRUE               ! Jacobian is full rank
INTEGER(4)                   :: nWorst                        ! The worst data point index
REAL(8)                      :: fWorst                        ! The worst function value
REAL(8)                      :: f                             ! Function value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
W_n = 1d0
DO

  T_m_m(1,:) = (/DOT_PRODUCT(W_n*X_n,X_n), DOT_PRODUCT(W_n,X_n)/)
  T_m_m(2,:) = (/DOT_PRODUCT(W_n,X_n)    , SUM(W_n)            /)

  p_m        = (/DOT_PRODUCT(W_n*X_n,Y_n), DOT_PRODUCT(W_n,Y_n)/)

  U_m_m = InverseCholeskyNxN(T_m_m, 2, FullRank) ! I_m_m = MATMUL(U_m_m, T_m_m)

  IF(FullRank) THEN
    q_m = MATMUL(U_m_m,p_m)
    LinearForm%A = q_m(1)
    LinearForm%B = q_m(2)
    LinearForm%C = 0
    LinearForm%D = 0
    LinearForm%E = 0
    LinearForm%degenerate = FALSE
    EXIT
  ELSE
    LinearForm%A = 0
    LinearForm%B = 0
    LinearForm%C = 0
    LinearForm%D = 0
    LinearForm%E = 0
    LinearForm%degenerate = TRUE
  ENDIF

  nWorst = 0
  fWorst = 0
  DO n = 1, nEnd
    IF(W_n(n) == 0d0) CYCLE
    f = LinearForm%A * x_n(n) &
      + LinearForm%B          &
      - y_n(n)
    IF(ABS(f) > fWorst) THEN
      fWorst = ABS(f)
      IF(fWorst > 3) THEN
        nWorst = n
      END IF
    END IF
  END DO
  IF (nWorst == 0) EXIT
  W_n(nWorst) = 0d0
END DO

RETURN
END FUNCTION LinearFunctionEstimate

FUNCTION ConstantFunctionEstimate(Y_n, W_n, nEnd) RESULT(LinearForm)
!+
! Description:
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
USE NumericalConstants
USE MatrixVectorPackage
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)    ,INTENT(IN)    :: nEnd                          ! Size of tables
REAL(8)       ,INTENT(IN)    :: Y_n(nEnd)                     ! y values
REAL(8)       ,INTENT(INOUT) :: W_n(nEnd)                     ! Weight of the values
TYPE(Quadratic)              :: LinearForm                    ! Average value with quality indicator
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: n                             ! Index through values
INTEGER(4)                   :: nWorst                        ! The worst data point index
REAL(8)                      :: fWorst                        ! The worst function value
REAL(8)                      :: f                             ! Function value
REAL(8)                      :: Yavg                          ! Estimate of constant value
REAL(8)                      :: dFavg                         ! Average residual
INTEGER(4)                   :: nAvg                          ! Number of samples in average residual
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
W_n = 1d0
DO
  Yavg = DOT_PRODUCT(W_n,Y_n)/(SUM(W_n) + 1d-12)

  dFavg = 0d0
  nAvg  = 0
  nWorst = 0
  fWorst = 0
  DO n = 1, nEnd
    IF(W_n(n) == 0d0) CYCLE
    f = Yavg - y_n(n)
    IF(ABS(f) > fWorst) THEN
      fWorst = ABS(f)
      IF(fWorst > 3) THEN
        nWorst = n
      END IF
    END IF
    dFavg = dFavg + ABS(f)
    nAvg  = nAvg + 1
  END DO
  IF(nWorst == 0) EXIT
  IF(fWorst < 3d0*(dFavg/(DBLE(nAvg) + 1d-12))) EXIT
  W_n(nWorst) = 0d0
END DO
LinearForm%A = 0
LinearForm%B = Yavg
LinearForm%C = 0
LinearForm%D = 0
LinearForm%E = 0
LinearForm%Degenerate = .false.
RETURN
END FUNCTION ConstantFunctionEstimate

FUNCTION ValueMaxFromDataTypeAndMask(Value_dat, Status_dat, Type_dat, MaskType, MaskGood, datEnd) RESULT(ValueResult)
!+
! Description: This function determines the maximum value of a set of data.  Only the elements of the data set with
!              Status data that match MaskGood and Type data that match a MaskType are included in the determination.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of tables
REAL(8)        , INTENT(IN)  :: Value_dat (datEnd)              ! Table of data values
INTEGER(4)     , INTENT(IN)  :: Status_dat(datEnd)              ! Table of data quality
INTEGER(4)     , INTENT(IN)  :: Type_dat  (datEnd)              ! Table of data type
INTEGER(4)     , INTENT(IN)  :: MaskType                        ! Mask for data usage
INTEGER(4)     , INTENT(IN)  :: MaskGood                        ! Mask for good data quality
TYPE(ValueAverage)           :: ValueResult                     ! Average value with quality indicator
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ValueResult%NumGood = COUNT( IAND(Type_dat,MaskType)/=0 .and. IAND(Status_dat, MaskGood)/=0 )
IF(ValueResult%NumGood > 0) THEN
  ValueResult%Value    = MAXVAL(Value_dat, IAND(Type_dat, MaskType)/=0 .and. IAND(Status_dat,MaskGood)/=0)
  ValueResult%Quality  = 1
ELSE
  ValueResult%Value    = 0
  ValueResult%Quality  = 2
END IF
RETURN
END FUNCTION ValueMaxFromDataTypeAndMask

SUBROUTINE WeightedLeastSquaresEstimate(MeasEnd        &
                                       ,Ztelex_Meas    &
                                       ,Weight_Meas    &
                                       ,BadData_Meas   &
                                       ,ZBias_Sign     &
                                       ,BadDataLimit   &
                                       ,ZEstimate      &
                                       ,Error)

!+
!
! Description:
!     This routine performs Linear Weighted Least Squares Estimation whenever
!     we have a single state variable and the Jacobian is the unity vector.
!
! Parameters:
!
!     MeasEnd
!        Number of measurements (Input)
!
!     Ztelex_Meas
!        Measurement Vector (Input)
!
!     Weight_Meas
!        Measurement Error Standard Deviation Inverse Vector. (Input)
!        Must be greater than .001
!
!     BadData_Meas
!        Measurement Logical Bad Data Flag Vector (Input/Output)
!
!     ZBias_Sign
!        Bias used to compensate for data that is biased positive or negative(Input)
!
!     BadDataLimit
!        Number of bad data allowed (Input)
!
!     ZEstimate
!        WLS Estimate of the State Variable (Input)
!
!     Error
!        Output Error Code (Output)
!          0  - No Error
!         -1  - MeasEnd outside range [1 .. MeasMax]
!         -2  - A Measurement Weight is less than .001
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE

!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-----------------------------------------------------
INTEGER(4), INTENT(IN)          :: MeasEnd                        ! Number Of Measurements
INTEGER(4), INTENT(IN)          :: BadDataLimit                   ! Number Of Bad Data to check for (Max)
REAL   (8), INTENT(IN)          :: ZBias_Sign(2)                  ! Bias on the measurement and the estimate
REAL   (8), INTENT(IN)          :: Ztelex_Meas (MeasEnd)          ! Array  Of Measurements
REAL   (8), INTENT(IN)          :: Weight_Meas (MeasEnd)          ! Measurement Covariance
LOGICAL(4), INTENT(INOUT)       :: BadData_Meas(MeasEnd)          ! Measurement Bad Data Flag
REAL   (8), INTENT(OUT)         :: ZEstimate                      ! WLS Estimate
INTEGER(4), INTENT(OUT)         :: Error                          ! Error Return (0 - OK)

!                                Local
!-------------------------------+---------------------------------+-----------------------------------------------------
INTEGER(4), PARAMETER           :: Meas$MX           = 128        ! Maximum Number of Measurements
REAL   (8), PARAMETER           :: ToleranceChiValue = 9d0        ! BadData tolerance (no. standard deviations)
INTEGER(4)                      :: Meas                           ! Index through Measurements
INTEGER(4)                      :: MeasBad                        ! Measurement with the largest Error
REAL   (8), ALLOCATABLE         :: W_Meas(:)                      ! Local copy of Weight_Meas
REAL   (8)                      :: WsumInverse                    ! Inverse sum of Weights inverse
REAL   (8)                      :: Residual                       ! Residual (difference between telemetry & estimate)
REAL   (8)                      :: ResidualNorm                   ! Normalized Residual
REAL   (8)                      :: ResidualNormMax                ! Largest Normalized Residual
INTEGER(4)                      :: BadData                        ! Number of bad data detected
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
IF(MeasEnd <= 0 .or. MeasEnd > Meas$MX)        THEN ; Error = -1  ; RETURN
ELSE IF(MINVAL(Weight_Meas) < WeightVerySmall) THEN ; Error = -2  ; RETURN
ELSE                                                ; Error =  0
END IF
ALLOCATE(W_Meas(MeasEnd))

BadData_Meas = .FALSE.
W_Meas       = Weight_Meas
BadData      = COUNT(Weight_Meas==WeightVerySmall)

DO
  ZEstimate = SUM(W_Meas*Ztelex_Meas)/SUM(W_Meas)

  ResidualNormMax = 0d0
  WsumInverse = 1d0/SUM(W_Meas)
  DO Meas = 1, MeasEnd
    IF(W_Meas(Meas) == WeightVerySmall) CYCLE
    IF(ZTelex_Meas(Meas) > Zestimate) THEN
      Residual     = ((ZTelex_Meas(Meas)+ZBias_Sign(1)) - Zestimate)**2
    ELSE
      Residual     = ((ZTelex_Meas(Meas)+ZBias_Sign(2)) - Zestimate)**2
    END IF
    ResidualNorm = Residual*W_Meas(Meas)/(1d0 - W_Meas(Meas)*WsumInverse)
    IF(ResidualNorm > ToleranceChiValue) THEN
      IF(ResidualNorm > ResidualNormMax) THEN
        ResidualNormMax = ResidualNorm
        MeasBad = Meas
      END IF
    END IF
  END DO

  IF(ResidualNormMax > 0d0) THEN
    W_Meas      (MeasBad) = WeightVerySmall
    BadData_Meas(MeasBad) = .TRUE.
    BadData               = BadData + 1
  END IF
  IF(BadData == BadDataLimit .or. ResidualNormMax==0d0) EXIT
END DO

DEALLOCATE(W_Meas)
RETURN
END SUBROUTINE WeightedLeastSquaresEstimate

SUBROUTINE PutValueIntoTable(Val, dat, datEnd, Val_dat)
!+
! Description: This subroutine puts a value into an array at a specified location.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)        , INTENT(IN)  :: Val                             ! Value to put into the table
INTEGER(4)     , INTENT(IN)  :: dat                             ! Index into table where value is to be placed
INTEGER(4)     , INTENT(IN)  :: datEnd                          ! Size of table
REAL(8)                      :: Val_dat (datEnd)                ! Table of data values
!
!                             Local Variables
!----------------------------+---------------
! None
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
IF(WithinClosedSet(1, dat, datEnd)) Val_dat(dat) = Val
RETURN
END SUBROUTINE PutValueIntoTable

FUNCTION NamedArgument(NameArgument) RESULT(AsciiArgument)
!+
!
! Description: This function returns a single named command line argument value.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*)                    :: NameArgument
CHARACTER(256)                  :: AsciiArgument
!
!                                Local Variables
!-------------------------------+---------------
CHARACTER(32)                   :: Ascii_Arg(32)                  !Actual arguments
INTEGER(4)                      :: ArgEnd
INTEGER(4)                      :: Arg

  AsciiArgument = ""
  CALL GetCommandLineArguments(ArgEnd, Ascii_Arg)
  PRINT *," ArgEnd:    ",ArgEnd
  PRINT *," Ascii_Arg: ",Ascii_Arg
  Arg = 0
  DO WHILE(Arg < ArgEnd)
    Arg = Arg + 1
    !PRINT *,Ascii_Arg(Arg)
    IF (TRIM(ADJUSTL(UpperCaseAscii(Ascii_Arg(Arg)))) == TRIM(ADJUSTL(UpperCaseAscii(NameArgument)))) THEN
      IF(Arg < ArgEnd) AsciiArgument = TRIM(ADJUSTL(Ascii_Arg(Arg+1)))
      RETURN
    END IF
  END DO

END FUNCTION NamedArgument

SUBROUTINE GetCommandLineArguments(ArgEnd, Ascii_Arg)
!+
!
! Description: This subroutine returns the number of command line arguments, and the arguments themselves
!              in a character array.
!
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+---------------------------------------------------------
CHARACTER(*),INTENT(OUT)      :: Ascii_Arg(32)                  ! Command line arguments (up to 32)
INTEGER(4),  INTENT(OUT)      :: ArgEnd                         ! Actual number of command line arguments
!
!                                Local
!-------------------------------+---------------
INTEGER(4)                    :: Arg                            ! Index through the command line arguments
INTEGER(4)                    :: ArgCount                       ! Count of command line arguments
INTEGER(4)                    :: AsciiLength                    ! Length of a command line argument
INTEGER(4)                    :: StatusGet                      ! Status of GET_COMMAND_ARGUMENT
CHARACTER(64)                 :: Ascii                          ! Command line argument
!
!---------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!---------------------------------------------------------------------------------------------------
!
  ArgEnd = 0

  ArgCount = COMMAND_ARGUMENT_COUNT()
  WRITE (*,FMT='(" Number of Command Arguments: ",I2)') ArgCount
  IF (ArgCount > 32) THEN           ! Make sure we don't overflow our array here
    RETURN
  END IF

  DO Arg = 1, ArgCount
    CALL GET_COMMAND_ARGUMENT (Arg, Ascii, AsciiLength, StatusGet)
    PRINT *,TRIM(ADJUSTL(Ascii))
    Ascii_Arg(Arg) = TRIM(ADJUSTL(Ascii))
    IF (statusGet .ne. UTIL_S_NORMAL) THEN
      WRITE (*,FMT='(" GET_COMMAND_ARGUMENT(",I2") Failed, Status: ",I4)') Arg,StatusGet
      RETURN
    END IF
  END DO
  ArgEnd = ArgCount
  RETURN

END SUBROUTINE GetCommandLineArguments

FUNCTION SystemCommand(Command, Echo) RESULT(StatusReturned)
!+
!
! Description: This function executes a system command
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 05-FEB-2025     | MA    | SCR 150: Remove dependence on Intel Fortran Extension, SYSTEM,  use EXECUTE_COMMAND_LINE
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                 Passed                            Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)        :: Command                        ! system command to execute
LOGICAL(4)  , INTENT(IN)        :: Echo                           ! echo the command output to STDOUT
OPTIONAL                        :: Echo                           ! echo argument cane be omitted
INTEGER(4)                      :: StatusReturned                 ! system call return status
!
!                                Local
!-------------------------------+---------------
INTEGER(4)                      :: StatusExit                     ! command return status
CHARACTER(8)                    :: StatusExitHex                  ! Exist status formatted as hex
INTEGER(4)                      :: StatusCmd                      ! status of executing the command
CHARACTER(255)                  :: MessageCmd                     ! command error message
LOGICAL(4)                      :: EchoCommand                    ! local copy of Echo(defaults to false)
CHARACTER(32)                   :: Pipe                           ! Optional pipe of output
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! Ensure that the command is not empty
  IF(LEN_TRIM(Command) < 1) THEN
    WRITE(*,FMT='(A)') "SystemCommand: Empty command sent"
    StatusReturned = UTIL_E_FAILURE
    RETURN
  END IF

  ! Ensure command output is piped properly
  IF(PRESENT(Echo)) THEN
    EchoCommand = Echo
  ELSE
    EchoCommand = .false.
  END IF
  Pipe = " "
  IF(INDEX(Command,">",BACK=.true.) == 0) THEN
    IF(.not.EchoCommand) THEN
      Pipe = " >> C:\temp\scratch.out"            ! Generalize when we have multiplethreads  - Each thread will have its own scratch.out
    END IF                                        !  e.g. You can get your UnitDebug, UnitPiping UnitSTDOUT etc for each thread, EPHMRS, MONITOR, etc
  END IF

  ! Initialize the Command Error Message
  MessageCmd = " "

  ! Issue the Command
  CALL EXECUTE_COMMAND_LINE(TRIM(Command//Pipe), WAIT =.true., EXITSTAT = StatusExit, CMDSTAT= StatusCmd, CMDMSG = MessageCmd)

  ! Print any diagnostics
  IF(StatusCmd > 0) THEN
    WRITE(*,FMT='(A)') "SystemCommand: "//TRIM(Command)//" failed with error "//TRIM(MessageCmd)
    StatusReturned = UTIL_E_FAILURE
  ELSE IF(StatusCmd < 0) THEN
    WRITE(*,FMT='(A)') "SystemCommand: "//TRIM(Command)//" is not supported"
    StatusReturned = UTIL_E_FAILURE
  ELSE IF(StatusExit /= 0) THEN
    WRITE(StatusExitHex,FMT='(Z8.8)') StatusExit
    WRITE(*,FMT='(A)') "SystemCommand: "//TRIM(Command)//" failed with status = "//StatusExitHex//"z"
    StatusReturned = UTIL_E_FAILURE
  ELSE
    StatusReturned = UTIL_S_NORMAL
  END IF

  RETURN
END FUNCTION SystemCommand

FUNCTION LogBase(Number, Base)  RESULT(Logarithm)
!+
!
! Description: This function returns the log of a number in a given base
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!IMPLICIT NONE
!                                  Passed                           Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)             :: Number                         ! number to take the logarithm of
REAL(8), INTENT(IN)             :: Base                           ! base for the logarithm
REAL(8)                         :: Logarithm                      ! logarithm value

!                                Local
!-------------------------------+---------------
! None
!
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(Base <= 0 .or. Number <= 0) THEN
    Logarithm = 0d0
  ELSE
    Logarithm = LOG(Number)/LOG(Base)
  END IF
END FUNCTION LogBase

FUNCTION NdxSortID(ID_ndx, ndxEnd)  RESULT(ndxSort_ndx)
!+
!
! Description:
!
!     This routine performs a shell sort on a list of device IDs to construct
!     a parallel pointer table which effectively sorts the devices in
!     alphabetical order.  This table is used for binary searching to locate
!     the device index given the ID.
!
! Parameters:
!
!     ID:
!    The device ID (input)
!
!     ID_ndx:
!    The table of device IDs (input).
!
!     ndx sort_ndx
!    A parallel table of sorted device IDs (input)
!
!     ndxEnd
!    number of devices (input)
!
! Design:
!     This routine performs a shell sort.  It records the order of sorting in a separate table and does
!     not disturb the input table.

! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                      :: ndxEnd                         ! number of devices.
CHARACTER(*)                    :: ID_ndx     (ndxEnd)            ! Table of valid device IDs
INTEGER(4)                      :: ndxSort_ndx(ndxEnd)            ! sorted order of ID_ndx
!
!                                Local
!-------------------------------+---------------
LOGICAL(4)                      :: done                           ! Done flag
INTEGER(4)                      :: n                              ! log base 2 of sub table size to sort
INTEGER(4)                      :: k                              ! counter
INTEGER(4)                      :: itop                           ! loop boundary
INTEGER(4)                      :: ndxSortj                       ! ptr to free lowest entry
INTEGER(4)                      :: ndxSortl                       ! ptr to contender for lowest entry
INTEGER(4)                      :: ndxj                           ! actual free lowest entry in table
INTEGER(4)                      :: ndxl                           ! actual contender for lowest entry
INTEGER(4)                      :: ndx                            ! index thru devices
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  !
  ! set up initial ordering
  DO ndx= 1, ndxEnd
    ndxSort_ndx(ndx) = ndx
  END DO

  n = 2
  DO  WHILE(n <= ndxEnd)
    n = 2*n
  END DO

  done = .false.

  DO WHILE( .not. done)
    n = (n-1)/2
    IF(n == 0) THEN
       done = .true.
    ELSE
      itop = ndxEnd- n
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxSortl = ndxSortj + n
          ndxj     = ndxSort_ndx(ndxSortj)
          ndxl     = ndxSort_ndx(ndxSortl)

          IF(ID_ndx(ndxl) < ID_ndx(ndxj)) THEN
            ndxSort_ndx(ndxSortj) = ndxl
            ndxSort_ndx(ndxSortl) = ndxj
            ndxSortj              = ndxSortj - n
          ELSE
            ndxSortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  RETURN
END FUNCTION NdxSortID

SUBROUTINE SortID2(ID_ndx, ndxSort_ndx, ndxEnd)
!+
!
! Description:
!
!     This routine performs a shell sort on a list of device IDs to construct
!     a parallel pointer table which effectively sorts the devices in
!     alphabetical order.  This table is used for binary searching to locate
!     the device index given the ID.
!
! Parameters:
!
!
!     ID:
!    The device ID (input)
!
!     ID_ndx:
!    The table of device IDs (input).
!
!     ndx sort_ndx
!    A parallel table of sorted device IDs (input)
!
!     ndxEnd
!    number of devices (input)
!
! Design:
!     This routine performs a shell sort.  It records the order of sorting in a separate table and does
!     not disturbe the input table.

! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
INTEGER(2)                      :: ndxEnd                         ! number of devices.
CHARACTER(*)                    :: ID_ndx     (ndxEnd)            ! Table of valid device IDs
INTEGER(2)                      :: ndxSort_ndx(ndxEnd)            ! sorted order of ID_ndx
!
!                                Local
!-------------------------------+---------------
LOGICAL(4)                      :: done                           ! Done flag
INTEGER(4)                      :: n                              ! log base 2 of sub table size to sort
INTEGER(4)                      :: k                              ! counter
INTEGER(4)                      :: itop                           ! loop boundary
INTEGER(4)                      :: ndxSortj                       ! ptr to free lowest entry
INTEGER(4)                      :: ndxSortl                       ! ptr to contender for lowest entry
INTEGER(4)                      :: ndxj                           ! actual free lowest entry in table
INTEGER(4)                      :: ndxl                           ! actual contender for lowest entry
INTEGER(4)                      :: ndx                            ! index thru devices
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  !
  ! set up initial ordering
  DO ndx= 1, ndxEnd
    ndxSort_ndx(ndx) = INT(ndx, KIND=2)
  END DO

  n = 2
  DO  WHILE(n <= ndxEnd)
    n = 2*n
  END DO

  done = .false.

  DO WHILE( .not. done)
    n = (n-1)/2
    IF(n == 0) THEN
       done = .true.
    ELSE
      itop = ndxEnd- n
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxSortl = ndxSortj + n
          ndxj     = ndxSort_ndx(ndxSortj)
          ndxl     = ndxSort_ndx(ndxSortl)

          IF(ID_ndx(ndxl) < ID_ndx(ndxj)) THEN
            ndxSort_ndx(ndxSortj) = INT(ndxl, KIND=2)
            ndxSort_ndx(ndxSortl) = INT(ndxj, KIND=2)
            ndxSortj              = ndxSortj - n
          ELSE
            ndxSortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  RETURN
END SUBROUTINE SortID2

SUBROUTINE SortID4(ID_ndx, ndxSort_ndx, ndxEnd)
!+
!
! Description:
!
!     This routine performs a shell sort on a list of device IDs to construct
!     a parallel pointer table which effectively sorts the devices in
!     alphabetical order.  This table is used for binary searching to locate
!     the device index given the ID.
!
! Parameters:
!
!
!     ID:
!    The device ID (input)
!
!     ID_ndx:
!    The table of device IDs (input).
!
!     ndx sort_ndx
!    A parallel table of sorted device IDs (input)
!
!     ndxEnd
!    number of devices (input)

! Design:
!     This routine performs a shell sort.  It records the order of sorting in a separate table and does
!     not disturbe the input table.

! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
INTEGER(4)                      :: ndxEnd                         ! number of devices.
CHARACTER(*)                    :: ID_ndx     (ndxEnd)            ! Table of valid device IDs
INTEGER(4)                      :: ndxSort_ndx(ndxEnd)            ! sorted order of ID_ndx
!
!                                Local
!-------------------------------+---------------
LOGICAL(4)                      :: done                           ! Done flag
INTEGER(4)                      :: n                              ! log base 2 of sub table size to sort
INTEGER(4)                      :: k                              ! counter
INTEGER(4)                      :: itop                           ! loop boundary
INTEGER(4)                      :: ndxSortj                       ! ptr to free lowest entry
INTEGER(4)                      :: ndxSortl                       ! ptr to contender for lowest entry
INTEGER(4)                      :: ndxj                           ! actual free lowest entry in table
INTEGER(4)                      :: ndxl                           ! actual contender for lowest entry
INTEGER(4)                      :: ndx                            ! index thru devices
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  !
  ! set up initial ordering
  DO ndx= 1, ndxEnd
    ndxSort_ndx(ndx) = ndx
  END DO

  n = 2
  DO  WHILE(n <= ndxEnd)
    n = 2*n
  END DO

  done = .false.

  DO WHILE( .not. done)
    n = (n-1)/2
    IF(n == 0) THEN
       done = .true.
    ELSE
      itop = ndxEnd- n
      DO k = 1, itop
        ndxSortj = k
        DO WHILE(ndxSortj > 0)
          ndxSortl = ndxSortj + n
          ndxj     = ndxSort_ndx(ndxSortj)
          ndxl     = ndxSort_ndx(ndxSortl)

          IF(ID_ndx(ndxl) < ID_ndx(ndxj)) THEN
            ndxSort_ndx(ndxSortj) = ndxl
            ndxSort_ndx(ndxSortl) = ndxj
            ndxSortj              = ndxSortj - n
          ELSE
            ndxSortj = 0
          END IF
        END DO
      END DO
    END IF
  END DO
  RETURN
END SUBROUTINE SortID4

FUNCTION NdxOfID(ID, ID_ndx, ndxSort_ndx, ndxEnd)   RESULT(ndxID)
!+
!
! Description:
!
!     This routine retrieves the index of a device from the device ID by
!     performing a binary search through the table of device IDs.
!
! Parameters:
!
!
!     ID:
!    The device ID (input)
!
!     ID_ndx:
!    The table of device IDs (input).
!
!     ndxSort_ndx
!    A parallel table of sorted device IDs (input)
!
!     ndxEnd
!    number of devices (input)
!
! Design:
!     This routine performs a binary search on the device ID's to match the
!     device ID to an entry in the table of device ID's.
!
! Audit Trail:
! ----------------+-------+-----------------------------------------------------
! ----------------+-------+-----------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                            Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*), INTENT(IN)        :: ID                             ! ID of the device
CHARACTER(*), INTENT(IN)        :: ID_ndx     (ndxEnd)            ! Table of valid device IDs
INTEGER(4)  , INTENT(IN)        :: ndxSort_ndx(ndxEnd)            ! Parallel table of sorted order of ID_ndx
INTEGER(4)  , INTENT(IN)        :: ndxEnd                         ! number of devices.
INTEGER(4)                      :: ndxID
!
!                                Local
!-------------------------------+---------------
INTEGER(4)                      :: ndxSort                        ! index through sorted order table
INTEGER(4)                      :: ndxSortLow                     ! lower bound on binary search
INTEGER(4)                      :: ndxSortHigh                    ! upper bound on binary search
INTEGER(4),PARAMETER            :: NULLDEVICE = 0                 ! return value if the device is not found
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  !
  ! Binary search for device index
  ndxSortLow  = 1
  ndxSortHigh = ndxEnd

  DO WHILE(ndxSortHigh .ge. ndxSortLow)
     ndxSort= (ndxSortLow + ndxSortHigh + 1)/2

     IF(ID_ndx(ndxSort_ndx(ndxSort)).gt.ID) THEN
        ndxSortHigh = ndxSort- 1

     ELSE IF(ID_ndx(ndxSort_ndx(ndxSort)).lt.ID) THEN
        ndxSortLow  = ndxSort+ 1

     ELSE
        ndxID = ndxSort_ndx(ndxSort)
        RETURN
     END if
  END DO

  ndxID = NULLDEVICE

  RETURN

END FUNCTION NdxOfID

FUNCTION ValueInterpolateTableValues(Value, Value_n_m, nEnd, mEnd) RESULT(Value_n)
!+
!
! Description: Utility routine to interpolate values in a table and return the interpolated table row
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!-
USE NumericalConstants
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
REAL(8)   ,INTENT(IN)           :: Value                          ! Value to interpolate
INTEGER(4),INTENT(IN)           :: nEnd                           ! Number of columns in the table
INTEGER(4),INTENT(IN)           :: mEnd                           ! Number of rows in the table
REAL(8)   ,INTENT(IN)           :: Value_n_m(nEnd,mEnd)           ! Row and columns values
REAL(8)                         :: Value_n(nEnd)                  ! Interpolated row values
!
!                                Local
!-------------------------------+---------------
INTEGER(4)                      :: m                              ! Index through rows in the table
INTEGER(4)                      :: m1                             ! 1st Breakpoint in the table for interpolation
INTEGER(4)                      :: m2                             ! 2nd Breakpoint in the table for interpolation
REAL(8)                         :: dV                             ! Delta between breakpoints
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Value_n(1) = Value
  m1 = 0
  m2 = 0
  IF(Value_n(1) <= Value_n_m(1,1)) THEN
    m1 = 1; m2 = 2
  ELSE IF(Value_n(1) >= Value_n_m(1,mEnd-1)) THEN
    m1 = mEnd - 1; m2 = mEnd
  ELSE
    DO m = 2, mEnd - 1
      IF(Value_n(1) < Value_n_m(1,m)) THEN
        m1 = m-1; m2 = m; EXIT
      END IF
    END DO
  END IF
  IF(m1 > 0 .and. m2 > 0) THEN
    dV = Value_n_m(1,m2) - Value_n_m(1,m1)
    IF(ABS(dV) > Epsilon) THEN
      Value_n(2:nEnd) = Value_n_m(2:nEnd,m1) + (Value_n_m(2:nEnd,m2) - Value_n_m(2:nEnd,m1)) * (Value_n(1)- Value_n_m(1,m1)) / dV
    END IF
  END IF
  RETURN
END FUNCTION ValueInterpolateTableValues

SUBROUTINE ParseCiscoConfigurationFileAtPath(PathCiscoConfiguration)
!+
!
! Description: Parses a Cisco Switch Configuration File to generate a spreadsheet showing how each port is configured
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)         :: PathCiscoConfiguration         ! Path to the Cisco Configuration File
!
!                                Local
!-------------------------------+---------------
INTEGER(4)                      :: UnitConfiguration              ! Fortran I/O unit of the configuration file
INTEGER(4)                      :: UnitPortInformation            ! Fortran I/O unit of the port results file
CHARACTER(255)                  :: AsciiChr                       ! Buffer used to read in the configuration file
CHARACTER(128)                  :: PathPortInformation            ! Path to the Port Information file
INTEGER(4)                      :: ChrDot                         ! "." location in the input configuration file
INTEGER(4)                      :: ChrVLAN                        ! "switchport access vlan" location in the buffer
CHARACTER(4)                    :: NamePort                       ! Cisco switch port name (e.g. 1/5, 3/12 ...)
INTEGER(4)                      :: ChrSlash                       ! Location of "/" in the buffer
LOGICAL(4)                      :: ErrorNumAscii                  ! Error decoding VLAN netber
INTEGER(4)                      :: VLAN                           ! Virtual local area network index
INTEGER(4)                      :: Subnet_VLAN(2:255)             ! Subnet for each VLAN
INTEGER(4)                      :: Subnet                         ! Subnet number
CHARACTER(3)                    :: NameSubnet                     ! Subnet number formatted
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Subnet = 2
  DO VLAN = 2, 255
    Subnet_VLAN(VLAN) = Subnet
    Subnet = Subnet + 1
  END DO
  Subnet_VLAN(2:15) = Subnet_VLAN(2:15) - 1
  DO
    IF(.not.OpenFileAndGetUnit(UnitConfiguration, PathCiscoConfiguration, "old")) THEN
      WRITE(*,FMT='(A)')"ParseCiscoConfigurationFileAtPath: "//TRIM(PathCiscoConfiguration)//" File open error."
      EXIT
    END IF
    ChrDot = INDEX(PathCiscoConfiguration,".",BACK=.true.)
    IF(ChrDot > 5) THEN
      PathPortInformation = PathCiscoConfiguration(1:ChrDot-1)//"_Processed.csv"
    ELSE
      PathPortInformation = TRIM(PathCiscoConfiguration)//"_Processed.csv"
    END IF
    IF(.not.OpenFileAndGetUnit(UnitPortInformation, TRIM(PathPortInformation), "unknown")) THEN
      WRITE(*,FMT='(A)')"ParseCiscoConfigurationFileAtPath: "//TRIM(PathPortInformation)//" File open error."
      EXIT
    END IF
    NamePort = "    "
    DO
      AsciiChr = " "
      READ(UnitConfiguration, FMT='(A)',END=1,ERR=2) AsciiChr
      ChrSlash = INDEX(TRIM(AsciiChr),"/")
      IF(INDEX(TRIM(AsciiChr),"interface") > 0 .and.ChrSlash > 1) THEN
        NamePort = TRIM(AsciiChr(ChrSlash-1:))
        CYCLE
      END IF
      IF(NamePort=="    ") CYCLE
      ChrVLAN = INDEX(TRIM(AsciiChr),"switchport access vlan")
      IF(ChrVLAN < 1) CYCLE
      VLAN = INT(NumAscii(AsciiChr(ChrVLAN+22:),ErrorNumAscii))
      IF(VLAN < 2 .or. VLAN > 255) THEN
        NamePort = "    "
        CYCLE
      END IF
      Subnet = Subnet_VLAN(VLAN)
      WRITE(NameSubnet, FMT='(I3)')Subnet
      WRITE(UnitPortInformation, FMT = '(A)') "SW-"//NamePort//",192.168."//TRIM(ADJUSTL(NameSubnet))//".XXX"
      CYCLE
    END DO
1   CLOSE(UnitConfiguration)
    CLOSE(UnitPortInformation)
    EXIT
  END DO
  RETURN
2 WRITE(*,FMT='(A)')"ParseCiscoConfigurationFileAtPath: "//TRIM(PathPortInformation)//" File write error."
  CLOSE(UnitConfiguration)
  CLOSE(UnitPortInformation)
  RETURN
END SUBROUTINE ParseCiscoConfigurationFileAtPath

FUNCTION AlphaNumericCharacter(CharacterToTest) RESULT(AlphaNumeric)
!+
!
! Description: Tests a character to see if it is alphanumeric
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(1),INTENT(IN)         :: CharacterToTest                ! system command to execute
LOGICAL(4)                      :: AlphaNumeric                   ! True if the CharacterToTest is alphanumeric
!
!                                Local
!-------------------------------+---------------
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  AlphaNumeric = WithinClosedSet(ICHAR('a'),ICHAR(CharacterToTest),ICHAR('z')) .or. &
                 WithinClosedSet(ICHAR('A'),ICHAR(CharacterToTest),ICHAR('Z')) .or. &
                 WithinClosedSet(ICHAR('0'),ICHAR(CharacterToTest),ICHAR('9'))

  RETURN
END FUNCTION AlphaNumericCharacter

SUBROUTINE ParseCparm(Cparm, IDRecord, Cvalue, Value_n, nEnd)
!+
!
! Description: Parse a parameter array into a record and associated values.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-------------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(48),INTENT(IN)        :: Cparm                          ! system command to execute
CHARACTER(12),INTENT(OUT)       :: IDRecord                       ! Record
CHARACTER(32),INTENT(OUT)       :: Cvalue                         ! Character values
REAL(8)      ,INTENT(OUT)       :: Value_n(13)                    ! Real values
INTEGER(4)   ,INTENT(OUT)       :: nEnd                           ! Number of Values
!
!                                Local
!-------------------------------+---------------
INTEGER(4)  , PARAMETER         :: sepEnd   = 1                   ! Number of separators
INTEGER(4)  , PARAMETER         :: parseMax = 14                  ! Maximum number of tokens
CHARACTER(1), PARAMETER         :: a_sep(sepEnd) = ' '            ! List of valid separator characters
INTEGER(4)                      :: start$chr_parse(parseMax)
INTEGER(4)                      :: end$chr_parse(parseMax)
INTEGER(4)                      :: parseEnd
INTEGER(4)                      :: parse
LOGICAL(4)                      :: ErrorNumAscii
LOGICAL(4)  , PARAMETER         :: DebugPrint = .false.
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
IDRecord = " "
Cvalue   = " "
Value_n  = 0d0
nEnd     = 0

CALL ParseString(Cparm,a_sep,sepEnd,parseMax,start$chr_parse,end$chr_parse,parseEnd)

DO parse = 1, parseEnd
  IF(DebugPrint) PRINT *,' Cparm(',parse,') : ',Cparm(Start$chr_parse(parse):End$chr_parse(parse))
  IF(parse==1) THEN
    IDRecord = Cparm(Start$chr_parse(parse):End$chr_parse(parse))
  ELSE
    nEnd          = parse-1
    Value_n(nEnd) = NumAscii(Cparm(Start$chr_parse(parse):End$chr_parse(parse)), ErrorNumascii)
    IF(ErrorNumAscii) Cvalue = Cparm(Start$chr_parse(parse):End$chr_parse(parse))
  END IF
END DO
RETURN
END SUBROUTINE ParseCparm

PURE SUBROUTINE AppendValueToArrayI8(Val_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                       Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(8),INTENT(INOUT), ALLOCATABLE  :: Val_n(:)                  ! Array to add a value to.
INTEGER(4),INTENT(INOUT)               :: nEnd                      ! Number of Values
INTEGER(8),INTENT(IN)                  :: Val                       ! Value to add
INTEGER(4),INTENT(IN)                  :: dn                        ! Resize increment
!
!                                       Local
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(8),ALLOCATABLE, TARGET         :: Vtemp_n(:)                ! Temporary array used to resize Val_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(nEnd+1>SIZE(Val_n)) THEN
    ALLOCATE(Vtemp_n(nEnd + dn))
    Vtemp_n(1:nEnd) = Val_n(1:nEnd)
    CALL MOVE_ALLOC(Vtemp_n, Val_n)
  END IF
  nEnd        = nEnd + 1
  Val_n(nEnd) = Val
  RETURN

END SUBROUTINE AppendValueToArrayI8

PURE SUBROUTINE AppendValueToArrayI4(Val_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                       Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(4),INTENT(INOUT), ALLOCATABLE  :: Val_n(:)                  ! Array to add a value to.
INTEGER(4),INTENT(INOUT)               :: nEnd                      ! Number of Values
INTEGER(4),INTENT(IN)                  :: Val                       ! Value to add
INTEGER(4),INTENT(IN)                  :: dn                        ! Resize increment
!
!                                       Local
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(4),ALLOCATABLE, TARGET         :: Vtemp_n(:)                ! Temporary array used to resize Val_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(nEnd+1>SIZE(Val_n)) THEN
    ALLOCATE(Vtemp_n(nEnd + dn))
    Vtemp_n(1:nEnd) = Val_n(1:nEnd)
    CALL MOVE_ALLOC(Vtemp_n, Val_n)
  END IF
  nEnd        = nEnd + 1
  Val_n(nEnd) = Val
  RETURN

END SUBROUTINE AppendValueToArrayI4

PURE SUBROUTINE AppendValueToArrayR8(Val_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                       Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
REAL(8)   ,INTENT(INOUT), ALLOCATABLE  :: Val_n(:)                  ! Array to add a value to.
INTEGER(4),INTENT(INOUT)               :: nEnd                      ! Number of Values
REAL(8)   ,INTENT(IN)                  :: Val                       ! Value to add
INTEGER(4),INTENT(IN)                  :: dn                        ! Resize increment
!
!                                       Local
!--------------------------------------+----------------------------+-----------------------------------------------------
REAL(8)   ,ALLOCATABLE, TARGET         :: Vtemp_n(:)                ! Temporary array used to resize Val_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(nEnd+1>SIZE(Val_n)) THEN
    ALLOCATE(Vtemp_n(nEnd + dn))
    Vtemp_n(1:nEnd) = Val_n(1:nEnd)
    CALL MOVE_ALLOC(Vtemp_n, Val_n)
  END IF
  nEnd        = nEnd + 1
  Val_n(nEnd) = Val
  RETURN

END SUBROUTINE AppendValueToArrayR8

PURE SUBROUTINE AppendValueToArrayR4(Val_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                       Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
REAL(4)   ,INTENT(INOUT), ALLOCATABLE  :: Val_n(:)                  ! Array to add a value to.
INTEGER(4),INTENT(INOUT)               :: nEnd                      ! Number of Values
REAL(4)   ,INTENT(IN)                  :: Val                       ! Value to add
INTEGER(4),INTENT(IN)                  :: dn                        ! Resize increment
!
!                                       Local
!--------------------------------------+----------------------------+-----------------------------------------------------
REAL(4)   ,ALLOCATABLE,  TARGET        :: Vtemp_n(:)                ! Temporary array used to resize Val_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(nEnd+1>SIZE(Val_n)) THEN
    ALLOCATE(Vtemp_n(nEnd + dn))
    Vtemp_n(1:nEnd) = Val_n(1:nEnd)
    CALL MOVE_ALLOC(Vtemp_n, Val_n)
  END IF
  nEnd        = nEnd + 1
  Val_n(nEnd) = Val
  RETURN

END SUBROUTINE AppendValueToArrayR4

PURE SUBROUTINE AppendValueToArrayI2(Val_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                       Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(2),INTENT(INOUT), ALLOCATABLE  :: Val_n(:)                  ! Array to add a value to.
INTEGER(4),INTENT(INOUT)               :: nEnd                      ! Number of Values
INTEGER(2),INTENT(IN)                  :: Val                       ! Value to add
INTEGER(4),INTENT(IN)                  :: dn                        ! Resize increment
!
!                                       Local
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(2),ALLOCATABLE, TARGET         :: Vtemp_n(:)                ! Temporary array used to resize Val_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(nEnd+1>SIZE(Val_n)) THEN
    ALLOCATE(Vtemp_n(nEnd + dn))
    Vtemp_n(1:nEnd) = Val_n(1:nEnd)
    CALL MOVE_ALLOC(Vtemp_n, Val_n)
  END IF
  nEnd        = nEnd + 1
  Val_n(nEnd) = Val
  RETURN

END SUBROUTINE AppendValueToArrayI2

PURE SUBROUTINE AppendValueToArrayI1(Val_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                       Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(1),INTENT(INOUT), ALLOCATABLE  :: Val_n(:)                  ! Array to add a value to.
INTEGER(4),INTENT(INOUT)               :: nEnd                      ! Number of Values
INTEGER(1),INTENT(IN)                  :: Val                       ! Value to add
INTEGER(4),INTENT(IN)                  :: dn                        ! Resize increment
!
!                                       Local
!--------------------------------------+----------------------------+-----------------------------------------------------
INTEGER(1),ALLOCATABLE, TARGET         :: Vtemp_n(:)                ! Temporary array used to resize Val_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  IF(nEnd+1>SIZE(Val_n)) THEN
    ALLOCATE(Vtemp_n(nEnd + dn))
    Vtemp_n(1:nEnd) = Val_n(1:nEnd)
    CALL MOVE_ALLOC(Vtemp_n, Val_n)
  END IF
  nEnd        = nEnd + 1
  Val_n(nEnd) = Val
  RETURN

END SUBROUTINE AppendValueToArrayI1

PURE SUBROUTINE AppendValueToArrayChar(Val_m_n, nEnd, Val, dn)
!+
!
! Description: Add a value to an allocatable array, increasing the size of the array if necessary. Val_m_n must be allocatable
!              but allocated.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                        Passed                          Description / (units)
!--------------------------------------+----------------------------+-----------------------------------------------------
CHARACTER(1),INTENT(INOUT), ALLOCATABLE:: Val_m_n(:,:)              ! Array to add a value to.
INTEGER(4)  ,INTENT(INOUT)             :: nEnd                      ! Number of Values
CHARACTER(*),INTENT(IN)                :: Val                       ! Value to add
INTEGER(4)  ,INTENT(IN)                :: dn                        ! Resize increment
!
!                                        Local
!--------------------------------------+----------------------------+-----------------------------------------------------
CHARACTER(1),ALLOCATABLE, TARGET       :: Vtemp_m_n(:,:)            ! Temporary array used to resize Val_m_n
INTEGER(4)                             :: m                         ! Index through 1st dimension of Vtemp_m_n
INTEGER(4)                             :: Shape_k(2)                ! Shape of Val_m_n
INTEGER(4)                             :: mSize                     ! Size of Val
INTEGER(4)                             :: mEnd                      ! First dimension of Val_m_n
EQUIVALENCE                              (mEnd, Shape_k(1))
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  Shape_k = SHAPE(Val_m_n)
  IF(nEnd+1>SIZE(Val_m_n,DIM=2)) THEN
    ALLOCATE(Vtemp_m_n(mEnd,nEnd + dn))
    Vtemp_m_n(:,1:nEnd) = Val_m_n(:,1:nEnd)
    CALL MOVE_ALLOC(Vtemp_m_n, Val_m_n)
  END IF
  nEnd = nEnd + 1
  mSize = LEN(Val)
  DO m = 1, mEnd
    IF(m > mSize) THEN
      Val_m_n(m,nEnd) = " "
    ELSE
      Val_m_n(m,nEnd) = Val(m:m)
    END IF
  END DO
  RETURN

END SUBROUTINE AppendValueToArrayChar
!
FUNCTION WildCardMatchAscii(ActualInput, TemplateInput, MatchCase) RESULT(Match)
!+
!
!  Description: This function checks an input character string for a wildcard match with a character template.
!
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed Variables                Description / (units)
!-------------------------------+-------------------------------+-------------------------------------------------------
CHARACTER(*),INTENT(IN)         :: ActualInput
CHARACTER(*),INTENT(IN)         :: TemplateInput
LOGICAL(4)  ,INTENT(IN)         :: MatchCase
LOGICAL(4)                      :: Match
!
!                                Local
!-------------------------------+------------
CHARACTER(80)                   :: TemplateLocal
CHARACTER(80)                   :: Actual
INTEGER(4)                      :: asterisk
INTEGER(4)                      :: white
INTEGER(4)                      :: LengthPartial
INTEGER(4)                      :: PartialInActual
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------

  !Don't corrupt input variables
  IF(MatchCase) THEN
    TemplateLocal = TemplateInput
    Actual        = ActualInput
  ELSE
    CALL UpperCase(TemplateInput,TemplateLocal)
    CALL UpperCAse(ActualInput  ,Actual)
  END IF

  !Assume it will match
  Match = .true.

  !Handle special cases
  IF(TemplateLocal=='*') THEN
     RETURN
  END IF
  asterisk = INDEX(TemplateLocal,'*')
  IF(asterisk > 1) THEN
     PartialInActual = INDEX(Actual, TemplateLocal(1:asterisk-1))
     IF(PartialInActual /= 1) THEN
       Match = .false.
       RETURN
     END IF
  ELSE IF(asterisk == 0 .and.TemplateLocal /= Actual  ) THEN
    Match = .false.
    RETURN
  END IF

  !Handle the general case
  DO WHILE (asterisk > 0 .and. Match)
    LengthPartial = asterisk-1
    PartialInActual = INDEX(Actual, TemplateLocal(1:asterisk-1))
    IF(PartialInActual == 0) THEN
      Match = .false.
      RETURN
    ELSE
      TemplateLocal = TemplateLocal(asterisk+1:)
      Actual        = Actual(PartialInActual+LengthPartial:)
    END IF
    asterisk = INDEX(TemplateLocal,'*')
    white    = INDEX(TemplateLocal,'  ')
    IF(asterisk == 0 .and. TemplateLocal/='  '.and.INDEX(Actual,TemplateLocal(1:white)) == 0) THEN
      Match = .false.
      RETURN
    END IF
  END DO

  RETURN
END FUNCTION WildCardMatchAscii

FUNCTION ValueContrast(Val, ValMin, ValMax) RESULT(ValContrast)
!+
!
!  Description: This function calculates a value for maximum contrast to the input value.
!
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                  Passed Variables               Description / (units)
!-------------------------------+-------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)             :: Val                          ! Value to be contrasted
REAL(8), INTENT(IN)             :: ValMin                       ! Minimum value
REAL(8), INTENT(IN)             :: ValMax                       ! Maximum value
REAL(8)                         :: ValContrast                  ! Contrasted Value
!
!                                  Local Variables
!-------------------------------+-------------------------------
REAL(8)                         :: ValMid                       ! Average value
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  ValMid = (ValMin + ValMax)/2d0
  IF(Val > ValMid) THEN
    ValContrast = ValMin
  ELSE
    ValContrast = ValMax
  END IF
  RETURN
END FUNCTION ValueContrast

SUBROUTINE TracePiecewiseLinearPoints(PntEnd, x_Pnt, y_Pnt, TraceEnd, x_Trace, y_Trace, P$Trace_Pnt)
!+
! Description: Prepare an array of individual points of a piecewise linear function
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------------
INTEGER(4),INTENT(IN)         :: PntEnd                         ! Number of piecewise linear points
INTEGER(4),INTENT(IN)         :: x_Pnt(PntEnd)                  ! Independent axis of piecewise linear function (distance)
INTEGER(4),INTENT(IN)         :: y_Pnt(PntEnd)                  ! Dependent axis of piecewise linear function   (distance)
INTEGER(4),INTENT(OUT)        :: TraceEnd                       ! Number of trace points
INTEGER(4),ALLOCATABLE        :: x_Trace(:)                     ! Independent axis of piecewise linear points   (distance)
INTEGER(4),ALLOCATABLE        :: y_Trace(:)                     ! Dependent axis of piecewise linear points     (distance)
INTEGER(4),ALLOCATABLE        :: P$Trace_Pnt(:)                 ! Heirarchical pointer to the Trace entries for each Pnt
!
!                                Local
!-----------------------------+-----------
INTEGER(4)                    :: xMin                           ! Minimum value of independent axis (distance)
INTEGER(4)                    :: xMax                           ! Maximum value of independent axis (distance)
INTEGER(4)                    :: Pnt                            ! Index through piecewise linear breakpoints
INTEGER(4)                    :: x                              ! Index through dependent axis points (distance)
INTEGER(4)                    :: Trace                          ! Number of dependent axis points

! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  IF(ALLOCATED(x_Trace)) THEN
    DEALLOCATE(x_Trace, y_Trace, P$Trace_Pnt)
  END IF
  xMin     = MINVAL(x_Pnt)
  xMax     = MAXVAL(x_Pnt)
  TraceEnd = xMax - xMin + 1
  ALLOCATE(x_Trace(TraceEnd), y_Trace(TraceEnd), P$Trace_Pnt(PntEnd+1))

  Pnt = 2
  x   = xMin
  DO Trace = 1, TraceEnd
    DO WHILE(x > x_Pnt(Pnt))
      Pnt = Pnt + 1
    END DO
    x_Trace(Trace) = x
    y_Trace(Trace) = y_Pnt(Pnt-1) + NINT( DBLE(y_Pnt(Pnt) - y_Pnt(Pnt-1)) * DBLE(x - x_Pnt(Pnt-1))/DBLE(x_Pnt(Pnt) - x_Pnt(Pnt-1)) )
    x              = x + 1
  END DO
  Pnt = 1                                          !!
  DO Trace = 1, TraceEnd                           !!
    IF(x_Trace(Trace)==x_Pnt(Pnt)) THEN            !!
      P$Trace_Pnt(Pnt) = Trace-1                   !!
      Pnt = Pnt + 1                                !!
    END IF                                         !!
  END DO                                           !!
  P$Trace_Pnt(Pnt) = TraceEnd                      !!
  RETURN

END SUBROUTINE TracePiecewiseLinearPoints

SUBROUTINE TraceLinearFunctionPoints(Yo, dYdX, Xmin, Xmax, TraceEnd, x_Trace, y_Trace)
!+
! Description: Prepare an array of individual points of a piecewise linear function
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                          Description / (units)
!-----------------------------+---------------------------------+------------------------------------------------------------
REAL(8)   ,INTENT(IN)         :: Yo                             ! Y-intercept of Linear Function    (distance)
REAL(8)   ,INTENT(IN)         :: dYdX                           ! Slope of Linear Function          (n/a)
INTEGER(4),INTENT(IN)         :: xMin                           ! Minimum value of independent axis (distance)
INTEGER(4),INTENT(IN)         :: xMax                           ! Maximum value of independent axis (distance)
INTEGER(4),INTENT(OUT)        :: TraceEnd                       ! Number of trace points
INTEGER(4),ALLOCATABLE        :: x_Trace(:)                     ! Independent axis of piecewise linear points   (distance)
INTEGER(4),ALLOCATABLE        :: y_Trace(:)                     ! Dependent axis of piecewise linear points     (distance)

!
!                                Local
!-----------------------------+-----------
INTEGER(4)                    :: x                              ! Index through dependent axis points (distance)
INTEGER(4)                    :: Trace                          ! Number of dependent axis points

! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  IF(ALLOCATED(x_Trace)) THEN
    DEALLOCATE(x_Trace, y_Trace)
  END IF
  TraceEnd = xMax - xMin + 1
  ALLOCATE(x_Trace(TraceEnd), y_Trace(TraceEnd))

  Trace = 0
  DO x = xMin, xMax
    Trace          = Trace + 1
    x_Trace(Trace) = x
    y_Trace(Trace) = NINT(Yo + dYdX * DBLE(x))
  END DO
  RETURN

END SUBROUTINE TraceLinearFunctionPoints

FUNCTION yBreakPointEstimate(p$m_n, y_m, mEnd, nEnd) RESULT(y0_n)
!+
!
! FUNCTIONAL DESCRIPTION:
!
! Description: This function estimates the values of a set of breakpoints that minimizes the Least Squares Error for a
!              set of evenly spaced measurements y vs x between each breakpoint.
!
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE MatrixVectorPackage
IMPLICIT NONE
!                                  Passed Variables               Description / (units)
!-------------------------------+-------------------------------+-------------------------------------------------------

INTEGER(4) , INTENT(IN)         :: mEnd                         ! Number of measurements
INTEGER(4) , INTENT(IN)         :: nEnd                         ! Number of breakpoints
INTEGER(4) , INTENT(IN)         :: p$m_n(nEnd+1)                ! Hierarchical pointer to the measurements for each breakpoint
REAL(8)    , INTENT(IN)         :: y_m(mEnd)                    ! Measurements
REAL(8)                         :: y0_n(nEnd)                   ! Estimated breakpoints
!
!                                Local
!-------------------------------+------------
REAL(8)   , ALLOCATABLE         :: J_n_n(:,:)                   ! Jacobian for the least squares fit
REAL(8)   , ALLOCATABLE         :: Jinv_n_n(:,:)                ! Inverse of the Jacobian
REAL(8)   , ALLOCATABLE         :: dF_n(:)                      ! Right side vector
INTEGER(4)                      :: n                            ! Index through breakpoints
INTEGER(4)                      :: mi                           ! Starting measurement for a breakpoint
INTEGER(4)                      :: mj                           ! Ending measurement for a breakpoint
INTEGER(4)                      :: m                            ! Index through measurement for a breakpoint
REAL(8)                         :: d                            ! Distance between y axis breakpoints (x-axis is sequential)
REAL(8)                         :: f                            ! Fraction of distance to measurement from breakpoint start
LOGICAL(4)                      :: FullRank                     ! Set true if the Jacobian is full rank
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------

  ALLOCATE(J_n_n(nEnd,nEnd),Jinv_n_n(nEnd,nEnd),dF_n(nEnd))

  J_n_n = 0d0
  dF_n  = 0d0
  DO n = 1, nEnd-1
    mi    = p$m_n(n)+1; mj = p$m_n(n+1); d = DBLE(mj - mi + 1)
    DO m = mi, mj
      f = DBLE(m-mi)/d
      J_n_n(n:n+1,n  ) = J_n_n(n:n+1,n  ) + (/(1d0-f)**2, (1d0-f)*f/)
      J_n_n(n:n+1,n+1) = J_n_n(n:n+1,n+1) + (/(1d0-f)*f ,     f**2 /)
      dF_n(n  )        = dF_n(n  ) + y_m(m)*(1d0-f)
      dF_n(n+1)        = dF_n(n+1) + y_m(m)*f
    END DO
  END DO
  J_n_n(nEnd,nEnd) = J_n_n(nEnd,nEnd) + 1d0
  dF_n(nEnd)       = dF_n(nEnd) + y_m(mEnd)
  Jinv_n_n         = InverseCholeskyNxN(J_n_n, N=nEnd, FullRank=FullRank)
  y0_n             = MATMUL(Jinv_n_n,dF_n)

  DEALLOCATE(J_n_n, Jinv_n_n, dF_n)
  RETURN
END FUNCTION yBreakPointEstimate

SUBROUTINE CreateAccessViolation
!+
!
! Description: This subroutine causes a memory access violation.  It is used for testing.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                  Local Variables                Description / (units)
!-------------------------------+-------------------------------+-------------------------------------------------------
INTEGER(4),ALLOCATABLE          :: Violation_Access(:)          ! Test array for access violation
INTEGER(4)                      :: Access                       ! Index through Violation_Access
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  ALLOCATE(Violation_Access(20))
  DO Access = 1, 1000000
    Violation_Access(Access) = 20
  END DO
  DEALLOCATE(Violation_Access)
END SUBROUTINE CreateAccessViolation

SUBROUTINE CheckPoint(Task, Text)
!+
! Description: This subroutine ensures that a task's progress file is allocated and prints a text message to that file.
!              ToDo This needs redesigned to be thread safe
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed Variables                  Description (Units)
!-----------------------------+----------------------------------+------------------------------------------------------
CHARACTER(LEN=*) ,INTENT(IN)  :: Task                            ! Name of the task recording the progress file
CHARACTER(LEN=*) ,INTENT(IN)  :: Text                            ! Text to output to the progress file

!                                Local Variables
!-----------------------------+----------------------------------
CHARACTER(LEN=128)            :: Path                            ! Path for progress file
LOGICAL(4)                    :: AllocatedFile = .false.         ! Set true if the progress file has been allocated
INTEGER(4)                    :: UnitProgress = 0                ! Fortran I/O unit for the progress file
CHARACTER(12)                 :: AsciiTime                       ! Time stamp for log entry
INTEGER(4)                    :: DaySave   = 0                   ! Progress file creation day.
INTEGER(4)                    :: MonthSave = 0                   ! Progress file creation month.
INTEGER(4)                    :: YearSave  = 0                   ! Progress file creation year.
INTEGER(4)                    :: values_rcv(8)                   ! Return value from DATE_AND_TIME
INTEGER(4)                    :: year,month,day                  ! Date components of returned value from DATE_AND_TIME
INTEGER(4)                    :: hour,minute,second,msec,zone    ! Time components of returned value from DATE_AND_TIME
EQUIVALENCE                     (values_rcv(1),year  )
EQUIVALENCE                     (values_rcv(2),month )
EQUIVALENCE                     (values_rcv(3),day   )
EQUIVALENCE                     (values_rcv(4),zone  )
EQUIVALENCE                     (values_rcv(5),hour  )
EQUIVALENCE                     (values_rcv(6),minute)
EQUIVALENCE                     (values_rcv(7),second)
EQUIVALENCE                     (values_rcv(8),msec  )

!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ! If checkpoint is turned off then close the progress file if it has already been allocated and return
  IF(BypassCheckpointLog) THEN
    IF(AllocatedFile) THEN
      CLOSE(UnitProgress)
      AllocatedFile = .false.
    END IF
    RETURN
  END IF

  CALL DATE_AND_TIME(VALUES = VALUES_RCV(1:8))

  ! If checkpoint is enabled but we've entered a new day, then close the existing progress file
  IF(AllocatedFile.and.(Day.ne.DaySave .or. Month.ne.MonthSave .or. Year.ne.YearSave)) THEN
    CLOSE(UnitProgress)
    AllocatedFile = .false.
  END IF

  ! Allocate the Progress File if necessary
  IF(.not.AllocatedFile) THEN
    Path = PathCSPDataFile(Directory="D:\CSP\Log",File=TRIM(Task)//"Progress",Extension="dat")
    OPEN(NewUnit=UnitProgress,File=TRIM(Path),Status="new",ERR=1)
    DaySave   = Day
    MonthSave = Month
    YearSave  = Year
    AllocatedFile = .true.
  END IF

  ! Write entry to progress file
  WRITE(AsciiTime,FMT='(I2.2,":",I2.2,":",I2.2,".",I3.3)') Hour,Minute,Second,Msec
  WRITE(UnitProgress,FMT='(A)') TRIM(AsciiTime//" "//TRIM(Text))
  FLUSH(UnitProgress)
  RETURN

1 PRINT *, Text
  RETURN

END SUBROUTINE CheckPoint

SUBROUTINE SetCheckPointLogging(Enable)
!
! Description: This subroutine enables/disables output of text messages to a tasks's progress file
!              ToDo - This need to have Task as input and BypassCheckPointLog_Task to make thread-safe
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
LOGICAL(4) ,INTENT(IN)        :: Enable

! Local Variables
!----------------------------
!None
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(Enable) THEN
    BypassCheckPointLog = .false.
  ELSE
    BypassCheckPointLog = .true.
  END IF
  RETURN

END SUBROUTINE SetCheckPointLogging

FUNCTION DirectoryCSP() RESULT(DirCSP)
!+
! Description: This function retrieves the CSP top level directory specified in the environment variable CSP_DIR.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=24)            :: DirCSP                          ! CSP top level directory (normally D:\CSP\
!
!                             Local Variables
!----------------------------+---------------
CHARACTER(LEN=24)            :: PathCSP
!
! ------------------------------------------ ----------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL GET_ENVIRONMENT_VARIABLE("CSP_DIR", PathCSP)
  DirCSP = PathCSP
!
END FUNCTION DirectoryCSP

FUNCTION PathCSPDataFile(Directory,File,Extension) RESULT(Path)
!+
! Description: This function constructs a Path for a CSP data file with the present time stamp in the file name portion
!              of the Path.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!-----------------------------+---------------------------------+-------------------------------------------------------
CHARACTER(LEN=*) ,INTENT(IN)  :: Directory
CHARACTER(LEN=*) ,INTENT(IN)  :: File
CHARACTER(LEN=3) ,INTENT(IN)  :: Extension
CHARACTER(LEN=128)            :: Path

! Local Variables
!----------------------------
INTEGER(4)                   :: statusEnvironment
INTEGER(4)                   :: Length
CHARACTER(LEN=80)            :: PathDirectory
CHARACTER(LEN=24)            :: TimeStampLocal
CHARACTER(LEN=4)             :: ZoneLocal
CHARACTER(LEN=1),PARAMETER   :: AsciiPercent = "%"
CHARACTER(LEN=80)            :: PathEnvironment
INTEGER(4)                   :: PercentEnd
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(INDEX(Directory,":") /= 0) THEN
    PathDirectory = Directory
  ELSE
    PathDirectory = "C:"
    IF(Directory(1:1)==AsciiPercent) THEN
      PercentEnd = INDEX(Directory,AsciiPercent,Back=.true.)
      IF(PercentEnd > 2) THEN
        CALL GET_ENVIRONMENT_VARIABLE (TRIM(Directory(2:PercentEnd-1)),PathEnvironment,Length, statusEnvironment, TRIM_NAME=.true.)
        IF(statusEnvironment == 0 .and. Length /= 0) THEN
          IF(PercentEnd < LEN(Directory)) THEN
            PathDirectory = TRIM(PathEnvironment)//Directory(PercentEnd+1:)
          ELSE
            PathDirectory = TRIM(PathEnvironment)//SlashDir()
          END IF
        END IF
      END IF
    END IF
  END IF
  CALL TimeStampLocalForUtilities(TimeStampLocal,ZoneLocal)
  Path = TRIM(PathDirectory)//SlashDir()//TRIM(File)//'_'//TimeStampLocal( 1: 2)//'_'// &
                                                           TimeStampLocal( 4: 6)//'_'// &
                                                           TimeStampLocal( 8:11)//'_'// &
                                                           TimeStampLocal(13:14)//'_'// &
                                                           TimeStampLocal(16:17)//'_'// &
                                                           TimeStampLocal(19:20)//'_'// &
                                                           TRIM(ZoneLocal)//'.'//Extension
!
END FUNCTION PathCSPDataFile

FUNCTION PathCSPLogging(File) RESULT(Path)
!+
! Description: This function constructs a Path for a CSP csv file with the present time stamp in the file name portion
!              of the Path.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=80)            :: Path
CHARACTER(LEN=*)             :: File
!
!                             Local Variables
!----------------------------+---------------
CHARACTER(LEN=80)            :: PathCSP
INTEGER(4)                   :: StatusEnvironment
INTEGER(4)                   :: length
CHARACTER(LEN=80)            :: PathLogCSP
CHARACTER(LEN=24)            :: TimeStampLocal
CHARACTER(LEN=4)             :: ZoneLocal
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  CALL GET_ENVIRONMENT_VARIABLE ("CSP_DIR", PathCSP, length, statusEnvironment, TRIM_NAME=.true.)
  IF(statusEnvironment == 0 .and. length /= 0) THEN
    PathLogCSP  = TRIM(PathCSP)//'\CSPLog\'
  ELSE
    PathLogCSP  = 'C:\CSPLog\'
  END IF
  CALL TimeStampLocalForUtilities(TimeStampLocal,ZoneLocal)
  Path = TRIM(PathLogCSP)//TRIM(File)//TimeStampLocal( 1: 2)//'_'// &
                                       TimeStampLocal( 4: 6)//'_'// &
                                       TimeStampLocal( 8:11)//'_'// &
                                       TimeStampLocal(13:14)//'_'// &
                                       TimeStampLocal(16:17)//'_'// &
                                       TimeStampLocal(19:20)//'_'// &
                                       TRIM(ZoneLocal)//'.csv'
!
END FUNCTION PathCSPLogging

SUBROUTINE TimeStampLocalForUtilities(TimeStamp,Zone)
!
! Description: This subroutine retrieves a local time stamp and using the DATE_AND_TIME intrinsic Function. It provides
!              a mechanism to get the local time stamp without using the TimeDatePackage Module.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=24)            :: TimeStamp                       ! Local timestamp dd-mmm-yyyy hh:mm:ss.mmm
CHARACTER(LEN=4)             :: Zone                            ! Local timezone
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: Values_rcv(9)                                ! System Date and Time (Array)
INTEGER(4)                   :: Year,Month,Day                               ! Date components from DATE_AND_TIME
INTEGER(4)                   :: Hour,Minute,Second,msec                      ! Time components from DATE_AND_TIME
INTEGER(4)                   :: dMinuteZone                                  ! Minute shift from UTC for Local time
EQUIVALENCE                    (values_rcv(1),year         )
EQUIVALENCE                    (values_rcv(2),month        )
EQUIVALENCE                    (values_rcv(3),day          )
EQUIVALENCE                    (values_rcv(4),dMinuteZone  )
EQUIVALENCE                    (values_rcv(5),hour         )
EQUIVALENCE                    (values_rcv(6),minute       )
EQUIVALENCE                    (values_rcv(7),second       )
EQUIVALENCE                    (values_rcv(8),msec         )
CHARACTER(3),PARAMETER       :: AsciiOf_Month(12)                         & ! Ascii of each month
                                                                              =(/"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"/)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
  !
  CALL DATE_AND_TIME(VALUES = Values_rcv(1:8))
  WRITE(TimeStamp,FMT='(I2.2,"-",A3,"-",I4," ",I2.2,":",I2.2,":",I2.2,".",I3.3)') Day,AsciiOf_Month(Month),Year,Hour,Minute,Second,msec
  IF(ABS(dMinuteZone)==8) THEN
    Zone ="PST"
  ELSE
    Zone = "PDT"
  END IF
  RETURN

END SUBROUTINE TimeStampLocalForUtilities

SUBROUTINE ReturnStatusInErrorLevel(StatusError)
!
! Description: This subroutine sets the ErrorLevel environment variable to a status value.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: StatusError                     ! Value to set the environment variable
!
!                             Local Variables
!----------------------------+---------------
CHARACTER(11)                :: StatusChar                      ! Character formatted value of StatusError
INTEGER(4)                   :: StatusCommand                   ! Return status from SystemCommand
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!

  WRITE(StatusChar,FMT='(I11)') StatusError
  StatusCommand = SystemCommand("Setx /M ERRORLEVEL = "//TRIM(ADJUSTL(StatusChar)))
  IF(StatusCommand == UTIL_E_FAILURE) THEN
    WRITE(*,FMT='(A)')"ReturnStatusInErrorLevel.Failure"
  END IF
  RETURN
END SUBROUTINE ReturnStatusInErrorLevel

FUNCTION ThresholdForMaximumSeparability(V_n, nEnd) RESULT(Vthreshold)
!+
! Description: This function calculates the threshold that separates a set of values into two populations, one less
!              than the threshold; the other greater than or equal to the threshold.  The threshold is chosen to
!              minimize the sum of the variances of the populations, weighted by their respective probabilities.
!
!              The algorithm used is based on the method developed by Nobuyuki Otsu:
!               https://en.wikipedia.org/wiki/Otsu%27s_method.
!
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE

!                            Passed Variables
! ------------------------+----------------------+----------------------------------------------------------------------
INTEGER(4), INTENT(IN)    :: nEnd                ! Total number of points
INTEGER(4), INTENT(IN)    :: V_n(nEnd)           ! Value for each point
INTEGER(4)                :: Vthreshold          ! Threshold that minimizes the total variance of the two populations

!                            Local Variables
! ------------------------+----------------------
INTEGER(4)                :: Vmin                ! Minimum value in V_n
INTEGER(4)                :: Vmax                ! Maximum value in V_n
INTEGER(4)                :: V                   ! Index through the values of Num_V, Vsqr_V and Vnom_V
INTEGER(4),ALLOCATABLE    :: Num_V(:)            ! Number of entries in V_n for each value
INTEGER(4),ALLOCATABLE    :: Vnom_V(:)           ! Number of entries in V_n for each value * value
INTEGER(4),ALLOCATABLE    :: Vsqr_V(:)           ! Number of entries in V_n for each value * value**2
INTEGER(4)                :: NumGE               ! Number of values in V_n at or above a candidate threshold
INTEGER(4)                :: NumLT               ! Number of values in V_n below a candidate threshold
INTEGER(4)                :: VnomGE              ! Sum of values in V_n at or above a candidate threshold
INTEGER(4)                :: VnomLT              ! Sum of values in V_n below a candidate threshold
INTEGER(4)                :: VsqrGE              ! Sum of square values in V_n at or above a candidate threshold
INTEGER(4)                :: VsqrLT              ! Sum of square values in V_n below a candidate threshold
REAL(8)                   :: Variance            ! Variance of a partition due to a candidate threshold scaled
REAL(8)                   :: VarianceMin         ! Minimum scaled variance over all candidate thresholds
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Vmin = MINVAL(V_n)
  Vmax = MAXVAL(V_n)
  ALLOCATE(Vsqr_V(Vmin:Vmax),Vnom_V(Vmin:Vmax),Num_V(Vmin:Vmax), SOURCE = 0)

  DO V = Vmin, Vmax
    Num_V(V) = COUNT(V_n == V)
    IF(Num_V(V) == 0) CYCLE
    Vsqr_V(V) = Num_V(V) * V**2
    Vnom_V(V) = Num_V(V) * V
  END DO

  NumGE   = SUM(Num_V)
  VnomGE  = SUM(Vnom_V)
  VsqrGE  = SUM(Vsqr_V)
  NumLT   = 0
  VnomLT  = 0
  VsqrLT  = 0
  VarianceMin = DBLE(VsqrGE) - DBLE(VnomGE**2)/DBLE(NumGE)
  VThreshold  = Vmin

  DO V = Vmin+1, Vmax
    IF(Num_V(V)==0 .and. Num_V(V-1)==0) CYCLE
    NumGE  = NumGE  - Num_V(V-1)
    VnomGE = VnomGE - Vnom_V(V-1)
    VsqrGE = VsqrGE - Vsqr_V(V-1)

    NumLT  = NumLT  + Num_V(V-1)
    VnomLT = VnomLT + Vnom_V(V-1)
    VsqrLT = VsqrLT + Vsqr_V(V-1)

    Variance   = DBLE(VsqrGE + VsqrLT) - DBLE(VnomGE**2)/DBLE(NumGE) - DBLE(VnomLT**2)/DBLE(NumLT)

    IF(VarianceMin > Variance) THEN
      VarianceMin = Variance
      Vthreshold  = V
    END IF
  END DO

  DEALLOCATE(Vsqr_V,Vnom_V,Num_V)

  RETURN
END FUNCTION ThresholdForMaximumSeparability

SUBROUTINE InquireDirectory(Directory, Exists)
!
! Description: This subroutine provides a Fortran standard method of checking if a directory exists
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: Directory                       ! Name of the directory
LOGICAL(4)      ,INTENT(OUT) :: Exists                          ! Set true if directory exists (false otherwise)
!
!                             Local Variables
!----------------------------+---------------
CHARACTER(LEN=1024)          :: DirectoryLocal
CHARACTER(LEN=1)             :: DirectoryLocal_chr(1024)
EQUIVALENCE                    (DirectoryLocal,DirectoryLocal_chr)
INTEGER(4),PARAMETER         :: chr$MX = 1024
INTEGER(4)                   :: chrEnd
INTEGER(4)                   :: UnitScratch
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  chrEnd = LEN_TRIM(Directory)
  IF(chrEnd >= chr$MX.or.chrEnd == 0) THEN
    Exists = .false.
    RETURN
  END IF
  DirectoryLocal(1:chrEnd) = Directory(1:chrEnd)
  IF(.not.DirectoryLocal_chr(chrEnd)=="\") THEN
    chrEnd = chrEnd + 1
    DirectoryLocal_chr(chrEnd) = "\"
  END IF

  OPEN(NewUnit=UnitScratch, FILE=DirectoryLocal(1:chrEnd)//"delete.scratch",ACTION="WRITE", ERR=1)
  CLOSE(UnitScratch, Status = 'DELETE')
  Exists = .true.
  RETURN

1 Exists = .false.
  RETURN

END SUBROUTINE InquireDirectory

FUNCTION GetWordsFromBuffer(WordSize, ByteSize, ByteStart, Data_Byte, Data_Word) RESULT(StatusGet)
!
! Description: Retrieves a 4 byte word from a byte buffer
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)      ,INTENT(IN)  :: WordSize                        ! Size of the Word data buffer
INTEGER(4)      ,INTENT(IN)  :: ByteSize                        ! Size of the Byte data buffer
INTEGER(4)      ,INTENT(IN)  :: ByteStart                       ! Start of Word data in the data buffer
INTEGER(1)      ,INTENT(IN)  :: Data_Byte(ByteSize)             ! Byte data buffer
INTEGER(4)      ,INTENT(OUT) :: Data_Word(WordSize)             ! Word data buffer
INTEGER(4)                   :: StatusGet                       ! Status return
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: ByteEnd                         ! End of Word data in the data buffer
INTEGER(4)                   :: Word                            ! Index through word buffer
INTEGER(4)                   :: Byte                            ! Index through byte buffer
INTEGER(4)                   :: DataWord                        ! Data word
INTEGER(1)                   :: Data_b(4)                       ! Data word cast as 4 byte array
EQUIVALENCE                    (Data_b,DataWord)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ByteEnd = ByteStart + 4*WordSize - 1
  IF(.not.(WithinClosedSet(ByteStart, ByteEnd, ByteSize) .and. WithinClosedSet(1, ByteStart, ByteSize)) ) THEN
    StatusGet = UTIL_E_FAILURE
    RETURN
  END IF
  Word = 1
  DO Byte = ByteStart, ByteEnd, 4
    Data_b (1:4) = Data_Byte(Byte:Byte+3)
    Data_Word(Word) = DataWord
    Word = Word + 1
  END DO
  StatusGet = UTIL_S_NORMAL
  RETURN
END FUNCTION GetWordsFromBuffer

FUNCTION GetHwrdsFromBuffer(HwrdSize, ByteSize, Data_Byte, ByteStart, Data_Hwrd) RESULT(StatusGet)
!
! Description: Retrieves a 4 byte word from a byte buffer
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)      ,INTENT(IN)  :: HwrdSize                        ! Size of the Hwrd data buffer
INTEGER(4)      ,INTENT(IN)  :: ByteSize                        ! Size of the Byte data buffer
INTEGER(4)      ,INTENT(IN)  :: ByteStart                       ! Start of Hwrd data in the data buffer
INTEGER(1)      ,INTENT(IN)  :: Data_Byte(ByteSize)             ! Byte data buffer
INTEGER(2)      ,INTENT(OUT) :: Data_Hwrd(HwrdSize)             ! Halfword data buffer
INTEGER(4)                   :: StatusGet                       ! Status return
!
!                             Local Variables
!----------------------------+---------------
INTEGER(4)                   :: ByteEnd                         ! End of Hwrd data in the data buffer
INTEGER(4)                   :: Hwrd                            ! Index through word buffer
INTEGER(4)                   :: Byte                            ! Index through byte buffer
INTEGER(2)                   :: DataHwrd                        ! Data halfword
INTEGER(1)                   :: Data_b(4)                       ! Data halfword cast as 2 byte array
EQUIVALENCE                    (Data_b,DataHwrd)
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  ByteEnd = ByteStart + 2*HwrdSize - 1
  IF(.not.(WithinClosedSet(ByteStart, ByteEnd, ByteSize) .and. WithinClosedSet(1, ByteStart, ByteSize)) ) THEN
    StatusGet = UTIL_E_FAILURE
    RETURN
  END IF
  Hwrd = 1
  DO Byte = ByteStart, ByteEnd, 2
    Data_b (1:2) = Data_Byte(Byte:Byte+1)
    Data_Hwrd(Hwrd) = DataHwrd
    Hwrd = Hwrd + 1
  END DO
  StatusGet = UTIL_S_NORMAL
  RETURN
END FUNCTION GetHwrdsFromBuffer

FUNCTION Array2D(Array3D_x_y_z,z) RESULT(Array2D_x_y)
!+
!
! Description: This routine returns a pointer to a 2D slice of a 3D array
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 12-APR-2025     | MA    | SCR 150: Update the architecture of 3D image processing to eliminate copying 2D slices
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                                Passed                           Description / (units)
!-----------------------------+---------------------------------+-------------------------------------------------------
REAL(8)   ,INTENT(IN) ,TARGET :: Array3D_x_y_z(:,:,:)           ! 3D array
INTEGER(4),INTENT(IN)         :: z                              ! Slice of 3rd dimension for the slice
REAL(8)   ,POINTER            :: Array2D_x_y(:,:)               ! 2D slice of the 3D array

!
!                                Local
!-----------------------------+-----------
INTEGER(4)                    :: xSize                          ! Size of x dimension
INTEGER(4)                    :: ySize                          ! Size of y dimension

  ! Get the dimensions of the 2D slice:
  xSize = SIZE(Array3D_x_y_z, DIM=1)
  ySize = SIZE(Array3D_x_y_z, DIM=2)
  !
  ! Set the pointer to the 2D slice
  Array2D_x_y => Array3D_x_y_z(:,:,z)
  RETURN
END FUNCTION Array2D

FUNCTION StandardizeFormatSourceFile(FileSourceCode) RESULT(statusFormat)
!+
!  Description: This routine standardizes a source file format
!
!  Audit Trail
! -----------------+-------+--------------------------------------------------------------------------------------------
! 15-APR-2025      | MA    | SCR 150: Standardize source code column location
! -----------------+-------+--------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                               Passed Variables         Description / (units)
!-----------------------------+----------------------------------+------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN)  :: FileSourceCode                  ! Heliostat File
INTEGER(4)                    :: statusFormat                    ! Status Return Code  (n/a)
!
!                                Local
!-----------------------------+-----------
CHARACTER(128)                :: FileFormattedSourceCode         ! Updated Source Code File
INTEGER(4)                    :: chrF90                          ! Start of .f90 in source file
CHARACTER(LEN=512)            :: AsciiChr                        ! Ascii buffer used to read file data
CHARACTER(LEN=512)            :: AsciiSource                     ! Source Code output
CHARACTER(LEN=48)             :: AsciiType
CHARACTER(LEN=96)             :: AsciiVariable
CHARACTER(LEN=96)             :: AsciiComment
INTEGER(4)                    :: chrBang
INTEGER(4)                    :: LengthType
INTEGER(4)                    :: LengthVariable

INTEGER(4)                    :: chr                             ! Start of the Locale name in the ascii buffer
INTEGER(4)                    :: UnitCode                        ! Fortran I/O unit for the input file
INTEGER(4)                    :: UnitFile                        ! Fortran I/O unit for the output file
!
!-----------------------------------------------------------------------------------------------------------------------
!    S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  UnitFile = 0
  UnitCode = 0
  chrF90 = INDEX(FileSourceCode,".f90")
  IF(chrF90 < 1) THEN
    StatusFormat = UTIL_E_FILEOPEN
    RETURN
  END IF
  FileFormattedSourceCode = FileSourceCode(1:chrF90-1)//"_Formatted.f90"
  OPEN(NewUnit=UnitFile, FILE=TRIM(FileFormattedSourceCode), status="unknown", ERR=2)
  OPEN(NewUnit=UnitCode, FILE=TRIM(FileSourceCode),status="old",ERR=3)
  DO
    AsciiChr = SPACE
    READ(UnitCode,FMT='(A)',END=1, ERR=4) asciiChr
    IF(AsciiChr(1:3) .eq. "!!!") CYCLE
    AsciiChr = CleanString(AsciiChr)
    AsciiSource = SPACE
    IF(AsciiChr(1:8) .eq. "!-------" .and. INDEX(AsciiChr,"+") > 0) THEN
      DO chr = 1,150
        AsciiSource(chr:chr) = "-"
      END DO
      AsciiSource(1:1)   = "!"
      AsciiSource(31:31) = "+"
      AsciiSource(71:71) = "+"
    ELSE
      chr = INDEX(AsciiChr,":: ")
      IF(chr > 0 .and. AsciiChr(1:1) .ne. "!") THEN
        AsciiSource = SPACE
        AsciiType   = AsciiChr(1:chr-1)
        LengthType  = LEN_TRIM(AsciiType)
        IF(LengthType < 31) THEN
          AsciiSource(1:LengthType) = TRIM(AsciiType)
          AsciiSource(31:32) = "::"
          chrBang = INDEX(AsciiChr,"!")
          IF(chrBang > 0) THEN
            AsciiVariable = TRIM(ADJUSTL(AsciiChr(chr+3:chrBang-1)))
            AsciiComment  = AsciiChr(chrBang:)
          ELSE
            AsciiVariable = TRIM(ADJUSTL(AsciiChr(chr+3:)))
            AsciiComment  = "!"
          END IF
          LengthVariable = LEN_TRIM(AsciiVariable)
          IF(LengthVariable <= 37) THEN
            AsciiSource(34:34+LengthVariable-1) = TRIM(AsciiVariable)
            AsciiSource(71:) = TRIM(AsciiComment)
          ELSE
            AsciiSource(34:) = TRIM(AsciiVariable)//" "//TRIM(AsciiComment)
          END IF

        ELSE
           AsciiSource = TRIM(AsciiChr)
        END IF
      ELSE
        AsciiSource = TRIM(AsciiChr)
      END IF
    END IF
    WRITE(UnitFile, FMT='(A)',ERR=5) TRIM(AsciiSource)
  END DO
1 CLOSE(UnitCode)
  CLOSE(UnitFile)
  statusFormat = UTIL_S_NORMAL
  RETURN

!
!------------------------------------------------------------------------------------------------------------------------
!    E R R O R   H A N D L I N G
!------------------------------------------------------------------------------------------------------------------------
!
2 statusFormat = UTIL_E_FILEOPEN  ; CLOSE(UnitCode); RETURN
3 statusFormat = UTIL_E_FILEOPEN  ; CLOSE(UnitCode); CLOSE(UnitFile); RETURN
4 statusFormat = UTIL_E_FILEREAD  ; CLOSE(UnitCode); CLOSE(UnitFile); RETURN
5 statusFormat = UTIL_E_FILEWRITE ; CLOSE(UnitCode); CLOSE(UnitFile); RETURN

END FUNCTION StandardizeFormatSourceFile

FUNCTION SetColumnsForSQLFromCSV(FileSourceCSV) RESULT(statusCSV)
!+
!  Description: This routine standardizes a source file format
!
!  Audit Trail
! -----------------+-------+--------------------------------------------------------------------------------------------
! 15-APR-2025      | MA    | SCR 150: Ensure Column Titles for SQLK
! -----------------+-------+--------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                               Passed Variables         Description / (units)
!-----------------------------+----------------------------------+------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN)  :: FileSourceCSV                   ! CSV File
INTEGER(4)                    :: statusCSV                       ! Status Return Code  (n/a)
!
!                                Local
!-----------------------------+-----------
CHARACTER(128)                :: FileTargetSQL                   ! PostgreSQL Table Columns File
CHARACTER(128)                :: FileSourceEXP                   ! Habitat Database Export File
CHARACTER(128)                :: FileTargetCSV                   ! Updated CSV File
INTEGER(4)                    :: chrCSV                          ! Start of .csv in source file
INTEGER(4)                    :: chrComma                        ! Start of .csv in source file
CHARACTER(LEN=8192)           :: AsciiSourceChr                  ! Ascii buffer used to read file data
CHARACTER(LEN=8192)           :: AsciiTargetChr                  ! Source Code output
CHARACTER(LEN=8192)           :: AsciiCSVChr                     ! Source Code output
LOGICAL(4)                    :: FirstLine                       ! Line number read from source file
INTEGER(4)                    :: chrSource                       ! Index through source buffer
INTEGER(4)                    :: chrTarget                       ! Index through target buffer
INTEGER(4)                    :: chrColumn                       ! Index to keep track of SQL column names
LOGICAL(4)                    :: Skip                            ! Skip characters in the 1st line
INTEGER(4)                    :: UnitSource                      ! Fortran I/O unit for the input file
INTEGER(4)                    :: UnitTarget                      ! Fortran I/O unit for the output file
INTEGER(4)                    :: UnitExport                      ! Fortran I/O unit for the input file
INTEGER(4)                    :: UnitColumn                      ! Fortran I/O unit for the output file
CHARACTER(12)                 :: NameRecord
INTEGER(4)                    :: LenRecord
INTEGER(4)                    :: Line = 0
!
!-----------------------------------------------------------------------------------------------------------------------
!    S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  Line = 0
  UnitSource = 0
  UnitTarget = 0
  chrCSV = INDEX(FileSourceCSV,".csv")
  IF(chrcsv < 1) THEN
    StatusCSV = UTIL_E_FILEOPEN; WRITE(*,FMT='(A)') "UTIL_E_FILEOPEN: "//TRIM(FileSourceCSV)
    RETURN
  END IF
  FirstLine = .true.
  FileTargetSQL = FileSourceCSV(1:chrCSV-1)//".columns"
  FileSourceEXP = FileSourceCSV(1:chrCSV-1)//".exp"
  FileTargetCSV = FileSourceCSV(1:chrCSV-1)//"_Formatted.csv"
  OPEN(NewUnit=UnitSource, FILE=TRIM(FileSourceCSV), status="old"    ,ERR=2)
  OPEN(NewUnit=UnitTarget, FILE=TRIM(FileTargetCSV), status="unknown",ERR=3)
  DO
    AsciiSourceChr = SPACE
    AsciiTargetChr = SPACE
    IF(FirstLine) THEN;
      FirstLine  = .false.
      UnitExport = 0
      UnitColumn = 0
      OPEN(NewUnit=UnitExport, FILE=TRIM(FileSourceEXP), status="old"    ,ERR=2)
      OPEN(NewUnit=UnitColumn, FILE=TRIM(FileTargetSQL), status="unknown",ERR=2)
      READ(UnitExport,FMT='(A)',END=1, ERR=4) AsciiSourceChr
      WRITE(*,FMT='(" Length Line = ",I8)') LEN_TRIM(AsciiSourceChr)
      chrComma = INDEX(AsciiSourceChr,",")
      NameRecord = TRIM(AsciiSourceChr(1:chrComma-1))
      LenRecord  = LEN_TRIM(NameRecord)
      chrTarget = 0
      DO
        ChrComma = ChrComma+1
        IF(AsciiSourceChr(chrComma:ChrComma)==",") EXIT
      END DO
      AsciiTargetChr(1:LenRecord + 10) = "INSTANCE_"//TRIM(NameRecord)//","
      WRITE(UnitColumn,FMT='(A)')AsciiTargetChr(1:LenRecord+9)
      Skip = .false.
      chrTarget = LenRecord + 10
      chrColumn = chrTarget + 1
      DO ChrSource = chrComma+1, LEN_TRIM(AsciiSourceChr)
        IF(AsciiSourceChr(ChrSource:ChrSource) == "=") THEN
          Skip = .true.
          AsciiTargetChr(chrTarget+1:ChrTarget+LenRecord+1) = "_"//TRIM(NameRecord)
          chrTarget = chrTarget + LenRecord + 1
          WRITE(UnitColumn,FMT='(A)')AsciiTargetChr(chrColumn:chrTarget)
        END IF
        IF(AsciiSourceChr(ChrSource:ChrSource) == ",") THEN
          Skip = .false.
          ChrColumn = chrTarget + 2
        END IF
        IF(.not.Skip) THEN
          chrTarget = chrTarget + 1
          AsciiTargetChr(chrTarget:chrTarget) = AsciiSourceChr(chrSource:chrSource)
          IF(ChrTarget > 1) THEN
            IF(AsciiTargetChr(ChrTarget-1:ChrTarget) == "__") THEN
              chrTarget = chrTarget - 1
              AsciiTargetChr(ChrTarget:ChrTarget) = "$"
            END IF
          END IF
        END IF
      END DO
      CLOSE(UnitExport)
      CLOSE(UnitColumn)
    ELSE
      AsciiSourceChr = SPACE
      AsciiCSVChr    = SPACE
      AsciiTargetChr = SPACE
      READ(UnitSource,FMT='(A)',END=1, ERR=4) AsciiSourceChr
      chrComma = INDEX(AsciiSourceChr,",")
      AsciiCSVChr = TRIM(AsciiSourceChr(ChrComma+1:))
      chrTarget = 0
      chrCSV    = 1
      DO WHILE(chrCSV <= LEN_TRIM(AsciiCSVChr))
         IF(AsciiCSVChr(chrCSV:chrCSV+1) == ",F") THEN
           AsciiTargetChr(chrTarget+1:ChrTarget+6) = ",false"
           chrTarget = chrTarget + 6
           chrCSV    = chrCSV + 2
         ELSE IF(AsciiCSVChr(chrCSV:ChrCSV+1) == ",T") THEN
           AsciiTargetChr(ChrTarget+1:ChrTarget+5) = ",true"
           chrTarget = chrTarget + 5
           chrCSV    = chrCSV + 2
         ELSE
           chrTarget = chrTarget+1
           AsciiTargetChr(chrTarget:chrTarget) = AsciiCSVChr(chrCSV:chrCSV)
           chrCSV = chrCSV + 1
         END IF
       END DO
    END IF
    WRITE(UnitTarget, FMT='(A)',ERR=5) TRIM(AsciiTargetChr)
  END DO
1 CLOSE(UnitSource)
  CLOSE(UnitTarget)
  statusCSV = UTIL_S_NORMAL
  RETURN

!
!------------------------------------------------------------------------------------------------------------------------
!    E R R O R   H A N D L I N G
!------------------------------------------------------------------------------------------------------------------------
!
2 statusCSV = UTIL_E_FILEOPEN  ; CLOSE(UnitSource);                   WRITE(*,FMT='(A)')"UTIL_E_FILEOPEN  2 "//TRIM(FileSourceCSV); RETURN
3 statusCSV = UTIL_E_FILEOPEN  ; CLOSE(UnitSource); CLOSE(UnitTarget);WRITE(*,FMT='(A)')"UTIL_E_FILEOPEN  2 "//TRIM(FileTargetCSV); RETURN
4 statusCSV = UTIL_E_FILEREAD  ; CLOSE(UnitSource); CLOSE(UnitTarget);WRITE(*,FMT='(A)')"UTIL_E_FILEREAD  2 "//TRIM(FileSourceCSV); RETURN
5 statusCSV = UTIL_E_FILEWRITE ; CLOSE(UnitSource); CLOSE(UnitTarget);WRITE(*,FMT='(A)')"UTIL_E_FILEWRITE 2 "//TRIM(FileTargetCSV); RETURN
END FUNCTION SetColumnsForSQLFromCSV

END MODULE Utilities

