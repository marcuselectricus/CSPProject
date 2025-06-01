! Source: MatrixVectorPackage.f90
!
! Module: MatrixVectorPackage
!
! Purpose: Provide a library of commonly used mathematical vector and matrix functions and types
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Replace 2d0 by 2d0 and ONE by 1d0
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
MODULE MatrixVectorPackage
IMPLICIT NONE
!
INTERFACE Rotation
  MODULE PROCEDURE RotationAboutCoordinateAxis, RotationAboutVectorAxis, &
                   RotationWithUnitsAboutCoordinateAxis, RotationWithUnitsAboutVectorAxis
END INTERFACE

INTERFACE Normalize
  MODULE PROCEDURE NormalizeMatrix, NormalizeVector, NormalizeVectorN
END INTERFACE

INTERFACE Magnitude
  MODULE PROCEDURE Magnitude3, MagnitudeN
END INTERFACE
!
INTERFACE ValEstimate
  MODULE PROCEDURE ValEstimateNonZeroFromVector, ValEstimateWithStdvFromVector, ValWLSEstimateFromVector,ValWLSEstimateWithTolerance,ValWLSEstimateWithHiLoRemoval
END INTERFACE

TYPE, PUBLIC :: Cartesian2D
  SEQUENCE
  REAL(8)                   :: x                                ! x-coord
  REAL(8)                   :: y                                ! y-coord
END TYPE Cartesian2D

TYPE, PUBLIC :: Cartesian3D
  SEQUENCE
  REAL(8)                   :: x                                ! x-coord
  REAL(8)                   :: y                                ! y-coord
  REAL(8)                   :: z                                ! z-coord
END TYPE Cartesian3D

TYPE, PUBLIC :: RotationSpeed
  SEQUENCE
  REAL(8)                   :: Az                               ! Speed Azimuthal motor (degrees/minute)
  REAL(8)                   :: El                               ! Speed Elevation motor (degrees/minute)
END TYPE RotationSpeed

TYPE, PUBLIC :: RotateAzEl
  SEQUENCE
  REAL(8)                   :: Az                               ! Azimuthal Angle (degrees)
  REAL(8)                   :: El                               ! Elevation Angle (degrees)
END TYPE RotateAzEl

TYPE, PUBLIC :: Cylinder3D
  REAL(8)                   :: Radius                           ! Radius of cylinder
  REAL(8)                   :: Zlo                              ! Lo point of cylinder's elevation range.
  REAL(8)                   :: Zhi                              ! Hi point of cylinder's elevation range
END TYPE Cylinder3D

TYPE, PUBLIC :: CircleKind
    REAL(8)                 :: Rcenter_xy(2)
    REAL(8)                 :: Radius
END TYPE CircleKind

!                               Variable                          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),PARAMETER         :: Xaxis = 1                       !Rotate about X-Axis
INTEGER(4),PARAMETER         :: Yaxis = 2                       !Rotate about Y-Axis
INTEGER(4),PARAMETER         :: Zaxis = 3                       !Rotate about Z-Axis
INTEGER(4),PARAMETER         :: AxisX = 1                       !Rotate about X-Axis
INTEGER(4),PARAMETER         :: AxisY = 2                       !Rotate about Y-Axis
INTEGER(4),PARAMETER         :: AxisZ = 3                       !Rotate about Z-Axis
INTEGER(4),PARAMETER         :: Radians = 1                     !Angle Arguments are in radians
INTEGER(4),PARAMETER         :: Degrees = 2                     !Angle Arguments are in degrees
INTEGER(4),PARAMETER         :: MVP_S_NORMAL  = 1               !Normal return from routine
INTEGER(4),PARAMETER         :: MVP_E_FAILED  = 2               !Routine failed
INTEGER(4),PARAMETER         :: MVP_W_INPUT   = 3               !Input Warning

CONTAINS
PURE FUNCTION VectorFromCartesian2D(R) RESULT(R_m)
!+
! Description: This function casts a Cartesian2D as a Real(8) 2 dimensional vector
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
TYPE(Cartesian2D),INTENT(IN) :: R                               ! Cartesian 2D point        (distance)
REAL(8)                      :: R_m(2)                          ! 2D vector equivalent to R (distance)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  R_m = (/R%x,R%y/)
  RETURN
END FUNCTION VectorFromCartesian2D

FUNCTION VectorArrayFromCartesian2D(R_n, nEnd) RESULT(R_m_n)
!+
! Description: This function casts a Cartesian2D as a Real(8) 2 dimensional vector
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)                   :: nEnd                            ! Number of vectors
TYPE(Cartesian2D),INTENT(IN) :: R_n(nEnd)                       ! Cartesian 2D points         (distance)
REAL(8)                      :: R_m_n(2,nEnd)                   ! 2D vector equivalent to R_n (distance)
!
!                             Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)                   :: n                               ! Index through polygon points
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  DO n = 1, nEnd
    R_m_n(:,n) = VectorFromCartesian2D(R_n(n))
  END DO
  RETURN
END FUNCTION VectorArrayFromCartesian2D

FUNCTION InsidePolygon(R, R_n, nEnd) RESULT(Inside)
!+
! Description: This function returns a good status if a point R is inside the polygon defined by R_n.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE Polygons
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: nEnd                            ! Number of 2D points in the defining polygon
TYPE(Cartesian2D),INTENT(IN) :: R_n(nEnd)                       ! 2D points arranged clockwise from north (distance)
TYPE(Cartesian2D),INTENT(IN) :: R                               ! 2D point to test if it is inside the polygon
LOGICAL(4)                   :: Inside
!
!                             Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)                      :: R_m(2)                          ! Point corresponding to R (distance)
REAL(8)   ,ALLOCATABLE       :: R_m_n(:,:)                      ! Polygon vertices corresponding to R_n
INTEGER(4)                   :: n                               ! Index through polygon points
LOGICAL(4),PARAMETER         :: UsePolygons = .true.            ! Use Polygons::InteriorPointOfPolygon for test
REAL(8)                      :: Angle                           ! Azimuthal angle of point R (deg)
REAL(8)                      :: AngleN                          ! Azimuthal angle of point R_n(n) (deg)
REAL(8)                      :: A_m_m(2,2)                      ! Matrix so solve for coordinates
REAL(8)                      :: k_m(2)                          ! coordinates in space with triangle sides as basis
LOGICAL(4)                   :: FullRank                        ! A_m_m is full rank
REAL(8)   ,ALLOCATABLE       :: R_xy_n(:,:)
REAL(8)                      :: Rc_xy(2)
TYPE(Cartesian2D),ALLOCATABLE:: Rv_n(:)
TYPE(Cartesian2D)            :: Ro
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ALLOCATE(R_m_n(2,0:nEnd+1))

R_m = (/R%x,R%y/)
DO n = 1, nEnd
  R_m_n(:,n)    = (/R_n(n)%x, R_n(n)%y/)
END DO
!
! Ensure "wrap-around" on both ends
R_m_n(:,nEnd+1) = R_m_n(:,1)
R_m_n(:,0)      = R_m_n(:,nEnd)

! Ensure that no segment is truly horizontal
DO n = 2, nEnd-1
  IF(R_m_n(2,n)==R_m_n(2,n-1)) R_m_n(2,n) = R_m_n(2,n) - 1d-10
END DO

Inside = InteriorOfPolygon(R_m, R_m_n, nEnd=nEnd)

DEALLOCATE(R_m_n)
IF(UsePolygons) RETURN

ALLOCATE(R_xy_n(2,nEnd))
ALLOCATE(Rv_n(nEnd))
!
! Old algorithm which performed triangle tests.  This algorithm will not work for non-convex polygons.
!
! Calculate the "Center" of the
Rc_xy = 0d0
DO n = 1, nEnd
  R_xy_n(:,n) = (/R_n(n)%x, R_n(n)%y/)
  Rc_xy       = Rc_xy + R_xy_n(:,n)
END DO
Rc_xy = Rc_xy/DBLE(nEnd)

Ro%x  = R%x - Rc_xy(1)
Ro%y  = R%y - Rc_xy(2)

DO n = 1, nEnd
 R_xy_n(:,n) = R_xy_n(:,n) - Rc_xy
 Rv_n(n)%x = R_xy_n(1,n)
 Rv_n(n)%y = R_xy_n(2,n)
END DO

Angle = DATAN2D(Ro%x, Ro%y) ; IF(Angle < 0) Angle = Angle + 360d0
DO n = 1, nEnd

  AngleN = DATAN2D(Rv_n(n)%x, Rv_n(n)%y) ; IF(AngleN < 0) AngleN = AngleN + 360d0
  IF(Angle <= AngleN) THEN
    IF(n==1) THEN
      A_m_m(1,:) = (/Rv_n(nEnd)%x, Rv_n(1)%x/)
      A_m_m(2,:) = (/Rv_n(nEnd)%y, Rv_n(1)%y/)
    ELSE
      A_m_m(1,:) = (/Rv_n(n-1)%x , Rv_n(n)%x/)
      A_m_m(2,:) = (/Rv_n(n-1)%y , Rv_n(n)%y/)
    END IF
    EXIT
  ELSE IF(n==nEnd) THEN
      A_m_m(1,:) = (/Rv_n(nEnd)%x, Rv_n(1)%x/)
      A_m_m(2,:) = (/Rv_n(nEnd)%y, Rv_n(1)%y/)
    EXIT
  END IF
END DO
k_m = MATMUL(Inverse2x2(A_m_m,FullRank),(/Ro%x,Ro%y/))
IF(FullRank .and. k_m(1) > 0d0 .and. k_m(2) > 0d0 .and. k_m(1)+k_m(2) < 1d0) THEN
  Inside = .true.
ELSE
  Inside = .false.
END IF


DEALLOCATE(R_xy_n)
DEALLOCATE(Rv_n)

RETURN
END FUNCTION InsidePolygon

FUNCTION NormalVectorAzEl(AngleAz, AngleEl) RESULT(N_ENU)
!+
! Description: This function returns the normal unit vector for an az/el rotation
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: AngleAz                         ! Azimuthal Drive Angle        (degrees)
REAL(8)   , INTENT(IN)       :: AngleEl                         ! Elevation Drive Angle        (degrees)
REAL(8)                      :: N_ENU(3)                        ! Normal vector from rotations (per-unit)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  N_ENU = (/SIND(AngleAz )*COSD(AngleEl ), COSD(AngleAz )*COSD(AngleEl ), SIND(AngleEl )/)
END FUNCTION NormalVectorAzEl

PURE FUNCTION ProductRotations(Axis1,Angle1, Axis2,Angle2, Axis3,Angle3) RESULT(Rotation_n_n)
!+
! Description: This function returns the matrix product of 3 rotation matrices about the (x,y and z) axis
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Explicit declare intermediate rotations in the product
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                  Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Axis1                           !Axis of 1st rotation, X, Y or Z
REAL(8)   , INTENT(IN)       :: Angle1                          !1st Rotation angle (radians)
INTEGER(4), INTENT(IN)       :: Axis2                           !Axis of 2nd rotation, X, Y or Z
REAL(8)   , INTENT(IN)       :: Angle2                          !2nd Rotation angle (radians)
INTEGER(4), INTENT(IN)       :: Axis3                           !Axis of 3rd rotation, X, Y or Z
REAL(8)   , INTENT(IN)       :: Angle3                          !3rd Rotation angle (radians)
REAL(8)                      :: Rotation_n_n(3,3)               !Rotation matrix
!
!                               Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)                      :: Rotation1_n_n(3,3)              !Rotation about the Axis1
REAL(8)                      :: Rotation2_n_n(3,3)              !Rotation about the Axis2
REAL(8)                      :: Rotation3_n_n(3,3)              !Rotation about the Axis3
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
Rotation1_n_n = Rotation(Axis1, Angle1)
Rotation2_n_n = Rotation(Axis2, Angle2)
Rotation3_n_n = Rotation(Axis3, Angle3)
!
Rotation_n_n = MATMUL(Rotation1_n_n,MATMUL(Rotation2_n_n,Rotation3_n_n))
RETURN
END FUNCTION ProductRotations

PURE FUNCTION RotationAboutCoordinateAxis(Axis, Angle) RESULT(Rotation_n_n)
!+
! Description: This function calculates the rotation matrix about one of the coordinate axis (x,y or z) with
!              an assumed that the rotation angle is in radians
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variable                    Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Axis                            !Axis of rotation, X, Y or Z
REAL(8)   , INTENT(IN)       :: Angle                           !Rotation angle (radians)
REAL(8)                      :: Rotation_n_n(3,3)               !Rotation matrix
!
!-----------------------------------------------------------------------------------------------------------------------

!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Rotation_n_n = RotationWithUnitsAboutCoordinateAxis(Axis, Angle, Units=Radians)
RETURN
END FUNCTION RotationAboutCoordinateAxis

PURE FUNCTION RotationByAngleAboutVector(R_n, Angle) RESULT(Rotation_n_n)
!+
! Description: This function calculates the rotation matrix about a vector through an angle.  It uses the
!              Rodrigues Rotation Formula.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variable                    Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: R_n(3)                          ! Vector defining the axis of rotation
REAL(8), INTENT(IN)          :: Angle                           ! Rotation angle counterclockwise about R_n with R_n pointing at the observer (degrees)
REAL(8)                      :: Rotation_n_n(3,3)               ! Rotation matrix
!
!                             Local Variable
!----------------------------+----------------------------------
REAL(8)                      :: K_n(3)                          ! Normal vector of R_n
REAL(8)                      :: K_n_n(3,3)                      ! Rodrigues Formula K matrix defined by [K]v = k x v
REAL(8)   , PARAMETER        :: IdentityMatrix_3x3(3,3)           = RESHAPE((/1,0,0,0,1,0,0,0,1/),(/3,3/))

ASSOCIATE  (Kx=>K_n(1), Ky=>K_n(2), Kz=>K_n(3))
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  K_n = Normalize(R_n)
  K_n_n = TRANSPOSE(RESHAPE((/0d0, -Kz, +Ky  &
                             ,+Kz, 0d0, -Kx  &
                             ,-Ky, +Kx, 0d0/), (/3,3/)))
  Rotation_n_n = IdentityMatrix_3x3 + SIND(Angle)*K_n_n + (1d0 - COSD(Angle))*MATMUL(K_n_n,K_n_n)

END ASSOCIATE
RETURN
END FUNCTION RotationByAngleAboutVector

PURE FUNCTION RotationMatrixEastThenNorthTilt(AngleEast,AngleNorth) RESULT(T_NET_ENU)
!+
! Description: This function calculates the rotation matrix for tilting East, then North.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variables sinAz and sinEl
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)           :: AngleEast                       ! East  tilt (degrees)
REAL(8),INTENT(IN)           :: AngleNorth                      ! North tilt (degrees)
REAL(8)                      :: T_NET_ENU(3,3)                  ! Rotation Matrix for Pedestal Tilt (east then north)
!
!                             Local Variables
!----------------------------+---------------
REAL(8)                      :: cosN                            ! North tilt angle cosine
REAL(8)                      :: cosE                            ! East  tilt angle cosine
REAL(8)                      :: sinN                            ! North tilt angle sine
REAL(8)                      :: sinE                            ! East  tilt angle sine
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
cosE = COSD(AngleEast); sinE = SIND(AngleEast); cosN = COSD(AngleNorth); sinN = SIND(AngleNorth)
T_NET_ENU = TRANSPOSE(RESHAPE((/ cosE, sinN*sinE,  cosN*sinE &
                               , 0d0 , cosN     , -sinN      &
                               ,-sinE, sinN*cosE,  cosN*cosE /), (/3,3/)))
RETURN
END FUNCTION RotationMatrixEastThenNorthTilt

PURE FUNCTION RotationWithUnitsAboutCoordinateAxis(Axis, Angle, Units)  RESULT(Rotation_n_n)
!+
! Description: This function calculates the rotation matrix about one of the coordinate axis (x,y or z)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                               Variable                          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: Axis                            !Axis of rotation, X, Y or Z
REAL(8)   , INTENT(IN)       :: Angle                           !Rotation angle (radians or degrees)
INTEGER(4), INTENT(IN)       :: Units                           !Angle Units, Radians or Degrees
REAL(8)                      :: Rotation_n_n(3,3)               !Rotation matrix
!
!                               Local
!----------------------------+-------
REAL(8)                      :: CosAng                          !angle cosine
REAL(8)                      :: SinAng                          !angle sine
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
!Initialize rotation matrix
Rotation_n_n = UnitaryMatrix_3x3

!Calculate terms needed for the rotation matrix
IF(Units == Radians) THEN
  cosAng = DCOS (angle); sinAng = DSIN (angle)
ELSE
  cosAng = DCOSD(angle); sinAng = DSIND(angle)
END IF

!Calculate matrix based on rotation axis
RotateAboutAxis: SELECT CASE (Axis)
CASE (Xaxis)
   Rotation_n_n(2,2) = +cosAng; Rotation_n_n(2,3) = +sinAng
   Rotation_n_n(3,2) = -sinAng; Rotation_n_n(3,3) = +cosAng
CASE (Yaxis)
   Rotation_n_n(1,1) = +cosAng; Rotation_n_n(1,3) = -sinAng
   Rotation_n_n(3,1) = +sinAng; Rotation_n_n(3,3) = +cosAng
CASE (Zaxis)
   Rotation_n_n(1,1) = +cosAng; Rotation_n_n(1,2) = +sinAng
   Rotation_n_n(2,1) = -sinAng; Rotation_n_n(2,2) = +cosAng
END SELECT RotateAboutAxis

RETURN
END FUNCTION RotationWithUnitsAboutCoordinateAxis
!
PURE FUNCTION RotationAboutVectorAxis(R_Axis, Angle) RESULT(Rotation_n_n)
!+
! Description: This function calculates the rotation matrix about an input vector.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Variable                          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: R_Axis(3)                       !Vector rotation axis (distance)
REAL(8), INTENT(IN)          :: Angle                           !Angle (radians)
REAL(8)                      :: Rotation_n_n(3,3)               !Rotation Matrix Result

Rotation_n_n = RotationWithUnitsAboutVectorAxis(R_Axis, Angle, Units=Radians)
RETURN
END FUNCTION RotationAboutVectorAxis
!
PURE FUNCTION RotationWithUnitsAboutVectorAxis(R_Axis, Angle, Units) RESULT(Rotation_n_n)
!+
! Description: This function calculates the rotation matrix about an input vector
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable q4
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variable                  Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: R_Axis(3)                       !Vector rotation axis (distance
REAL(8), INTENT(IN)          :: Angle                           !Rotation Angle (radians or degrees)
INTEGER(4), INTENT(IN)       :: Units                           !Angle Units, Radians or Degrees
REAL(8)                      :: Rotation_n_n(3,3)               !Rotation Matrix Result (n/a)
!
!                               Local
!----------------------------+---------------------------------
REAL(8)                      :: q0,q1,q2,q3                    !Quaternion representing rotation
REAL(8)                      :: N_Axis(3)                      !R_Axis normalized
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
N_Axis = Normalize(R_Axis)

IF(Units==Radians) THEN
  q0  = DCOS(Angle/2d0)
  q1  = DSIN(Angle/2d0)*N_Axis(1)
  q2  = DSIN(Angle/2d0)*N_Axis(2)
  q3  = DSIN(Angle/2d0)*N_Axis(3)
ELSE
  q0  = DCOSD(Angle/2d0)
  q1  = DSIND(Angle/2d0)*N_Axis(1)
  q2  = DSIND(Angle/2d0)*N_Axis(2)
  q3  = DSIND(Angle/2d0)*N_Axis(3)
END IF

Rotation_n_n(1,:) = (/q1**2-q2**2-q3**2+q0**2,  2d0*(q1*q2+q3*q0)       , 2d0*(q1*q3-q2*q0)       /)
Rotation_n_n(2,:) = (/2d0*(q1*q2-q3*q0)      ,  -q1**2+q2**2-q3**2+q0**2, 2d0*(q2*q3+q1*q0)       /)
Rotation_n_n(3,:) = (/2d0*(q1*q3+q2*q0)      ,  2d0*(q2*q3-q1*q0)       , -q1**2-q2**2+q3**2+q0**2/)

END FUNCTION RotationWithUnitsAboutVectorAxis

PURE FUNCTION CrossProduct(a_vec,b_vec) RESULT(CrossProduct_vec)
!+
! Description: This function returns a vector that is the cross product of the two input vectors
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables                 Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: a_vec(3)                        !Vector input
REAL(8), INTENT(IN)          :: b_vec(3)                        !Vector input
REAL(8)                      :: CrossProduct_vec(3)             !Cross product of a_vec and b_vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
CrossProduct_vec(1) = a_vec(2)*b_vec(3) - b_vec(2)*a_vec(3)
CrossProduct_vec(2) = a_vec(3)*b_vec(1) - b_vec(3)*a_vec(1)
CrossProduct_vec(3) = a_vec(1)*b_vec(2) - b_vec(1)*a_vec(2)

RETURN
END FUNCTION CrossProduct

PURE FUNCTION NormalizeMatrix(M_n_n) RESULT(N_n_n)
!+
! Description: This function returns a matrix with rows that are normalized
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable mag
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variable                    Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: M_n_n(3,3)                      !Matrix input
REAL(8)                      :: N_n_n(3,3)                      !Normalized matrix of M_n_n
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
N_n_n(:,1) = Normalize(M_n_n(:,1))
N_n_n(:,2) = Normalize(M_n_n(:,2))
N_n_n(:,3) = Normalize(M_n_n(:,3))
RETURN
END FUNCTION NormalizeMatrix

PURE FUNCTION NormalizeVector(a_vec) RESULT(n_vec)
!+
! Description: This function returns a normal vector that is a constant multiple of the input vector.  If the
!              input vector is null then it returns a null vector.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: a_vec(3)                        !Vector input
REAL(8)                      :: n_vec(3)                        !Unit vector derived from a_vec
!
!                               Local
!----------------------------+-------
REAL(8)                      :: mag
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
mag = Magnitude(a_vec)
IF(mag > 0) THEN
  n_vec = a_vec/Mag
ELSE
  n_vec = 0
END IF
RETURN
END FUNCTION NormalizeVector

PURE FUNCTION NormalizeVectorN(a_vec, nVec) RESULT(n_vec)
!+
! Description: This function returns a normal vector that is a constant multiple of the input vector.  If the
!              input vector is null then it returns a null vector.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: nVec                            !Size of vector
REAL(8)   , INTENT(IN)       :: a_vec(nVec)                     !Vector input
REAL(8)                      :: n_vec(nVec)                     !Unit vector derived from a_vec
!
!                               Local
!----------------------------+-------
REAL(8)                      :: mag
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
mag = Magnitude(a_vec,nVec)
IF(mag > 0) THEN
  n_vec = a_vec/Mag
ELSE
  n_vec = 0
END IF
RETURN
END FUNCTION NormalizeVectorN

FUNCTION CosAngleVectors(a_vec, b_vec) RESULT(cosAngle)
!+
! Description: Returns the angle cosine between two vectors.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                               Variable                          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: a_vec(3)                        !Vector input
REAL(8), INTENT(IN)          :: b_vec(3)                        !Vector input
REAL(8)                      :: cosAngle                        !Angle Between the two vectors (radians)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
cosAngle = DOT_PRODUCT(a_vec,b_vec)/(MAGNITUDE(a_vec)*MAGNITUDE(b_vec) + Epsilon)
RETURN
END FUNCTION CosAngleVectors

PURE FUNCTION Magnitude3(a_vec) RESULT(Mag)
!+
! Description: Returns the magnitude of a 3D vector.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Variable                          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: a_vec(3)                        !Vector input
REAL(8)                      :: mag                             !magnitude of a_vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Mag = DSQRT(DOT_PRODUCT(a_vec,a_vec))
RETURN
END FUNCTION Magnitude3

PURE FUNCTION MagnitudeN(a_vec, vecEnd) RESULT(Mag)
!+
! Description: Returns the magnitude of a nD vector.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Variable                          Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: vecEnd                          !Number of elements in the vector
REAL(8), INTENT(IN)          :: a_vec(vecEnd)                   !Vector input
REAL(8)                      :: mag                             !magnitude of a_vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Mag = DSQRT(DOT_PRODUCT(a_vec,a_vec))
RETURN
END FUNCTION MagnitudeN

PURE FUNCTION OrthogonalVector(R_n) RESULT(M_n)
!+
! Description: This function calculates a unit vector orthogonal to a input vector, R_n.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: R_n(3)                          !Input Vector (any units)
REAL(8)                      :: M_n(3)                          !Unit Vector Orthogonal to R_n (n/a)

!                             Local Variables
!----------------------------+----------------------------------
REAL(8)                      :: Norm                            !Local for vector norm
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Norm = SQRT(DOT_PRODUCT(R_n,R_n))
IF(Norm == 0d0) THEN
  M_n = (/1d0,0d0,0d0/)
ELSE
  IF(ABS(R_n(3)) < Norm/2d0) THEN
    M_n = (/-R_n(2),R_n(1),0d0/)
  ELSE
    M_n = (/0.5d0+R_n(2),-(0.5d0+R_n(1)),-0.5d0*(R_n(1)-R_n(2))/R_n(3)/)
  END IF
END IF
Norm = SQRT(DOT_PRODUCT(M_n,M_n))
M_n  = M_n/Norm
RETURN
!
END FUNCTION OrthogonalVector

FUNCTION Inverse2x2(M_n_n,FullRank) RESULT(I_n_n)
!+
! Description: This function calculates the inverse of a 2x2 matrix.  A logical flag is returned
!              false if the matrix is not of rank 2
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: M_n_n(2,2)                      ! 2x2 matrix
LOGICAL(4), INTENT(OUT)      :: FullRank                        ! Set true if matrix is full rank
REAL(8)                      :: I_n_n(2,2)                      ! Inverse of M_n_n

!                             Local Variables
! ---------------------------+----------------------------------
REAL(8)                      :: det                             ! Matrix determinant
REAL(8)                      :: Minv_n_n(2,2)                   ! Working copy
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!

det = M_n_n(1,1)*M_n_n(2,2) - M_n_n(1,2)*M_n_n(2,1)
IF(ABS(det) > Epsilon) THEN
  Minv_n_n   = RESHAPE((/M_n_n(2,2),-M_n_n(2,1),-M_n_n(1,2),M_n_n(1,1)/),(/2,2/))/det
  I_n_n      = Minv_n_n
  FullRank   = TRUE
ELSE
  I_n_n      = 0d0
  FullRank   = FALSE
END IF
RETURN
END FUNCTION Inverse2x2

PURE SUBROUTINE Inverse2x2Matrix(M_n_n,FullRank,I_n_n)
!+
! Description: This function calculates the inverse of a 2x2 matrix.  A logical flag is returned
!              false if the matrix is not of rank 2
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: M_n_n(2,2)                      ! 2x2 matrix
LOGICAL(4), INTENT(OUT)      :: FullRank                        ! Set true if matrix is full rank
REAL(8)   , INTENT(OUT)      :: I_n_n(2,2)                      ! Inverse of M_n_n

!                             Local Variables
! ---------------------------+----------------------------------
REAL(8)                      :: det                             ! Matrix determinant
REAL(8)                      :: Minv_n_n(2,2)                   ! Working copy
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!

det = M_n_n(1,1)*M_n_n(2,2) - M_n_n(1,2)*M_n_n(2,1)
IF(ABS(det) > Epsilon) THEN
  Minv_n_n   = RESHAPE((/M_n_n(2,2),-M_n_n(2,1),-M_n_n(1,2),M_n_n(1,1)/),(/2,2/))/det
  I_n_n      = Minv_n_n
  FullRank   = TRUE
ELSE
  I_n_n      = 0d0
  FullRank   = FALSE
END IF
END SUBROUTINE Inverse2x2Matrix

FUNCTION Inverse3x3(M_n_n,FullRank) RESULT(I_n_n)
!+
! Description: This function calculates the inverse of a 3x3 matrix.  A logical flag is returned
!              false if the matrix is not of rank 3
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                             Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: M_n_n(3,3)                      ! 3x3 matrix
LOGICAL(4), INTENT(OUT)      :: FullRank                        ! Set true if matrix is full rank
REAL(8)                      :: I_n_n(3,3)                      ! Inverse of M_n_n

!                                Local Variables
!----------------------------+----------------------------------
REAL(8)                      :: det                             ! Matrix determinant
REAL(8)                      :: Iii
REAL(8)                      :: Iij
REAL(8)                      :: Iik
REAL(8)                      :: Iji
REAL(8)                      :: Ijj
REAL(8)                      :: Ijk
REAL(8)                      :: Iki
REAL(8)                      :: Ikj
REAL(8)                      :: Ikk
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Iii = M_n_n(3,3)*M_n_n(2,2) - M_n_n(3,2)*M_n_n(2,3)      ! 33 22  - 32 23
Iij = M_n_n(3,2)*M_n_n(1,3) - M_n_n(3,3)*M_n_n(1,2)      ! 32 13  - 33 12
Iik = M_n_n(2,3)*M_n_n(1,2) - M_n_n(2,2)*M_n_n(1,3)      ! 23 12  - 22 13

det = M_n_n(1,1)*Iii + M_n_n(2,1)*Iij + M_n_n(3,1)*Iik
IF(ABS(det) > Epsilon) THEN
  Iji = M_n_n(3,1)*M_n_n(2,3) - M_n_n(3,3)*M_n_n(2,1)    ! 31 23 - 33 21
  Ijj = M_n_n(3,3)*M_n_n(1,1) - M_n_n(3,1)*M_n_n(1,3)    ! 33 11 - 31 13
  Ijk = M_n_n(2,1)*M_n_n(1,3) - M_n_n(2,3)*M_n_n(1,1)    ! 21 13 - 23 11
  Iki = M_n_n(3,2)*M_n_n(2,1) - M_n_n(3,1)*M_n_n(2,2)    ! 32 21 - 31 22
  Ikj = M_n_n(3,1)*M_n_n(1,2) - M_n_n(3,2)*M_n_n(1,1)    ! 31 12 - 32 11
  Ikk = M_n_n(2,2)*M_n_n(1,1) - M_n_n(2,1)*M_n_n(1,2)    ! 22 11 - 21 12
  I_n_n      = RESHAPE((/Iii,Iji,Iki,Iij,Ijj,Ikj,Iik,Ijk,Ikk/),(/3,3/))/det
  FullRank   = TRUE
ELSE
  I_n_n      = 0d0
  FullRank   = FALSE
END IF
RETURN
END FUNCTION Inverse3x3

FUNCTION InverseCholeskyNxN(HTH, N, FullRank)  RESULT(HTHinv)
USE NumericalConstants
!+
! Description: Calculates the inverse of an NxN symmetric positive definite matrix
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                   Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: N                               ! Rank of matrix
REAL(8),    INTENT(IN)       :: HTH   (1:N,1:N)                 ! Matrix ( H'H )
LOGICAL(4), INTENT(OUT)      :: FullRank                        ! Set true if matrix is full rank
REAL(8)                      :: HTHinv(1:N,1:N)                 ! Inverse of HTH
!
!                             Local Variables
!----------------------------+----------------------------------
REAL(8)                      :: L     (1:N,1:N)
REAL(8)                      :: A     (1:N,1:N)
REAL(8)                      :: D     (1:N)
REAL(8)                      :: M     (1:N,1:N)
REAL(8)                      :: Dinv  (1:N,1:N)
INTEGER(4)                   :: i
INTEGER(4)                   :: j
INTEGER(4)                   :: k
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
! Initialize for Cholesky Factorization
A = HTH; L = 0d0; D = 0d0; M = 0d0; Dinv = 0d0

DO i = 1, N
  M(i,i)    = 1d0
  D(i)      = A(i,i)
  IF(ABS(D(i)) < Epsilon) THEN
    HTHinv   = 0d0
    FullRank = FALSE
    RETURN
  END IF
  Dinv(i,i) = 1d0/D(i)
  L(i,i)    = 1d0
  IF (i == N) EXIT
  L(i+1:N, i) = A(i, i+1:N) / D(i)
  DO j=i,N
    A(j, j:N) = A(j, j:N) - (L(j, i)*D(i))*L(j:N, i)
  END DO
END DO
FullRank = TRUE

! Matrix Inversion
DO i = 1, N - 1
  M(i + 1:N, i) = -L(i + 1:N, i)
  DO j = i + 1, N
    DO k = j + 1, N
      M(k, i) = M(k, i) - L(k, j) * M(j, i)
    END DO
  END DO
END DO
HTHinv = MATMUL(MATMUL(TRANSPOSE(M), Dinv), M)
RETURN
END FUNCTION InverseCholeskyNxN

PURE FUNCTION kRayIntersectionWithCone(Rray_ENU, Nray_ENU, Zo, tanAngle) RESULT(k_n)
!
! Note: Improvement Possible: This routine needs to be more thoroughly tested.  It is not presently used
!+
! Description: This routine calculates the intersection points of a ray with a con.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE

!  Passed Variables                                     Description/(Units)
! --------------------------------------------------------------------------------------------
REAL(8)   , INTENT(IN)           :: Rray_ENU(3)        !Ray origin (m)
REAL(8)   , INTENT(IN)           :: Nray_ENU(3)        !Ray direction (unit vector)
REAL(8)   , INTENT(IN)           :: Zo                 !Origin of Cone (m)
REAL(8)   , INTENT(IN)           :: tanAngle           !Tangent of Cone Angle
REAL(8)                          :: k_n(2)             !Ray intersection with cone (m)

!  Local Variables
! -----------------------------------------------------
REAL(8)                          :: a                  !Square Term of the quadratic equation
REAL(8)                          :: b                  !Linear Term of the quadratic equation
REAL(8)                          :: c                  !Scalar Term of the quadratic equation
REAL(8)                          :: d                  !Discriminant of the quadratic equation
REAL(8)                          :: t                  !Inverse of TanAngle squared
REAL(8)                          :: k1                 !1st solution of quadratic equation
REAL(8)                          :: k2                 !2nd Solution of quadratic equation

!  S T A R T   O F   E X E C U T A B L E   C O D E
! -----------------------------------------------------
t = 1.0d0/tanAngle**2
a = Nray_ENU(1)**2 + Nray_ENU(2)**2 - t*Nray_ENU(3)**2
b = 2.0d0 * (Rray_ENU(1)*Nray_ENU(1) + Rray_ENU(2)*Nray_ENU(2)- t*Nray_ENU(3)*(Rray_ENU(3) - Zo))
c = Rray_ENU(1)**2 + Rray_ENU(2)**2 - t*(Rray_ENU(3) + Zo**2 - 2.0d0*Zo*Rray_ENU(3))
d = b**2 - 4.0d0 * a * c
IF (d<0 .or. a==0.0d0) THEN
  k_n = -1.0d0
ELSE
  k1 = (-b + DSQRT(d))/2.0d0/a
  k2 = (-b - DSQRT(d))/2.0d0/a
  IF(k1 <= k2) THEN
    k_n(1) = k1
    k_n(2) = k2
  ELSE
    k_n(1) = k2
    k_n(2) = k1
  END IF
END IF
END FUNCTION kRayIntersectionWithCone

PURE FUNCTION FindRayIntersectionWithPlane(Nb_n_m, Nr_n, Rc_n, Rh_n) RESULT(k_m)
!
! Note: Improvement Possible: This routine needs to be more thoroughly tested.  It is not presently used
!+
! Description: This routine calculates the intersection points of a ray with a plane.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!
!

!  Passed Variables                                     Description/(Units)
! -----------------------------------------------------+-------------------------------------------
REAL(8)   , INTENT(IN)           :: Nb_n_m(3,2)        !Basis vectors for plane
REAL(8)   , INTENT(IN)           :: Rh_n(3)            !Point outside of plane
REAL(8)   , INTENT(IN)           :: Nr_n(3)            !Ray proceeding from point, Rh_n
REAL(8)   , INTENT(IN)           :: Rc_n(3)            !Reference point in plane
REAL(8)                          :: k_m(2)             !Plane Coordinates of intersection point

!  Local Variables
! -----------------------------------------------------
REAL(8)                          :: A_m_m(2,2)
REAL(8)                          :: B_m_i(2,1)
REAL(8)                          :: C_i_m(1,2)
REAL(8)                          :: D
REAL(8)                          :: G_m_m(2,2)
REAL(8)                          :: H_m_m(2,2)
REAL(8)                          :: I_m_m(2,2)
REAL(8)                          :: b_m(2)
LOGICAL(4)                       :: FullRank
!
!---------------------------------------------------------------------------------------------------
!  S T A R T   O F   E X E C U T A B L E   C O D E
! --------------------------------------------------------------------------------------------------
!
!Check to see if ray cannot possibly intersect the plane
IF(ABS(Nr_n(3)) < Epsilon) THEN
  k_m = Infinity
ELSE
!Other
  A_m_m(:,:) = Nb_n_m(1:2,:)
  B_m_i(:,1) = Nr_n  (1:2)
  C_i_m(1,:) = Nb_n_m(3,:)
  D          = Nr_n  (3)
  G_m_m      = A_m_m - MATMUL(B_m_i,C_i_m)/D
  b_m        = Rh_n(1:2) - Rc_n(1:2) - B_m_i(:,1)*(Rh_n(3) - Rc_n(3))/D
  CALL Inverse2x2Matrix(G_m_m,FullRank,H_m_m )
  I_m_m = MATMUL(G_m_m,H_m_m)
  IF(FullRank) THEN
    k_m = MATMUL(H_m_m,b_m)
  ELSE
    k_m = Infinity
  END IF
END IF
RETURN

END FUNCTION FindRayIntersectionWithPlane

FUNCTION IntersectionPointOnPlaneWithRay(P_n, Nb_n_m, dNr_n, Rh_n) RESULT(k_n)
!+
! Description: This routine calculates the intersection points of a ray with a plane.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!
!  Passed Variables                                     Description/(Units)
! -----------------------------------------------------+-------------------------------------------
REAL(8)   , INTENT(IN)           :: P_n(3)             ! Coordinates of a reference in the plane
REAL(8)   , INTENT(IN)           :: Nb_n_m(3,2)        ! Basis vectors for plane
REAL(8)   , INTENT(IN)           :: dNr_n(3)           ! Ray proceeding from point, Rh_n
REAL(8)   , INTENT(IN)           :: Rh_n(3)            ! Point outside of plane
REAL(8)                          :: k_n(3)             ! distance ray travels to intersection and local coordinates on place

!  Local Variables
! -----------------------------------------------------
REAL(8)                          :: N_n_n   (3,3)
REAL(8)                          :: Ninv_n_n(3,3)
REAL(8)                          :: kLocal_n(3)
REAL(8)                          :: dX
REAL(8)                          :: dY
REAL(8)                          :: dR
EQUIVALENCE                        (kLocal_n(1),dX),(kLocal_n(2),dY),(kLocal_n(3),dR)
LOGICAL(4)                       :: FullRank
!
!---------------------------------------------------------------------------------------------------
!  S T A R T   O F   E X E C U T A B L E   C O D E
! --------------------------------------------------------------------------------------------------
  !
  N_n_n(1,:) = (/Nb_n_m(1,1),Nb_n_m(1,2),-dNr_n(1)/)
  N_n_n(2,:) = (/Nb_n_m(2,1),Nb_n_m(2,2),-dNr_n(2)/)
  N_n_n(3,:) = (/Nb_n_m(3,1),Nb_n_m(3,2),-dNr_n(3)/)

  Ninv_n_n   = Inverse3x3(N_n_n,FullRank)

  IF(.not.FullRank) THEN
    k_n = 0d0
  ELSE
    k_n = MATMUL(Ninv_n_n,Rh_n-P_n)
  END IF
  RETURN
END FUNCTION IntersectionPointOnPlaneWithRay

FUNCTION kRayIntersectionWithPlane(P_n, Nb_n_m, Nr_n, Rh_n) RESULT(k)
!+
! Description: This routine calculates the intersection points of a ray with a plane.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!
!  Passed Variables                                     Description/(Units)
! -----------------------------------------------------+-------------------------------------------
REAL(8)   , INTENT(IN)           :: P_n(3)             ! Coordinates of a reference in the plane
REAL(8)   , INTENT(IN)           :: Nb_n_m(3,2)        ! Basis vectors for plane
REAL(8)   , INTENT(IN)           :: Nr_n(3)            ! Ray proceeding from point, Rh_n
REAL(8)   , INTENT(IN)           :: Rh_n(3)            ! Point outside of plane
REAL(8)                          :: k                  ! distance ray travels to intersection

!  Local Variables
! -----------------------------------------------------
REAL(8)                          :: N_n_n   (3,3)
REAL(8)                          :: Ninv_n_n(3,3)
REAL(8)                          :: k_n     (3)
LOGICAL(4)                       :: FullRank
!
!---------------------------------------------------------------------------------------------------
!  S T A R T   O F   E X E C U T A B L E   C O D E
! --------------------------------------------------------------------------------------------------
!
N_n_n(1,:) = (/Nb_n_m(1,1),Nb_n_m(1,2),-Nr_n(1)/)
N_n_n(2,:) = (/Nb_n_m(2,1),Nb_n_m(2,2),-Nr_n(2)/)
N_n_n(3,:) = (/Nb_n_m(3,1),Nb_n_m(3,2),-Nr_n(3)/)

Ninv_n_n   = Inverse3x3(N_n_n,FullRank)

IF(.not.FullRank) THEN
  k   = 0d0
ELSE
  k_n = MATMUL(Ninv_n_n,Rh_n-P_n)
  k   = k_n(3)
END IF
RETURN
END FUNCTION kRayIntersectionWithPlane

PURE FUNCTION kRayIntersectionWithCircle(Rray_ENU, Nray_ENU, radius) RESULT(k_n)
!+
! Description: This routine calculates the intersection points of a ray with a circle in the x,y plane centered
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8),INTENT(IN)           :: Rray_ENU(3)                     !Ray origin (m)
REAL(8),INTENT(IN)           :: Nray_ENU(3)                     !Ray direction (unit vector)
REAL(8),INTENT(IN)           :: radius                          !Radius of circle (m)
REAL(8)                      :: k_n(2)                          !Ray intersection with circle (m)

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: a                               !Square Term of the quadratic equation
REAL(8)                      :: b                               !Linear Term of the quadratic equation
REAL(8)                      :: c                               !Scalar Term of the quadratic equation
REAL(8)                      :: d                               !Discriminant of the quadratic equation
REAL(8)                      :: k1                              !1st solution of quadratic equation
REAL(8)                      :: k2                              !2nd Solution of quadratic equation
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
a = Nray_ENU(1)**2 + Nray_ENU(2)**2
b = 2.0d0 * (Rray_ENU(1)*Nray_ENU(1) + Rray_ENU(2)*Nray_ENU(2))
c = Rray_ENU(1)**2 + Rray_ENU(2)**2 - radius**2
d = b**2 - 4.0d0 * a * c
IF (d<0d0 .or. a==0d0) THEN
  k_n = -1.0d0
ELSE
  k1 = (-b + DSQRT(d))/2.0d0/a
  k2 = (-b - DSQRT(d))/2.0d0/a
  IF(k1 <= k2) THEN
    k_n(1) = k1
    k_n(2) = k2
  ELSE
    k_n(1) = k2
    k_n(2) = k1
  END IF
END IF
RETURN
END FUNCTION kRayIntersectionWithCircle

FUNCTION AreaQuadrilateral(R_xy_corner) RESULT(Area)
!+
! Description: Returns area of a quadrilateral.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!

!  Passed Variables                                     Description/(Units)
! -----------------------------------------------------+-------------------------------------------
REAL(8)   , INTENT(IN)           :: R_xy_corner(2,4)   ! Four vertices in clockwise order
REAL(8)                          :: Area               ! Area

!  Local Variables
! -----------------------------------------------------
REAL(8)                          :: D1_XY(2)
REAL(8)                          :: D2_XY(2)
!
!---------------------------------------------------------------------------------------------------
!  S T A R T   O F   E X E C U T A B L E   C O D E
! --------------------------------------------------------------------------------------------------
!
D1_XY = R_xy_Corner(:,1)-R_xy_Corner(:,3)
D2_XY = R_xy_Corner(:,2)-R_xy_Corner(:,4)
Area  = SQRT( DOT_PRODUCT(D1_xy,D1_xy) * DOT_PRODUCT(D2_xy,D2_xy) - DOT_PRODUCT(D1_xy,D2_xy)**2 )/2d0
RETURN
END FUNCTION AreaQuadrilateral

FUNCTION ValEstimateNonZeroFromVector(V_Vec, VecEnd) RESULT (Vavg)
!+
! Description: This routine performs an estimate of a mean value for a vector
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Declare Vec as I*4 instead of R*8 for compliance with Fortran Standard
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: VecEnd                          !Number of elements in the V_VEC array
REAL(8)   ,INTENT(IN OUT)    :: V_Vec(VecEnd)                   !Array of redundant values
REAL(8)                      :: Vavg                            !Estimate of average value in V_Vec

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Vsum                            !Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           !Sum of elements of V_Vec squared
REAL(8)                      :: Nsum                            !Number of non-zero elements in V_Vec
REAL(8)                      :: V2avg                           !Average value of V_Vec squared
REAL(8)                      :: Stdv                            !Standard deviation of estimate
INTEGER(4)                   :: Vec                             !Location inn V_Vec with maximum error
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
Vavg = 0d0
DO
  Vsum  = SUM(V_Vec, V_Vec>0)
  V2sum = SUM(V_Vec*V_Vec)
  Nsum  = COUNT(V_Vec > 0)                          ; IF(Nsum < 1d0) EXIT
  Vavg  = Vsum/Nsum
  V2avg = V2sum/Nsum
  Stdv  = SQRT(V2avg - Vavg**2)                     ; IF(Stdv < 1d0) EXIT
  Vec   = MAXLOC(ABS(V_Vec - Vavg), 1, V_Vec>0d0)
  IF(ABS(V_Vec(Vec) - Vavg) > 2d0*Stdv) THEN
    V_Vec(Vec) = 0d0
  ELSE
    EXIT
  END IF
END DO
RETURN
END FUNCTION ValEstimateNonZeroFromVector

FUNCTION ValEstimateWithStdvFromVector(Val_Vec, VecEnd, Stdv) RESULT (Vavg)
!+
! Description: This routine performs an estimate of a mean value for a vector
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Declare Vec as I*4 instead of R*8 for compliance with Fortran Standard
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable V2avg
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: VecEnd                          !Number of elements in the V_VEC array
REAL(8)   ,INTENT(IN)        :: Val_Vec(VecEnd)                 !Array of redundant values
REAL(8)   ,INTENT(IN)        :: Stdv                            !Standard deviation specified
REAL(8)                      :: Vavg                            !Estimate of average value in V_Vec

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Vsum                            !Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           !Sum of elements of V_Vec squared
REAL(8)                      :: Nsum                            !Number of non-zero elements in V_Vec
INTEGER(4)                   :: Vec                             !Location in V_Vec with maximum error
REAL(8),ALLOCATABLE          :: V_Vec(:)                        !Local copy of V_Vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ALLOCATE(V_Vec(VecEnd))
V_Vec = Val_Vec
Vavg = 0d0
DO
  Vsum  = SUM(V_Vec, V_Vec>0)
  V2sum = SUM(V_Vec*V_Vec)
  Nsum  = COUNT(V_Vec > 0)                          ; IF(Nsum < 1d0) EXIT
  Vavg  = Vsum/Nsum
  Vec   = MAXLOC(ABS(V_Vec - Vavg), 1, V_Vec>0d0)
  IF(ABS(V_Vec(Vec) - Vavg) > 2d0*Stdv) THEN
    V_Vec(Vec) = 0d0
  ELSE
    EXIT
  END IF
END DO
DEALLOCATE(V_Vec)
RETURN
END FUNCTION ValEstimateWithStdvFromVector

FUNCTION ValWLSEstimateFromVector(Val_Vec, Variance_Vec, VecEnd, NumMin) RESULT (Vavg)
!+
! Description: This routine performs an estimate of a mean value for a vector
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable V2avg
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: VecEnd                          !Number of elements in the V_VEC array
REAL(8)   ,INTENT(IN)        :: Val_Vec(VecEnd)                 !Array of redundant measurements
REAL(8)   ,INTENT(IN)        :: Variance_Vec(VecEnd)            !Array of variances of the measurements
INTEGER(4),INTENT(IN)        :: NumMin                          !Minimum number of elements in the V_VEC array used.
REAL(8)                      :: Vavg                            !Estimate of average value in V_Vec

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Vsum                            !Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           !Sum of elements of V_Vec squared
INTEGER(4)                   :: Nsum                            !Number of non-zero elements in V_Vec
INTEGER(4)                   :: Vec                             !Location in V_Vec with maximum error
REAL(8),ALLOCATABLE          :: V_Vec(:)                        !Local copy of V_Vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ALLOCATE(V_Vec(VecEnd))
V_Vec = Val_Vec
Vavg = 0d0
DO
  Nsum  = COUNT(V_Vec > 0)                          ; IF(Nsum <  NumMin) EXIT
  Vsum  = SUM(V_Vec/Variance_Vec, V_Vec > 0d0)
  V2sum = SUM(1d0/Variance_Vec  , V_Vec > 0d0)
  Vavg  = Vsum/V2Sum                                ; IF(Nsum == NumMin) EXIT
  Vec   = MAXLOC(  ((V_Vec - Vavg)**2) / (Variance_Vec - 1d0/V2sum), 1, V_Vec > 0d0)
  IF((V_Vec(Vec) - Vavg)**2 > 2d0*(Variance_Vec(Vec) - 1d0/V2sum)) THEN
    V_Vec(Vec) = 0d0
  ELSE
    EXIT
  END IF
END DO
DEALLOCATE(V_Vec)
RETURN
END FUNCTION ValWLSEstimateFromVector

FUNCTION ValWLSEstimateWithTolerance(Val_Vec, Variance_Vec, VecEnd, NumMin, dVTolerance) RESULT (Vavg)
!+
! Description: This routine performs an estimate of a mean value for a vector
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable V2avg
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: VecEnd                          !Number of elements in the V_VEC array
REAL(8)   ,INTENT(IN)        :: Val_Vec(VecEnd)                 !Array of redundant measurements
REAL(8)   ,INTENT(IN)        :: Variance_Vec(VecEnd)            !Array of variances of the measurements
INTEGER(4),INTENT(IN)        :: NumMin                          !Minimum number of elements in the V_VEC array used.
REAL(8)   ,INTENT(IN)        :: dVTolerance                     !Bad data detection tolerance
REAL(8)                      :: Vavg                            !Estimate of average value in V_Vec

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Vsum                            !Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           !Sum of elements of V_Vec squared
INTEGER(4)                   :: Nsum                            !Number of non-zero elements in V_Vec
INTEGER(4)                   :: Vec                             !Location in V_Vec with maximum error
REAL(8),ALLOCATABLE          :: V_Vec(:)                        !Local copy of V_Vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ALLOCATE(V_Vec(VecEnd))
V_Vec = Val_Vec
Vavg = 0d0
DO
  Nsum  = COUNT(V_Vec > 0)                          ; IF(Nsum <  NumMin) EXIT
  Vsum  = SUM(V_Vec/Variance_Vec, V_Vec > 0d0)
  V2sum = SUM(1d0/Variance_Vec  , V_Vec > 0d0)
  Vavg  = Vsum/V2Sum                                ; IF(Nsum == NumMin) EXIT
  Vec   = MAXLOC(  ((V_Vec - Vavg)**2) / (Variance_Vec - 1d0/V2sum), 1, V_Vec > 0d0)
  IF((V_Vec(Vec) - Vavg)**2 > dVTolerance*(Variance_Vec(Vec) - 1d0/V2sum)) THEN
    V_Vec(Vec) = 0d0
  ELSE
    EXIT
  END IF
END DO
DEALLOCATE(V_Vec)
RETURN
END FUNCTION ValWLSEstimateWithTolerance

FUNCTION ValWLSEstimateWithHiLoRemoval(Val_Vec, Variance_Vec, VecEnd, NumMin, dVTolerance, FractionHigh, FractionLow) RESULT (Vavg)
!+
! Description: This routine performs an estimate of a mean value for a vector
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable V2avg
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: VecEnd                          !Number of elements in the V_VEC array
REAL(8)   ,INTENT(IN)        :: Val_Vec(VecEnd)                 !Array of redundant measurements
REAL(8)   ,INTENT(IN)        :: Variance_Vec(VecEnd)            !Array of variances of the measurements
INTEGER(4),INTENT(IN)        :: NumMin                          !Minimum number of elements in the V_VEC array used.
REAL(8)   ,INTENT(IN)        :: dVTolerance                     !Bad data detection tolerance
REAL(8)   ,INTENT(IN)        :: FractionHigh                    !Fraction of hi values to remove
REAL(8)   ,INTENT(IN)        :: FractionLow                     !Fraction of lo values to remove
REAL(8)                      :: Vavg                            !Estimate of average value in V_Vec

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Vsum                            !Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           !Sum of elements of V_Vec squared
INTEGER(4)                   :: Nsum                            !Number of non-zero elements in V_Vec
INTEGER(4)                   :: Nhigh                           !Number of the hightest values to eliminate
INTEGER(4)                   :: Nlow                            !Number of the lowest values eliminate
INTEGER(4)                   :: Vec                             !Location in V_Vec with maximum error
REAL(8),ALLOCATABLE          :: V_Vec(:)                        !Local copy of V_Vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ALLOCATE(V_Vec(VecEnd))
V_Vec   = Val_Vec
Vavg    = 0d0
Nsum  = COUNT(V_Vec > 0)
Nhigh = INT(FractionHigh * DBLE(Nsum))
Nlow  = INT(FractionLow  * DBLE(Nsum))
DO
  Nsum = COUNT(V_Vec > 0)                           ; IF(Nsum <= NumMin .or. Nhigh == 0) EXIT
  Vec  = MAXLOC( V_Vec, 1, V_Vec > 0)
  V_Vec(Vec) = 0d0
  Nhigh      = Nhigh - 1
END DO

DO
  Nsum = COUNT(V_Vec > 0)                           ; IF(Nsum <= NumMin .or. Nlow  == 0) EXIT
  Vec  = MINLOC( V_Vec, 1, V_Vec > 0)
  V_Vec(Vec) = 0d0
  Nlow       = Nlow - 1
END DO

DO
  Nsum  = COUNT(V_Vec > 0)                          ; IF(Nsum <= NumMin) EXIT
  Vsum  = SUM(V_Vec/Variance_Vec, V_Vec > 0d0)
  V2sum = SUM(1d0/Variance_Vec  , V_Vec > 0d0)
  Vavg  = Vsum/V2Sum
  Vec   = MAXLOC(  ((V_Vec - Vavg)**2) / (Variance_Vec - 1d0/V2sum), 1, V_Vec > 0d0)
  IF((V_Vec(Vec) - Vavg)**2 > dVTolerance*(Variance_Vec(Vec) - 1d0/V2sum)) THEN
    V_Vec(Vec) = 0d0
  ELSE
    EXIT
  END IF
END DO
DEALLOCATE(V_Vec)
RETURN
END FUNCTION ValWLSEstimateWithHiLoRemoval

SUBROUTINE WLSEstimateAndBadDataDetection(Val_Vec, Variance_Vec, VecEnd, NumMin, dVTolerance, BadData_Vec, Vavg)
!+
! Description: This routine performs an estimate of a mean value for a vector
!              at (0,0,0)
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable V2avg
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4),INTENT(IN)        :: VecEnd                          !Number of elements in the V_VEC array
REAL(8)   ,INTENT(IN)        :: Val_Vec(VecEnd)                 !Array of redundant measurements
REAL(8)   ,INTENT(IN)        :: Variance_Vec(VecEnd)            !Array of variances of the measurements
INTEGER(4),INTENT(IN)        :: NumMin                          !Minimum number of elements in the V_VEC array used.
REAL(8)   ,INTENT(IN)        :: dVTolerance                     !Bad data detection tolerance
LOGICAL(1),INTENT(OUT)       :: BadData_Vec(VecEnd)             !Set true for bad data detected
REAL(8)   ,INTENT(OUT)       :: Vavg                            !Estimate of average value in V_Vec

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Vsum                            !Sum of non-zero elements in V_Vec
REAL(8)                      :: V2sum                           !Sum of elements of V_Vec squared
INTEGER(4)                   :: Nsum                            !Number of non-zero elements in V_Vec
INTEGER(4)                   :: Vec                             !Location in V_Vec with maximum error
REAL(8),ALLOCATABLE          :: V_Vec(:)                        !Local copy of V_Vec
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
ALLOCATE(V_Vec(VecEnd))
V_Vec = Val_Vec
Vavg  = 0d0
BadData_Vec = .FALSE.
DO
  Nsum  = COUNT(V_Vec > 0)                          ; IF(Nsum <  NumMin) EXIT
  Vsum  = SUM(V_Vec/Variance_Vec, V_Vec > 0d0)
  V2sum = SUM(1d0/Variance_Vec  , V_Vec > 0d0)
  Vavg  = Vsum/V2Sum                                ; IF(Nsum == NumMin) EXIT
  Vec   = MAXLOC(  ((V_Vec - Vavg)**2) / (Variance_Vec - 1d0/V2sum), 1, V_Vec > 0d0)
  IF((V_Vec(Vec) - Vavg)**2 > dVTolerance*(Variance_Vec(Vec) - 1d0/V2sum)) THEN
    V_Vec(Vec) = 0d0 ; BadData_Vec(Vec) = .TRUE.
  ELSE
    EXIT
  END IF
END DO
DEALLOCATE(V_Vec)

END SUBROUTINE WLSEstimateAndBadDataDetection

FUNCTION BCSApplyCorrectionsForTest(a_m, angleAz, angleEl, dAngAz, dAngEl) RESULT(status)
!---------------------------------------------------------------------------------------------------
! Description: Calculates and stores the a Correction factors for BCS processing
!
! Scientific documentation:
!  N/A
!
! Detail Design Documentation:
!  N/A
!
! Processing:
!
! Global Variables Accessed (Variable Name - Desciption):
!
!
! Global Variables Modified - Description:
!  None.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                              Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , INTENT(IN)       :: a_m(6)                          ! BCS Corrections
REAL(8)   , INTENT(IN)       :: angleAz                         ! Azimuthal angle      (degrees)
REAL(8)   , INTENT(IN)       :: angleEl                         ! Elevation angle      (degrees)
REAL(8)   , INTENT(OUT)      :: dAngAz                          ! Azimuthal correction (degrees)
REAL(8)   , INTENT(OUT)      :: dAngEl                          ! Elevation correction (degrees)
INTEGER(4)                   :: status                          ! Status return code
!
!                              Local Variables
!----------------------------+------------------
REAL(8)                      :: aCorrection_m(6)                ! BCS Correction factors (radians)
REAL(8)                      :: H_n_m(2, 6)                     ! Gain Matrix
REAL(8)                      :: dAng_n(2)                       ! Angle Corrections    (radians)
ASSOCIATE                       (dAz => dAng_n(1),            & ! Azimuthal correction (radians)
                                 dEl => dAng_n(2))              ! Elevation correction (radians)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
 aCorrection_m(1) = a_m(1) !AngleZAxisAdjustment_Heliostat (Heliostat)
 aCorrection_m(2) = a_m(2) !ElOffsetAdjustment_Heliostat   (Heliostat)
 aCorrection_m(3) = a_m(3) !AzWheelAdjustment_Heliostat    (Heliostat)
 aCorrection_m(4) = a_m(4) !ElWheelAdjustment_Heliostat    (Heliostat)
 aCorrection_m(5) = a_m(5) !AngleXAxisAdjustment_Heliostat (Heliostat)
 aCorrection_m(6) = a_m(6) !AngleYAxisAdjustment_Heliostat (Heliostat)
!aCorrection_m(7) = AxisNonOrthogonality_Heliostat (Heliostat)
!aCorrection_m(8) = BoreSightMisalignment_Heliostat(Heliostat)

H_n_m         = HmatrixWLSLinearTest(angleAz,angleEl)
dAng_n        = MATMUL(H_n_m,aCorrection_m)
dAngAz        = dAz*180d0/Pi
dAngEl        = dEl*180d0/Pi
status = 0
END ASSOCIATE

END FUNCTION BCSApplyCorrectionsForTest

FUNCTION HmatrixWLSLinearTest(angAz, angEl) RESULT(H_n_m)
!---------------------------------------------------------------------------------------------------
! Description: Calculates the H matrix for BCS processing
!
! Scientific documentation:
!  N/A
!
! Detail Design Documentation:
!  N/A
!
! Processing:
!
! Global Variables Accessed (Variable Name - Desciption):
!
!
! Global Variables Modified (Variable Name - Desciption):
!  None.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variables angZeRad, cscAz, and secAz
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
USE NumericalConstants
IMPLICIT NONE
!                              Passed Variables                   Description / (units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8), INTENT(IN)          :: angAz                           !Azimuthal Angle Counter Clockwise from North (Degrees)
REAL(8), INTENT(IN)          :: angEl                           !Elevation Angle (Degrees)
REAL(8)                      :: H_n_m(2,6)                      !H matrix (n/a)
!
!                              Local Variables
!----------------------------+------------------
REAL(8)                      :: angAzRad                        !Azimuthal Angle Clockwise from North (radians)
REAL(8)                      :: angElRad                        !Elevation Angle (radians)
REAL(8)                      :: sinAz                           !Sine of azimuthal angle (n/a)
REAL(8)                      :: cosAz                           !Cosine of azimuthal angle (n/a)
REAL(8)                      :: tanEl                           !Tangent of elevation angle (n/a)
REAL(8)                      :: cosEl                           !Cosine of the elevation angle (n/a)
REAL(8),PARAMETER            :: AngElMaxRad = 0.95d0*Pi/2d0     !Maximum elevation angle (radians)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
angAzRad = angAz * Pi / 180.0d0
angElRad = angEl * Pi / 180.0d0
sinAz    = SIN(angAzRad)
cosAz    = COS(angAzRad)
tanEl    = TAN(MAX(0d0,MIN(angElRad,angElMaxRad)))
cosEl    = COS(MAX(0d0,MIN(angElRad,angElMaxRad)))

!
H_n_m(1,:) = (/  1d0,  0d0, angAzRad,   0d0   ,  tanEl*cosAz,  -tanEl*sinAz /)
H_n_m(2,:) = (/  0d0,  1d0,   0d0   , angElRad,    -sinAz   ,     -cosAz    /)
!
END FUNCTION HmatrixWLSLinearTest

FUNCTION AngleShiftAtRadiusForRcenter(Radius, Angle, Rcenter_xy) RESULT(dAngleShift)
!+
! Description: This routine calculates the angle shift for projecting a vector at radius through a non-zero point
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                             Passed Variables                  Description (Units)
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   ,INTENT(IN)        :: Radius                          ! Radius of Circle that vector must intersect
REAL(8)   ,INTENT(IN)        :: Angle                           ! Angle of Vector if projected from (0,0) Orientation is Clockwise from (0,1) (degrees).
REAL(8)   ,INTENT(IN)        :: Rcenter_xy(2)                   ! Coordinates of new projection point of vector
REAL(8)                      :: dAngleShift                     ! Angle shift along Circle needed. Orientation is Clockwise from (0,1) (degrees).

!                               Local Variables
! ---------------------------+-----------------
REAL(8)                      :: Nangle_xy(2)                    ! Normal vector orientated by Angle
REAL(8)                      :: k                               ! Distance from Rcenter_xy to the Circle
REAL(8)                      :: Rshift_xy(2)                    ! Projection of Rcenter_xy to the Circle
REAL(8)                      :: AngleShift                      ! Angle of the projected point. Orientation is Clockwise from (0,1) (degrees)
REAL(8),PARAMETER            :: A = 1d0                         ! Square Coefficient of Quadratic  A*x^2 + B*x + C
REAL(8)                      :: B                               ! Linear Coefficient of Quadratic  A*x^2 + B*x + C
REAL(8)                      :: C                               ! Offset Coefficient of Quadratic  A*x^2 + B*x + C
REAL(8)                      :: Discriminant                    ! Quadratic Equation Discriminant  B**2 - 4*A*C
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
!
  dAngleShift = 0d0
  DO
    IF(Rcenter_xy(1) == 0d0 .and. Rcenter_xy(2) == 0d0) EXIT
    Nangle_xy    = (/DSIND(Angle), DCOSD(Angle)/)
    B            = DOT_PRODUCT(Nangle_xy , Rcenter_xy) * 2d0
    C            = DOT_PRODUCT(Rcenter_xy, Rcenter_xy) - Radius**2
    Discriminant = B**2 - 4d0*A*C                                         ; IF(Discriminant < 0d0) EXIT
    k            = (-B + SQRT(B**2 - 4d0*A*C))/2d0*A
    Rshift_xy    = Rcenter_xy + k*Nangle_xy
    AngleShift   = DATAN2D(Rshift_xy(1),Rshift_xy(2))
    IF(AngleShift < 0) AngleShift = AngleShift + 360
    dAngleShift  = AngleShift - Angle
    EXIT
  END DO
END FUNCTION AngleShiftAtRadiusForRcenter

FUNCTION ConvolutionSeparableFilter(X_dw, Y_dh, dwEnd, dhEnd, wEnd, hEnd, In_w_h) RESULT(Io_w_h)
!+
! Description: This function returns the results of applying the convolution of a separable filter to a input matrix.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variables dh and dw
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: dwEnd                           ! Straddle of the vector X_dw
INTEGER(4), INTENT(IN)       :: dhEnd                           ! Straddle of the vector Y_dh
INTEGER(4), INTENT(IN)       :: X_dw(-dwEnd:+dwEnd)             ! Vector multiplier of the separable filter, X'Y
INTEGER(4), INTENT(IN)       :: Y_dh(-dhEnd:+dhEnd)             ! Vector multiplier of the separable filter, X'Y
INTEGER(4), INTENT(IN)       :: wEnd                            ! Number of rows in the matrices
INTEGER(4), INTENT(IN)       :: hEnd                            ! Number of columns in the matrices
INTEGER(4), INTENT(IN)       :: In_w_h(wEnd,hEnd)               ! Input matrix
INTEGER(4)                   :: Io_w_h(wEnd,hEnd)               ! The result of applying the convolution to In_w_h

!                             Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)                   :: w                               ! Index through the columns of In_w_h and Io_w_h
INTEGER(4)                   :: h                               ! Index through the rows of In_w_h and Io_w_h
INTEGER(4),SAVE,ALLOCATABLE  :: Iu_w_h(:,:)                     ! Temporary matrix used to perform the Y convolution
INTEGER(4),SAVE,ALLOCATABLE  :: Iv_h_w(:,:)                     ! Temporary matrix used to perform the X convolution
INTEGER(4),SAVE,ALLOCATABLE  :: Io_h_w(:,:)                     ! Temporary matrix used to perform the X convolution
INTEGER(4),SAVE              :: wEndSave = 0                    ! Last positive value for wEnd
INTEGER(4),SAVE              :: hEndSave = 0                    ! Last positive value for hEnd
!----------------------------+---------------
INTEGER(4)                   :: Values_rcv(9)                   ! System Date and Time (Array)
INTEGER(4)                   :: Year,Month,Day                  ! Date components from DATE_AND_TIME
INTEGER(4)                   :: Hour,Minute,Second,msec         ! Time components from DATE_AND_TIME
INTEGER(4)                   :: dMinuteZone                     ! Minute shift from UTC for Local time
EQUIVALENCE                    (values_rcv(1),year         )
EQUIVALENCE                    (values_rcv(2),month        )
EQUIVALENCE                    (values_rcv(3),day          )
EQUIVALENCE                    (values_rcv(4),dMinuteZone  )
EQUIVALENCE                    (values_rcv(5),hour         )
EQUIVALENCE                    (values_rcv(6),minute       )
EQUIVALENCE                    (values_rcv(7),second       )
EQUIVALENCE                    (values_rcv(8),msec         )

!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  IF(wEndSave .ne. wEnd .or. hEndSave .ne. hEnd) THEN
    IF(ALLOCATED(Iu_w_h)) DEALLOCATE(Iu_w_h,Iv_h_w,Io_h_w)
    wEndSave = wEnd
    hEndSave = hEnd
    IF(wEnd <= 0 .or. hEnd <= 0) RETURN
    ALLOCATE(Iu_w_h(wEnd,hEnd),Iv_h_w(hEnd,wEnd),Io_h_w(hEnd,wEnd))
  END IF
  DO CONCURRENT(h=1:hEnd)
    CALL Convolution1D(X_dw,In_w_h(:,h),Iu_w_h(:,h),dwEnd,wEnd)
  END DO
  Iv_h_w = TRANSPOSE(Iu_w_h)
  DO CONCURRENT(w=1:wEnd)
    CALL Convolution1D(Y_dh,Iv_h_w(:,w),Io_h_w(:,w),dhEnd,hEnd)
  END DO
  Io_w_h = TRANSPOSE(Io_h_w)
  RETURN
END FUNCTION ConvolutionSeparableFilter

PURE SUBROUTINE Convolution1D(F_dv, In_v, Io_v, dvEnd, vEnd)
!+
! Description: This function returns the results of applying the convolution of a separable filter to a input matrix.
!
! Audit Trail
!        Date      Name    Description
! ----------------+-------+---------------------------------------------------------------------------------------------
! 10-FEB-2025     | MA    | SCR 150: Removed unused variable dv
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!                               Passed Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4), INTENT(IN)       :: dvEnd                           ! Straddle of the convolution vector F_dv
INTEGER(4), INTENT(IN)       :: F_dv(-dvEnd:+dvEnd)             ! Convolution vector
INTEGER(4), INTENT(IN)       :: vEnd                            ! Size of the vectors In_v and Io_v
INTEGER(4), INTENT(IN)       :: In_v(vEnd)                      ! Input vector
INTEGER(4), INTENT(OUT)      :: Io_v(vEnd)                      ! The result of applying the convolution to In_v

!                             Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)                   :: v                               ! Index through vectors In_v and Io_v
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
  !
  DO CONCURRENT(v=dvEnd+1:vEnd-dvEnd)
    Io_v(v) = DOT_PRODUCT(F_dv, In_v(v-dvEnd:v+dvEnd))
  END DO
  RETURN
END SUBROUTINE Convolution1D

!FUNCTION CircleMaxAreaInPolygon(R_xy, nEnd) RESULT(Circle)
!!+
!! Description: This function returns the maxmimum circle and center point vertex of the largest circle that fits
!!               within a generalized irregular polygon.
!!
!! Audit Trail
!!        Date      Name    Description
!! ----------------+-------+---------------------------------------------------------------------------------------------
!! ----------------+-------+---------------------------------------------------------------------------------------------
!! [change_entry]
!!-
!IMPLICIT NONE
!!                             Passed Variables
!!----------------------------+----------------------------------+-------------------------------------------------------
!INTEGER(4), INTENT(IN)       :: R_xy                            ! List of vertices used to define boundary polygon (x,y)
!REAL(8), TYPE(CircleKind)    :: circle                          ! Outputs max area circle center (x,y) and radius
!!
!!                             Local Variables
!!----------------------------+----------------------------------+-------------------------------------------------------
!REAL(8)                      :: Rcenter_xy(2)                   ! Avg x,y value of the input polygon vertices
!INTEGER(4)                   :: nEnd                            ! Number of points that define bounding polygon
!INTEGER(4)                   :: n                               ! Index through polygon points
!INTEGER(4)                   :: nRand                           ! Number of rand points generated within subset triangle
!!
!!
!!-----------------------------------------------------------------------------------------------------------------------
!!                         S T A R T   O F   E X E C U T A B L E   C O D E
!!-----------------------------------------------------------------------------------------------------------------------
!  !
!  !
!  nEnd = SIZE(R_xy, 2)
!  Rcenter_xy(1) = SUM(R_xy(1,:))/nEnd  !  average xVal center
!  Rcenter_xy(2) = SUM(R_xy(2,:))/nEnd  !  average yVal center
!
!  !      Triangle Ti
!  !
!  !        Rcenter
!  !          / \
!  !         /   \
!  !        /     \
!  !       /       \
!  !      /         \
!  !     / __________\
!  ! R_xy-n        R_xy-n+1
!  !   V1             V2
!
!  !   Generate (nEnd-1) number of subset triangles.  Formed by vertex n & n+1 & Rcenter
!  DO n = 1:nEnd-1
!      V1 = R_xy(:,n)
!      V2 = R_xy(:,n+1)
!
!
!      !  Generate nRand points within subset triangle
!
!
!
!
!  ENDDO
!  RETURN
!END FUNCTION CircleMaxAreaInPolygon

END MODULE MatrixVectorPackage
