MODULE RandomNumbers
!+
! Description: This module generates of random numbers.  It supports two distributions, a gaussian and a Uniform
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
! Public Variables
!                              Name                              Description / (units)
!-----------------------------+---------------------------------+-----------------------------------
LOGICAL(4)                    :: UseGaussian = .true.           ! Default is to use Gaussian
!                                                               ! ToDo  SCR 150: Add UseGaussian_Task so it can be thread safe
CONTAINS

FUNCTION RandomNumber(sigma)  RESULT(vRandom)
!+
! Description: Returns a random number.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!

!                                Passed                                 Description / (units)
!-----------------------------+---------------------------------------+-------------------------------------------------
REAL(8), INTENT(IN)           :: sigma                                ! standard deviation
REAL(8)                       :: vRandom                              ! result

!                                 Local                                       Description / (units)
!-----------------------------+---------------------------------------+-------------------------------------------------
REAL(8),SAVE                  :: vUniform                             ! Uniform distribution
REAL(8),SAVE                  :: vGauss_n(2)                          ! Gaussian distribution
REAL(8),SAVE                  :: v_n(2)                               ! 2 values used to generate gaussian samples
REAL(8),SAVE                  :: r                                    ! Auxiliary value in algorithm
LOGICAL(1),SAVE               :: validResult = .false.                ! A 2nd values is ready
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  IF(.not.UseGaussian) THEN
  ! Uniform Distribution
    CALL RANDOM_NUMBER(vGauss_n(1))
    CALL RANDOM_NUMBER(vGauss_n(2))
    vUniform = vGauss_n(1)
    vRandom  = (vUniform - 0.5d0)*DSQRT(3.0d0)*sigma
    RETURN
  END IF

  IF(.not.validResult) THEN

    DO WHILE(.not.validResult)
      CALL RANDOM_NUMBER(vGauss_n(1))
      CALL RANDOM_NUMBER(vGauss_n(2))
      v_n = 2.0d0*vGauss_n - 1.0d0
      r = DOT_PRODUCT(v_n,v_n)
      IF( r < 1.0d0) THEN
        validResult = .true.
      END IF
    END DO
    r = DSQRT(-2.0d0*DLOG(r)/r)
    vRandom = r*v_n(1)*sigma !/3.0d0

  ELSE

    vRandom = r*v_n(2)*sigma !/3.0d0
    validResult = .false.
  END IF
  RETURN

END FUNCTION RandomNumber

FUNCTION RandomNumberTriangular(vMin, vMid, vMax) RESULT(vRandom)
!+
! Description: Returns a random number from a triangular distribution.
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
IMPLICIT NONE
!
!                                Passed                                 Description / (units)
!-----------------------------+---------------------------------------+-------------------------------------------------
REAL(8), INTENT(IN)           :: vMin                                 ! vMinimum value of triangular distribution
REAL(8), INTENT(IN)           :: vMid                                 ! vMid Pnt value of triangular distribution
REAL(8), INTENT(IN)           :: vMax                                 ! vMaximum value of triangular distribution
REAL(8)                       :: vRandom                              ! result
!
!                                Local                                  Description / (units)
!-----------------------------+---------------------------------------+-------------------------------------------------
REAL(8)                       :: vUniform                             ! Uniform random number
!
! S T A R T   O F   E X E C U T A B L E   C O D E
! -----------------------------------------------
!
  CALL RANDOM_NUMBER(vUniform)
  IF(vUniform > (vMid-vMin)/(vMax-vMin)) THEN
    vRandom = vMax - DSQRT((1.0d0-vUniform)*(vMid-vMin)*(vMax-vMin))
  ELSE
    vRandom = vMin + DSQRT((vUniform)*(vMid-vMin)*(vMax-vMin))
  END IF
  RETURN

  END FUNCTION RandomNumberTriangular

END MODULE RandomNumbers
