MODULE NumericalConstants
!+
! Description: Definition Of miscellaneous Numerical Constants
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!                              Constant                           Value
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , PARAMETER        :: PI                               = 4.0d0*ATAN(1.0d0)
REAL(8)   , PARAMETER        :: TWOPI                            = PI * 2.0d0
REAL(8)   , PARAMETER        :: SQRT2                            = SQRT(2.0d0)
REAL(8)   , PARAMETER        :: eNL                              = 2.71828182845904523536d0
LOGICAL(4), PARAMETER        :: TRUE                             = .true.
LOGICAL(4), PARAMETER        :: FALSE                            = .false.

REAL(8)   , PARAMETER        :: ZERO                             = 0.0d0
REAL(8)   , PARAMETER        :: ONE                              = 1.0d0
REAL(8)   , PARAMETER        :: TWO                              = 2.0d0
REAL(8)   , PARAMETER        :: THREE                            = 3.0d0
REAL(8)   , PARAMETER        :: FOUR                             = 4.0d0
REAL(8)   , PARAMETER        :: FIVE                             = 5.0d0
REAL(8)   , PARAMETER        :: HALF                             = 0.5d0
REAL(8)   , PARAMETER        :: TEN                              = 1.0d1
REAL(8)   , PARAMETER        :: ONEPERCENT                       = 0.01d0
REAL(8)   , PARAMETER        :: INFINITY                         = 1.0d70
REAL(8)   , PARAMETER        :: EPSILON                          = 1.0d-12
REAL(8)   , PARAMETER        :: UnitaryMatrix_3x3(3,3)           = RESHAPE((/1,0,0,0,1,0,0,0,1/),(/3,3/))
REAL(8)   , PARAMETER        :: UnitaryMatrix_2x2(2,2)           = RESHAPE((/1,0,0,1/),(/2,2/))
REAL(8)   , PARAMETER        :: IdentityMatrix_3x3(3,3)          = RESHAPE((/1,0,0,0,1,0,0,0,1/),(/3,3/))
REAL(8)   , PARAMETER        :: IdentityMatrix_2x2(2,2)          = RESHAPE((/1,0,0,1/),(/2,2/))

END MODULE NumericalConstants

MODULE FluidConstants
!+
! Description: Definition Of Fluids Constants
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!                              Constant                           Value
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)   , PARAMETER        :: ReynoldsMax                      = 1.0D10
REAL(8)   , PARAMETER        :: ReynoldsMin                      = 2.0D2
REAL(8)   , PARAMETER        :: ReynoldsLaminarZone              = 2.0D3

END MODULE FluidConstants
