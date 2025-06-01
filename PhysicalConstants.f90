MODULE PhysicalConstants
!+
! Description: Definition Of Physical Constants for CSP Controls
!
! Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! [change_entry]
!-
!                               Constant                          Units
!----------------------------+------------------------------------+-----------------------------------------------------
REAL(8), PARAMETER           :: SolarFluxAtOneAU = 1367.6d0       ! w/m^2
REAL(8), PARAMETER           :: GasConstantSI    = 8.3144621d0    ! J/(mol K)
REAL(8), PARAMETER           :: GasConstant      = 10.73159d0     ! psi ft^3/lb-mol/degR
REAL(8), PARAMETER           :: StefanBoltzmannConstantSI     &   ! Watts/m^2/degK^4
                                                                    = 5.670373d-8
REAL(8), PARAMETER           :: StefanBoltzmannConstantEnglish &  ! BTU/Hr/ft^2/degR^4
                                                                    = 0.1714d-8
END MODULE PhysicalConstants
