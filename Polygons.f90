!  Source:  Polygons.f90
!
!  Module:  Polygons
MODULE Polygons
!+
!  Description: This Module provides common routines and functions for processing polygons.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!
INTEGER(4),PARAMETER :: MX$VERTEX=64
TYPE Polygon
SEQUENCE
  INTEGER(4) :: Align
  INTEGER(4) :: nEnd                              !Number of vertices in the polygon
  REAL(8)    :: P_n(2,MX$VERTEX)                  !Polygon Vertices
END TYPE Polygon
!
! Area: Returns the area of a polygon
!
! INPUTS: Array X with the X coords
!         Array Y with the Y coords.
!         N: Number of verts.
!  OUT:   The area.
!
!
!
!If there is only one connected component, the first vertex must be
! repeated as the last vertex.
!
!The polygon can have multiple separate components, possibly nested,
!provided that each component has its first vertex repeated as the last
!vertex of that component. The vertices of each outside component must
!be listed in positive order, those of any immediately contained
!component in negative order, etc.
!
! Wm. Randolph Franklin, wrfATecse.rpi.edu, (518) 276-6077; Fax: -6261
!ECSE Dept., 6026 JEC, Rensselaer Polytechnic Inst, Troy NY, 12180 USA
!
!Last modified: Fri Sep 30 14:33:18 1994
!
!This is modelled on a function that I wrote about 1971.
!
!  Wm. Randolph Franklin, Associate Professor
!  Email: wrfATecse.rpi.edu
!  http://wrfranklin.org/
!  +1 (518) 276-6077; Fax: -6261
!  ECSE Dept., 6026 JEC, Rensselaer Polytechnic Inst, Troy NY, 12180 USA
! (GPG and PGP keys available)
!
CONTAINS

FUNCTION AreaSignedPolygon(P_m_n,nEnd)  RESULT(Area)
!
! Description: This function returns the signed area of a polygon.
!
!               The (x,y) coordinates of the vertices are specified by the elements of the array P_m_n around the.
!               perimeter.  The last vertex must be the same as the first vertex.  If the vertices are specified in
!               counter-clockwise order, then the returned area is positive. The returned area is negative if the
!               vertices are are specified in clockwise order.
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
IMPLICIT NONE
!
!                            Passed Variables     Description / (Units)
! ------------------------+----------------------+----------------------------------------------------------------------
INTEGER(4), INTENT(IN)    :: nEnd                ! Number of vertices + 1
REAL(8)   , INTENT(IN)    :: P_m_n(2,1:nEnd)     ! Vertices of the polygon (distance)
REAL(8)                   :: Area                ! Signed Area of the polygon (distance^2)
!
!                            Local Variables
! -----------------------------------------------
INTEGER(4)                :: n                   ! Index through vertices
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Area = 0d0
  IF (nEnd < 4) RETURN
  DO n = 1, nEnd-1
    Area = Area + (P_m_n(1,n)*P_m_n(2,n+1) - P_m_n(1,n+1)*P_m_n(2,n))
  END DO
  Area = Area/2d0
  RETURN
END FUNCTION AreaSignedPolygon

FUNCTION CentroidPolygon(P_m_n,nEnd)  RESULT(Centroid_m)
!
! Description: This function returns the centroid of a polygon.
!
!              Determines the centroid coordinates of a polygon.  The (x,y) coordinates of the vertices are specified
!              by the elements of the array P_m_n around the perimeter in clockwise order. The last vertex is the same
!              as the first vertex.  Reference file://D:/CSP/Documentation/CentroidOfPolygon.MHT
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! 05-FEB-2025     | MA    | SCR 150: Ensure Area is initialized before being updated.
! ----------------+-------+---------------------------------------------------------------------------------------------
IMPLICIT NONE

!                           Passed Variables       Description / (Units)
! ------------------------+----------------------+----------------------------------------------------------------------
INTEGER(4), INTENT(IN)    :: nEnd                ! Number of vertices + 1
REAL(8)   , INTENT(IN)    :: P_m_n(2,1:nEnd)     ! Vertices of the polygon (distance)
REAL(8)                   :: Centroid_m(2)       ! Centroid of the polygon (distance)

! Local Variables
! -----------------------------------------------
INTEGER(4)                :: n                   ! Index through vertices
REAL(8)                   :: Area                ! Signed Area of the polygon (distance^2)
REAL(8)                   :: dP12                ! Intermediate
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Centroid_m = 0d0
  IF (nEnd < 4) RETURN
  Area = 0.0d0
  DO n = 1, nEnd-1
    Area = Area + (P_m_n(1,n)*P_m_n(2,n+1) - P_m_n(1,n+1)*P_m_n(2,n))
  END DO
  Area = Area/2d0

  DO n = 1, nEnd-1
    dP12 = P_m_n(1,n)*P_m_n(2,n+1) - P_m_n(1,n+1)*P_m_n(2,n)
    Centroid_m(1) = Centroid_m(1) + (P_m_n(1,n) + P_m_n(1,n+1))*dP12
    Centroid_m(2) = Centroid_m(2) + (P_m_n(2,n) + P_m_n(2,n+1))*dP12
  END DO
  Centroid_m = Centroid_m/(6d0*Area + 1d-17)
  RETURN
END FUNCTION CentroidPolygon

FUNCTION AreaPolygon(P_m_n,nEnd)  RESULT(area)
!
! Description: This function returns the area of a polygon.
!
!              The (x,y) coordinates of the vertices are specified by the elements of the array P_m_n around the.
!              perimeter.  The last vertex must be the same as the first vertex.  If the vertices are specified in
!              clockwise order, then the returned area is positive.  The returned area is negative if the vertices
!              are specified in counter-clockwise order
!
!  Audit Trail
! ----------------+-------+---------------------------------------------------------------------------------------------
! ----------------+-------+---------------------------------------------------------------------------------------------
!-
!
IMPLICIT NONE

!                           Passed Variables       Description / (Units)
! ------------------------+----------------------+----------------------------------------------------------------------
INTEGER(4), INTENT(IN)    :: nEnd                ! Number of vertices + 1
REAL(8)   , INTENT(IN)    :: P_m_n(2,1:nEnd)     ! Vertices of the polygon (distance)
REAL(8)                   :: area                ! Area of the polygon (distance^2)

! Local Variables
! -----------------------------------------------
INTEGER(4)                :: n                   ! Index through vertices
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
area = 0d0
IF (nEnd < 4) RETURN
DO n = 3, nEnd, 2
  area = area + (P_m_n(2,n-2) - P_m_n(2,n)) * P_m_n(1,n-1) - (P_m_n(1,n-2) - P_m_n(1,n)) * P_m_n(2,n-1)
END DO
IF(MOD(nEnd,2)==0) area = area + P_m_n(2,nEnd-1) * P_m_n(1,nEnd) - P_m_n(1,nEnd-1) * P_m_n(2,nEnd)
area = area/2d0
RETURN
END FUNCTION AreaPolygon

FUNCTION AngleInteriorPolygon(P_m_n,nEnd)  RESULT(Angle_n)
!+
! Description: This function returns the interior angles of a polygon.
!
!              Determines the interior angles of the polygon vertices.  The (x,y) coordinates of the vertices
!              are specified by the elements of the array P_m_n around the perimeter in clockwise order. The last
!              vertex is the same as the first vertex.
!
IMPLICIT NONE

!                           Passed Variables       Description / (Units)
! ------------------------+----------------------+----------------------------------------------------------------------
INTEGER(4), INTENT(IN)    :: nEnd                ! Number of vertices + 1
REAL(8)   , INTENT(IN)    :: P_m_n(2,1:nEnd)     ! Vertices of the polygon (distance)
REAL(8)                   :: Angle_n(1:nEnd-1)   ! Interior angles of the polygon (distance^2)

! Local Variables
! -----------------------------------------------
INTEGER(4)                :: n                   ! Index through vertices
INTEGER(4)                :: nPrior              ! Previous vertex to Vertex n
REAL(8)                   :: Vi_m(2)             ! Vector from vertex n to the previous vertex
REAL(8)                   :: Vj_m(2)             ! Vector from vertex n to the next vertex
REAL(8)                   :: Cosine              ! Cosine of the interior angle for vertex n
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  Angle_n = 0d0
  IF (nEnd < 4) RETURN

  nPrior = nEnd-1
  DO n = 1, nEnd-1
    Vi_m       = P_m_n(:,n  ) - P_m_n(:,nPrior)
    Vj_m       = P_m_n(:,n+1) - P_m_n(:,n)
    nPrior     = n
    Cosine     = DOT_PRODUCT(Vi_m,Vj_m)/(SQRT(DOT_PRODUCT(Vi_m,Vi_m)*DOT_PRODUCT(Vj_m,Vj_m)) + 1d-17)
    Angle_n(n) = ACOSD(Cosine)
  END DO
  RETURN
END FUNCTION AngleInteriorPolygon

FUNCTION IntersectionSegments(P_m_n,Q_m_n,K_n) RESULT(Intersection)
!+
! Description: This function returns a true status if two segments of a polygon have an intersection.
!
!              Given two segments of the form P + kp(P2 - P1) P1 and P2 are vertices
!                                             Q + kq(Q2 - Q1) Q2 and Q2 are vertices
!              This routine determines the values of kp and kq where they intersect. The two values
!              are returned in K_n.
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
REAL(8)   , INTENT(IN)    :: P_m_n(2,2)          ! Points of 1st segment (distance)
REAL(8)   , INTENT(IN)    :: Q_m_n(2,2)          ! Points of 2nd segment (distance)
REAL(8)   , INTENT(OUT)   :: K_n(2)              ! Intersection parameters
LOGICAL(4)                :: Intersection        ! Set true if segments intersect

!                            Local Variables
! ------------------------+----------------------
REAL(8)                   :: det                 ! Determinant of the segment matrix
REAL(8)                   :: M_n_n(2,2)          ! 2x2 matrix for linear system
REAL(8)                   :: I_n_n(2,2)          ! 2x2 Inverse matrix of M_n_n
!
! ----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
! ----------------------------------------------------------------------------------------------------------------------
!
  M_n_n(1,:)   = P_m_n(:,2) - P_m_n(:,1)
  M_n_n(2,:)   = Q_m_n(:,1) - Q_m_n(:,2)
  det          = M_n_n(1,1)*M_n_n(2,2) - M_n_n(1,2)*M_n_n(2,1)
  Intersection = ABS(det) > EPSILON(1d0)
  IF(Intersection) THEN
    I_n_n = RESHAPE((/M_n_n(2,2),-M_n_n(2,1),-M_n_n(1,2),M_n_n(1,1)/),(/2,2/))/det
    K_n   = MATMUL(M_n_n, Q_m_n(:,1) - P_m_n(:,1) )
  ELSE
    K_n   = 0d0
  END IF
  RETURN

END FUNCTION IntersectionSegments

FUNCTION InteriorOfPolygon(P_m, P_m_n, nEnd) RESULT(Interior)
!+
! Description: This function returns a true status if a point P_m is an interior point of the polygon defined by P_m_n.
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
INTEGER(4),INTENT(IN)        :: nEnd                            ! Number of 2D points in the defining polygon
REAL(8)   ,INTENT(IN)        :: P_m_n(2,0:nEnd+1)               ! 2D points arranged clockwise from north (distance)
REAL(8)   ,INTENT(IN)        :: P_m(2)                          ! 2D point to test if it is inside the polygon
LOGICAL(4)                   :: Interior                        ! Return true if R is inside polygond defined by R_n
!
!                             Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
INTEGER(4)                   :: n                               ! Index through polygon vertices
REAL(8)                      :: k1,k2
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------
ASSOCIATE(X=>P_m(1), Y=>P_m(2))

  Interior = .false.

  IF(Y >= MAXVAL(P_m_n(2,:)) .or. Y <= MINVAL(P_m_n(2,:)) .or. X >= MAXVAL(P_m_n(1,:))) RETURN
  DO n = 1, nEnd
    IF(PointIsOnSegment(P_m, P_m_n(:,n), P_m_n(:,n+1))) RETURN
  END DO

  DO n = 1, nEnd
    !
    ! Quick checks to see if there is no chance a horizontal ray from R will go through the segment Pn -> Pn+1
    IF(Y < MINVAL(P_m_n(2,n:n+1)) .or. Y > MAXVAL(P_m_n(2,n:n+1)) .or. X > MAXVAL(P_m_n(1,n:n+1))) CYCLE
    !
    ! Skip Flat and Extrema Segments that align with P
    IF(P_m(2)==P_m_n(2,n)) THEN
      IF(P_m_n(2,n)==P_m_n(2,n+1)) CYCLE
      IF(P_m_n(2,n) > P_m_n(2,n-1) .and. P_m_n(2,n) > P_m_n(2,n+1) .or. P_m_n(2,n) < P_m_n(2,n-1) .and. P_m_n(2,n) < P_m_n(2,n+1)) CYCLE
    END IF
    !
    ! Calculate scalars k1 and k2 such that  Pk + (Pj-Pk)*k1 = P + k2 * (/1,0/)
    k1 = (P_m(2) - P_m_n(2,n+1))/(P_m_n(2,n) - P_m_n(2,n+1)) ; IF(k1<=0d0 .or. k1>1d0) CYCLE
    k2 = P_m_n(1,n+1) - P_m(1) + k1*(P_m_n(1,n) - P_m_n(1,n+1))
    IF(k2 > 0d0) Interior = .not.Interior
  END DO
  RETURN
END ASSOCIATE
END FUNCTION InteriorOfPolygon

FUNCTION PointIsOnSegment(Pi_m, Pj_m, Pk_m) RESULT(OnSegment)
!+
! Description: This function returns a true status if the point, Pi_m is on the half open segment [ Pj_m, Pk_m )
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
REAL(8)   ,INTENT(IN)        :: Pi_m(2)                         ! Point to test if is on a segment (distance)
REAL(8)   ,INTENT(IN)        :: Pj_m(2)                         ! Starting point of segment (distance)
REAL(8)   ,INTENT(IN)        :: Pk_m(2)                         ! Ending point of segment (distance)
LOGICAL(4)                   :: OnSegment                       ! Return status is true if the point is on the segment
!
!                             Local Variables
!----------------------------+----------------------------------+-------------------------------------------------------
REAL(8)                      :: k                               ! Ratio of Pi-Pk to Pj-Pk
REAL(8)                      :: dPik_m(2)                       ! Pi-Pk (distance)
REAL(8)                      :: dPjk_m(2)                       ! Pj-Pk (distance)
REAL(8)                      :: dP_m  (2)                       ! Difference between Pi and Pk + k*(Pj - Pk) (distance)
!
!-----------------------------------------------------------------------------------------------------------------------
!                         S T A R T   O F   E X E C U T A B L E   C O D E
!-----------------------------------------------------------------------------------------------------------------------

  dPik_m = Pi_m - Pk_m
  dPjk_m = Pj_m - Pk_m
  k      = DOT_PRODUCT(dPik_m,dPjk_m)/DOT_PRODUCT(dPjk_m,dPjk_m)
  IF(0d0 < k .and. k <= 1d0) THEN
    dP_m = ABS(Pk_m + k*dPjk_m - Pi_m)
    OnSegment = MAXVAL(dP_m) < 1d-8
  ELSE
    OnSegment = .false.
  END IF
  RETURN
END FUNCTION PointIsOnSegment

END MODULE Polygons
