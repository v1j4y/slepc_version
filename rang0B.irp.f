BEGIN_PROVIDER [integer, izb0,(natomax,natomax,maxial)]
      BEGIN_DOC
      ! gives the adresses without holes
      END_DOC

!     SUBROUTINE RANG0B(nati,NEL)
      implicit none
      integer,allocatable :: isz(:,:),C(:,:)
      integer :: ial,i,j,ifin,l,izz
      integer :: m,k,nel,nati
      allocate (C(0:natomax,-1:natomax))
      allocate (isz(natomax,natomax))
!     allocate (izb0(natomax,natomax,maxial))
      nati=natrest
      nel=ial0
      ial=nel
      IF(NEL.EQ.0) RETURN
!===CALCUL DE C
      DO I=0,natomax
      DO J=-1,natomax-1
      C(I,J)=-50
      ENDDO
      ENDDO
      DO I=0,nati
      C(I,-1)=0
      C(I,0)=1
      C(I,1)=I
      DO J=2,NEL-1
      IF(J.GT.I)CYCLE
      C(I,J)=(C(I,J-1)*(I-J+1))/(J)
!2    CONTINUE
      ENDDO
!1    CONTINUE
      ENDDO
      DO L=1,nati
      IFIN=MIN0(NEL-1,L)
      DO K=1,IFIN
      IZZ=0
      DO M=nati-L+1,nati-K
      IZZ=IZZ+C(M,NEL-K)-C(M-1,NEL-K-1)
      ENDDO
      ISZ(K,L)=IZZ
      izb0(k,l,ial)=izz
!3    CONTINUE
      ENDDO
      ISZ(NEL,L)=L-NEL
      izb0(nel,l,ial)=ISZ(nel,l)
!4    CONTINUE
      ENDDO
!     RETURN
!     END
END_PROVIDER

