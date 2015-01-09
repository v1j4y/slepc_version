      subroutine adressA(ideter,iaa)
      integer :: ideter(natomax),i,iaa,irang,ialo,l,kk,ID
      integer :: idet1n(natomax)
      logical :: yw

!-------------------------------------------------
!----------------------------------------------------
        yw=.FALSE.
	l=0
	do kk=1,natom
	 if(ideter(kk).eq.3) then
            l=l+1
            idet1n(l)=kk
	 endif
	enddo
!-------------calcul de ial---------------------------
          irang=1
          ialo=l
          if(yw)write(6,*)'ialo',ialo,(idet1n(i),i=1,l)
	  if(ialo.ge.1)then
          DO I=1,ialo
             ID=idet1n(I)
             irang=irang+iza0(I,ID,ialo)
!5        continue
          ENDDO
	     iaa=irang
	     else
	     iaa=1
	     endif
      return
      end


!-----------------------------------------------------
!            Fonction d'adressage
!-----------------------------------------------------
