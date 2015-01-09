      subroutine adressB(ideter,iaa)
      integer :: ideter(natomax),l,kk,m,ialo,irang,ID
      integer :: idet1n(natomax)
      logical :: yw

!-------------------------------------------------
!----------------------------------------------------
        yw=.FALSE.
	l=0
	natrest=natom-ntrou
	do kk=1,natrest
	 if(ideter(kk).eq.1) then
            l=l+1
            idet1n(l)=kk
	 endif
	enddo
!-------------calcul de ial---------------------------
          irang=1
          ialo=l
!         if(yw)write(6,*)'izb0',(izb0(ialo,ID,ialo),i=1,l)
          if(yw)write(6,*)'ialo addB',ialo,(idet1n(i),i=1,l)
	  if(ialo.ge.1)then
          DO  I=1,ialo
             ID=idet1n(I)
             irang=irang+izb0(I,ID,ialo)
!            if(yw)write(6,*)'izb0=',izb0(I,ID,ialo),'I',I,'ID',ID
!5        continue
          ENDDO
	     iaa=irang
	     else
	     iaa=1
	     endif
      return
      end

