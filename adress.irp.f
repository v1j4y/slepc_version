      subroutine adress(ideter,iaa)
      integer :: ideter(natomax)
      integer :: idet1n(natomax)
      integer :: ideter2(natomax)
      integer :: ntot1,ntot2,iaa
      logical :: yw

      ntot1=nt1
      ntot2=nt2
      yw=.FALSE.
!-------------------------------------------------
!----------------------------------------------------
      if(yham)then
        call adressA(ideter,ia1)
	if(yw)write(6,*)'ideter dans ad',    &
         (ideter(kkio),kkio=1,natom),ia1
	iat=0
	do i=1,natom
	   if(ideter(i).ne.3)then
	     iat=iat+1
	     ideter2(iat)=ideter(i)
	   endif
	enddo
	natrest=natom-ntrou
	call adressB(ideter2,ia2)
	if(yw)write(6,*)'ideter2 dans ad',   &
         (ideter2(kkio),kkio=1,natrest),ia2
	if(yw)write(6,*)'ia2',ia2
	iaa=(ia1-1)*ntot2+ia2
	if(yw)write(6,*)'iaa',iaa
      else
	l=0
	do kk=1,natom
	 if(ideter(kk).eq.1) then
            l=l+1
            idet1n(l)=kk
	 endif
	enddo
!-------------calcul de ial---------------------------
          irang=1
          ialo=l
	  if(ialo.ge.1)then
          DO I=1,ialo
             ID=idet1n(I)
             irang=irang+izb0(I,ID,ialo)
!5        continue
          ENDDO
	     iaa=irang
	     else
	     iaa=1
	     endif
      endif
      return
      end

!-----------------------------------------------------
!            Fonction d'adressage
!-----------------------------------------------------
