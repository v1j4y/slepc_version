        subroutine extra_diag(iaa)
        implicit none

        integer(kind=selected_int_kind(16)) :: iaa,iaa2
        integer(kind=selected_int_kind(16)) :: imat4,jmat4
        integer :: i,ik,iik
        integer :: ik1,ik2,IC,k,ikmax,ikmin,count
        integer,allocatable :: ideter2(:)
        real*8 :: dmat4
        logical :: yw

!---------------------------------------------------------------------
!                  Calcul des elements extradiagonaux
!---------------------------------------------------------------------
!----------boucle premier voisin

      allocate (ideter2(natomax))
    
      foundet=0
      foundetadr=0
      foundetdmat=0d0
      detfound=0
      foundadd=0
      foundaddh=0

      count=0
      yw=.FALSE.
      do ik=1,nlientot
          ik1=iliatom1(ik)
          ik2=iliatom2(ik)
         do k=1,natom
	    ideter2(k)=deter(k)
         enddo
	 if(yalt(ik)) then
	    if (deter(ik1).eq.2) then
	       ideter2(ik1)=1
	       ideter2(ik2)=2
	    else
	       ideter2(ik1)=2
	       ideter2(ik2)=1
	    endif
   	       dmat4=xjz(ik)
               if(dmat4.ne.0d0)then
            count+=1
            foundet(:,count)=ideter2
            foundetdmat(count)=dmat4
            endif
         endif
	 if(ytrou(ik)) then
	    if(deter(ik2).eq.3)then
	      if (deter(ik1).eq.1) then
	        ideter2(ik1)=3
	        ideter2(ik2)=1
	      else
	        ideter2(ik1)=3
	        ideter2(ik2)=2
	      endif
	    else
	      if (deter(ik2).eq.1) then
	        ideter2(ik1)=1
	        ideter2(ik2)=3
	      else
	        ideter2(ik1)=2
	        ideter2(ik2)=3
	      endif
            endif
	       ikmin=min(ik1,ik2)
	       ikmax=max(ik1,ik2)
	       IC=0
	       do iik=ikmin+1,ikmax-1
		    if(deter(iik).ne.3)IC=IC+1
	       enddo
  	       dmat4=(xt(ik))*(-1)**(IC)
               if(dmat4.ne.0d0)then
            count+=1
            foundet(:,count)=ideter2
            foundetdmat(count)=dmat4
            endif
         endif
      enddo

      detfound=count
      Touch foundet foundetadr detfound foundadd foundaddh foundetdmat
      call adrfull()
      do i=1,count
	       imat4=iaa
	       jmat4=foundetadr(i)
               dmat4=foundetdmat(i)
               if(jmat4.le.(nt1*nt2) .and. dmat4 .ne. 0d0)then
                countcol+=1
                col(countcol)=jmat4
                val(countcol)=dmat4
              endif
        enddo

    return
    end
