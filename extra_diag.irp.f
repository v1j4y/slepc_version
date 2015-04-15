        integer function extra_diag()
        implicit none

        integer :: i,ik,imat4,iaa2,iik,iaa
        integer :: ik1,ik2,jmat4,IC,k,ikmax,ikmin
        integer,allocatable :: ideter2(:)
        real*8 :: dmat4
        logical :: yw

!---------------------------------------------------------------------
!                  Calcul des elements extradiagonaux
!---------------------------------------------------------------------
!----------boucle premier voisin

      allocate (ideter2(natomax))

      yw=.FALSE.
      extra_diag=0
      do ik=1,nlientot
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
	    call adr(ideter2,iaa2)
	    call adr(deter,iaa)
	       imat4=iaa
	       jmat4=iaa2
   	       dmat4=xjz(ik)
               if(jmat4.le.(nt1*nt2))then
               countcol+=1
               col(countcol)=jmat4
               val(countcol)=dmat4
               extra_diag+=1
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
	    call adr(ideter2,iaa2)
	    call adr(deter,iaa)
	       imat4=iaa
	       jmat4=iaa2
	       ikmin=min(ik1,ik2)
	       ikmax=max(ik1,ik2)
	       IC=0
	       do iik=ikmin+1,ikmax-1
		    if(deter(iik).ne.3)IC=IC+1
	       enddo
  	       dmat4=(xt(ik))*(-1)**(IC)
               if(jmat4.le.(nt1*nt2))then
                countcol+=1
                col(countcol)=jmat4
                val(countcol)=dmat4
                      extra_diag+=1
              endif
         endif
      enddo

    return
    end function extra_diag
