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
         ik1=iliatom1(ik)
         ik2=iliatom2(ik)
         do k=1,natom
	    ideter2(k)=deter(k)
         enddo
	 if(yalt(ik)) then
      print *,"enter extardiag"
	    if (deter(ik1).eq.2) then
	       ideter2(ik1)=1
	       ideter2(ik2)=2
	    else
	       ideter2(ik1)=2
	       ideter2(ik2)=1
	    endif
	    call adress(ideter2,iaa2)
	    call adress(deter,iaa)
!    if(iaa.lt.iaa2) then
	       imat4=iaa
	       jmat4=iaa2
!  	       dmat4(isto4+1)=xj1+xeneJ(ik)*xbJ
   	       dmat4=xjz(ik)
!              if(yw)write(6,*)iaa,iaa2,'exdiag,J1',dmat4(isto4+1)
!              write(6,*)'testnt1',(nt1*nt2/2.d0)
               if(jmat4.le.(nt1*nt2))then
	       call sto44(imat4,jmat4,dmat4)
               countcol+=1
               col(countcol)=jmat4
               val(countcol)=dmat4
               extra_diag+=1
!              endif
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
	    call adress(ideter2,iaa2)
	    call adress(deter,iaa)
!    if(iaa.lt.iaa2) then
	       imat4=iaa
	       jmat4=iaa2
	       ikmin=min(ik1,ik2)
	       ikmax=max(ik1,ik2)
	       IC=0
	       do iik=ikmin+1,ikmax-1
		    if(deter(iik).ne.3)IC=IC+1
	       enddo
!  	       dmat4(isto4+1)=(xt1+xeneT(ik)*xbT)*(-1)**(IC)
  	       dmat4=(xt(ik))*(-1)**(IC)
!       if(yw)write(6,*)iaa,iaa2,'exdiag,t1',dmat4(isto4+1)
               if(jmat4.le.(nt1*nt2))then
                
                 if(redo)then
                   if(jmat4-imat4.ne.nt2)then
                      call sto44(imat4,jmat4,dmat4)
                      extra_diag+=2
                   endif
                 else

                      call sto44(imat4,jmat4,dmat4)
               countcol+=1
               col(countcol)=jmat4
               val(countcol)=dmat4
                      extra_diag+=1

!                endif

               endif
	    endif
         endif
      enddo

    return
    end function extra_diag
