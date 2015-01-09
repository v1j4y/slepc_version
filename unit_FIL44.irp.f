    subroutine unit()

    BEGIN_DOC
    ! file units for writing
    END_DOC

    implicit none
    integer :: i,j,k,ia1,ia2,l,m,chcind,chcval
    integer :: count,unit_44,unit_33
    integer :: iat,nbtots,iaa
    integer :: kkio,kkiok,n,nz
    integer,allocatable :: ideter1(:),ideter2(:),deti(:),detj(:)
    real*8 :: xmat
    ! BEGIN_DOC
    ! provides unit of FIL33 & FIL44
    ! END_DOC

        allocate (ideter2(natomax))

        open(unit=2,file='FIL2')
        open(unit=33,file='FIL33',form='formatted')



    unit_44=44
    unit_33=33
    print *,"start and end",istart
        nnk=0
        xmat=0d0
        count=0
             do i=1,nt1
                do k=1,nt2
                   do kkio=1,natom
                      deter(kkio)=2
                   enddo
                   do l=1,ntrou
                      deter(idet1(l,i))=3
                   enddo
                   do m=1,natrest
                      ideter2(m)=2
                   enddo
                   do n=1,ial0
                      ideter2(idet2(n,k))=1
                   enddo
                   iat=0
                   do kkio=1,natom
                      if(deter(kkio).ne.3)then
            	    iat=iat+1
            	    deter(kkio)=ideter2(iat)
                      endif
                   enddo
!                  write(6,*)'iaa',iaa
                   count+=1

                   if(count.eq.istart)then

                   Touch deter
!                  write(6,*)'yalt',(ytrou(l),l=1,natom)
                   call adress(deter,iaa)
!                  write(6,*)'iaa',iaa
                   write(2,*)(deter(kkiok),kkiok=1,natom),iaa
!           call ylogic(deter,yalt,ytrou,yrep1)
                   call elem_diag(xmat)
!    	       write(6,*)xmat/1.d0
                    countcol+=1
                    col(countcol)=iaa
                    val(countcol)=xmat*2.0d0
                   write(33,*)(xmat/1.0d0),count,count
!                  nz=extra_diag()
!                  write(6,*)nnk
!                  nnk+=nz
                    
                  endif

                enddo
             enddo
             nnk+=rank
        close(33)
        close(44)


    end
