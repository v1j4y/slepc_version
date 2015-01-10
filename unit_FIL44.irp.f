    subroutine unit(tistart,tcountcol,tcol,tval)

    BEGIN_DOC
    ! file units for writing
    END_DOC

    implicit none
    integer :: i,j,k,ia1,ia2,l,m,chcind,chcval,ii
    integer :: count,unit_44,unit_33
    integer :: iat,nbtots,iaa
    integer :: kkio,kkiok,n,nz
    integer,allocatable ::ideter1(:),ideter2(:),deti(:),detj(:),tl1(:),tl2(:),tktyp(:)
    integer::tcountcol,tistart
    real,dimension(32)::tval
    integer,dimension(32)::tcol
    real*8 :: xmat
    ! BEGIN_DOC
    ! provides unit of FIL33 & FIL44
    ! END_DOC

        allocate (ideter2(natomax))
!       allocate (tcol(natomax))
!       allocate (tval(natomax))

    do i=1,natomax
        tval(i)=0d0
        tcol(i)=0d0
    enddo
        tcountcol=0
        countcol=0
    unit_44=44
    unit_33=33
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
                   count+=1

                   if(count.eq.tistart)then

                   Touch deter
                   call adress(deter,iaa)
                   call elem_diag(xmat)
                    countcol+=1
                    col(countcol)=iaa
                    val(countcol)=xmat*1.0d0
                    
                  endif

                enddo
             enddo
             nnk+=rank
        close(33)
        close(44)

        tcountcol=countcol
        do i=1,32
            if(col(i).ne.0)then
            tcol(i)=col(i)-1
            endif
            tval(i)=val(i)
        enddo
        print *,(tcol(i),i=1,32)
    end
