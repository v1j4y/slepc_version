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
        integer :: ik,imat4,iaa2,iik
        integer :: ik1,ik2,jmat4,IC,ikmax,ikmin
        real*8 :: dmat4
        logical :: yw
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

            i=1+tistart/nt2
            k=1+mod(tistart , nt2)

            call getdet(tistart,ideter2)
            deter=ideter2
            write(6,*)(deter(i),i=1,natom)
            Touch deter
            call elem_diag(xmat)
            call extra_diag()

        tcountcol=countcol
        do i=1,32
            if(val(i).ne.0d0)then
            tcol(i)=col(i)
            endif
            tval(i)=val(i)
        enddo
        print *,(tcol(i),i=1,32)
    end
