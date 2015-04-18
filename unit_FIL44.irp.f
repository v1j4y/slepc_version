    subroutine unit(tistart,tcountcol,tcol,tval)

    BEGIN_DOC
    ! file units for writing
    END_DOC

    implicit none
    integer :: i,j,k,ia1,ia2,l,m,chcind,chcval,ii
    integer :: count,unit_44,unit_33
    integer :: iat,nbtots
    integer(kind=selected_int_kind(16))::iaa
    integer :: kkio,kkiok,n,nz
    integer,allocatable ::ideter1(:),ideter2(:),deti(:),detj(:)
    integer(kind=selected_int_kind(16)),dimension(42) ::tl1,tl2,tktyp
    integer(kind=selected_int_kind(16))::tcountcol,tistart
    real,dimension(52)::tval
    integer(kind=selected_int_kind(16)),dimension(52)::tcol
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
        col(i)=0d0
        val(i)=0d0
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
            Touch deter
            call adr(deter,iaa)
            call elem_diag(xmat)
            countcol+=1
            col(countcol)=iaa
            val(countcol)=xmat*1.0d0

            call extra_diag(iaa)

        tcountcol=countcol
        do i=1,52
            if(col(i).ne.0)then
                if(val(i) .ne. 0 .or. col(i).eq.tistart)then
                    tcol(i)=col(i)
                    tval(i)=val(i)
                endif
            endif
        enddo
        print *,tistart
        print *,(tcol(i),i=1,42)
    end
