    subroutine unit(tistart,tcountcol,tcol,tval)

    BEGIN_DOC
    ! file units for writing
    END_DOC

    implicit none
    integer :: i,j,k,ia1,ia2,l,m,chcind,chcval,ii,tistart2
    integer :: count,unit_44,unit_33
    integer :: iat,nbtots
    integer(kind=selected_int_kind(16))::iaa
    integer :: kkio,kkiok,n,nz,cdiag,cexdiag
    integer,allocatable ::ideter1(:),ideter2(:),deti(:),detj(:)
    integer(kind=selected_int_kind(16)),dimension(maxlien) ::tl1,tl2,tktyp
    integer(kind=selected_int_kind(16)),dimension(22)::tcountcol
    integer(kind=selected_int_kind(16))::tistart
    real*8,dimension(maxlien)::tval
    integer(kind=selected_int_kind(16)),dimension(maxlien)::tcol
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

        countcol=0
    unit_44=44
    unit_33=33
        nnk=0
        xmat=0d0
        count=0
        cdiag=1
        cexdiag=0
        tistart2=tistart


            do i=1,natomax
                col(i)=0
                val(i)=0d0
                tval(i)=0d0
                tcol(i)=0
            enddo
                tcountcol=0
                countcol=0
                xmat=0d0
                count=0

            tistart=tistart
            i=1+tistart/nt2
            k=1+mod(tistart , nt2)

!           call getdet(tistart,ideter2)
!           deter=ideter2
!           Touch deter
!           call adr(deter,iaa)
!           call elem_diag(xmat)
!           countcol+=1
!           col(countcol)=iaa
!           val(countcol)=xmat*1.0d0

            call extra_diag(tistart)

        tcountcol=countcolfull
        do i=1,maxlien
            if(col(i).ne.0)then
                if(val(i) .ne. 0 .or. col(i).eq.tistart2)then
                    tcol(i)=col(i)
                    tval(i)=val(i)
                endif
                if(col(i).eq.tistart2)then
                    cexdiag+=1
                elseif(cexdiag .eq. countcolfull(cdiag))then
                    cexdiag=0
                    if(cdiag.lt.22)then
                    cdiag+=1
                    endif
                    tistart2+=1
                else
                    cexdiag+=1
                endif
            endif
        enddo
!       print *,tistart,countcol,(tcountcol(i),i=1,22)
!       print *,""
!       print *,(tcol(i),i=1,maxlien)
    end
