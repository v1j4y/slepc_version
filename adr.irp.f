subroutine adr(ideter,add)
    implicit none
    BEGIN_DOC
    ! this subroutine provides the address of a detrminant 
    ! given in old format.
    ! It searches in a list of generated determinants and
    ! matches the given determinant.
    END_DOC
    integer,INTENT(INOUT)::ideter(natomax)
    integer,INTENT(INOUT)::add
    integer::det,i,deth,addh,detnew,count

    det=0
    detnew=0
    deth=0
    count=0
    call conv(ideter,det,deth)
    print *,'---'
    Do i=0,natom-1
        if(BTEST(deth,i))then
            count=count+1
            print *,count,i
        endif
        if(BTEST(det,i))then
            detnew=IBSET(detnew,i-count)
            print *,'-',count,i
        endif
    enddo
    det=detnew
    call searchdet(det,add,deth,addh)
    add = add + (nt1-addh)*(nt2)


10  FORMAT(B64,I8,F8.2)
15  FORMAT(B64,I8,I8,I8)
11  FORMAT(B64,I3,B64)
12  FORMAT(I5,$)
13  FORMAT(B64,B64)
14  FORMAT(B64,I8)
16  FORMAT(B64,I8,I8)
end
