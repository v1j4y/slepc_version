BEGIN_PROVIDER [integer, nt1]
&BEGIN_PROVIDER [integer, idet1,(natomax,jrangmax)]
    BEGIN_DOC
    ! calculates the number of det the 3's moving
    END_DOC

!   call combin(idet1(1,nt1+1),natom,ntrou,nt1,32,jrangmax)
    nt1=   nint(gamma(real(natom+1,16))/(gamma(real(natom-ntrou+1,16))*gamma(real(ntrou+1,16))),selected_int_kind(16))
    write(6,*)'nt1',nt1
    if(FAM1)nt1=nt1/2
    write(6,*)'nt1',nt1
END_PROVIDER
