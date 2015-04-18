BEGIN_PROVIDER [integer(kind=selected_int_kind(16)), nt1]
&BEGIN_PROVIDER [integer(kind=selected_int_kind(16)), nt1full]
    BEGIN_DOC
    ! calculates the number of det the 3's moving
    END_DOC
    implicit none
    integer(kind=selected_int_kind(16))::natom2

!   call combin(idet1(1,nt1+1),natom,ntrou,nt1,32,jrangmax)
    nt1full=   nint(gamma(real(natom+1,16))/(gamma(real(natom-ntrou+1,16))*gamma(real(ntrou+1,16))),selected_int_kind(16))
    natom2=natom
    if(FAM1)natom2=natom/2
    nt1=   nint(gamma(real(natom2+1,16))/(gamma(real(natom2-ntrou+1,16))*gamma(real(ntrou+1,16))),selected_int_kind(16))
    write(6,*)'nt1',nt1,'nt1full',nt1full
END_PROVIDER
