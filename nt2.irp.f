BEGIN_PROVIDER [integer, nt2]
&BEGIN_PROVIDER [integer, idet2,(natomax,jrangmax)]
    BEGIN_DOC
    ! calculates the number of det the 1's moving
    END_DOC


!   call combin(idet2(1,nt2+1),natrest,ial0,nt2,32,jrangmax)
    nt2=   nint(gamma(real(natom-ntrou+1,16))/((gamma(real(nalpha+1,16))*gamma(real(nbeta+1,16)))),selected_int_kind(16))
    print *,"nt2=",nt2,natom,ntrou,nalpha,nbeta
END_PROVIDER
