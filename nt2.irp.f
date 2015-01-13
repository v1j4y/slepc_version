BEGIN_PROVIDER [integer, nt2]
&BEGIN_PROVIDER [integer, idet2,(natomax,jrangmax)]
    BEGIN_DOC
    ! calculates the number of det the 1's moving
    END_DOC


    call combin(idet2(1,nt2+1),natrest,ial0,nt2,32,jrangmax)
    print *,"nt2=",nt2
END_PROVIDER
