subroutine searchdet(det,add,deth,addh)
    BEGIN_DOC
    ! this subroutine is at the heart of the idea
    ! it will generate all the determinants in a fixed order
    ! then find the posistion of the determinant given and
    ! return it's position in add.
    END_DOC
    integer,INTENT(INOUT)::det
    integer,INTENT(INOUT)::add
    integer,INTENT(INOUT)::deth
    integer,INTENT(INOUT)::addh
    integer::i,a,const
    i=1
    a=0
    add=0
    print *,"enter"
    const=0

    If(ntrou.ge.1)then

            const=0
            a=0
            addh=0
            i=1
            do while (i.le.(nt1))
                if(a.eq.deth)then
                    addh=i-2
                    print *,"leave",addh,ah
                    EXIT
                endif

                const=1
                i+=1
                do while(popcnt(a).ne.ntrou .or. const==1)
                    a+=1
                    const=0
                enddo
            enddo
            if(a.eq.deth)then
            addh=i-1
            endif

    endif

    !C if det=0 then exit
    a=0
    i=0
    count=0
    if(a.eq.det)then
    add=1
    Return
    endif

    do while (i.le.(nt2))
        if(a.eq.det)then
            if(a.eq.1)then
               add=i
               count=-1
               print *,"le",add,a
               EXIT
            else
               add=i
               print *,"le",add,a
               count=-1
               EXIT
            endif
        endif

        const=1
        i+=1
!C      write(6,16)a,a,i-2
        do while(popcnt(a).ne.nbeta .or. const==1)
            a+=1
            const=0
        enddo
    enddo
    if(a.eq.det .and. count.ne.-1)then
    add=i-1
    endif
    

10  FORMAT(B64,I8,F8.2)
15  FORMAT(B64,I8,I8,I8)
11  FORMAT(B64,I3,B64)
12  FORMAT(I5,$)
13  FORMAT(B64,B64)
14  FORMAT(B64,I8)
16  FORMAT(B64,I8,I8)
end