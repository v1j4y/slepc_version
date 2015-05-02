        subroutine unit_l1(tl1,&
        tl2,                   &
        tktyp,                 &
        tistart,               &
        txjjxy,                &
        txjjz ,                &
        txtt  ,                &
        tcountcol,             &
        tntrou,                &
        tcol,tval)
        implicit none
        integer,INTENT(INOUT)::tistart,tntrou
        integer::i
        real,INTENT(INOUT)::tval(maxlien)
        integer(kind=selected_int_kind(16)),INTENT(INOUT)::tcol(maxlien)
        integer(kind=selected_int_kind(16)),INTENT(INOUT),dimension(22)::tcountcol
        integer(kind=selected_int_kind(16)),INTENT(INOUT)::tl1(maxlien),tl2(maxlien),tktyp(maxlien)
        real*8,INTENT(INOUT)::txtt(maxlien),txjjz(maxlien),txjjxy(maxlien)
    
        do i=1,maxlien
        l1(i)=tl1(i)
        l2(i)=tl2(i)
        ktyp(i)=tktyp(i)
        xtt(i)     = txtt(i)
        xjjxy(i)   = txjjxy(i)
        xjjz (i)   = txjjz (i)
        enddo
        ntrou = tntrou
        tcol=0
        tval=0d0
        provide l1 l2 ktyp xtt xjjxy xjjz ntrou
        call unit(tistart,tcountcol,tcol,tval)

        end


