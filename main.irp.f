        program main
        implicit none
        integer(kind=selected_int_kind(16)),allocatable::tl1(:),tl2(:),tktyp(:)
        integer::i,tistart
        real::t1,t2
        real,allocatable::tval(:)
        integer(kind=selected_int_kind(16)),allocatable::tcol(:)
        integer(kind=selected_int_kind(16))::tcountcol
        allocate (tl1(42))
        allocate (tl2(42))
        allocate (tktyp(42))
        allocate (tcol(52))
        allocate (tval(52))
        tl1=0
        tl2=0
        tktyp=0
        tcountcol=0
        tcol=0
        tval=0d0
        do i= 1,30
        istart = i
        tistart = istart
!       tl1=(/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,0,0,0,0,0,0,0,0/)
!       tl2=(/2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12,24,23,22,21,20,19,18,17,16,15, 14, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,0,0,0,0,0,0,0,0/)
!     tktyp=(/1, 1, 1, 1, 1, 1, 1, 1, 1,  1,  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,0,0,0,0,0,0,0,0/)
!       tl1=(/1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
!       tl2=(/2, 3, 4, 5, 6, 7, 8, 9,18,17,16,15,14,13,12,11,10, 11, 12, 13, 14, 15, 16, 17, 18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
!       tktyp=(/1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,  3,  3,  3,  3,  3,  3,  3,  3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
!       tl1=  (/1, 1, 2, 6, 2, 5, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
!       tl2=  (/2, 6, 5, 5, 3, 4, 4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
!       tktyp=(/1, 2, 2, 3, 1, 3, 2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        tl1=  (/1, 2, 3, 4, 1, 2, 3,4,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        tl2=  (/2, 3, 4, 5,10, 9, 8,7,6,7,8,9,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        tktyp=(/1, 1, 1, 1, 2, 2, 2,2,2,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        t1=secnds(0.0)
        call unit_l1(tl1,tl2,tktyp,tistart,tcountcol,tcol,tval)
        t2=secnds(t1)
        print *,'time=',t2
        enddo
        
        end
