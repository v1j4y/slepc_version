        program main
        implicit none
        integer::i,tistart
        real,allocatable::tval(:)
        integer,allocatable::tcol(:)
        integer::tcountcol
        allocate (tcol(natomax))
        allocate (tval(natomax))
        istart = 10
        tistart = 1
        call unit(tistart,tcountcol,tcol,tval)
        
        end
