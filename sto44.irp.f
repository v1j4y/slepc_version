        SUBROUTINE sto44(i,j,xmat)

        implicit none
        integer :: i,j
        real*8 ::xmat

        open(unit=44,file='FIL44',ACCESS='append',form='formatted')

        write(44,*)i,j,xmat
        write(44,*)j,i,xmat
        close(44)

        return
        end

