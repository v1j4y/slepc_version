        program main
        implicit none
        integer::i
        istart = 10

        call unit()
        
        print *,countcol
        print *,(col(i),i=1,natomax)
        print *,(val(i),i=1,natomax)
        end
