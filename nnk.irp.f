BEGIN_PROVIDER [integer, nnk]

    implicit none
    BEGIN_DOC
    ! provides nnk = total number of non zero elements in H
    END_DOC
    integer :: extra_diag
    
    nnk+=extra_diag()
END_PROVIDER
