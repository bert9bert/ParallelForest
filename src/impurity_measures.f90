!------------------------------------------------------------------------------
!   Possible impurity measures to be used in tree building.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


module impurity_measures


use utils

implicit none

contains


function gini_impurity_measure(Y, N, opt_check_valid) result(impurity)
    integer, intent(in) :: N
    integer, intent(in) :: Y(N)
    logical, optional, intent(in) :: opt_check_valid
    real(dp) :: impurity

    integer :: num1s
    real(dp) :: f0, f1

    integer :: i

    ! if input says to do so, check that the inputs are valid dependent
    ! variable entries (0s and 1s)
    if(present(opt_check_valid)) then
        if(opt_check_valid) then
            do i=1,N
                if(.not. ( Y(i)==0 .or. Y(i)==1 )) then
                    call rexit("Invalid dependent variable entries to Gini impurity function.")
                endif
            enddo
        endif
    endif

    ! compute Gini impurity measure
    num1s = sum(Y)
    f1 = real(num1s, dp)/N
    f0 = 1 - f1

    impurity = f0*(1-f0) + f1*(1-f1)
end function


end module impurity_measures