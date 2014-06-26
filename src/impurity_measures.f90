!------------------------------------------------------------------------------
!   Possible impurity measures to be used in tree building.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


module impurity_measures


use utils

implicit none

contains


function gini_impurity_measure(Y, N) result(impurity)
    integer, intent(in) :: N
    integer, intent(in) :: Y(N)
    real(dp) :: impurity
    integer :: num1s
    real(dp) :: f0, f1

    ! check that Y only contains 1s or 0s
    ! ...

    num1s = sum(Y)
    f1 = real(num1s, dp)/N
    f0 = 1 - f1

    impurity = f0*(1-f0) + f1*(1-f1)
end function


end module impurity_measures