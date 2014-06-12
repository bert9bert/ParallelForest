program junk
    implicit none
    integer :: yout

    yout = fn(5)
    print *, "yout = ", yout


contains

function fn(x) result(y)
    integer, intent(in) :: x
    integer, pointer :: y
    integer, target :: yval

    print *, "Entering fn"

    yval = x+1

    print *, "yval = ", yval

    y => yval

    print *, "y (the pointer to yval) = ", y

    print *, "Exiting fn"
end function

end program