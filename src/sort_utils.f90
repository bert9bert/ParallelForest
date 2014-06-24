module sort_utils


!------------------------------------------------------------------------------
!   
!   Author: Bertram Ieong
!------------------------------------------------------------------------------

use utils

implicit none

contains


! TODO: Will use basic, but very slow, insertion sort with carry 
! for integer vec for now. This will 
! be replaced by a better sort later and this subroutine will be deleted.
pure subroutine insertion_sort(N, arr, arrcarry, opt_perform_index_carry)
    ! variable declarations
    integer, intent(in) :: N
    real(dp), intent(inout) :: arr(N)
    integer, intent(inout) :: arrcarry(N)
    logical, optional, intent(in) :: opt_perform_index_carry
    logical :: perform_index_carry

    real(dp) :: elem, elemcarry
    integer :: i,j

    ! figure out if arrcarry should be carried as inputted or if 
    ! a carried index array should be placed into it
    if(present(opt_perform_index_carry)) then
        perform_index_carry = opt_perform_index_carry
    else
        perform_index_carry = .true.
    endif

    if(perform_index_carry) then
        arrcarry = (/(i, i=1,N)/)
    endif

    ! sort
    do i=2,N
        elem = arr(i)
        elemcarry = arrcarry(i)

        j = i - 1
        do while (j>=1)
            if(.not. arr(j)>elem) exit

            arr(j+1) = arr(j)
            arrcarry(j+1) = arrcarry(j)
            j = j - 1
        enddo
        arr(j+1) = elem
        arrcarry(j+1) = elemcarry
    enddo

end subroutine




function test_insertion_sort_01() result(exitflag)
    ! test that insertion_sort() works
    integer, parameter :: N = 20
    real(dp) :: arr(N), arr_correct(N)
    integer :: arrcarry(N), arrcarry_correct(N)
    integer :: exitflag

    print *, " "
    print *, "---------- Running Test Function test_insertion_sort_01 -------------------"

    ! define real array to sort, integer array to carry, and corresponding
    ! sorted arrays
    arr = (/0.92_dp, 0.80_dp, 0.51_dp, 0.03_dp, 0.99_dp, &
            0.40_dp, 0.66_dp, 0.83_dp, 0.61_dp, 0.35_dp, &
            0.47_dp, 0.37_dp, 0.25_dp, 0.84_dp, 0.70_dp, &
            0.19_dp, 0.41_dp, 0.28_dp, 0.29_dp, 0.96_dp/)

    arrcarry = (/109, 106, 102, 112, 111, &
                 119, 104, 107, 103, 117, &
                 101, 118, 114, 108, 105, &
                 113, 120, 115, 116, 110/)

    arr_correct = (/0.03_dp, 0.19_dp, 0.25_dp, 0.28_dp, 0.29_dp, &
                    0.35_dp, 0.37_dp, 0.40_dp, 0.41_dp, 0.47_dp, &
                    0.51_dp, 0.61_dp, 0.66_dp, 0.70_dp, 0.80_dp, &
                    0.83_dp, 0.84_dp, 0.92_dp, 0.96_dp, 0.99_dp/)

    arrcarry_correct = (/112, 113, 114, 115, 116, &
                         117, 118, 119, 120, 101, &
                         102, 103, 104, 105, 106, &
                         107, 108, 109, 110, 111/)

    ! sort
    call insertion_sort(N, arr, arrcarry, .false.)

    ! check failure conditions
    if(any(arr .ne. arr_correct)) stop "Sort failed."
    if(any(arrcarry .ne. arrcarry_correct)) stop "Sort carry failed."

    print *, "Test successful if test executed without error."
end function





end module sort_utils