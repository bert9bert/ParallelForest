!------------------------------------------------------------------------------
!   Module providing sorting functions and subroutines.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


module sort_utils


use utils

implicit none

type pair_dp_int
    real(dp) :: key
    integer :: index
end type pair_dp_int


contains


! -----  Inline Quick Sort for a double precision real key and integer index  -----

pure subroutine qsort_inline_dp_int(N, array, array_carry, opt_perform_index_carry)
    ! ---  Variable Declarations  ---
    ! input/output arguments
    integer, intent(in) :: N
    real(dp), intent(inout) :: array(N)
    integer, intent(inout) :: array_carry(N)
    logical, optional, intent(in) :: opt_perform_index_carry

    ! private variables
    logical :: perform_index_carry
    type(pair_dp_int) :: array_pairs(N)

    ! counting variables
    integer :: i


    ! ---  Resolve optional arguments  ---
    ! figure out if arrcarry should be carried as inputted or if 
    ! a carried index array should be placed into it
    if(present(opt_perform_index_carry)) then
        perform_index_carry = opt_perform_index_carry
    else
        perform_index_carry = .true.
    endif

    if(perform_index_carry) then
        array_carry = (/(i, i=1,N)/)
    endif


    ! ---  create one-dimensional array of dp-int pairs for sort subroutine  ---
    do i=1,N
        array_pairs(i)%key = array(i)
        array_pairs(i)%index = array_carry(i)
    enddo

    ! ---  Perform inline quick sort  ---
    call qsort_inline_dp_int_helper(array_pairs)

    ! ---  Turn one-dimensional array of dp-int pairs back into two separate arrays ---
    do i=1,N
        array(i) = array_pairs(i)%key
        array_carry(i) = array_pairs(i)%index
    enddo

end subroutine


pure subroutine qsort_inline_dp_int_helper(array)
    implicit none

    type(pair_dp_int), intent(inout) :: array(:)  ! array of pairs
    type(pair_dp_int) :: hold
    integer, parameter :: QSORT_THRESHOLD = 96

    include "qsort_inline.f90"

contains

    logical pure function QSORT_COMPARE(a,b)
        integer, intent(in) :: a,b
        QSORT_COMPARE = (array(a)%key < array(b)%key)
    end function
end subroutine



! -----  Insertion Sort for a double precision real key and integer index  -----


pure subroutine insertion_sort(N, arr, arrcarry, opt_perform_index_carry)
    ! variable declarations
    integer, intent(in) :: N
    real(dp), intent(inout) :: arr(N)
    integer, intent(inout) :: arrcarry(N)
    logical, optional, intent(in) :: opt_perform_index_carry
    logical :: perform_index_carry

    real(dp) :: elem
    integer :: elemcarry

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



! -----  Unit tests  -----

function test_qsort_01() result(exitflag)
    ! test that qsort_inline_dp_int() works
    integer, parameter :: N = 20
    real(dp) :: arr(N), arr_correct(N)
    integer :: arrcarry(N), arrcarry_correct(N)
    integer :: exitflag

    logical, parameter :: debug = .false.

    exitflag = -1

    if(debug) then
        print *, " "
        print *, "---------- Running Test Function test_qsort_01 -------------------"
    endif

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
    call qsort_inline_dp_int(N, arr, arrcarry, .false.)

    ! check failure conditions
    if(any(arr .ne. arr_correct)) call rexit("Sort failed.")
    if(any(arrcarry .ne. arrcarry_correct)) call rexit("Sort carry failed.")

    exitflag = 0

end function



function test_insertion_sort_01() result(exitflag)
    ! test that insertion_sort() works
    integer, parameter :: N = 20
    real(dp) :: arr(N), arr_correct(N)
    integer :: arrcarry(N), arrcarry_correct(N)
    integer :: exitflag

    logical, parameter :: debug = .false.

    exitflag = -1

    if(debug) then
        print *, " "
        print *, "---------- Running Test Function test_insertion_sort_01 -------------------"
    endif

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
    if(any(arr .ne. arr_correct)) call rexit("Sort failed.")
    if(any(arrcarry .ne. arrcarry_correct)) call rexit("Sort carry failed.")

    exitflag = 0

end function

end module sort_utils
