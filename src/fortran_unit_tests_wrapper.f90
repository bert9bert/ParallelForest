!------------------------------------------------------------------------------
!   This is a wrapper for R to call that will run all the unit tests that
!       are written in Fortran.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------

subroutine fortran_unit_tests_wrapper(exitflag)

use random_utils
use forest_parallel
use classification
use tree_utils
use sort_utils

implicit none

integer, intent(out) :: exitflag

integer, parameter :: NUM_TESTS = 20
integer :: test_exitflag(NUM_TESTS)

! classification module tests

test_exitflag(01) = test_splitnode_01()
test_exitflag(02) = test_splitnode_02()
test_exitflag(03) = test_splitnode_03()
test_exitflag(04) = test_splitnode_04()
test_exitflag(05) = test_splitnode_05()
test_exitflag(06) = test_splitnode_06()
test_exitflag(07) = test_splitnode_07()
test_exitflag(08) = test_qsort_01()
test_exitflag(09) = test_insertion_sort_01()
test_exitflag(10) = test_grow_predict_01()
test_exitflag(11) = test_tree2flat_flat2tree_01()
test_exitflag(12) = test_grow_01()
test_exitflag(13) = test_grow_02()
test_exitflag(14) = test_grow_03()
test_exitflag(15) = test_grow_04()
test_exitflag(16) = test_grow_05()
test_exitflag(17) = test_bootstrap_01()
test_exitflag(18) = test_bootstrap_balanced_01()
test_exitflag(19) = test_grow_forest_01()
test_exitflag(20) = test_grow_predict_forest_01()

if(minval(test_exitflag)==0 .and. maxval(test_exitflag)==0) then
    exitflag = 0
else
    exitflag = -1
endif


end subroutine
