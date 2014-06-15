program testing_program

use classification

implicit none

integer :: exitflag

! classification module tests
exitflag = test_splitnode_01()
!exitflag = test_splitnode_02()
exitflag = test_splitnode_03()
exitflag = test_splitnode_04()
exitflag = test_splitnode_05()
exitflag = test_splitnode_06()
exitflag = test_insertion_sort_01()
exitflag = test_grow_01()

end program
