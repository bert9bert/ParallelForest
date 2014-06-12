program testing_program

use classification

implicit none

integer :: exitflag

exitflag = test_splitnode_01()
exitflag = test_splitnode_02()
exitflag = test_splitnode_03()
exitflag = test_splitnode_04()
exitflag = test_splitnode_05()

end program
