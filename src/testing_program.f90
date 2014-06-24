program testing_program

use random_utils
use forest_parallel
use classification
use tree_utils
use sort_utils

implicit none

integer :: exitflag


! classification module tests
exitflag = test_splitnode_01()
exitflag = test_splitnode_02()
exitflag = test_splitnode_03()
exitflag = test_splitnode_04()
exitflag = test_splitnode_05()
exitflag = test_splitnode_06()
exitflag = test_insertion_sort_01()
exitflag = test_grow_predict_01()
exitflag = test_tree2flat_flat2tree_01()
exitflag = test_grow_01()
exitflag = test_grow_02()
exitflag = test_grow_03()
exitflag = test_grow_04()
exitflag = test_grow_05()
exitflag = test_bootstrap_01()
exitflag = test_grow_forest_01()
exitflag = test_grow_predict_forest_01()


end program
