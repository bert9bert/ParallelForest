module classification
!------------------------------------------------------------------------------
!   Module for fitting and estimating a classification tree.
!   Author: B.I.
!------------------------------------------------------------------------------

implicit none

integer, parameter :: dp = kind(0.d0)  ! double precision

type node
    ! variables set regardless of whether this node has subnodes
    type (node), pointer :: parentnode
    integer :: depth
    integer :: majority
    logical :: has_subnodes
    ! variables only not null when node has subnodes
    type (node), pointer :: leftnode, rightnode
    integer :: splitvarnum
    real(dp) :: splitvalue
end type


contains

!-----  PUBLIC FUNCTIONS AND SUBROUTINES  -----
function grow(Y,X) result(fittedtree)
    real(dp), intent(in) :: Y(:), X(:,:)
    integer :: N, P
    integer :: fittedtree  ! TODO

    ! ...
end function

!-----  PRIVATE FUNCTIONS AND SUBROUTINES  -----

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


function loss(impurity_left, impurity_right) result(loss_val)
    real(dp), intent(in) :: impurity_left, impurity_right
    real(dp) :: loss_val

    loss_val = impurity_left + impurity_right
end function


recursive function splitnode(sortedYcorresp, sortedX, P, N, &
    min_node_obs, max_depth, &
    thisdepth, build_tree, parentnode) &
    result(thisnode)

    real(dp), intent(in) :: sortedX(N,P) 
    integer, intent(in) :: sortedYcorresp(N,P)
    integer, intent(in) :: P, N
    integer, intent(in) :: min_node_obs, max_depth, thisdepth
    logical, optional :: build_tree
    type (node), target, optional :: parentnode

    type (node), pointer :: thisnode
    
    integer :: varnum, rownum
    integer :: bestsplit_varnum, bestsplit_rownum
    real(dp) :: bestsplit_loss_val
    real(dp) :: impurity_left, impurity_right, loss_val
    logical :: first_split_computed
    logical :: base1, base2, base3
    integer :: num1s

    logical, parameter :: debug01 = .false.
    character(len=50) :: fmt ! TODO: delete this when no longer needed for debugging

    allocate(thisnode)

    if(.not. present(build_tree)) build_tree = .false.

    num1s = sum(sortedYcorresp(:,1))

    ! add pointer to parent node if parent node is given in input
    if(present(parentnode)) thisnode%parentnode => parentnode

    ! set majority flag for this node
    ! TODO: figure what to do if tie
    if( max(num1s, N-num1s)==num1s ) then
        thisnode%majority = 1
    else
        thisnode%majority = 0
    endif


    ! set depth for this node
    thisnode%depth = thisdepth


    if(debug01 .eqv. .true.) then
        print *, "------------------------"
        print *, "DEBUG: Inside splitnode."
    endif

    !----- Check input assertions -----
    ! check that sortedX is sorted
    ! ...
    ! check that sortedX and sortedYcorresp are N x P


    ! Check base cases to see if tree growth can continue, 
    ! then find split and recursively call this function
    ! to attach two subnodes to this node. Otherwise, this is a terminal node.

    base1 = (N>min_node_obs)  ! min node size not met
    base2 = (thisdepth<max_depth)  ! max depth not met
    base3 = ( num1s>0 .and. num1s<N )  ! homogenous node not met

    if(debug01 .eqv. .true.) then
        print *, "------------------------"
        print *, "DEBUG: Min Mode Size OK to Grow?: ", base1
        print *, "DEBUG: Max Depth OK to Grow?: ", base2
        print *, "DEBUG: Homogenous OK to Grow?: ", base3
    endif


    if(base1 .and. base2 .and. base3) then

        ! loop through all variables and possible splits of variables to find
        ! the node split (variable, split value tuple) that
        ! minimizes the loss function

        first_split_computed = .false.

        do varnum = 1,P
            if(debug01 .eqv. .true.) then
                print *, "------------------------"
                print *, "DEBUG: Looping through variables, at P=", varnum
            endif

            do rownum = 1,N

            if(debug01 .eqv. .true.) then
                print *, "------------------------"
                print *, "DEBUG: Looping through splits, at N=", rownum
            endif

            ! skip to the next row if it has the same value for this variable as
            ! this row, unless this is the last row
            if(rownum < N) then
                if(sortedX(rownum,varnum) == sortedX(rownum+1,varnum)) then
                    cycle
                endif
            endif

            ! compute loss that results from this split
            impurity_left = gini_impurity_measure( &
            	sortedYcorresp(1:rownum,varnum), rownum)
            impurity_right = gini_impurity_measure( &
            	sortedYcorresp(rownum+1:N,varnum), N-rownum)
            loss_val = loss(impurity_left,impurity_right)

            if(debug01 .eqv. .true.) then
                print *, "DEBUG: impurity_left = ", impurity_left
                print *, "      impurity_right = ", impurity_right
                print *, "            loss_val = ", loss_val
            endif

            ! if this split has a lower loss than any previous split, then store it
            ! and discard the previous best split
            if(first_split_computed .eqv. .false.) then
                bestsplit_varnum = 1
                bestsplit_rownum  = rownum
                bestsplit_loss_val = loss_val

                first_split_computed = .true.
                
                if(debug01 .eqv. .true.) then
                    print *, "DEBUG: Initial bests set"
                endif

            else if(loss_val < bestsplit_loss_val) then
                bestsplit_varnum = varnum
                bestsplit_rownum = rownum
                bestsplit_loss_val = loss_val

                if(debug01 .eqv. .true.) then
                    print *, "DEBUG: Better bests set"
                endif

            endif

            if(debug01 .eqv. .true.) then
                print *, "DEBUG: bestsplit_varnum = ", bestsplit_varnum
                print *, "       bestsplit_rownum = ", bestsplit_rownum
                print *, "     bestsplit_loss_val = ", bestsplit_loss_val
            endif

            enddo
        enddo

        ! set the splitting variable and splitting value for this node
        thisnode%splitvarnum = bestsplit_varnum
        thisnode%splitvalue  = sortedX(bestsplit_rownum, bestsplit_varnum)

        ! create subnodes and attach
        thisnode%has_subnodes = .true.

        ! TODO: this is inefficient splicing; see if this can be made to splice column-wise
        
        ! construct and attach left node
        allocate(thisnode%leftnode)

        thisnode%leftnode = splitnode( &
            sortedYcorresp(1:bestsplit_rownum,:), sortedX(1:bestsplit_rownum,:), &
            P, bestsplit_rownum, &
            min_node_obs, max_depth, &
            thisdepth+1, .true., thisnode)


        ! construct and attach right node
        allocate(thisnode%rightnode)

        thisnode%rightnode = splitnode( &
            sortedYcorresp(bestsplit_rownum+1:N,:), sortedX(bestsplit_rownum+1:N,:), &
            P, N-bestsplit_rownum, &
            min_node_obs, max_depth, &
            thisdepth+1, .true., thisnode)


    else
        ! otherwise, this is a terminal node
        thisnode%has_subnodes = .false.

    endif

    
end function



!-----  TESTING AND DEBUGGING FUNCTIONS AND SUBROUTINES  -----
function test_splitnode_01() result(exitflag)
    ! Test the ability of the splitnode function to split a node ONCE
    ! on a dataset with just one variable. Test will be with sorted data.

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node) :: thisnode
    integer :: exitflag

    logical, parameter :: verbose = .false.

    exitflag = -1

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
    	shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
    	0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
    	shape(sortedX))

    ! set correct splits for this data
    bestsplit_varnum_correct = 1
    bestsplit_value_correct = 0.7_dp

    ! get node split
    thisnode = splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .false.)

    ! print results
    print *, " "
    print *, "---------- Test Function test_splitnode_01 -------------------"
    if(verbose) then
        print '("Fitted Var to Split   = ", i5,   ";       Correct Split Val = ", i5)', &
            thisnode%splitvarnum, bestsplit_varnum_correct
        print '("Fitted Value to Split = ", f5.3, ";  Correct Value to Split = ", f5.3)', &
            thisnode%splitvalue, bestsplit_value_correct
    endif

    ! test failure conditions
    if(thisnode%splitvarnum /= bestsplit_varnum_correct) &
        stop "Test failed: Wrong splitting variable"
    if(thisnode%splitvalue /= bestsplit_value_correct) &
        stop "Test failed: Wrong value to split variable at"

    print *, "Test successful if test executed without error."

    exitflag = 0
end function


function test_splitnode_02() result(exitflag)
! Test the ability of the splitnode function to split a node ONCE
! on a dataset with two variables. One test will be for when the proper
! split is on the first variable, and a second test will be for when
! the proper split is on the second variable. Test will be with sorted data.

    integer, parameter :: N=10, P=2
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node) :: thisnode
    integer :: exitflag
    integer :: i,j

    logical, parameter :: verbose = .false.

    exitflag = -1

    print *, " "
    print *, "---------- Test Function test_splitnode_02 -------------------"

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/ &
        1,1,1,0,0,0,0,1,1,1, &
        1,1,1,1,1,1,0,0,0,0 &
        /), &
        (/size(sortedYcorresp,1),size(sortedYcorresp,2)/) )
    sortedX = reshape((/ &
        0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, 0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp, &
        0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, 0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp  &
        /), &
        (/size(sortedX,1),size(sortedX,2)/) )

    if(verbose) then
        print *, "sortedYcorresp = "
        do i=1,size(sortedYcorresp,1)
            write(*,'("    ",i1,"        ",i1)') ( sortedYcorresp(i,j), j=1,size(sortedYcorresp,2) )
        enddo
        print *, "sortedX = "
        do i=1,size(sortedX,1)
            write(*,'(f5.3,"    ",f5.3)') ( sortedX(i,j), j=1,size(sortedX,2) )
        enddo
    endif

    ! set correct splits for this data
    bestsplit_varnum_correct = 2
    bestsplit_value_correct = 0.6_dp

    ! get node split
    thisnode = splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .false.)

    ! print results
    if(verbose) then
        print '("Fitted Var to Split   = ", i5,   ";       Correct Split Val = ", i5)', &
            thisnode%splitvarnum, bestsplit_varnum_correct
        print '("Fitted Value to Split = ", f5.3, ";  Correct Value to Split = ", f5.3)', &
            thisnode%splitvalue, bestsplit_value_correct
    endif

    ! test failure conditions
    if(thisnode%splitvarnum /= bestsplit_varnum_correct) &
        stop "Test failed: Wrong splitting variable"
    if(thisnode%splitvalue /= bestsplit_value_correct) &
        stop "Test failed: Wrong value to split variable at"

    print *, "Test successful if test executed without error."

    exitflag = 0
end function


function test_splitnode_03() result(exitflag)
    ! test that the minimum node size base case works

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node) :: thisnode
    integer :: min_node_obs
    integer :: exitflag

    exitflag = -1

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
        shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split
    min_node_obs = N
    thisnode = splitnode(sortedYcorresp, sortedX, P, N, min_node_obs, 2, 1, .false.)

    ! print results
    print *, " "
    print *, "---------- Test Function test_splitnode_03 -------------------"

    ! test failure conditions
    if(thisnode%has_subnodes .eqv. .true.) &
        stop "Test failed: Node has same as min number of obs per node but was split."

    print *, "Test successful if test executed without error."

    exitflag = 0
end function


function test_splitnode_04() result(exitflag)
    ! test that the maximum depth node size base case works

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node) :: thisnode
    integer :: max_depth
    integer :: exitflag

    exitflag = -1

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
        shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split
    max_depth = 5
    thisnode = splitnode(sortedYcorresp, sortedX, P, N, 2, max_depth, max_depth, .false.)

    ! print results
    print *, " "
    print *, "---------- Test Function test_splitnode_04 -------------------"

    ! test failure conditions
    if(thisnode%has_subnodes .eqv. .true.) &
        stop "Test failed: Node at max depth but was split."

    print *, "Test successful if test executed without error."

    exitflag = 0
end function


function test_splitnode_05() result(exitflag)
    ! test that the homogenous node base case works

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp0(N,P), sortedYcorresp1(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node) :: thisnode0, thisnode1
    integer :: exitflag

    exitflag = -1

    ! set up homogenous Y with all 0s, homogenous Y with all 1s, and X data
    sortedYcorresp0 = reshape((/0,0,0,0,0,0,0,0,0,0/), &
        shape(sortedYcorresp0))
    sortedYcorresp1 = reshape((/1,1,1,1,1,1,1,1,1,1/), &
        shape(sortedYcorresp1))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split for the two homogenous Y arrays
    thisnode0 = splitnode(sortedYcorresp0, sortedX, P, N, 2, 2, 1, .false.)
    thisnode1 = splitnode(sortedYcorresp1, sortedX, P, N, 2, 2, 1, .false.)

    ! print results
    print *, " "
    print *, "---------- Test Function test_splitnode_05 -------------------"

    ! test failure conditions
    if(thisnode0%has_subnodes .eqv. .true.) &
        stop "Test failed: Homogenous node but was split."
    if(thisnode1%has_subnodes .eqv. .true.) &
        stop "Test failed: Homogenous node but was split."

    print *, "Test successful if test executed without error."

    exitflag = 0
end function

function test_splitnode_06() result(exitflag)
    ! test that splitnode correctly points to its parent node if given, and
    ! points to nothing if not given; and that subnodes are correct

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)

    integer :: exitflag

    type (node) :: node1, node2

    character(len=50) :: fmt

    logical :: verbose = .true.

    print *, " "
    print *, "---------- Test Function test_splitnode_06 -------------------"

    node1%depth = 87
    node1%majority = 1
    node1%has_subnodes = .false.
    node1%splitvarnum = 76
    node1%splitvalue = 42

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
        shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split
    node2 = splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .false., node1)

    if(verbose) then
        fmt = '(i6, i11, l15, i16, f14.2)'

        print *, "Depth | Majority | Has Subnodes | Split Var Num | Split Value"

        print *, "For NODE 1"
        print fmt, node1%depth, node1%majority, node1%has_subnodes, &
            node1%splitvarnum, node1%splitvalue

        print *, "For NODE 2"
        print *, "parent node (should be NODE 1)"
        print fmt, node2%parentnode%depth, node2%parentnode%majority, node2%parentnode%has_subnodes, &
            node2%parentnode%splitvarnum, node2%parentnode%splitvalue
        print *, "this node (NODE 2)"
        print fmt, node2%depth, node2%majority, node2%has_subnodes, &
            node2%splitvarnum, node2%splitvalue
        print *, "left node (NODE 2's left subnode)"
        print fmt, node2%leftnode%depth, node2%leftnode%majority, node2%leftnode%has_subnodes, &
            node2%leftnode%splitvarnum, node2%leftnode%splitvalue
        print *, "right node (NODE 2's right subnode)"
        print fmt, node2%rightnode%depth, node2%rightnode%majority, node2%rightnode%has_subnodes, &
            node2%rightnode%splitvarnum, node2%rightnode%splitvalue
    endif

    !-- test for failure conditions --
    ! check that node2's parentnode points to node 1
    if( (node2%parentnode%depth        /= node1%depth) .or. &
        (node2%parentnode%majority     /= node1%majority) .or. &
        (node2%parentnode%has_subnodes .neqv. node1%has_subnodes) .or. &
        (node2%parentnode%splitvarnum  /= node1%splitvarnum) .or. &
        (node2%parentnode%splitvalue   /= node1%splitvalue) ) then

        stop "Test failed: node 2's parent attributes do not match those of node 1"
    endif



    ! check that node2's left subnode's parentnode points to node 2
    if( (node2%leftnode%parentnode%depth        /= node2%depth) .or. &
        (node2%leftnode%parentnode%majority     /= node2%majority) .or. &
        (node2%leftnode%parentnode%has_subnodes .neqv. node2%has_subnodes) .or. &
        (node2%leftnode%parentnode%splitvarnum  /= node2%splitvarnum) .or. &
        (node2%leftnode%parentnode%splitvalue   /= node2%splitvalue) ) then

        print *, "Test failed, program stop coming..."

        print *, "left node's (NODE 2's left subnode) parent node's attributes:"
        print fmt, node2%leftnode%parentnode%depth, &
            node2%leftnode%parentnode%majority, &
            node2%leftnode%parentnode%has_subnodes, &
            node2%leftnode%parentnode%splitvarnum, &
            node2%leftnode%parentnode%splitvalue

        stop "Test failed: node 2's left subnode's parentnode attributes do not match those of node 2"
    endif

    ! check that node2's right subnode's parentnode points to node 2
    if( (node2%rightnode%parentnode%depth        /= node2%depth) .or. &
        (node2%rightnode%parentnode%majority     /= node2%majority) .or. &
        (node2%rightnode%parentnode%has_subnodes .neqv. node2%has_subnodes) .or. &
        (node2%rightnode%parentnode%splitvarnum  /= node2%splitvarnum) .or. &
        (node2%rightnode%parentnode%splitvalue   /= node2%splitvalue) ) then
        stop "Test failed: node 2's right subnode's parentnode attributes do not match those of node 2"
    endif

    ! miscellaneous additional tests on node2
    if( (node2%majority /= 1) .or. &
        (node2%has_subnodes .neqv. .true.) .or. &
        (node2%splitvarnum /= 1) .or. &
        (node2%splitvalue /= 0.7_dp) ) then
        stop "Test failed: miscellaneous additional tests on node2"
    endif

    ! miscellaneous additional tests on node2's left subnode
    if( (node2%leftnode%depth /= node2%depth+1) .or. &
        (node2%leftnode%majority /= 1) .or. &
        (node2%leftnode%has_subnodes .neqv. .false.) ) then
        stop "Test failed: miscellaneous additional tests on node2's left subnode"
    endif

    ! miscellaneous additional tests on node2's right subnode
    if( (node2%rightnode%depth /= node2%depth+1) .or. &
        (node2%rightnode%majority /= 0) .or. &
        (node2%rightnode%has_subnodes .neqv. .false.) ) then
        stop "Test failed: miscellaneous additional tests on node2's right subnode"
    endif

    print *, "Test successful if test executed without error."
end function


function test_splitnode_07() result(exitflag)  ! TODO
    ! test that splitnode correct creates subnodes that both point to 
    ! the constructed node if node was split, and
    ! points to nothing if not split

    integer :: exitflag

end function

end module classification

