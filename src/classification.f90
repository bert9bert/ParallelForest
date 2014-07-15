!------------------------------------------------------------------------------
!   Module for fitting and predicting on a classification tree.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------

module classification


use utils
use tree_utils
use sort_utils
use impurity_measures

implicit none


contains

!-----  PUBLIC FUNCTIONS AND SUBROUTINES  -----

function grow(Y, X, min_node_obs, max_depth, opt_splittable) result(fittedtree)
    ! variable declarations
    integer, intent(in) :: Y(:)
    real(dp), intent(in) :: X(:,:)
    integer, intent(in) :: min_node_obs, max_depth
    logical, optional, intent(in) :: opt_splittable(:)
    type (node), pointer :: fittedtree

    integer :: N, P
    logical, allocatable :: splittable(:)

    integer, parameter :: TOP_NODE_NUM = 0


    ! find out number of variables and number of observations
    N = size(X,1)
    P = size(X,2)

    if(N /= size(Y)) call rexit("Y has different number of obs than X")

    ! splittable
    allocate(splittable(P))
    if(present(opt_splittable)) then
        splittable = opt_splittable
    else
        splittable = .true.
    endif

    
    ! fit decision tree classifier
    fittedtree => splitnode(Y, X, P, N, &
        min_node_obs, max_depth, TOP_NODE_NUM, &
        .true., &
        opt_splittable=splittable)


end function

function predict(fittedtree, X) result(Ypred)
    ! variable declarations
    type (node), intent(in) :: fittedtree
    real(dp), intent(in) :: X(:,:)
    integer, allocatable :: Ypred(:)

    integer :: N, P
    integer :: obs

    ! find out number of variables and number of observations
    N = size(X,1)
    P = size(X,2)

    ! allocate array storing predictions
    allocate(Ypred(N))

    ! for each observation, go through fitted tree to get prediction
    do obs = 1,N
        Ypred(obs) = predict_rec_hlpr(fittedtree, X(obs,:), P)
    enddo

end function


!-----  PRIVATE FUNCTIONS AND SUBROUTINES  -----




recursive function predict_rec_hlpr(t, X_row, P) result(pred)
    ! variable declarations
    type (node), intent(in) :: t
    real(dp), intent(in) :: X_row(:)
    integer, intent(in) :: P
    integer :: pred

    ! argument checks
    if(size(X_row) /= P) call rexit("Error: Length of X_row is not P.")

    ! get prediction for this row of data
    if(.not. t%has_subnodes) then 
        ! this is a terminal node, so return prediction
        pred = t%majority
    else
        ! this is not a terminal node, so keep recursing deeper to get prediction, so
        ! need to choose left node or right node
        if( X_row(t%splitvarnum) <= t%splitvalue ) then
            pred = predict_rec_hlpr(t%leftnode, X_row, P)
        else
            pred = predict_rec_hlpr(t%rightnode, X_row, P)
        endif
    endif

end function



function loss(impurity_this, impurity_left, impurity_right, prob_left) result(loss_val)
    real(dp), intent(in) :: impurity_this, impurity_left, impurity_right, prob_left
    real(dp) :: loss_val

    if((prob_left<0) .or. (prob_left>1)) call rexit("Probability not between 0 and 1.")

    loss_val = prob_left*impurity_left + (1-prob_left)*impurity_right - impurity_this
end function


recursive function splitnode(Y, X, P, N, &
    min_node_obs, max_depth, thisdepth, &
    opt_build_tree, &
    opt_parentnode, opt_impurity_this, opt_reset_tag_ctr, opt_splittable) &
    result(thisnode)

    !----- Variable Declarations -----
    ! input variables
    integer, intent(in) :: N, P
    integer, intent(in) :: Y(N)
    real(dp), intent(in) :: X(N,P)
    integer, intent(in) :: min_node_obs, max_depth, thisdepth
    logical, optional :: opt_build_tree
    type (node), target, optional :: opt_parentnode
    real(dp), optional, intent(in) :: opt_impurity_this
    logical, optional, intent(in) :: opt_reset_tag_ctr
    logical, optional, intent(in) :: opt_splittable(P)

    ! output variables
    type (node), pointer :: thisnode

    ! private variables
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)

    logical :: build_tree
    real(dp) :: impurity_this
    logical :: reset_tag_ctr 
    logical :: splittable(P)

    integer, save :: tag = 0
    
    integer :: varnum, rownum
    integer :: bestsplit_varnum, bestsplit_rownum
    real(dp) :: bestsplit_impurity_left, bestsplit_impurity_right, bestsplit_loss_val
    real(dp) :: impurity_left, impurity_right, loss_val
    logical :: first_split_computed
    logical :: base1, base2, base3
    integer :: num1s
    logical :: Xi_homog(P)
    real(dp), allocatable :: Xleft(:,:), Xright(:,:)
    integer, allocatable :: Yleft(:), Yright(:)

    logical :: valid_split

    ! counting variables
    integer :: i,j,k

    ! debugging variables
    logical, parameter :: verbose = .false.


    ! give initial values
    bestsplit_varnum = -1

    ! allocate memory for this node
    allocate(thisnode)

    ! splittable variables
    if(present(opt_splittable)) then
        splittable = opt_splittable
    else
        splittable = .true.
    endif

    ! tag
    if(present(opt_reset_tag_ctr)) then
        reset_tag_ctr = opt_reset_tag_ctr
    else
        reset_tag_ctr = .true.
    endif

    if(reset_tag_ctr) then
        tag = 0
    else
        tag = tag + 1
    endif

    thisnode%tag = tag

    if(verbose) then
        print *, "==============================================================================="
        print *, "Recursive function SPLITNODE invoked..."
        print '("P=", i5, "; min_node_obs=", i5, "; max_depth=", i5, "; build_tree=", l5)', P, min_node_obs, max_depth, build_tree
        print '("N=", i5, "; thisdepth=", i5)', N, thisdepth
        print '("tag=",(A))', tag
    endif

    if(verbose .and. P==2) then
        print *, "[X1 X2 | Y ] = ["
        do i=1,N
            print '(f10.5, "    ", f10.5, "    ", i3)', X(i,1), X(i,2), Y(i)
        enddo
        print *, "]"
    endif

    ! take Y vector and X matrix, sort X by column and output
    ! a matrix with each column being the corresponding carried
    ! sort of the Y vector
    sortedX = X

    do j=1,P
        sortedYcorresp(:,j) = Y
        call qsort_inline_dp_int(N, sortedX(:,j), sortedYcorresp(:,j), .false.)
    enddo

    if(.not. present(opt_build_tree)) then
        build_tree = .false.
    else
        build_tree = opt_build_tree
    endif

    if(.not. present(opt_impurity_this)) then
        impurity_this = gini_impurity_measure(sortedYcorresp(:,1), N)
    else
        impurity_this = opt_impurity_this
    endif

    if(verbose) then
        print '("impurity of this node = ", f5.3)', impurity_this
    endif

    num1s = sum(sortedYcorresp(:,1))


    ! add pointer to parent node if parent node is given in input
    if(present(opt_parentnode)) thisnode%parentnode => opt_parentnode

    ! set majority flag for this node.
    ! if tie, defaults to 1
    if( max(num1s, N-num1s)==num1s ) then
        thisnode%majority = 1
    else
        thisnode%majority = 0
    endif


    ! set depth for this node
    thisnode%depth = thisdepth



    !----- Check input assertions -----

    ! Check base cases to see if tree growth can continue, 
    ! then find split and recursively call this function
    ! to attach two subnodes to this node. Otherwise, this is a terminal node.

    base1 = (N>min_node_obs)  ! min node size not met
    base2 = (thisdepth<max_depth)  ! max depth not met
    base3 = ( num1s>0 .and. num1s<N )  ! homogenous node not met



    if(verbose) then
        print *, "Pass base cases to grow?"
        print *, "!Min Node Size     !Max Depth    !Homogeneous"
        print '(3l15)', base1, base2, base3
    endif


    if(base1 .and. base2 .and. base3) then

        ! loop through all variables and possible splits of variables to find
        ! the node split (variable, split value tuple) that
        ! minimizes the loss function

        first_split_computed = .false.

        if(verbose) then
            print *, "Base cases passed. Beginning exhaustive search loops..."
            print *, " var  row      impL      impR      loss var* row*     loss*"
        endif

        Xi_homog = .true.
        do varnum = 1,P
            if(.not. splittable(varnum)) cycle  ! TODO: account for if all says not splittable

            
            do rownum = 1,N

                ! skip to the next row if it has the same value for this variable as
                ! this row, unless this is the last row that can be split on
                

                if(rownum < N) then
                    if(sortedX(rownum,varnum) == sortedX(rownum+1,varnum)) then
                        cycle
                    else
                        Xi_homog(varnum) = .false.
                    endif
                else if(rownum==N) then
                    cycle
                endif

                ! compute loss that results from this split
                impurity_left = gini_impurity_measure( &
                    sortedYcorresp(1:rownum,varnum), rownum)
                impurity_right = gini_impurity_measure( &
                    sortedYcorresp(rownum+1:N,varnum), N-rownum)
                loss_val = loss(impurity_this, impurity_left, impurity_right, real(rownum, dp)/N)


                ! if this split has a lower loss than any previous split, then store it
                ! and discard the previous best split
                if(first_split_computed .eqv. .false.) then
                    bestsplit_varnum = varnum
                    bestsplit_rownum  = rownum
                    bestsplit_impurity_left = impurity_left
                    bestsplit_impurity_right = impurity_right
                    bestsplit_loss_val = loss_val

                    first_split_computed = .true.
                    
                else if(loss_val < bestsplit_loss_val) then
                    bestsplit_varnum = varnum
                    bestsplit_rownum = rownum
                    bestsplit_impurity_left = impurity_left
                    bestsplit_impurity_right = impurity_right
                    bestsplit_loss_val = loss_val


                endif


                if(verbose) then
                    print '(i5, i5, 3f10.3, i5, i5, f10.3)', varnum, rownum, &
                        impurity_left, impurity_right, loss_val, &
                        bestsplit_varnum, bestsplit_rownum, bestsplit_loss_val
                endif

            enddo
        enddo

        valid_split = any((.not. Xi_homog) .and. splittable)

        if(valid_split) then
            ! set the splitting variable and splitting value for this node
            thisnode%splitvarnum = bestsplit_varnum
            thisnode%splitvalue  = sortedX(bestsplit_rownum, bestsplit_varnum)
        endif

        if(build_tree .and. valid_split) then          

            ! create subnodes and attach
            thisnode%has_subnodes = .true.

            ! create left and right data to be passed to the subnodes

            allocate(Xleft(bestsplit_rownum,P))
            allocate(Yleft(bestsplit_rownum))
            allocate(Xright(N-bestsplit_rownum,P))
            allocate(Yright(N-bestsplit_rownum))

            j=1
            k=1
            do i=1,N
                if(X(i,thisnode%splitvarnum)<=thisnode%splitvalue) then
                    Xleft(j,:) = X(i,:)
                    Yleft(j) = Y(i)
                    j = j+1
                else
                    Xright(k,:) = X(i,:)
                    Yright(k) = Y(i)
                    k = k+1
                endif
            enddo

            ! construct and attach left node
            allocate(thisnode%leftnode)

            thisnode%leftnode => splitnode( &
                Yleft, Xleft, &
                P, bestsplit_rownum, &
                min_node_obs, max_depth, &
                thisdepth+1, .true., thisnode, bestsplit_impurity_left, .false., &
                splittable)

            ! construct and attach right node
            allocate(thisnode%rightnode)

            thisnode%rightnode => splitnode( &
                Yright, Xright, &
                P, N-bestsplit_rownum, &
                min_node_obs, max_depth, &
                thisdepth+1, .true., thisnode, bestsplit_impurity_right, .false., &
                splittable)
        else
            thisnode%has_subnodes = .false.
        endif
    else
        ! otherwise, this is a terminal node
        thisnode%has_subnodes = .false.
    endif

end function



!-----  TESTING AND DEBUGGING FUNCTIONS AND SUBROUTINES  -----
function test_splitnode_01() result(exitflag)
    ! Test the ability of the splitnode function to split a node ONCE
    ! on a dataset with just one variable. Test will be with sorted data.

    integer, parameter :: N=13, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node), pointer :: thisnode
    integer :: exitflag

    logical, parameter :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_01 -------------------"
    endif

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,1,1,0,0,0,0/), &
        shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.2_dp, 0.2_dp, &
        0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! set correct splits for this data
    bestsplit_varnum_correct = 1
    bestsplit_value_correct = 0.7_dp

    ! get node split
    thisnode => splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .false.)

    ! print results
    if(verbose) then
        print '("Fitted Var to Split   = ", i5,   ";       Correct Split Val = ", i5)', &
            thisnode%splitvarnum, bestsplit_varnum_correct
        print '("Fitted Value to Split = ", f5.3, ";  Correct Value to Split = ", f5.3)', &
            thisnode%splitvalue, bestsplit_value_correct
    endif

    ! test failure conditions
    if(thisnode%splitvarnum /= bestsplit_varnum_correct) &
        call rexit("Test failed: Wrong splitting variable")
    if(thisnode%splitvalue /= bestsplit_value_correct) &
        call rexit("Test failed: Wrong value to split variable at")


    exitflag = 0
end function


function test_splitnode_02() result(exitflag)
! Test the ability of the splitnode function to split a node ONCE
! on a dataset with two variables. One test will be for when the proper
! split is on the first variable, and a second test will be for when
! the proper split is on the second variable. Test will be with sorted data.

    integer, parameter :: sqrtN=10, P=2
    real(dp), allocatable :: X(:,:) 
    integer, allocatable :: Y(:)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node), pointer :: thisnode
    integer :: exitflag

    integer :: i,j,ctr
    integer :: N

    logical, parameter :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_02 -------------------"
    endif

    ! set up Y and X data
    N = sqrtN**2

    allocate(X(N,P))
    allocate(Y(N))

    ctr = 1
    do i=1,sqrtN
        do j=1,sqrtN
            X(ctr,1) = real(i,dp)/100
            X(ctr,2) = real(j,dp)/100

            ctr = ctr + 1
        enddo
    enddo

    do ctr=1,N
        if(X(ctr,2)>=0.04_dp) then
            Y(ctr) = 1
        else if(X(ctr,1)<=0.03) then
            Y(ctr) = 0
        else if((X(ctr,1)>=0.05) .and. (X(ctr,1)<=0.07)) then
            Y(ctr) = 0
        else
            Y(ctr) = 1
        endif
    enddo

    ! set correct splits for this data
    bestsplit_varnum_correct = 2
    bestsplit_value_correct = 0.03_dp

    ! get node split
    thisnode => splitnode(Y, X, P, N, 2, 2, 1, .false.)


    ! test failure conditions
    if(thisnode%splitvarnum /= bestsplit_varnum_correct) &
        call rexit("Test failed: Wrong splitting variable")

    if(thisnode%splitvalue /= bestsplit_value_correct) then
        if(verbose) then
            print *, "Test failure upcoming..."
            print *, "Computed split value = ", thisnode%splitvalue
        endif

        call rexit("Test failed: Wrong value to split variable at")
    endif


    exitflag = 0
end function


function test_splitnode_03() result(exitflag)
    ! test that the minimum node size base case works

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    type (node), pointer :: thisnode
    integer :: min_node_obs
    integer :: exitflag

    logical, parameter :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_03 -------------------"
    endif

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
        shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split
    min_node_obs = N
    thisnode => splitnode(sortedYcorresp, sortedX, P, N, min_node_obs, 2, 1, .false.)


    ! test failure conditions
    if(thisnode%has_subnodes .eqv. .true.) then
        call rexit("Test failed: Node has same as min number of obs per node but was split.")
    endif


    exitflag = 0
end function


function test_splitnode_04() result(exitflag)
    ! test that the maximum depth node size base case works

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    type (node), pointer :: thisnode
    integer :: max_depth
    integer :: exitflag

    logical, parameter :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_04 -------------------"
    endif

    ! set up sorted Y and X data
    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
        shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split
    max_depth = 5
    thisnode => splitnode(sortedYcorresp, sortedX, P, N, 2, max_depth, max_depth, .false.)


    ! test failure conditions
    if(thisnode%has_subnodes .eqv. .true.) then
        call rexit("Test failed: Node at max depth but was split.")
    endif

    exitflag = 0
end function


function test_splitnode_05() result(exitflag)
    ! test that the homogeneous node base case works

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P), Xdata_alt(8,2)
    integer :: sortedYcorresp0(N,P), sortedYcorresp1(N,P)
    integer :: Ydata_alt(8)
    type (node), pointer :: thisnode0, thisnode1, thisnode_alt
    integer :: exitflag

    logical, parameter :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_05 -------------------"
    endif

    ! set up homogenous Y with all 0s, homogenous Y with all 1s, and X data
    sortedYcorresp0 = reshape((/0,0,0,0,0,0,0,0,0,0/), &
        shape(sortedYcorresp0))
    sortedYcorresp1 = reshape((/1,1,1,1,1,1,1,1,1,1/), &
        shape(sortedYcorresp1))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
        0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
        shape(sortedX))

    ! get node split for the two homogenous Y arrays
    thisnode0 => splitnode(sortedYcorresp0, sortedX, P, N, 2, 2, 1, .false.)
    thisnode1 => splitnode(sortedYcorresp1, sortedX, P, N, 2, 2, 1, .false.)

    ! alternate test
    Ydata_alt = (/1,1,1,1,1,1,1,1/)
    Xdata_alt = reshape((/0.06000_dp, 0.06000_dp, 0.08000_dp, 0.08000_dp, 0.08000_dp, 0.07000_dp, 0.06000_dp, 0.10000_dp, &
                          0.02000_dp, 0.01000_dp, 0.03000_dp, 0.02000_dp, 0.03000_dp, 0.03000_dp, 0.03000_dp, 0.02000_dp/), &
                        shape(Xdata_alt))

    thisnode_alt => splitnode(Ydata_alt, Xdata_alt, 2, 8, 1, 10, 1, .false.)


    ! test failure conditions
    if(thisnode0%has_subnodes .eqv. .true.) &
        call rexit("Test failed: Homogenous node but was split.")
    if(thisnode1%has_subnodes .eqv. .true.) &
        call rexit("Test failed: Homogenous node but was split.")
    if(thisnode_alt%has_subnodes .eqv. .true.) &
        call rexit("Test failed: Homogenous node but was split.")


    exitflag = 0
end function

function test_splitnode_06() result(exitflag)
    ! test that splitnode correctly points to its parent node and
    ! to its left and right subnodes

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)

    integer :: exitflag

    type (node) :: node1
    type (node), pointer :: node2

    character(len=50) :: fmt

    logical :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_06 -------------------"
    endif

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
    node2 => splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .true., node1)

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

        call rexit("Test failed: node 2's parent attributes do not match those of node 1")
    endif



    ! check that node2's left subnode's parentnode points to node 2
    if( (node2%leftnode%parentnode%depth        /= node2%depth) .or. &
        (node2%leftnode%parentnode%majority     /= node2%majority) .or. &
        (node2%leftnode%parentnode%has_subnodes .neqv. node2%has_subnodes) .or. &
        (node2%leftnode%parentnode%splitvarnum  /= node2%splitvarnum) .or. &
        (node2%leftnode%parentnode%splitvalue   /= node2%splitvalue) ) then

        if(verbose) then
            print *, "Test failed, program stop coming..."

            print *, "left node's (NODE 2's left subnode) parent node's attributes:"
            print fmt, node2%leftnode%parentnode%depth, &
                node2%leftnode%parentnode%majority, &
                node2%leftnode%parentnode%has_subnodes, &
                node2%leftnode%parentnode%splitvarnum, &
                node2%leftnode%parentnode%splitvalue
        endif

        call rexit("Test failed: node 2's left subnode's parentnode attributes do not match those of node 2")
    endif

    ! check that node2's right subnode's parentnode points to node 2
    if( (node2%rightnode%parentnode%depth        /= node2%depth) .or. &
        (node2%rightnode%parentnode%majority     /= node2%majority) .or. &
        (node2%rightnode%parentnode%has_subnodes .neqv. node2%has_subnodes) .or. &
        (node2%rightnode%parentnode%splitvarnum  /= node2%splitvarnum) .or. &
        (node2%rightnode%parentnode%splitvalue   /= node2%splitvalue) ) then
        call rexit("Test failed: node 2's right subnode's parentnode attributes do not match those of node 2")
    endif

    ! miscellaneous additional tests on node2
    if( (node2%majority /= 1) .or. &
        (node2%has_subnodes .neqv. .true.) .or. &
        (node2%splitvarnum /= 1) .or. &
        (node2%splitvalue /= 0.7_dp) ) then
        call rexit("Test failed: miscellaneous additional tests on node2")
    endif

    ! miscellaneous additional tests on node2's left subnode
    if( (node2%leftnode%depth /= node2%depth+1) .or. &
        (node2%leftnode%majority /= 1) .or. &
        (node2%leftnode%has_subnodes .neqv. .false.) ) then
        call rexit("Test failed: miscellaneous additional tests on node2's left subnode")
    endif

    ! miscellaneous additional tests on node2's right subnode
    if( (node2%rightnode%depth /= node2%depth+1) .or. &
        (node2%rightnode%majority /= 0) .or. &
        (node2%rightnode%has_subnodes .neqv. .false.) ) then
        call rexit("Test failed: miscellaneous additional tests on node2's right subnode")
    endif

    exitflag = 0

end function




function test_splitnode_07() result(exitflag)
    integer, parameter :: N=53, P=2
    real(dp) :: X(N,P) 
    integer :: Y(N)

    type (node), pointer :: tree

    integer :: exitflag

    logical, parameter :: verbose = .false.


    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_splitnode_07 -------------------"
    endif

    exitflag = -1

    ! set up data
    X(:,1) = (/ &
            0.02000_dp, &
            0.01000_dp, &
            0.04000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.05000_dp, &
            0.03000_dp, &
            0.04000_dp, &
            0.04000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.05000_dp, &
            0.04000_dp, &
            0.02000_dp, &
            0.04000_dp, &
            0.02000_dp, &
            0.05000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.04000_dp, &
            0.05000_dp, &
            0.02000_dp, &
            0.05000_dp, &
            0.01000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.05000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.04000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.06000_dp, &
            0.07000_dp, &
            0.08000_dp, &
            0.06000_dp, &
            0.10000_dp, &
            0.06000_dp, &
            0.10000_dp, &
            0.07000_dp &
        /)

    X(:,2) = (/ &
            0.01000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.01000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.03000_dp, &
            0.02000_dp, &
            0.03000_dp, &
            0.01000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.01000_dp, &
            0.01000_dp, &
            0.02000_dp, &
            0.03000_dp &
        /)

    Y = (/ &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            0, &
            1, &
            1, &
            1, &
            1, &
            1, &
            1, &
            1, &
            1 &
        /)

    ! fit tree to the data
    tree => splitnode(Y, X, P, N, &
        min_node_obs=1, max_depth=10, &
        thisdepth=2, opt_build_tree=.true., &
        opt_splittable=(/.false.,.true./))

    ! make sure has expected results; test failure conditions
    if(.not. tree%splitvarnum==2) call rexit("Error.")
    if(.not. tree%splitvalue==0.01_dp) call rexit("Error.")


    exitflag = 0

end function


function test_grow_predict_01() result(exitflag)
    ! tests the function that grows a decision tree using a simple example

    ! variable declarations
    integer, parameter :: sqrtN=10, P=2
    integer, parameter :: N = sqrtN**2
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node), pointer :: fittedtree
    integer :: exitflag

    logical, parameter :: verbose = .false.
    character(len=50) :: fmt

    integer :: i,j,ctr

    integer :: Yhat(N)

    integer :: Nnew
    real(dp), allocatable :: Xnew(:,:)
    integer, allocatable :: Ynew(:)
    integer, allocatable :: Ynewhat(:)

    integer :: numfittednodes


    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_predict_01 -------------------"
    endif

    ! -----  Fit tree on pre-determined X and Y data, and compare  -----
    ! -----  against expected results  -----

    ! set base conditions
    min_node_obs = 1
    max_depth = 10

    ctr = 1
    do i=1,sqrtN
        do j=1,sqrtN
            X(ctr,1) = real(i,dp)/100
            X(ctr,2) = real(j,dp)/100

            ctr = ctr + 1
        enddo
    enddo

    do ctr=1,N
        if(X(ctr,2)>=0.04_dp) then
            Y(ctr) = 1
        else if(X(ctr,1)<=0.05) then
            Y(ctr) = 0
        else
            Y(ctr) = 1
        endif
    enddo


    if(verbose) then
        print *, "[X1 X2 | Y ] = ["
        do ctr=1,N
            print '(f10.5, "    ", f10.5, "    ", i3)', X(ctr,1), X(ctr,2), Y(ctr)
        enddo
        print *, "]"
    endif

    ! fit tree
    fittedtree => grow(Y, X, min_node_obs, max_depth)


    if(verbose) then
        fmt = '(i6, i11, l15, i16, f14.3, i6)'

        print *, ""
        print *, "For fitted tree:"
        print *, "---------------+--------+----------+--------------+---------------+-------------+-----"
        print *, "Node           |  Depth | Majority | Has Subnodes | Split Var Num | Split Value | Tag "
        print *, "---------------+--------+----------+--------------+---------------+-------------+-----"



        write (*,'(A)',advance="no") "parent          | "
        print fmt, fittedtree%parentnode%depth, fittedtree%parentnode%majority, fittedtree%parentnode%has_subnodes, &
            fittedtree%parentnode%splitvarnum, fittedtree%parentnode%splitvalue, fittedtree%parentnode%tag

        write (*,'(A)',advance="no") "-TOP (THIS)     | "
        print fmt, fittedtree%depth, fittedtree%majority, fittedtree%has_subnodes, &
            fittedtree%splitvarnum, fittedtree%splitvalue, fittedtree%tag

        write (*,'(A)',advance="no") "--left          | "
        print fmt, fittedtree%leftnode%depth, fittedtree%leftnode%majority, fittedtree%leftnode%has_subnodes, &
            fittedtree%leftnode%splitvarnum, fittedtree%leftnode%splitvalue, fittedtree%leftnode%tag

        write (*,'(A)',advance="no") "---left's left  | "
        print fmt, fittedtree%leftnode%leftnode%depth, fittedtree%leftnode%leftnode%majority, &
            fittedtree%leftnode%leftnode%has_subnodes, &
            fittedtree%leftnode%leftnode%splitvarnum, fittedtree%leftnode%leftnode%splitvalue, fittedtree%leftnode%leftnode%tag

        write (*,'(A)',advance="no") "---left's right | "
        print fmt, fittedtree%leftnode%rightnode%depth, fittedtree%leftnode%rightnode%majority, &
            fittedtree%leftnode%rightnode%has_subnodes, &
            fittedtree%leftnode%rightnode%splitvarnum, fittedtree%leftnode%rightnode%splitvalue, fittedtree%leftnode%rightnode%tag

        write (*,'(A)',advance="no") "--right         | "
        print fmt, fittedtree%rightnode%depth, fittedtree%rightnode%majority, fittedtree%rightnode%has_subnodes, &
            fittedtree%rightnode%splitvarnum, fittedtree%rightnode%splitvalue, fittedtree%rightnode%tag

        print *, "---------------+--------+----------+--------------+---------------+------------+------"
    
        numfittednodes = countnodes(fittedtree)
        print '("There are ", i5, " nodes in the fitted tree.")', numfittednodes

    endif



    ! -- test for failure conditions --
    ! - test that expected values of nodes are realized -
    if((fittedtree%depth /= 0) .or. &
        (fittedtree%splitvarnum /= 2) .or. &
        (fittedtree%splitvalue /= 0.03_dp) .or. &
        (fittedtree%has_subnodes .neqv. .true.) .or. &
        (fittedtree%majority /= 1)) then
        call rexit("Fitted tree does not have the expected attributes.")
    endif

    if((fittedtree%leftnode%depth /= 1) .or. &
        (fittedtree%leftnode%splitvarnum /= 1) .or. &
        (fittedtree%leftnode%splitvalue /= 0.05_dp) .or. &
        (fittedtree%leftnode%has_subnodes .neqv. .true.) .or. &
        (fittedtree%leftnode%majority /= 1)) then
        call rexit("Fitted tree's left subnode does not have the expected attributes.")
    endif

    if((fittedtree%leftnode%leftnode%depth /= 2) .or. &
        (fittedtree%leftnode%leftnode%has_subnodes .neqv. .false.) .or. &
        (fittedtree%leftnode%leftnode%majority /= 0)) then
        call rexit("Fitted tree's left subnode's left subnode does not have the expected attributes.")
    endif

    if((fittedtree%leftnode%rightnode%depth /= 2) .or. &
        (fittedtree%leftnode%rightnode%has_subnodes .neqv. .false.) .or. &
        (fittedtree%leftnode%rightnode%majority /= 1)) then
        call rexit("Fitted tree's left subnode's right subnode does not have the expected attributes.")
    endif

    if((fittedtree%rightnode%depth /= 1) .or. &
        (fittedtree%rightnode%has_subnodes .neqv. .false.) .or. &
        (fittedtree%rightnode%majority /= 1)) then
        call rexit("Fitted tree's right subnode does not have the expected attributes.")
    endif


    ! - test that subnodes refer correctly to parent nodes -
    ! check that fittedtree's left subnode's parentnode points to fittedtree
    if( (fittedtree%leftnode%parentnode%depth        /= fittedtree%depth) .or. &
        (fittedtree%leftnode%parentnode%majority     /= fittedtree%majority) .or. &
        (fittedtree%leftnode%parentnode%has_subnodes .neqv. fittedtree%has_subnodes) .or. &
        (fittedtree%leftnode%parentnode%splitvarnum  /= fittedtree%splitvarnum) .or. &
        (fittedtree%leftnode%parentnode%splitvalue   /= fittedtree%splitvalue) ) then

        call rexit("Test failed: fittedtree's left subnode's parentnode attributes do not match those of fittedtree.")
    endif

    ! check that fittedtree's right subnode's parentnode points to fittedtree
    if( (fittedtree%rightnode%parentnode%depth        /= fittedtree%depth) .or. &
        (fittedtree%rightnode%parentnode%majority     /= fittedtree%majority) .or. &
        (fittedtree%rightnode%parentnode%has_subnodes .neqv. fittedtree%has_subnodes) .or. &
        (fittedtree%rightnode%parentnode%splitvarnum  /= fittedtree%splitvarnum) .or. &
        (fittedtree%rightnode%parentnode%splitvalue   /= fittedtree%splitvalue) ) then
        call rexit("Test failed: fittedtree's right subnode's parentnode attributes do not match those of fittedtree.")
    endif

    ! check that fittedtree's left subnode's left subnode's parentnode points to fittedtree's left subnode
    if( (fittedtree%leftnode%leftnode%parentnode%depth        /= fittedtree%leftnode%depth) .or. &
        (fittedtree%leftnode%leftnode%parentnode%majority     /= fittedtree%leftnode%majority) .or. &
        (fittedtree%leftnode%leftnode%parentnode%has_subnodes .neqv. fittedtree%leftnode%has_subnodes) .or. &
        (fittedtree%leftnode%leftnode%parentnode%splitvarnum  /= fittedtree%leftnode%splitvarnum) .or. &
        (fittedtree%leftnode%leftnode%parentnode%splitvalue   /= fittedtree%leftnode%splitvalue) ) then

        call rexit("Test failed: fittedtree's left subnode's left subnode's &
            &parentnode attributes do not match those of fittedtree's left subnode.")
    endif

    ! check that fittedtree's left subnode's right subnode's parentnode points to fittedtree's left subnode
    if( (fittedtree%leftnode%rightnode%parentnode%depth        /= fittedtree%leftnode%depth) .or. &
        (fittedtree%leftnode%rightnode%parentnode%majority     /= fittedtree%leftnode%majority) .or. &
        (fittedtree%leftnode%rightnode%parentnode%has_subnodes .neqv. fittedtree%leftnode%has_subnodes) .or. &
        (fittedtree%leftnode%rightnode%parentnode%splitvarnum  /= fittedtree%leftnode%splitvarnum) .or. &
        (fittedtree%leftnode%rightnode%parentnode%splitvalue   /= fittedtree%leftnode%splitvalue) ) then
        call rexit("Test failed: fittedtree's left subnode's right subnode's &
            &parentnode attributes do not match those of fittedtree's left subnode.")
    endif

    ! check that the tags are in the order of expected tree trasversal
    if (    (fittedtree%tag /= 0) .or. &
            (fittedtree%leftnode%tag /= 1) .or. &
            (fittedtree%leftnode%leftnode%tag /= 2) .or. &
            (fittedtree%leftnode%rightnode%tag /= 3) .or. &
            (fittedtree%rightnode%tag /= 4) ) then
        call rexit("Test failed: check tags.")
    endif



    ! -----  Use the fitted tree to predict on X, and check that Yhat = Y  -----
    Yhat = predict(fittedtree, X)
    
    ! check failure condition
    if(any(Y/=Yhat)) call rexit("Predicted values not same as actual values.")


    ! -----  Use the fitted tree to predict on a new set of data, and  -----
    ! -----  check that the predicted results are as expected -----

    ! construct new data
    Nnew = 9
    allocate(Xnew(Nnew,P))
    allocate(Ynew(Nnew))
    allocate(Ynewhat(Nnew))
    
    Xnew = transpose(reshape( (/ &
    real(6, dp)/real(100 ,dp),  real(3, dp)/real(100, dp)  , &
    real(5, dp)/real(100, dp),  real(2, dp)/real(100, dp)  , &
    real(5, dp)/real(100, dp),  real(5, dp)/real(100, dp)  , &
    real(1, dp)/real(100, dp),  real(3, dp)/real(100, dp)  , &
    real(9, dp)/real(100, dp),  real(4, dp)/real(100, dp)  , &
    real(5, dp)/real(100, dp),  real(-1000, dp)            , &
    real(5, dp)/real(100, dp),  real( 1000, dp)            , &
    real(-1000, dp)          ,  real(4, dp)/real(100, dp)  , &
    real( 1000, dp)          ,  real(3, dp)/real(100, dp)    &
    /) , (/size(Xnew,2),size(Xnew,1)/)))

    Ynew = (/1, 0, 1, 0, 1, 0, 1, 1, 1/)

    ! fit new data
    Ynewhat = predict(fittedtree, Xnew)

    ! check failure condition
    if(any(Ynew/=Ynewhat)) then
        if(verbose) then
            print *, "Test failed. Program stop upcomming..."

            print *, "Xnew = "
            do i=1,Nnew
                print '(2f12.3)', Xnew(i,1), Xnew(i,2)
            enddo

            print *, "Ynewhat Ynew"
            do i=1,Nnew
                print '(i8,i5)', Ynewhat(i), Ynew(i)
            enddo
        endif
        
        call rexit("Predicted values not same as actual values.")
    endif


    exitflag = 0

end function


function test_grow_01() result(exitflag)
    ! test homogeneous dependent data base case for grow

    integer, parameter :: N=10, P=1
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node), pointer :: fittedtree

    integer :: exitflag

    logical, parameter :: verbose = .false.

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_01 -------------------"
    endif

    exitflag = -1
    
    ! set up test
    min_node_obs = 1
    max_depth = 100

    Y = (/1,1,1,1,1,1,1,1,1,1/)
    X(:,1) = (/1,2,3,4,5,6,7,8,9,10/)

    fittedtree => grow(Y, X, min_node_obs, max_depth)


    ! test for failure conditions
    if(fittedtree%has_subnodes .neqv. .false.) then
        call rexit("Test failure. Y homogeneous but still split.")
    endif


    exitflag = 0

end function

function test_grow_02() result(exitflag)
    ! test homogeneous independent data base case for grow

    integer, parameter :: N=10, P=1
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node), pointer :: fittedtree

    logical, parameter :: verbose = .false.

    integer :: exitflag

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_02 -------------------"
    endif

    exitflag = -1
    
    ! set up test
    min_node_obs = 1
    max_depth = 100

    Y = (/1,1,1,1,1,0,0,0,0,0/)
    X(:,1) = (/1,1,1,1,1,1,1,1,1,1/)

    fittedtree => grow(Y, X, min_node_obs, max_depth)


    ! test for failure conditions
    if(fittedtree%has_subnodes .neqv. .false.) then
        call rexit("Test failure. X homogeneous but still split.")
    endif


    exitflag = 0

end function

function test_grow_03() result(exitflag)
    ! test max depth base case for grow

    integer, parameter :: N=10, P=1
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node), pointer :: fittedtree

    integer :: exitflag

    logical, parameter :: verbose = .false.

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_03 -------------------"
    endif

    exitflag = -1
    
    ! set up test
    min_node_obs = 1
    max_depth = 0

    Y = (/1,1,1,1,1,0,0,0,0,0/)
    X(:,1) = (/0.1_dp,0.2_dp,0.3_dp,0.4_dp,0.5_dp,0.6_dp,0.7_dp,0.8_dp,0.9_dp,1.0_dp/)

    fittedtree => grow(Y, X, min_node_obs, max_depth)


    ! test for failure conditions
    if(fittedtree%has_subnodes .neqv. .false.) then
        call rexit("Test failure. Max depth is zero (root only), but still split.")
    endif


    exitflag = 0

end function


function test_grow_04() result(exitflag)
    ! test min node obs base case for grow

    integer, parameter :: N=10, P=1
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node), pointer :: fittedtree

    integer :: exitflag

    logical, parameter :: verbose = .false.

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_04 -------------------"
    endif

    exitflag = -1
    
    ! set up test
    min_node_obs = 10
    max_depth = 100

    Y = (/1,1,1,1,1,0,0,0,0,0/)
    X(:,1) = (/0.1_dp,0.2_dp,0.3_dp,0.4_dp,0.5_dp,0.6_dp,0.7_dp,0.8_dp,0.9_dp,1.0_dp/)

    fittedtree => grow(Y, X, min_node_obs, max_depth)


    ! test for failure conditions
    if(fittedtree%has_subnodes .neqv. .false.) then
        call rexit("Test failure. Min node size is equal to data root node size, but still splits.")
    endif


    exitflag = 0

end function


function test_grow_05() result(exitflag)
    ! test not splittable case for grow

    integer, parameter :: N=10, P=1
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node), pointer :: fittedtree

    integer :: exitflag

    logical, parameter :: verbose = .false.

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_05 -------------------"
    endif

    exitflag = -1
    
    ! set up test
    min_node_obs = 1
    max_depth = 100

    Y = (/1,1,1,1,1,0,0,0,0,0/)
    X(:,1) = (/0.1_dp,0.2_dp,0.3_dp,0.4_dp,0.5_dp,0.6_dp,0.7_dp,0.8_dp,0.9_dp,1.0_dp/)

    fittedtree => grow(Y, X, min_node_obs, max_depth, opt_splittable=(/.false./))


    ! test for failure conditions
    if(fittedtree%has_subnodes) then
        call rexit("Test failure. No variables are indicated as splittable, but still splits.")
    endif


    exitflag = 0

end function

end module classification
