module classification
!------------------------------------------------------------------------------
!   Module for fitting and estimating a classification tree.
!   Author: Bertram Ieong
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

function grow(Y, X, min_node_obs, max_depth) result(fittedtree)
    ! variable declarations
    integer, intent(in) :: Y(:)
    real(dp), intent(in) :: X(:,:)
    integer, intent(in) :: min_node_obs, max_depth
    type (node) :: fittedtree

    integer :: N, P

    integer, parameter :: TOP_NODE_NUM = 0

    ! find out number of variables and number of observations
    N = size(X,1)
    P = size(X,2)

    if(N /= size(Y)) stop "Y has different number of obs than X"

    
    ! fit decision tree classifier
    fittedtree = splitnode(Y, X, P, N, &
    min_node_obs, max_depth, &
    TOP_NODE_NUM, .true.)


end function

function predict(fittedtree, X) result(Ypred)
    ! variable declarations
    type (node), intent(in) :: fittedtree
    real(dp), intent(in) :: X(:,:)
    real(dp), allocatable :: Ypred(:)

    integer :: N, P
    integer :: obs

    ! find out number of variables and number of observations
    N = size(X,1)
    P = size(X,2)

    ! allocate array storing predictions
    allocate(Ypred(N))

    ! for each observation, go through fitted tree to get prediction
    do obs = 1,N
        ! TODO: might be possible to rewrite below to make column-wise so
        ! it is more efficient
        Ypred(obs) = predict_rec_hlpr(fittedtree, X(obs,:), P)
    enddo

end function


!-----  PRIVATE FUNCTIONS AND SUBROUTINES  -----


recursive function predict_rec_hlpr(t, X_row, P) result(pred)
    ! variable declarations
    type (node), intent(in) :: t
    real(dp), intent(in) :: X_row(:)
    integer, intent(in) :: P
    real(dp) :: pred

    ! argument checks
    if(size(X_row) /= P) stop "Error: Length of X_row is not P."

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
        do while (j>=1 .and. arr(j)>elem)
            arr(j+1) = arr(j)
            arrcarry(j+1) = arrcarry(j)
            j = j - 1
        enddo
        arr(j+1) = elem
        arrcarry(j+1) = elemcarry
    enddo

end subroutine


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

! TODO
function loss(impurity_this, impurity_left, impurity_right, prob_left) result(loss_val)
    real(dp), intent(in) :: impurity_this, impurity_left, impurity_right, prob_left
    real(dp) :: loss_val

    if((prob_left<0) .or. (prob_left>1)) stop "Probability not between 0 and 1."

    loss_val = prob_left*impurity_left + (1-prob_left)*impurity_right - impurity_this
end function


recursive function splitnode(Y, X, P, N, &
    min_node_obs, max_depth, &
    thisdepth, build_tree, &
    parentnode, opt_impurity_this, opt_tag) &
    result(thisnode)

    ! variable declarations
    real(dp), intent(in) :: X(N,P) 
    integer, intent(in) :: Y(N)
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer, intent(in) :: P, N
    integer, intent(in) :: min_node_obs, max_depth, thisdepth
    logical, optional :: build_tree ! TODO: fix no opt
    type (node), target, optional :: parentnode ! TODO: fix no opt
    real(dp), optional, intent(in) :: opt_impurity_this
    character(len=50), optional, intent(in) :: opt_tag  ! TODO: implement better way to track this

    character(len=50) :: tag 
    character(len=50) :: tagleft, tagright
    character(len=6), parameter :: tagleftstr=" LEFT", tagrightstr=" RIGHT"

    real(dp) :: impurity_this

    type (node), pointer :: thisnode
    
    integer :: varnum, rownum
    integer :: bestsplit_varnum, bestsplit_rownum
    real(dp) :: bestsplit_impurity_left, bestsplit_impurity_right, bestsplit_loss_val
    real(dp) :: impurity_left, impurity_right, loss_val
    logical :: first_split_computed
    logical :: base1, base2, base3
    integer :: num1s
    logical :: Xi_homog
    real(dp), allocatable :: Xleft(:,:), Xright(:,:)
    integer, allocatable :: Yleft(:), Yright(:)
    integer :: i,j,k

    logical, parameter :: verbose = .false.
    logical, parameter :: debug01 = .false.
    character(len=50) :: fmt ! TODO: delete this when no longer needed for debugging

    if(present(opt_tag)) then
        tag = opt_tag
    else
        tag = "NO TAG"
    endif

    if(verbose) then
        print *, "==============================================================================="
        print *, "Recursive function SPLITNODE invoked..."
        print '("P=", i5, "; min_node_obs=", i5, "; max_depth=", i5, "; build_tree=", l5)', P, min_node_obs, max_depth, build_tree
        print '("N=", i5, "; thisdepth=", i5)', N, thisdepth
        print '("tag=",(A))', tag
    endif

    if(verbose) then
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
        call insertion_sort(N, sortedX(:,j), sortedYcorresp(:,j), .false.)
    enddo




    allocate(thisnode)

    if(.not. present(build_tree)) build_tree = .false.

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



    if(verbose) then
        print *, "Pass base cases to grow?"
        print *, "!Min Node Size     !Max Depth    !Homogenous"
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

        
        do varnum = 1,P
            Xi_homog = .true.
            do rownum = 1,N

                ! skip to the next row if it has the same value for this variable as
                ! this row, unless this is the last row that can be split on
                

                if(rownum < N) then
                    if(sortedX(rownum,varnum) == sortedX(rownum+1,varnum)) then
                        cycle
                    else
                        Xi_homog = .false.
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
                    bestsplit_varnum = 1
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

        if(verbose) then
            if(Xi_homog) then
                print *, "Xi homogeneous. Subnodes will not be attached."
            endif
        endif

        if(.not. Xi_homog) then
            ! set the splitting variable and splitting value for this node
            thisnode%splitvarnum = bestsplit_varnum
            thisnode%splitvalue  = sortedX(bestsplit_rownum, bestsplit_varnum)
        endif

        if(build_tree .and. (.not. Xi_homog)) then          
            ! create subnodes and attach
            thisnode%has_subnodes = .true.

            ! create left and right data to be passed to the subnodes
            ! TODO: check if memory will build up in the stack like this

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
            tagleft = trim(tag)//tagleftstr

            allocate(thisnode%leftnode)

            thisnode%leftnode = splitnode( &
                Yleft, Xleft, &
                P, bestsplit_rownum, &
                min_node_obs, max_depth, &
                thisdepth+1, .true., thisnode, bestsplit_impurity_left, tagleft)

            ! construct and attach right node
            tagright = trim(tag)//tagrightstr

            allocate(thisnode%rightnode)

            thisnode%rightnode = splitnode( &
                Yright, Xright, &
                P, N-bestsplit_rownum, &
                min_node_obs, max_depth, &
                thisdepth+1, .true., thisnode, bestsplit_impurity_right, tagright)
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
    type (node) :: thisnode
    integer :: exitflag

    logical, parameter :: verbose = .false.

    exitflag = -1

    print *, " "
    print *, "---------- Running Test Function test_splitnode_01 -------------------"

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
    print *, "---------- Running Test Function test_splitnode_02 -------------------"

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
    print *, "---------- Running Test Function test_splitnode_03 -------------------"

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
    print *, "---------- Running Test Function test_splitnode_04 -------------------"

    ! test failure conditions
    if(thisnode%has_subnodes .eqv. .true.) &
        stop "Test failed: Node at max depth but was split."

    print *, "Test successful if test executed without error."

    exitflag = 0
end function


function test_splitnode_05() result(exitflag)
    ! test that the homogeneous node base case works

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
    print *, "---------- Running Test Function test_splitnode_05 -------------------"

    ! test failure conditions
    if(thisnode0%has_subnodes .eqv. .true.) &
        stop "Test failed: Homogenous node but was split."
    if(thisnode1%has_subnodes .eqv. .true.) &
        stop "Test failed: Homogenous node but was split."

    print *, "Test successful if test executed without error."

    exitflag = 0
end function

function test_splitnode_06() result(exitflag)
    ! test that splitnode correctly points to its parent node and
    ! to its left and right subnodes

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)

    integer :: exitflag

    type (node) :: node1, node2

    character(len=50) :: fmt

    logical :: verbose = .false.

    print *, " "
    print *, "---------- Running Test Function test_splitnode_06 -------------------"

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
    node2 = splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .true., node1)

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





function test_grow_01() result(exitflag)
    ! tests the function that grows a decision tree using a simple example

    ! variable declarations
    integer, parameter :: sqrtN=10, P=2
    integer, parameter :: N = sqrtN**2
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer :: min_node_obs, max_depth
    type (node) :: fittedtree
    integer :: exitflag

    logical, parameter :: verbose = .false.
    character(len=50) :: fmt

    integer :: i,j,ctr

    exitflag = -1

    print *, " "
    print *, "--------- Running Test Function test_grow_01 ------------------"

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
    fittedtree = grow(Y, X, min_node_obs, max_depth)


    if(verbose) then
        fmt = '(i6, i11, l15, i16, f14.3)'

        print *, ""
        print *, "For fitted tree:"
        print *, "---------------+--------+----------+--------------+---------------+------------"
        print *, "Node           |  Depth | Majority | Has Subnodes | Split Var Num | Split Value"
        print *, "---------------+--------+----------+--------------+---------------+------------"

        write (*,'(A)',advance="no") "parent          | "
        print fmt, fittedtree%parentnode%depth, fittedtree%parentnode%majority, fittedtree%parentnode%has_subnodes, &
            fittedtree%parentnode%splitvarnum, fittedtree%parentnode%splitvalue
        write (*,'(A)',advance="no") "-TOP (THIS)     | "
        print fmt, fittedtree%depth, fittedtree%majority, fittedtree%has_subnodes, &
            fittedtree%splitvarnum, fittedtree%splitvalue
        write (*,'(A)',advance="no") "--left          | "
        print fmt, fittedtree%leftnode%depth, fittedtree%leftnode%majority, fittedtree%leftnode%has_subnodes, &
            fittedtree%leftnode%splitvarnum, fittedtree%leftnode%splitvalue
        write (*,'(A)',advance="no") "---left's left  | "
        print fmt, fittedtree%leftnode%leftnode%depth, fittedtree%leftnode%leftnode%majority, &
            fittedtree%leftnode%leftnode%has_subnodes, &
            fittedtree%leftnode%leftnode%splitvarnum, fittedtree%leftnode%leftnode%splitvalue
        write (*,'(A)',advance="no") "---left's right | "
        print fmt, fittedtree%leftnode%rightnode%depth, fittedtree%leftnode%rightnode%majority, &
            fittedtree%leftnode%rightnode%has_subnodes, &
            fittedtree%leftnode%rightnode%splitvarnum, fittedtree%leftnode%rightnode%splitvalue
        write (*,'(A)',advance="no") "--right         | "
        print fmt, fittedtree%rightnode%depth, fittedtree%rightnode%majority, fittedtree%rightnode%has_subnodes, &
            fittedtree%rightnode%splitvarnum, fittedtree%rightnode%splitvalue

        print *, "---------------+--------+----------+--------------+---------------+------------"
    endif




    ! -- test for failure conditions --
    ! - test that expected values of nodes are realized -
    if((fittedtree%depth /= 0) .or. &
        (fittedtree%splitvarnum /= 2) .or. &
        (fittedtree%splitvalue /= 0.03_dp) .or. &
        (fittedtree%has_subnodes .neqv. .true.) .or. &
        (fittedtree%majority /= 1)) then
        stop "Fitted tree does not have the expected attributes."
    endif

    if((fittedtree%leftnode%depth /= 1) .or. &
        (fittedtree%leftnode%splitvarnum /= 1) .or. &
        (fittedtree%leftnode%splitvalue /= 0.05_dp) .or. &
        (fittedtree%leftnode%has_subnodes .neqv. .true.) .or. &
        (fittedtree%leftnode%majority /= 1)) then
        stop "Fitted tree's left subnode does not have the expected attributes."
    endif

    if((fittedtree%leftnode%leftnode%depth /= 2) .or. &
        (fittedtree%leftnode%leftnode%has_subnodes .neqv. .false.) .or. &
        (fittedtree%leftnode%leftnode%majority /= 0)) then
        stop "Fitted tree's left subnode's left subnode does not have the expected attributes."
    endif

    if((fittedtree%leftnode%rightnode%depth /= 2) .or. &
        (fittedtree%leftnode%rightnode%has_subnodes .neqv. .false.) .or. &
        (fittedtree%leftnode%rightnode%majority /= 1)) then
        stop "Fitted tree's left subnode's right subnode does not have the expected attributes."
    endif

    if((fittedtree%rightnode%depth /= 1) .or. &
        (fittedtree%rightnode%has_subnodes .neqv. .false.) .or. &
        (fittedtree%rightnode%majority /= 1)) then
        stop "Fitted tree's right subnode does not have the expected attributes."
    endif


    ! - test that subnodes refer correctly to parent nodes -
    ! check that fittedtree's left subnode's parentnode points to fittedtree
    if( (fittedtree%leftnode%parentnode%depth        /= fittedtree%depth) .or. &
        (fittedtree%leftnode%parentnode%majority     /= fittedtree%majority) .or. &
        (fittedtree%leftnode%parentnode%has_subnodes .neqv. fittedtree%has_subnodes) .or. &
        (fittedtree%leftnode%parentnode%splitvarnum  /= fittedtree%splitvarnum) .or. &
        (fittedtree%leftnode%parentnode%splitvalue   /= fittedtree%splitvalue) ) then

        stop "Test failed: fittedtree's left subnode's parentnode attributes do not match those of fittedtree."
    endif

    ! check that fittedtree's right subnode's parentnode points to fittedtree
    if( (fittedtree%rightnode%parentnode%depth        /= fittedtree%depth) .or. &
        (fittedtree%rightnode%parentnode%majority     /= fittedtree%majority) .or. &
        (fittedtree%rightnode%parentnode%has_subnodes .neqv. fittedtree%has_subnodes) .or. &
        (fittedtree%rightnode%parentnode%splitvarnum  /= fittedtree%splitvarnum) .or. &
        (fittedtree%rightnode%parentnode%splitvalue   /= fittedtree%splitvalue) ) then
        stop "Test failed: fittedtree's right subnode's parentnode attributes do not match those of fittedtree."
    endif

    ! check that fittedtree's left subnode's left subnode's parentnode points to fittedtree's left subnode
    if( (fittedtree%leftnode%leftnode%parentnode%depth        /= fittedtree%leftnode%depth) .or. &
        (fittedtree%leftnode%leftnode%parentnode%majority     /= fittedtree%leftnode%majority) .or. &
        (fittedtree%leftnode%leftnode%parentnode%has_subnodes .neqv. fittedtree%leftnode%has_subnodes) .or. &
        (fittedtree%leftnode%leftnode%parentnode%splitvarnum  /= fittedtree%leftnode%splitvarnum) .or. &
        (fittedtree%leftnode%leftnode%parentnode%splitvalue   /= fittedtree%leftnode%splitvalue) ) then

        stop "Test failed: fittedtree's left subnode's left subnode's &
            &parentnode attributes do not match those of fittedtree's left subnode."
    endif

    ! check that fittedtree's left subnode's right subnode's parentnode points to fittedtree's left subnode
    if( (fittedtree%leftnode%rightnode%parentnode%depth        /= fittedtree%leftnode%depth) .or. &
        (fittedtree%leftnode%rightnode%parentnode%majority     /= fittedtree%leftnode%majority) .or. &
        (fittedtree%leftnode%rightnode%parentnode%has_subnodes .neqv. fittedtree%leftnode%has_subnodes) .or. &
        (fittedtree%leftnode%rightnode%parentnode%splitvarnum  /= fittedtree%leftnode%splitvarnum) .or. &
        (fittedtree%leftnode%rightnode%parentnode%splitvalue   /= fittedtree%leftnode%splitvalue) ) then
        stop "Test failed: fittedtree's left subnode's right subnode's &
            &parentnode attributes do not match those of fittedtree's left subnode."
    endif

    exitflag = 0

    print *, ""
    print *, "Test successful if test executed without error."

    ! TODO: now add some impurities to the data
end function


! TODO: then test the stopping conditions for grow


end module classification