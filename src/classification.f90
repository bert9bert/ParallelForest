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


function splitnode(sortedYcorresp, sortedX, P, N, &
    min_node_obs, max_depth, &
    thisdepth, build_tree) &
    result(thisnode)

    real(dp), intent(in) :: sortedX(N,P) 
    integer, intent(in) :: sortedYcorresp(N,P)
    integer, intent(in) :: P, N
    integer, intent(in) :: min_node_obs, max_depth, thisdepth
    logical, optional :: build_tree
    type (node) :: thisnode
    integer :: varnum, rownum
    integer :: bestsplit_varnum, bestsplit_rownum
    real(dp) :: bestsplit_loss_val
    real(dp) :: impurity_left, impurity_right, loss_val
    logical :: first_split_computed
    logical :: base1, base2, base3
    integer :: num1s

    logical, parameter :: debug01 = .false.

    if(.not. present(build_tree)) build_tree = .false.

    num1s = sum(sortedYcorresp(:,1))

    ! add pointer to parent node
    ! ...

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
        ! ...

    else
        ! otherwise, this is a terminal node
        thisnode%has_subnodes = .false.

    endif


    
end function


!-----  TESTING AND DEBUGGING FUNCTIONS AND SUBROUTINES  -----
function test_splitnode_01() result(exitflag)
    ! test the ability of the splitnode function to split a node ONCE
    ! on a dataset with just one variable

    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    type (node) :: thisnode
    integer :: exitflag

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
    print *, "---------- Test Function test_splitnode_01 -------------------"
    print '("Fitted Var to Split   = ", i5,   ";       Correct Split Val = ", i5)', &
        thisnode%splitvarnum, bestsplit_varnum_correct
    print '("Fitted Value to Split = ", f5.3, ";  Correct Value to Split = ", f5.3)', &
        thisnode%splitvalue, bestsplit_value_correct

    ! test failure conditions
    if(thisnode%splitvarnum /= bestsplit_varnum_correct) &
        stop "Test failed: Wrong splitting variable"
    if(thisnode%splitvalue /= bestsplit_value_correct) &
        stop "Test failed: Wrong value to split variable at"

    exitflag = 0
end function

function test_splitnode_02() result(exitflag)
    integer :: exitflag

    exitflag = -1

    ! ...

    exitflag = 0
end function

end module classification

