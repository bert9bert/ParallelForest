module classification
!------------------------------------------------------------------------------
!   Module for fitting and estimating a classification tree.
!   Author: B.I.
!------------------------------------------------------------------------------

implicit none
integer, parameter :: dp = kind(0.d0)  ! double precision

! TODO: create a node type that stores for this node
! 1. pointer to the parent node
! 2. pointers to the two children nodes
! 3. constants to store variable, variable split, level

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
    real(dp) :: thisnode(3)  ! TODO: later change this to node type
    integer :: varnum, rownum
    integer :: bestsplit_varnum, bestsplit_rownum
    real(dp) :: bestsplit_loss_val
    real(dp) :: impurity_left, impurity_right, loss_val
    logical :: first_split_computed
    logical, parameter :: debug01 = .false.

    if(.not. present(build_tree)) build_tree = .false.


    if(debug01 .eqv. .true.) then
        print *, "------------------------"
        print *, "DEBUG: Inside splitnode."
    endif

    !----- Check input assertions -----
    ! check that sortedX is sorted
    ! ...
    ! check that sortedX and sortedYcorresp are N x P

    ! check base cases
    ! min node size
    ! max depth
    ! homgenous
    ! ...


    ! if tree growth can continue, then find split and recursively call this function
    ! to attach two subnodes to this node
    if(.true.) then ! TODO

        ! loop through all variables and possible splits of variables to find
        ! the node split (variable, split value, direction tuple) that
        ! minimizes the impurity function

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

            ! compute impurity that results from this Xi<=Sj,Xi>Sj split
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
            ! if tree growing stop condition has not been met
            if(first_split_computed .eqv. .false.) then
                bestsplit_varnum = 1
                bestsplit_rownum  = rownum
                bestsplit_loss_val = loss_val

                first_split_computed = .true.
                
                if(debug01 .eqv. .true.) then
                    print *, "DEBUG: Initial bests set"
                endif

            else if(loss_val < bestsplit_loss_val) then
                if(min(rownum,N-rownum)>min_node_obs) then
                        bestsplit_varnum = varnum
                        bestsplit_rownum = rownum
                        bestsplit_loss_val = loss_val
                endif

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

        ! create subnodes and attach
        ! ...

    else
        ! otherwise, this is the terminal node, so determine the prediction at this terminal node
        ! ...
    endif

    ! debugging/testing
    thisnode = (/ real(bestsplit_varnum, dp), real(bestsplit_rownum, dp), sortedX(bestsplit_rownum,bestsplit_varnum)/)
    
end function


!-----  TESTING AND DEBUGGING FUNCTIONS AND SUBROUTINES  -----
function test_splitnode_01() result(exitflag)
    integer, parameter :: N=10, P=1
    real(dp) :: sortedX(N,P) 
    integer :: sortedYcorresp(N,P)
    integer :: bestsplit_varnum_correct
    real(dp) :: bestsplit_value_correct
    real(dp) :: thisnode(3)  ! TODO: later change this to node type
    integer :: exitflag

    exitflag = -1

    sortedYcorresp = reshape((/1,1,1,1,1,1,1,0,0,0/), &
    	shape(sortedYcorresp))
    sortedX = reshape((/ 0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp, 0.5_dp, 0.6_dp, &
    	0.7_dp, 0.8_dp, 0.9_dp, 1.0_dp /), &
    	shape(sortedX))

    bestsplit_varnum_correct = 1
    bestsplit_value_correct = 0.7_dp

    ! debugging/testing: fix once splitnode outputs a node
    thisnode = splitnode(sortedYcorresp, sortedX, P, N, 2, 2, 1, .false.)

    print *, "---------- Test Function test_splitnode_01 -------------------"
    print *, "Fitted Var to Split = ", nint(thisnode(1)), &
        "Correct Split Val = ", bestsplit_varnum_correct
    print *, "Fitted Value to Split = ", thisnode(3), &
        "Correct Value to Split = ", bestsplit_value_correct

    if(nint(thisnode(1)) /= bestsplit_varnum_correct) &
        stop "Test failed: Wrong splitting variable"
    if(thisnode(3) /= bestsplit_value_correct) &
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

