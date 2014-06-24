module forest_parallel
!------------------------------------------------------------------------------
!   Module for fitting and predicting on a random forest classifier using
!       parallel computing.
!   Author: Bertram Ieong
!------------------------------------------------------------------------------

use random_utils
use utils
use tree_utils
use sort_utils
use impurity_measures
use classification

implicit none


contains


subroutine bootstrap(Y, X, numsamps, Y_boot, X_boot)
    ! Returns a bootstrapped sample of the data.

    ! --- Declare Variables ---
    ! --- Input/Output Variables ---
    integer, intent(in) :: Y(:)
    real(dp), intent(in) :: X(:,:)
    integer, intent(in) :: numsamps
    integer, allocatable, intent(out) :: Y_boot(:)
    real(dp), allocatable, intent(out) :: X_boot(:,:)

    ! Private Variables
    real(dp) :: randarr(numsamps)
    integer :: rand_obs_num(numsamps)
    integer :: N, P
    integer :: i

    ! Debugging variables
    character(len=50) :: fmt
    logical, parameter :: verbose = .false.


    ! --- setup ---
    ! Get data size
    N = size(X,1)
    P = size(X,2)

    if(N /= size(Y)) stop "Data dimensions do not match."

    ! allocate memory to allocatable arrays
    allocate(Y_boot(numsamps))
    allocate(X_boot(numsamps,P))


    ! --- create bootstrapped sample ---
    call init_random_seed()
    call random_number(randarr)

    rand_obs_num = 1 + int(randarr * ((N - 1) + 1))  ! get array of random integers in [1,N]

    do i=1,numsamps
        Y_boot(i) = Y(rand_obs_num(i))
        X_boot(i,:) = X(rand_obs_num(i),:)
    enddo

    if(verbose .and. P==1) then
        print *, "   Y         X    Y_boot    X_boot"

        fmt = '(i5, f10.5, i10, f10.5)'
        do i=1,N
            if(i<=numsamps) then
                print fmt, Y(i), X(i,1), Y_boot(i), X_boot(i,1)
            else
                print fmt, Y(i), X(i,1)
            endif
        enddo
    else if(verbose .and. P==2) then
        print *, "  Y1        X1        X2   Y1_boot   X1_boot   X2_boot"

        fmt = '(i5, f10.5, f10.5, i10, f10.5, f10.5)'
        do i=1,N
            if(i<=numsamps) then
                print fmt, Y(i), X(i,1), X(i,2), Y_boot(i), X_boot(i,1), X_boot(i,2)
            else
                print fmt, Y(i), X(i,1), X(i,2)
            endif
        enddo
    endif

    if(verbose .and. (P==1 .or. P==2)) then
        print *, "Note: Above table only prints up to row N. If numsamps>N, then some rows will not be shown."
    endif

end subroutine



function grow_forest(Y, X, min_node_obs, max_depth, numsamps, numvars, numboots) result(fittedforest)
    ! --- Declare Variables ---
    ! --- Input/Output Variables ---
    integer, intent(in) :: Y(:)
    real(dp), intent(in) :: X(:,:)
    integer, intent(in) :: min_node_obs, max_depth
    integer, intent(in) :: numsamps, numvars, numboots
    type (node) :: fittedforest(numboots)

    ! Private Variables
    integer :: N, P
    integer :: treenum

    integer, allocatable :: Y_boot(:)
    real(dp), allocatable :: X_boot(:,:)
    logical, allocatable :: variables_selected(:)
    integer :: variables_selected_nums(numvars)
    real(dp), allocatable :: randarr(:)

    integer :: i, j, idx


    ! --- setup ---
    ! Get data size
    N = size(X,1)
    P = size(X,2)

    ! input checks
    if(N /= size(Y))    stop "Data dimensions do not match."
    if(min_node_obs<=0) stop
    if(max_depth<0)     stop
    if(numsamps<=0)     stop
    if(numvars<=0)      stop
    if(numboots<=0)     stop

    ! allocate memory to allocatable arrays
    allocate(X_boot(numsamps,P))
    allocate(variables_selected(P))
    allocate(randarr(P))


    ! --- grow forest ---
    do treenum=1,numboots
        ! -- randomly select variables for this tree without replacement --
        ! draw indices of variables without replacement that can be split on
        call init_random_seed()  ! TODO: make this random
        call random_number(randarr)

        do i=1,numvars
            idx = minloc(randarr, dim=1)
            variables_selected_nums(i) = idx
            randarr(idx) = huge(1_dp)
        enddo

        ! turn this list of indices into a logical array
        variables_selected = .false.
        do j=1,P
            variables_selected(variables_selected_nums(j)) = .true.
        enddo

        ! -- create bootstrapped data --
        call bootstrap(Y, X, numsamps, Y_boot, X_boot)

        ! -- fit tree to the bootstrapped data and select variables --
        fittedforest(treenum) = grow(Y_boot, X_boot, min_node_obs, max_depth, &
            opt_splittable=variables_selected)
    enddo

end function



function test_bootstrap_01() result(exitflag)
    integer :: exitflag

    integer, parameter :: N=10, P=2
    integer, parameter :: numsamps = 5
    integer :: Y(N)
    integer, allocatable :: Y_boot1(:), Y_boot2(:), Y_boot3(:)
    real(dp) :: X(N,P)
    real(dp), allocatable :: X_boot1(:,:), X_boot2(:,:), X_boot3(:,:)

    integer :: i,j

    exitflag = -1

    print *, " "
    print *, "---------- Running Test Function test_bootstrap_01 -------------------"

    ! --- create bootstrap sample on pre-defined data ---
    Y = (/0,1,2,3,4,5,6,7,8,9/)
    X(:,1) = (/10,11,12,13,14,15,16,17,18,19/)
    X(:,2) = (/20,21,22,23,24,25,26,27,28,29/)

    call bootstrap(Y, X, numsamps, Y_boot1, X_boot1)
    call bootstrap(Y, X, numsamps, Y_boot2, X_boot2)
    call bootstrap(Y, X, numsamps, Y_boot3, X_boot3)


    ! --- test failure conditions ---
    ! test that bootstrapped data has correct dimensions
    if(.not. size(Y_boot1)==numsamps) stop "Test failed."
    if(.not. size(Y_boot2)==numsamps) stop "Test failed."
    if(.not. size(Y_boot3)==numsamps) stop "Test failed."

    if(.not. all((/size(X_boot1,1),size(X_boot1,2)/)==(/numsamps,P/))) stop "Test failed."
    if(.not. all((/size(X_boot2,1),size(X_boot2,2)/)==(/numsamps,P/))) stop "Test failed."
    if(.not. all((/size(X_boot3,1),size(X_boot3,2)/)==(/numsamps,P/))) stop "Test failed."

    ! check that values have been correctly carried
    do i=1,numsamps
        do j=1,P
            if(.not. all(X_boot1(:,j)==Y_boot1+(j*10_dp))) stop "Test failed."
            if(.not. all(X_boot2(:,j)==Y_boot2+(j*10_dp))) stop "Test failed."
            if(.not. all(X_boot3(:,j)==Y_boot3+(j*10_dp))) stop "Test failed."
        enddo
    enddo

    ! do rudimentary check for randomness
    if(all(Y_boot1==Y_boot2) .and. all(Y_boot2==Y_boot3)) stop "Test failed."


    print *, "Test successful if test executed without error."

    exitflag = 0

end function



end module forest_parallel
