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

    if(verbose) print *, "Entering bootstrap(...)"

    ! --- setup ---
    ! Get data size
    N = size(X,1)
    P = size(X,2)

    if(N /= size(Y)) stop "Data dimensions do not match."

    ! allocate memory to allocatable arrays
    allocate(Y_boot(numsamps))
    allocate(X_boot(numsamps,P))


    ! --- create bootstrapped sample ---
    ! init random number generator seed
    call init_random_seed()

    ! generate array of random numbers [0,1)
    call random_number(randarr)

    rand_obs_num = 1 + int(randarr * ((N - 1) + 1))  ! get array of random integers in [1,N]

    do i=1,numsamps
        Y_boot(i) = Y(rand_obs_num(i))
        X_boot(i,:) = X(rand_obs_num(i),:)
    enddo

    ! --- verbose prints ---
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

    if(verbose) print *, "Exiting bootstrap(...)"
end subroutine



function grow_forest(Y, X, min_node_obs, max_depth, &
    numsamps, numvars, numboots) &
    result(fittedforest)

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

    ! Debugging variables
    logical, parameter :: verbose = .false.


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
    allocate(variables_selected(P))
    allocate(randarr(P))

    ! --- grow forest ---
    do treenum=1,numboots
        if(verbose) then
            print *, "-----------------------------------------------"
            print '("Generating Tree (Bootstrap Sample) No. ", i5)', treenum
        endif

        ! -- randomly select variables for this tree without replacement --
        ! draw indices of variables without replacement that can be split on
        ! init random number generator seed
        call init_random_seed()

        ! generate array of random numbers [0,1)
        call random_number(randarr)

        do i=1,numvars
            idx = minloc(randarr, dim=1)
            variables_selected_nums(i) = idx
            randarr(idx) = huge(1_dp)
        enddo

        ! turn this list of indices into a logical array
        variables_selected = .false.
        do j=1,numvars
            variables_selected(variables_selected_nums(j)) = .true.
        enddo

        if(verbose) then
            print *, "Variables selected:"
            print *, variables_selected
        endif

        ! -- create bootstrapped data --
        call bootstrap(Y, X, numsamps, Y_boot, X_boot)


        if(verbose) then
            print '("Bootstrapped Y Size = ", i6)', size(Y_boot)
            print '("Bootstrapped X Size = (", i5, ",", i5, ")")', size(X_boot,1), size(X_boot,2)
        endif

        ! -- fit tree to the bootstrapped data and select variables --
        fittedforest(treenum) = grow(Y_boot, X_boot, min_node_obs, max_depth, &
            opt_splittable=variables_selected)

        deallocate(Y_boot)
        deallocate(X_boot)
    enddo
end function



function predict_forest(fittedforest, X) result(Ypred)
    ! --- Variable Declarations ---
    ! Input/Output variables
    type (node), intent(in) :: fittedforest(:)
    real(dp), intent(in) :: X(:,:)
    integer, allocatable :: Ypred(:)

    ! Private variables
    integer, allocatable :: Ypred_trees(:,:)
    integer :: N, P

    ! Counting variables
    integer :: i, j, num1

    ! Debugging variables
    logical, parameter :: verbose = .false.

    ! --- setup ---
    N = size(X,1)
    P = size(X,2)

    allocate(Ypred_trees(N,size(fittedforest)))
    allocate(Ypred(N))

    ! --- each tree makes prediction ---
    do j=1,size(fittedforest)
        Ypred_trees(:,j) = predict(fittedforest(j), X)
    enddo

    if(verbose) then
        print *, "Split info for each tree:"
        do j=1,size(fittedforest)
            print *, "Var split = ", fittedforest(j)%splitvarnum, &
                " Split value = ", fittedforest(j)%splitvalue, &
                " Has Subnodes = ", fittedforest(j)%has_subnodes
        enddo

        print *, "Prediction for each tree:"
        do i=1,size(Ypred_trees,1)
            print *, (Ypred_trees(i,j), j=1,size(Ypred_trees,2))
        enddo
    endif


    ! --- create ensemble prediction ---
    do i=1,N
        num1 = 0
        do j=1,size(fittedforest)
            if(Ypred_trees(i,j)==1) num1=num1+1
            if(Ypred_trees(i,j)==0) num1=num1-1
        enddo

        if(num1 >= 0) Ypred(i) = 1
        if(num1 <  0) Ypred(i) = 0
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


function test_grow_forest_01() result(exitflag)
    integer :: exitflag

    integer, parameter :: N=10, P=2
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer, parameter :: min_node_obs=1, max_depth=10
    integer, parameter :: numsamps = 5, numvars=1, numboots=3

    type (node) :: ff(numboots)

    integer :: i,j

    exitflag = -1

    print *, " "
    print *, "---------- Running Test Function test_grow_forest_01 -------------------"

    ! --- create pre-defined data and grow a forest ---
    Y = (/1,1,1,1,1,1,0,0,0,0/)
    X(:,1) = (/10,11,12,13,14,15,16,17,18,19/)
    X(:,2) = (/20,21,22,23,24,25,26,27,28,29/)

    ff = grow_forest(Y, X, min_node_obs, max_depth, &
        numsamps, numvars, numboots)

    ! --- test failure conditions, automated ---
    if(.not. size(ff)==numboots) stop "Test failed."

    ! --- test failure conditions, manual ---
    print *, "== Manual Testing Instructions =="
    print *, "Part of this test cannot be automated and must be done manually."
    print *, "To do so, set the verbose flag to .true. in grow_forest(...),"
    print *, "recompile the test framework, and check the following:"
    print *, "    1. The correct number of trees are generated"
    print *, "    2. The correct number of variables are selected"
    print *, "    3. There is some randomness in the variables selected"
    print *, "    4. The size of the bootstrapped Y and X are correct"

    print *, ""
    print *, "Test successful if test executed without error."

    exitflag = 0

end function



function test_grow_predict_forest_01() result(exitflag)
    integer :: exitflag

    integer, parameter :: N=10, P=2
    integer :: Y(N)
    real(dp) :: X(N,P)
    integer, allocatable :: Ynew(:), Ynewhat(:)
    real(dp), allocatable :: Xnew(:,:)
    integer, parameter :: min_node_obs=1, max_depth=10
    integer, parameter :: numsamps = 50, numvars=1, numboots=5

    integer :: Ysamehat(N)

    type (node) :: ff(numboots)

    integer :: i,j

    logical, parameter :: verbose = .false.

    exitflag = -1

    print *, " "
    print *, "---------- Running Test Function test_grow_predict_forest_01 -------------------"

    ! --- create pre-defined data and grow a forest ---
    Y      = (/ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0/)
    X(:,1) = (/10,11,12,13,14,15,16,17,18,19/)
    X(:,2) = (/20,21,22,23,24,25,26,27,28,29/)

    ff = grow_forest(Y, X, min_node_obs, max_depth, &
        numsamps, numvars, numboots)

    ! predict on the training set
    Ysamehat = predict_forest(ff, X)

    if(verbose) then
        print *, "Y same hat ="
        print '(i5)', Ysamehat
    endif

    ! predict on a new set of data with known solution
    allocate(Ynew(4))
    allocate(Ynewhat(4))
    allocate(Xnew(4,P))

    Ynew = (/1,1,0,0/)
    Xnew(:,1) = (/-100,14,15,100/)
    Xnew(:,2) = (/-100,24,25,100/)

    Ynewhat = predict_forest(ff, Xnew)

    if(verbose) then
        print *, "Y new hat ="
        print '(i5)', Ynewhat
    endif

    ! --- test failure conditions ---
    if(.not. all(Ysamehat==Y)) stop "Test failed."
    if (.not. all(Ynewhat==Ynew)) stop "Test failed."


    print *, ""
    print *, "Test successful if test executed without error."

    exitflag = 0

end function



end module forest_parallel
