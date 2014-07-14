!------------------------------------------------------------------------------
!   Module for fitting and predicting on a random forest classifier using
!       parallel computing.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------

module forest_parallel


use utils
use random_utils
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

    if(N /= size(Y)) call rexit("Data dimensions do not match.")

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



subroutine bootstrap_balanced(Y, X, in_numsamps, Y_boot, X_boot, opt_Yunique, opt_force_numsamps_evensplits)
    ! Returns a BALANCED bootstrapped sample of the data.

    ! --- Declare Variables ---
    ! --- Input/Output Variables ---
    integer, intent(in) :: Y(:)
    real(dp), intent(in) :: X(:,:)
    integer, intent(in) :: in_numsamps
    integer, allocatable, intent(out) :: Y_boot(:)
    real(dp), allocatable, intent(out) :: X_boot(:,:)
    integer, optional, intent(in) :: opt_Yunique(:)
    logical, optional, intent(in) :: opt_force_numsamps_evensplits

    ! Private Variables
    integer :: numsamps
    integer, allocatable :: Yunique(:)
    real(dp), allocatable :: randarr(:,:)
    integer, allocatable :: rand_obs_num(:,:)
    integer :: N, P, num_Yunique
    integer :: i, j, ctr
    real(dp), allocatable :: tmp_dbl_arr_1d_no1(:)
    integer, allocatable :: tmp_int_arr_1d_no1(:)
    integer, allocatable :: tmp_int_arr_1d_no2(:)

    integer, allocatable :: Yunique_counts(:)
    integer, allocatable :: Ydata_unique(:,:)
    real(dp), allocatable :: Xdata_unique(:,:,:)

    ! Debugging variables
    character(len=50) :: fmt
    logical, parameter :: verbose = .false.

    if(verbose) print *, "Entering bootstrap(...)"

    ! --- setup ---

    ! Get data size
    N = size(X,1)
    P = size(X,2)

    if(N /= size(Y)) call rexit("Data dimensions do not match.")


    ! determine the unique values of Y
    if(present(opt_Yunique)) then
        allocate(Yunique(size(opt_Yunique)))
        Yunique = opt_Yunique
        num_Yunique = size(Yunique)
    else
        allocate(tmp_int_arr_1d_no1(N))
        allocate(tmp_int_arr_1d_no2(N))  ! this is needed because of how this sort works, 
                                         ! may be able to discard when sort is replaced
                                         ! with another sort
        allocate(tmp_dbl_arr_1d_no1(N))  ! same comment as above

        tmp_int_arr_1d_no1 = Y
        tmp_dbl_arr_1d_no1 = real(tmp_int_arr_1d_no1, dp)
        call qsort_inline_dp_int(N, tmp_dbl_arr_1d_no1, tmp_int_arr_1d_no2)
        tmp_int_arr_1d_no1 = int(tmp_dbl_arr_1d_no1)  ! TODO: make sure rounding is OK


        num_Yunique = 1
        do i=2,N
            if(tmp_int_arr_1d_no1(i) /= tmp_int_arr_1d_no1(i-1)) then
                num_Yunique = num_Yunique + 1
            endif
        enddo

        deallocate(tmp_int_arr_1d_no1)
        deallocate(tmp_dbl_arr_1d_no1)
        deallocate(tmp_int_arr_1d_no2)

        allocate(Yunique(num_Yunique))
        Yunique(1) = Y(1)
        i = 2
        do j=2,N
            if(Y(j) /= Y(j-1)) then
                Yunique(i) = Y(j)
                i = i + 1
            endif
        enddo
    endif



    ! take care of uneven split input to numsamps

    numsamps = -1
    if(mod(in_numsamps,num_Yunique)/=0) then
        if(present(opt_force_numsamps_evensplits)) then
            if(opt_force_numsamps_evensplits) then
                numsamps = in_numsamps + mod(in_numsamps,num_Yunique)
                if(verbose) print *, "Requested num of samples not split even, forcing to split even number."
            else
                call rexit("ERROR")
            endif
        else
            call rexit("ERROR: Option to force even splits not supplied, so assumed false, but splits not even")
        endif
    else
        numsamps = in_numsamps
    endif



    ! allocate memory to allocatable arrays
    allocate(Y_boot(numsamps))
    allocate(X_boot(numsamps,P))


    ! create array to hold data for different Y values
    ! count how many of each
    allocate(Yunique_counts(num_Yunique))
    Yunique_counts = 0
    do i=1,num_Yunique
        do j=1,N
            if(Y(j) == Yunique(i)) then
                Yunique_counts(i) = Yunique_counts(i) + 1
            endif
        enddo
    enddo

    allocate(Ydata_unique(maxval(Yunique_counts), num_Yunique))
    allocate(Xdata_unique(maxval(Yunique_counts), P, num_Yunique))

    ! put into working arrays
    do i=1,num_Yunique
        ctr = 1
        do j=1,N
            if(Y(j) == Yunique(i)) then
                Ydata_unique(ctr,i) = Y(j)
                Xdata_unique(ctr,:,i) = X(j,:)                

                ctr=ctr+1
            endif
        enddo
    enddo


    ! --- create bootstrapped sample ---
    ! init random number generator seed
    call init_random_seed()

    ! generate array of random numbers [0,1)
    allocate(randarr(numsamps/num_Yunique, num_Yunique))
    allocate(rand_obs_num(numsamps/num_Yunique, num_Yunique))
    call random_number(randarr)

    ! get array of random integers in the right range of each column
    do i=1,num_Yunique
        rand_obs_num(:,i) = 1 + int(randarr(:,i) * ((Yunique_counts(i) - 1) + 1))
    enddo

    ctr = 1
    do j=1,num_Yunique
        do i=1,numsamps/num_Yunique
            Y_boot(ctr)   = Ydata_unique(rand_obs_num(i,j),j)
            X_boot(ctr,:) = Xdata_unique(rand_obs_num(i,j),:,j)

            ctr=ctr+1
        enddo
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
    type (node_ptr) :: fittedforest_ptrarr(numboots)

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
    logical, parameter :: verbose_parallel = .true.

    ! --- setup ---
    ! Get data size
    N = size(X,1)
    P = size(X,2)

    ! input checks
    if(N /= size(Y))    call rexit("Data dimensions do not match.")
    if(min_node_obs<=0) call rexit("ERROR.")
    if(max_depth<0)     call rexit("ERROR.")
    if(numsamps<=0)     call rexit("ERROR.")
    if(numvars<=0)      call rexit("ERROR.")
    if(numboots<=0)     call rexit("ERROR.")

    ! allocate memory to allocatable arrays
    allocate(variables_selected(P))
    allocate(randarr(P))

    ! --- grow forest ---

    !$OMP PARALLEL DO &
    !$OMP       PRIVATE(randarr, idx, i, &
    !$OMP           variables_selected, variables_selected_nums, &
    !$OMP           Y_boot, X_boot)
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
        call bootstrap_balanced(Y, X, numsamps, Y_boot, X_boot, opt_Yunique=(/0,1/), opt_force_numsamps_evensplits=.true.)
        ! TODO: should implement even split option into arguments for grow_forest


        if(verbose) then
            print '("Bootstrapped Y Size = ", i6)', size(Y_boot)
            print '("Bootstrapped X Size = (", i5, ",", i5, ")")', size(X_boot,1), size(X_boot,2)
        endif

        ! -- fit tree to the bootstrapped data and select variables --
        fittedforest_ptrarr(treenum)%t => grow(Y_boot, X_boot, min_node_obs, max_depth, &
            opt_splittable=variables_selected)     

        deallocate(Y_boot)
        deallocate(X_boot)


    enddo
    !$OMP END PARALLEL DO


    do treenum=1,numboots
        ! store the forest as an array of nodes as opposed to 
        ! an array of node pointers

        fittedforest(treenum)%depth = fittedforest_ptrarr(treenum)%t%depth
        fittedforest(treenum)%majority = fittedforest_ptrarr(treenum)%t%majority
        fittedforest(treenum)%has_subnodes = fittedforest_ptrarr(treenum)%t%has_subnodes
        fittedforest(treenum)%tag = fittedforest_ptrarr(treenum)%t%tag

        fittedforest(treenum)%splitvarnum = fittedforest_ptrarr(treenum)%t%splitvarnum
        fittedforest(treenum)%splitvalue = fittedforest_ptrarr(treenum)%t%splitvalue

        fittedforest(treenum)%leftnode => fittedforest_ptrarr(treenum)%t%leftnode
        fittedforest(treenum)%rightnode => fittedforest_ptrarr(treenum)%t%rightnode

        fittedforest(treenum)%parentnode => null()

    enddo


    if(verbose) then
        do i=1,size(fittedforest)
            print *, "-----------------------------"
            print '("For tree no. ", i5, ", root node splits variable no. ", i5, " at value ", f10.5)', &
                i, fittedforest(i)%splitvarnum, fittedforest(i)%splitvalue
        enddo
    endif
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
    !$OMP PARALLEL DO &
    !$OMP       PRIVATE(j)
    do j=1,size(fittedforest)
        Ypred_trees(:,j) = predict(fittedforest(j), X)
    enddo
    !$OMP END PARALLEL DO

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
    !$OMP PARALLEL DO &
    !$OMP       PRIVATE(i,j,num1)
    do i=1,N
        num1 = 0
        do j=1,size(fittedforest)
            if(Ypred_trees(i,j)==1) num1=num1+1
            if(Ypred_trees(i,j)==0) num1=num1-1
        enddo

        if(num1 >= 0) Ypred(i) = 1
        if(num1 <  0) Ypred(i) = 0
    enddo
    !$OMP END PARALLEL DO

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

    logical, parameter :: verbose = .false.

    exitflag = -1


    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_bootstrap_01 -------------------"
    endif

    ! --- create bootstrap sample on pre-defined data ---
    Y = (/0,1,2,3,4,5,6,7,8,9/)
    X(:,1) = (/10,11,12,13,14,15,16,17,18,19/)
    X(:,2) = (/20,21,22,23,24,25,26,27,28,29/)

    call bootstrap(Y, X, numsamps, Y_boot1, X_boot1)
    call bootstrap(Y, X, numsamps, Y_boot2, X_boot2)
    call bootstrap(Y, X, numsamps, Y_boot3, X_boot3)


    ! --- test failure conditions ---
    ! test that bootstrapped data has correct dimensions
    if(.not. size(Y_boot1)==numsamps) call rexit("Test failed.")
    if(.not. size(Y_boot2)==numsamps) call rexit("Test failed.")
    if(.not. size(Y_boot3)==numsamps) call rexit("Test failed.")

    if(.not. all((/size(X_boot1,1),size(X_boot1,2)/)==(/numsamps,P/))) call rexit("Test failed.")
    if(.not. all((/size(X_boot2,1),size(X_boot2,2)/)==(/numsamps,P/))) call rexit("Test failed.")
    if(.not. all((/size(X_boot3,1),size(X_boot3,2)/)==(/numsamps,P/))) call rexit("Test failed.")

    ! check that values have been correctly carried
    do i=1,numsamps
        do j=1,P
            if(.not. all(X_boot1(:,j)==Y_boot1+(j*10_dp))) call rexit("Test failed.")
            if(.not. all(X_boot2(:,j)==Y_boot2+(j*10_dp))) call rexit("Test failed.")
            if(.not. all(X_boot3(:,j)==Y_boot3+(j*10_dp))) call rexit("Test failed.")
        enddo
    enddo

    ! do rudimentary check for randomness
    if(all(Y_boot1==Y_boot2) .and. all(Y_boot2==Y_boot3)) call rexit("Test failed.")



    exitflag = 0

end function



function test_bootstrap_balanced_01() result(exitflag)
    integer :: exitflag

    integer, parameter :: N=10, P=2
    integer, parameter :: in_numsamps = 5
    integer, parameter :: numsamps = 6  ! true value needed to achieve even splits
    integer :: Y(N)
    integer, allocatable :: Y_boot1(:), Y_boot2(:), Y_boot3(:)
    real(dp) :: X(N,P)
    real(dp), allocatable :: X_boot1(:,:), X_boot2(:,:), X_boot3(:,:)

    integer :: j

    logical, parameter :: verbose = .false.

    exitflag = -1



    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_bootstrap_balanced_01 -------------------"
    endif

    ! --- create bootstrap sample on pre-defined data ---
    Y = (/1,1,1,1,1,1,0,0,0,0/)
    X(:,1) = (/10,11,12,13,14,15,16,17,18,19/)
    X(:,2) = (/20,21,22,23,24,25,26,27,28,29/)

    call bootstrap_balanced(Y, X, in_numsamps, Y_boot1, X_boot1, opt_force_numsamps_evensplits=.true.)
    call bootstrap_balanced(Y, X, in_numsamps, Y_boot2, X_boot2, opt_force_numsamps_evensplits=.true.)
    call bootstrap_balanced(Y, X, in_numsamps, Y_boot3, X_boot3, opt_Yunique=(/0,1/), opt_force_numsamps_evensplits=.true.)


    ! --- test failure conditions ---
    ! test that bootstrapped data has correct dimensions
    if(.not. size(Y_boot1)==numsamps) call rexit("Test failed: bootstrapped Y data has wrong number of rows.")
    if(.not. size(Y_boot2)==numsamps) call rexit("Test failed: bootstrapped Y data has wrong number of rows.")
    if(.not. size(Y_boot3)==numsamps) call rexit("Test failed: bootstrapped Y data has wrong number of rows.")

    if(.not. all((/size(X_boot1,1),size(X_boot1,2)/)==(/numsamps,P/))) &
        call rexit("Test failed: bootstrapped X data has wrong dimensions.")
    if(.not. all((/size(X_boot2,1),size(X_boot2,2)/)==(/numsamps,P/))) &
        call rexit("Test failed: bootstrapped X data has wrong dimensions.")
    if(.not. all((/size(X_boot3,1),size(X_boot3,2)/)==(/numsamps,P/))) &
        call rexit("Test failed: bootstrapped X data has wrong dimensions.")

    ! roughly check that values have been correctly carried
    do j=1,(P-1)
        if(.not. all(X_boot1(:,j)==(X_boot1(:,j+1) - 10_dp) )) call rexit("Test failed.")
        if(.not. all(X_boot2(:,j)==(X_boot2(:,j+1) - 10_dp) )) call rexit("Test failed.")
        if(.not. all(X_boot3(:,j)==(X_boot3(:,j+1) - 10_dp) )) call rexit("Test failed.")
    enddo


    ! do rudimentary check for randomness
    if(all(X_boot1==X_boot2) .and. all(X_boot2==X_boot3)) call rexit("Test failed: not random.")


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

    logical, parameter :: verbose = .false.

    exitflag = -1



    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_forest_01 -------------------"
    endif

    ! --- create pre-defined data and grow a forest ---
    Y = (/1,1,1,1,1,1,0,0,0,0/)
    X(:,1) = (/10,11,12,13,14,15,16,17,18,19/)
    X(:,2) = (/20,21,22,23,24,25,26,27,28,29/)

    ff = grow_forest(Y, X, min_node_obs, max_depth, &
        numsamps, numvars, numboots)

    ! --- test failure conditions, automated ---
    if(.not. size(ff)==numboots) call rexit("Test failed.")

    ! --- test failure conditions, manual ---
    if(verbose) then
        print *, "== Manual Testing Instructions =="
        print *, "Part of this test cannot be automated and must be done manually."
        print *, "To do so, set the verbose flag to .true. in grow_forest(...),"
        print *, "recompile the test framework, and check the following:"
        print *, "    1. The correct number of trees are generated"
        print *, "    2. The correct number of variables are selected"
        print *, "    3. There is some randomness in the variables selected"
        print *, "    4. The size of the bootstrapped Y and X are correct"
    endif

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


    logical, parameter :: verbose = .false.

    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "---------- Running Test Function test_grow_predict_forest_01 -------------------"
    endif

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
    if(.not. all(Ysamehat==Y)) call rexit("Test failed.")
    if (.not. all(Ynewhat==Ynew)) call rexit("Test failed.")



    exitflag = 0

end function



end module forest_parallel
