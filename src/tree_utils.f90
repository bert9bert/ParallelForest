!------------------------------------------------------------------------------
!   Module of tree (and forest) utilities.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


module tree_utils

use utils

implicit none


type node
    ! variables set regardless of whether this node has subnodes
    type (node), pointer :: parentnode
    integer :: depth
    integer :: majority
    logical :: has_subnodes
    integer :: tag
    ! variables only not null when node has subnodes
    type (node), pointer :: leftnode, rightnode
    integer :: splitvarnum
    real(dp) :: splitvalue
end type node


type node_ptr
    type (node), pointer :: t
end type node_ptr


contains



subroutine tree2flat(tree, max_depth, tag, tagparent, tagleft, tagright, is_topnode, &
    depth, majority, has_subnodes, splitvarnum, splitvalue)

    !--- variable declarations ---
    type (node), intent(in) :: tree
    integer, intent(in) :: max_depth

    ! tree descriptors
    integer, allocatable, intent(out) :: tag(:), tagparent(:), tagleft(:), tagright(:)
    logical, allocatable, intent(out) :: is_topnode(:)

    ! node attributes
    integer, allocatable, intent(out) :: depth(:)
    integer, allocatable, intent(out) :: majority(:)
    logical, allocatable, intent(out) :: has_subnodes(:)
    integer, allocatable, intent(out) :: splitvarnum(:)
    real(dp), allocatable, intent(out) :: splitvalue(:)

    ! private constants
    integer, parameter :: TOP_NODE_NUM = 0

    ! private variables
    logical, allocatable :: has_node_entry_tmp(:)
    integer :: max_nodes, numnodes

    integer, allocatable :: tag_tmp(:), tagparent_tmp(:), tagleft_tmp(:), tagright_tmp(:)
    logical, allocatable :: is_topnode_tmp(:)

    integer, allocatable :: depth_tmp(:)
    integer, allocatable :: majority_tmp(:)
    logical, allocatable :: has_subnodes_tmp(:)
    integer, allocatable :: splitvarnum_tmp(:)
    real(dp), allocatable :: splitvalue_tmp(:)
    


    ! counting variables
    integer :: i, j


    ! find max nodes possible for this max depth
    max_nodes = 2**(max_depth + 1 - TOP_NODE_NUM) - 1

    ! allocate memory to allocatable arrays
    allocate(tag_tmp(max_nodes))
    allocate(tagparent_tmp(max_nodes))
    allocate(tagleft_tmp(max_nodes))
    allocate(tagright_tmp(max_nodes))
    allocate(is_topnode_tmp(max_nodes))
    allocate(depth_tmp(max_nodes))
    allocate(majority_tmp(max_nodes))
    allocate(has_subnodes_tmp(max_nodes))
    allocate(splitvarnum_tmp(max_nodes))
    allocate(splitvalue_tmp(max_nodes))
    allocate(has_node_entry_tmp(max_nodes))


    !- recursively build arrays of flattened tree data -
    has_node_entry_tmp = .false.

    call tree2flat_recursion_helper(&
        tree, max_depth, tag_tmp, tagparent_tmp, tagleft_tmp, tagright_tmp, is_topnode_tmp, &
        depth_tmp, majority_tmp, has_subnodes_tmp, splitvarnum_tmp, splitvalue_tmp, has_node_entry_tmp)

    !- get rid of unnecessary rows -
    ! get number of nodes
    numnodes = 0

    do i=1,size(has_node_entry_tmp)
        if(has_node_entry_tmp(i)) numnodes = numnodes + 1
    enddo

    ! allocate arrays according to the number of nodes
    allocate( tag(numnodes) )
    allocate( tagparent(numnodes) )
    allocate( tagleft(numnodes) )
    allocate( tagright(numnodes) )
    allocate( is_topnode(numnodes) )
    allocate( depth(numnodes) )
    allocate( majority(numnodes) )
    allocate( has_subnodes(numnodes) )
    allocate( splitvarnum(numnodes) )
    allocate( splitvalue(numnodes) )

    ! get rid of those unnecessary nodes, so that the number of rows is the number of nodes
    j = 1
    do i=1,size(has_node_entry_tmp)
        if(has_node_entry_tmp(i)) then
            tag(j) = tag_tmp(i)
            tagparent(j) = tagparent_tmp(i)
            tagleft(j) = tagleft_tmp(i)
            tagright(j) = tagright_tmp(i)
            is_topnode(j) = is_topnode_tmp(i)
            depth(j) = depth_tmp(i)
            majority(j) = majority_tmp(i)
            has_subnodes(j) = has_subnodes_tmp(i)
            splitvarnum(j) = splitvarnum_tmp(i)
            splitvalue(j) = splitvalue_tmp(i)

            j = j + 1
        endif
    enddo

    ! if no subnodes, standardize fields that will consequently be missing
    ! to something uniform
    do j=1,numnodes
        if(.not. has_subnodes(j)) then
            tagleft(j) = -1111
            tagright(j) = -2222
            splitvarnum(j) = -8888
            splitvalue(j) = -9999
        endif
    enddo

end subroutine



recursive function flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
    depth, majority, has_subnodes, splitvarnum, splitvalue, &
    opt_thistag) result(tree)

    !--- variable declarations ---
    type (node), pointer :: tree

    ! tree descriptors
    integer, intent(in) :: tag(:), tagparent(:), tagleft(:), tagright(:)
    logical, intent(in) :: is_topnode(:)

    ! node attributes
    integer, intent(in) :: depth(:)
    integer, intent(in) :: majority(:)
    logical, intent(in) :: has_subnodes(:)
    integer, intent(in) :: splitvarnum(:)
    real(dp), intent(in) :: splitvalue(:)

    integer, optional, intent(in) :: opt_thistag
    integer :: thistag

    ! private constants
    integer, parameter :: TOP_NODE_NUM = 0

    ! private variables
    integer :: numnodes

    ! counting and indexing variables
    integer :: idx, i


    !--- Allocate memory for this (sub)tree ---
    allocate(tree)


    !--- Find the number of nodes in this tree ---
    numnodes = size(tag)

    ! check that inputs are of the right size
    if( size(tag) /= numnodes )          call rexit("Input dimensions do not match")
    if( size(tagparent) /= numnodes )    call rexit("Input dimensions do not match")
    if( size(tagleft) /= numnodes )      call rexit("Input dimensions do not match")
    if( size(tagright) /= numnodes )     call rexit("Input dimensions do not match")
    if( size(is_topnode) /= numnodes )   call rexit("Input dimensions do not match")
    if( size(depth) /= numnodes )        call rexit("Input dimensions do not match")
    if( size(majority) /= numnodes )     call rexit("Input dimensions do not match")
    if( size(has_subnodes) /= numnodes ) call rexit("Input dimensions do not match")
    if( size(splitvarnum) /= numnodes )  call rexit("Input dimensions do not match")
    if( size(splitvalue) /= numnodes )   call rexit("Input dimensions do not match")


    !--- figure out what the tag for the root node is/should be ---
    if(present(opt_thistag)) then
        thistag = opt_thistag
    else
        thistag = TOP_NODE_NUM
    endif


    !--- fill in this node ---

    ! find the index for this tag
    idx = -1
    do i=1,numnodes
        if(tag(i)==thistag) then
            idx=i
            exit
        endif
    enddo

    ! fill in node attributes
    tree%depth = depth(idx)
    tree%majority = majority(idx)
    tree%has_subnodes = has_subnodes(idx)
    tree%tag = tag(idx)
    tree%splitvarnum = splitvarnum(idx)
    tree%splitvalue = splitvalue(idx)

    ! fill in subnodes if there are any
    if(has_subnodes(idx)) then
        allocate(tree%leftnode)
        allocate(tree%rightnode)

        tree%leftnode => flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
            depth, majority, has_subnodes, splitvarnum, splitvalue, &
            tagleft(idx))

        tree%rightnode => flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
            depth, majority, has_subnodes, splitvarnum, splitvalue, &
            tagright(idx))

        ! fill in subnodes' parent pointers
        tree%leftnode%parentnode => tree
        tree%rightnode%parentnode => tree
        
    endif


end function



recursive subroutine tree2flat_recursion_helper(&
    t, max_depth, tag_tmp, tagparent_tmp, tagleft_tmp, tagright_tmp, is_topnode_tmp, &
    depth_tmp, majority_tmp, has_subnodes_tmp, splitvarnum_tmp, splitvalue_tmp, has_node_entry_tmp)

    !--- variable declarations ---
    type (node), intent(in) :: t
    integer, intent(in) :: max_depth

    ! tree descriptors
    integer, intent(inout) :: tag_tmp(:), tagparent_tmp(:), tagleft_tmp(:), tagright_tmp(:)
    logical, intent(inout) :: is_topnode_tmp(:)

    ! node attributes
    integer, intent(inout) :: depth_tmp(:)  ! not used, but here to preserve calling symtax similarity
    integer, intent(inout) :: majority_tmp(:)
    logical, intent(inout) :: has_subnodes_tmp(:)
    integer, intent(inout) :: splitvarnum_tmp(:)
    real(dp), intent(inout) :: splitvalue_tmp(:)

    logical, intent(inout) :: has_node_entry_tmp(:)

    ! private constants
    integer, parameter :: TOP_NODE_NUM = 0

    ! private variables
    integer :: idx_tmp



    ! --- Store flattened equivalent of this node ---
    idx_tmp = t%tag + 1

    tag_tmp(idx_tmp) = t%tag

    if(t%tag>TOP_NODE_NUM) then
        tagparent_tmp(idx_tmp) = t%parentnode%tag
    else if(t%tag==TOP_NODE_NUM) then
        tagparent_tmp(idx_tmp) = -1
    else
        call rexit("ERROR.")
    endif



    if(t%has_subnodes) then
        tagleft_tmp(idx_tmp) = t%leftnode%tag
        tagright_tmp(idx_tmp) = t%rightnode%tag
    endif

    depth_tmp(idx_tmp) = t%depth
    majority_tmp(idx_tmp) = t%majority
    has_subnodes_tmp(idx_tmp) = t%has_subnodes
    splitvarnum_tmp(idx_tmp) = t%splitvarnum
    splitvalue_tmp(idx_tmp) = t%splitvalue

    if(t%tag == TOP_NODE_NUM) then
        is_topnode_tmp(idx_tmp) = .true.
    else
        is_topnode_tmp(idx_tmp) = .false.
    endif

    has_node_entry_tmp(idx_tmp) = .true.

    ! --- Do the same for its subnodes, if any ---
    if(t%has_subnodes) then
        call tree2flat_recursion_helper(&
            t%leftnode, &
            max_depth, tag_tmp, tagparent_tmp, tagleft_tmp, tagright_tmp, is_topnode_tmp, &
            depth_tmp, majority_tmp, has_subnodes_tmp, splitvarnum_tmp, splitvalue_tmp, has_node_entry_tmp)

        call tree2flat_recursion_helper(&
            t%rightnode, &
            max_depth, tag_tmp, tagparent_tmp, tagleft_tmp, tagright_tmp, is_topnode_tmp, &
            depth_tmp, majority_tmp, has_subnodes_tmp, splitvarnum_tmp, splitvalue_tmp, has_node_entry_tmp)
    endif

end subroutine



function countnodes(t) result(numnodes)
    ! Returns the number of nodes in a tree.

    type (node), intent(in) :: t
    integer :: numnodes

    numnodes = 0

    call countnodes_rec_hlpr(t,numnodes)

end function

recursive subroutine countnodes_rec_hlpr(t, n)
    type (node), intent(in) :: t
    integer, intent(inout) :: n  ! for initial function call, set to 0

    n=n+1

    if(t%has_subnodes) then
        call countnodes_rec_hlpr(t%leftnode, n)
        call countnodes_rec_hlpr(t%rightnode, n)
    endif
end subroutine




function test_tree2flat_flat2tree_01() result(exitflag)
    integer :: exitflag

    type (node), target :: tree, tree_left, tree_right, tree_right_left, tree_right_right
    type (node), pointer :: tree_unflattened

    integer :: max_depth

    ! flattened variables
    integer, allocatable :: tag(:), tagparent(:), tagleft(:), tagright(:)
    logical, allocatable :: is_topnode(:)

    integer, allocatable :: depth(:)
    integer, allocatable :: majority(:)
    logical, allocatable :: has_subnodes(:)
    integer, allocatable :: splitvarnum(:)
    real(dp), allocatable :: splitvalue(:)

    ! counting variables
    integer :: i

    ! debugging variables
    logical, parameter :: verbose = .false.
    character(len=50) :: fmt


    exitflag = -1

    if(verbose) then
        print *, " "
        print *, "--------- Running Test Function test_tree2flat_flat2tree_01 ------------------"
    endif

    !--- create test tree ---
    ! create root node
    tree%depth = 0
    tree%majority = 1
    tree%has_subnodes = .true.
    tree%tag = 0
    tree%splitvarnum = 3
    tree%splitvalue = 0.05_dp

    ! create left subnode
    tree_left%depth = 1
    tree_left%majority = 1
    tree_left%has_subnodes = .false.
    tree_left%tag = 1
    tree_left%splitvarnum = -6
    tree_left%splitvalue = -0.10_dp

    ! create right subnode
    tree_right%depth = 1
    tree_right%majority = 1
    tree_right%has_subnodes = .true.
    tree_right%tag = 2
    tree_right%splitvarnum = 9
    tree_right%splitvalue = 0.15_dp
  
    ! create right subnode's left subnode
    tree_right_left%depth = 2
    tree_right_left%majority = 1
    tree_right_left%has_subnodes = .false.
    tree_right_left%tag = 3
    tree_right_left%splitvarnum = -1
    tree_right_left%splitvalue = -0.20_dp

    ! create right subnode's right subnode
    tree_right_right%depth = 2
    tree_right_right%majority = 1
    tree_right_right%has_subnodes = .false.
    tree_right_right%tag = 4
    tree_right_right%splitvarnum = -2
    tree_right_right%splitvalue = -0.25_dp

    ! connect the nodes
    tree%leftnode => tree_left
    tree%rightnode => tree_right
    tree_right%leftnode => tree_right_left
    tree_right%rightnode => tree_right_right

    tree_right_left%parentnode => tree_right
    tree_right_right%parentnode => tree_right
    tree_left%parentnode => tree
    tree_right%parentnode => tree

    !--- flatten this tree ---
    max_depth = 2

    call tree2flat(tree, max_depth, tag, tagparent, tagleft, tagright, is_topnode, &
        depth, majority, has_subnodes, splitvarnum, splitvalue)

    ! display if verbose is on

    if(verbose) then
        print *, "Flattended Tree"
        print *, "tag tagparent tagleft tagright is_topnode &
                & depth majority has_subnodes splitvarnum splitvalue"

        fmt = '(i4, i10, i8, i9, l11, i6, i9, l13, i12, f11.3 )'
        do i=1,size(tag)
            print fmt, &
                tag(i), tagparent(i), tagleft(i), tagright(i), is_topnode(i), &
                depth(i), majority(i), has_subnodes(i), splitvarnum(i), splitvalue(i)
        enddo
    endif


    !--- turn this flattened tree back into a tree ---
    tree_unflattened => flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
        depth, majority, has_subnodes, splitvarnum, splitvalue)


    if(verbose) then
        fmt = '(i6, i11, l15, i16, f14.3, i6)'

        print *, ""
        print *, "Unflattened Tree"
        print *, "---------------+--------+----------+--------------+---------------+-------------+-----"
        print *, "Node           |  Depth | Majority | Has Subnodes | Split Var Num | Split Value | Tag "
        print *, "---------------+--------+----------+--------------+---------------+-------------+-----"

        write (*,'(A)',advance="no") "-TOP (THIS)     | "
        print fmt, tree_unflattened%depth, tree_unflattened%majority, &
            tree_unflattened%has_subnodes, &
            tree_unflattened%splitvarnum, tree_unflattened%splitvalue, &
            tree_unflattened%tag

        write (*,'(A)',advance="no") "--left          | "
        print fmt, tree_unflattened%leftnode%depth, tree_unflattened%leftnode%majority, &
            tree_unflattened%leftnode%has_subnodes, &
            tree_unflattened%leftnode%splitvarnum, tree_unflattened%leftnode%splitvalue, &
            tree_unflattened%leftnode%tag


        write (*,'(A)',advance="no") "--right         | "
        print fmt, tree_unflattened%rightnode%depth, tree_unflattened%rightnode%majority, &
            tree_unflattened%rightnode%has_subnodes, &
            tree_unflattened%rightnode%splitvarnum, tree_unflattened%rightnode%splitvalue, &
            tree_unflattened%rightnode%tag

        write (*,'(A)',advance="no") "---right's left | "
        print fmt, tree_unflattened%rightnode%leftnode%depth, tree_unflattened%rightnode%leftnode%majority, &
            tree_unflattened%rightnode%leftnode%has_subnodes, &
            tree_unflattened%rightnode%leftnode%splitvarnum, tree_unflattened%rightnode%leftnode%splitvalue, &
            tree_unflattened%rightnode%leftnode%tag

        write (*,'(A)',advance="no") "---right's right| "
        print fmt, tree_unflattened%rightnode%rightnode%depth, tree_unflattened%rightnode%rightnode%majority, &
            tree_unflattened%rightnode%rightnode%has_subnodes, &
            tree_unflattened%rightnode%rightnode%splitvarnum, tree_unflattened%rightnode%rightnode%splitvalue, &
            tree_unflattened%rightnode%rightnode%tag


        print *, "---------------+--------+----------+--------------+---------------+------------+------"
    endif


    !--- test failure conditions ---
    if( (tree%depth        /=     tree_unflattened%depth) .or. &
        (tree%majority     /=     tree_unflattened%majority) .or. &
        (tree%has_subnodes .neqv. tree_unflattened%has_subnodes) .or. &
        (tree%splitvarnum  /=     tree_unflattened%splitvarnum) .or. &
        (tree%splitvalue   /=     tree_unflattened%splitvalue) .or. &
        (tree%tag          /=     tree_unflattened%tag) ) then

        call rexit("Test failed: root node &
            & was flattened and unflattened incorrectly")
    endif

    if( (tree%leftnode%depth        /=     tree_unflattened%leftnode%depth) .or. &
        (tree%leftnode%majority     /=     tree_unflattened%leftnode%majority) .or. &
        (tree%leftnode%has_subnodes .neqv. tree_unflattened%leftnode%has_subnodes) .or. &
        (tree%leftnode%tag          /=     tree_unflattened%leftnode%tag) ) then

        call rexit("Test failed: left node &
            & was flattened and unflattened incorrectly")
    endif

    if( (tree%rightnode%depth        /=     tree_unflattened%rightnode%depth) .or. &
        (tree%rightnode%majority     /=     tree_unflattened%rightnode%majority) .or. &
        (tree%rightnode%has_subnodes .neqv. tree_unflattened%rightnode%has_subnodes) .or. &
        (tree%rightnode%tag          /=     tree_unflattened%rightnode%tag) ) then

        call rexit("Test failed: right node &
            & was flattened and unflattened incorrectly")
    endif

    if( (tree%rightnode%leftnode%depth        /=     tree_unflattened%rightnode%leftnode%depth) .or. &
        (tree%rightnode%leftnode%majority     /=     tree_unflattened%rightnode%leftnode%majority) .or. &
        (tree%rightnode%leftnode%has_subnodes .neqv. tree_unflattened%rightnode%leftnode%has_subnodes) .or. &
        (tree%rightnode%leftnode%tag          /=     tree_unflattened%rightnode%leftnode%tag) ) then

        call rexit("Test failed: right node's left node &
            & was flattened and unflattened incorrectly")
    endif

    if( (tree%rightnode%rightnode%depth        /=     tree_unflattened%rightnode%rightnode%depth) .or. &
        (tree%rightnode%rightnode%majority     /=     tree_unflattened%rightnode%rightnode%majority) .or. &
        (tree%rightnode%rightnode%has_subnodes .neqv. tree_unflattened%rightnode%rightnode%has_subnodes) .or. &
        (tree%rightnode%rightnode%tag          /=     tree_unflattened%rightnode%rightnode%tag) ) then

        call rexit("Test failed: right node's right node &
            & was flattened and unflattened incorrectly")
    endif


    exitflag = 0

end function




end module tree_utils