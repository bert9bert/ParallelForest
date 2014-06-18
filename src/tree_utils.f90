module tree_utils

!------------------------------------------------------------------------------
!   
!   Author: Bertram Ieong
!------------------------------------------------------------------------------

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
end type


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

end subroutine



recursive function flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
    depth, majority, has_subnodes, splitvarnum, splitvalue, &
    opt_thistag) result(tree)

    !--- variable declarations ---
    type (node), target :: tree

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
    integer :: idx
    integer :: i


    !--- Find the number of nodes in this tree ---
    numnodes = size(tag)

    ! check that inputs are of the right size
    if( size(tag) /= numnodes )          stop "Input dimensions do not match"
    if( size(tagparent) /= numnodes )    stop "Input dimensions do not match"
    if( size(tagleft) /= numnodes )      stop "Input dimensions do not match"
    if( size(tagright) /= numnodes )     stop "Input dimensions do not match"
    if( size(is_topnode) /= numnodes )   stop "Input dimensions do not match"
    if( size(depth) /= numnodes )        stop "Input dimensions do not match"
    if( size(majority) /= numnodes )     stop "Input dimensions do not match"
    if( size(has_subnodes) /= numnodes ) stop "Input dimensions do not match"
    if( size(splitvarnum) /= numnodes )  stop "Input dimensions do not match"
    if( size(splitvalue) /= numnodes )   stop "Input dimensions do not match"


    !--- figure out what the tag for the root node is/should be ---
    if(present(opt_thistag)) then
        thistag = opt_thistag
    else
        thistag = TOP_NODE_NUM
    endif


    !--- fill in this node ---

    ! find the index for this tag
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

        tree%leftnode = flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
            depth, majority, has_subnodes, splitvarnum, splitvalue, &
            tagleft(idx))

        tree%rightnode = flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
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
        stop
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




end module tree_utils