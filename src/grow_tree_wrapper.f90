!------------------------------------------------------------------------------
!   Fortran subroutine to be used as wrapper to grow tree in R.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


subroutine grow_tree_wrapper(n, p, xtrain, ytrain, min_node_obs, max_depth, retlen, &
    tag_padded, tagparent_padded, tagleft_padded, tagright_padded, is_topnode_padded, &
    depth_padded, majority_padded, has_subnodes_padded, splitvarnum_padded, splitvalue_padded, &
    numnodes)

    use utils
    use tree_utils
    use classification

    implicit none
    !--- variable declarations ---

    ! DEBUGGING VARIABLES, DELETE WHEN NO LONGER NEEDED
    logical, parameter :: verbose = .false.
    integer :: i
    character(len=50) :: fmt
    integer :: numfittednodes

    ! input variables
    integer, intent(in) :: n, p
    real(dp), intent(in) :: xtrain(n,p)
    integer, intent(in) :: ytrain(n)
    integer, intent(in) :: min_node_obs, max_depth
    integer, intent(in) :: retlen  ! length of array to return

    ! output (flattened tree) variables
    integer, intent(out)  :: tag_padded(retlen)
    integer, intent(out)  :: tagparent_padded(retlen), tagleft_padded(retlen), tagright_padded(retlen)
    logical, intent(out)  :: is_topnode_padded(retlen)
    integer, intent(out)  :: depth_padded(retlen)
    integer, intent(out)  :: majority_padded(retlen)
    logical, intent(out)  :: has_subnodes_padded(retlen)
    integer, intent(out)  :: splitvarnum_padded(retlen)
    real(dp), intent(out) :: splitvalue_padded(retlen)
    integer, intent(out)  :: numnodes

    ! private variables
    integer, allocatable  :: tag(:), tagparent(:), tagleft(:), tagright(:)
    logical, allocatable  :: is_topnode(:)
    integer, allocatable  :: depth(:)
    integer, allocatable  :: majority(:)
    logical, allocatable  :: has_subnodes(:)
    integer, allocatable  :: splitvarnum(:)
    real(dp), allocatable :: splitvalue(:)

    
    type (node) fittedtree



    !--- fit tree ---
    fittedtree = grow(ytrain, xtrain, min_node_obs, max_depth)

    
    if(verbose) then
        print *, "[X1 X2 | Y ] = ["
        do i=1,N
            print '(f10.5, "    ", f10.5, "    ", i3)', xtrain(i,1), xtrain(i,2), ytrain(i)
        enddo
        print *, "]"
    endif

    if(verbose) then
        numfittednodes = 0
        call countnodes(fittedtree, numfittednodes)
        print '("Before flatten, there are ", i5, " nodes in the fitted tree.")', numfittednodes
    endif

    !--- flatten tree and pad ---
    call tree2flat(fittedtree, max_depth, &
        tag, tagparent, tagleft, tagright, is_topnode, &
        depth, majority, has_subnodes, splitvarnum, splitvalue)

    numnodes = size(tag)

    if(numnodes>retlen) then
        call rexit("Returned array length needs to be at least the length of the number of nodes.")
    endif

    tag_padded(1:numnodes) = tag
    tagparent_padded(1:numnodes) = tagparent
    tagleft_padded(1:numnodes) = tagleft
    tagright_padded(1:numnodes) = tagright
    is_topnode_padded(1:numnodes) = is_topnode
    depth_padded(1:numnodes) = depth
    majority_padded(1:numnodes) = majority
    has_subnodes_padded(1:numnodes) = has_subnodes
    splitvarnum_padded(1:numnodes) = splitvarnum
    splitvalue_padded(1:numnodes) = splitvalue

    if(verbose) then
        numfittednodes = 0
        call countnodes(fittedtree, numfittednodes)
        print '("There are ", i5, " nodes in the fitted tree.")', numfittednodes

        print *, "Flattended Tree"
        print *, "tag tagparent tagleft tagright is_topnode &
                depth majority has_subnodes splitvarnum splitvalue"

        fmt = '(i4, i10, i8, i9, l11, i6, i9, l13, i12, f11.3 )'
        do i=1,size(tag)
            print fmt, &
                tag(i), tagparent(i), tagleft(i), tagright(i), is_topnode(i), &
                depth(i), majority(i), has_subnodes(i), splitvarnum(i), splitvalue(i)
        enddo
    endif
end subroutine
