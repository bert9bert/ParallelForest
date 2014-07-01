!------------------------------------------------------------------------------
!   Fortran subroutine to be used as wrapper to grow forest in R.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


subroutine grow_forest_wrapper(n, p, xtrain, ytrain, min_node_obs, max_depth, retlen, &
    numsamps, numvars, numboots, &
    treenum_padded, &
    tag_padded, tagparent_padded, tagleft_padded, tagright_padded, is_topnode_padded, &
    depth_padded, majority_padded, has_subnodes_padded, splitvarnum_padded, splitvalue_padded, &
    numnodes)

    use utils
    use tree_utils
    use classification
    use forest_parallel

    implicit none
    !--- variable declarations ---

    ! input variables
    integer, intent(in) :: n, p
    real(dp), intent(in) :: xtrain(n,p)
    integer, intent(in) :: ytrain(n)
    integer, intent(in) :: min_node_obs, max_depth
    integer, intent(in) :: retlen  ! length of array divided by number of bootstrapped samples to return
    integer, intent(in) :: numsamps, numvars, numboots

    ! output (flattened tree) variables
    integer, intent(out)  :: treenum_padded(retlen*numboots)
    integer, intent(out)  :: tag_padded(retlen*numboots)
    integer, intent(out)  :: tagparent_padded(retlen*numboots), &
        tagleft_padded(retlen*numboots), tagright_padded(retlen*numboots)
    logical, intent(out)  :: is_topnode_padded(retlen*numboots)
    integer, intent(out)  :: depth_padded(retlen*numboots)
    integer, intent(out)  :: majority_padded(retlen*numboots)
    logical, intent(out)  :: has_subnodes_padded(retlen*numboots)
    integer, intent(out)  :: splitvarnum_padded(retlen*numboots)
    real(dp), intent(out) :: splitvalue_padded(retlen*numboots)

    integer, intent(out)  :: numnodes(numboots)

    ! private variables
    integer, allocatable  :: tag(:), tagparent(:), tagleft(:), tagright(:)
    logical, allocatable  :: is_topnode(:)
    integer, allocatable  :: depth(:)
    integer, allocatable  :: majority(:)
    logical, allocatable  :: has_subnodes(:)
    integer, allocatable  :: splitvarnum(:)
    real(dp), allocatable :: splitvalue(:)

    integer :: this_treenum
    
    type (node) :: fittedforest(numboots)

    ! counting variables
    integer :: ctr

    !--- fit forest ---
    fittedforest = grow_forest(ytrain, xtrain, min_node_obs, max_depth, &
        numsamps, numvars, numboots)

    !--- flatten forest and pad ---
    ! iterate through each tree in the forest
    ctr = 1
    do this_treenum=1,numboots
        ! flatten this tree
        call tree2flat(fittedforest(this_treenum), max_depth, &
            tag, tagparent, tagleft, tagright, is_topnode, &
            depth, majority, has_subnodes, splitvarnum, splitvalue)    

        ! get the number of nodes in this tree
        numnodes(this_treenum) = size(tag)

        if(numnodes(this_treenum)>retlen) then
            call rexit("Returned array length divided by the number of bootstrap samples &
                needs to be at least the length of the number of nodes.")
        endif

        ! add to padded return array, stacking trees on top of each other
        ! with all padding at the end

        treenum_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )      = this_treenum
        tag_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )          = tag
        tagparent_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )    = tagparent
        tagleft_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )      = tagleft
        tagright_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )     = tagright
        is_topnode_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )   = is_topnode
        depth_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )        = depth
        majority_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )     = majority
        has_subnodes_padded(ctr:(ctr+numnodes(this_treenum)-1 ) ) = has_subnodes
        splitvarnum_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )  = splitvarnum
        splitvalue_padded(ctr:(ctr+numnodes(this_treenum)-1 ) )   = splitvalue


        ! increment array index counter
        ctr=ctr+numnodes(this_treenum)

        ! deallocate arrays
        deallocate(tag)
        deallocate(tagparent)
        deallocate(tagleft)
        deallocate(tagright)
        deallocate(is_topnode)
        deallocate(depth)
        deallocate(majority)
        deallocate(has_subnodes)
        deallocate(splitvarnum)
        deallocate(splitvalue)
    enddo

end subroutine

