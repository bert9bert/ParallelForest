!------------------------------------------------------------------------------
!   Fortran subroutine to be used as wrapper to predict forest in R.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------

subroutine predict_forest_wrapper( &
    tag, tagparent, tagleft, tagright, is_topnode, &
    depth, majority, has_subnodes, splitvarnum, splitvalue, numnodes, &
    numboots, &
    N, P, Xnew, Ynew_pred)


    use utils
    use tree_utils
    use classification
    use forest_parallel

    implicit none
    !--- variable declarations ---


    ! input variables
    integer, intent(in) :: numboots, N, P
    integer, intent(in) :: numnodes(numboots)

    integer, intent(in) :: tag(sum(numnodes)), &
        tagparent(sum(numnodes)), tagleft(sum(numnodes)), tagright(sum(numnodes))
    logical, intent(in) :: is_topnode(sum(numnodes))
    integer, intent(in) :: depth(sum(numnodes))
    integer, intent(in) :: majority(sum(numnodes))
    logical, intent(in) :: has_subnodes(sum(numnodes))
    integer, intent(in) :: splitvarnum(sum(numnodes))
    real(dp), intent(in) :: splitvalue(sum(numnodes))

    real(dp), intent(in) :: Xnew(N,P)

    ! output variables
    integer, intent(out) :: Ynew_pred(N)

    ! private variables
    type (node) :: fittedforest(numboots)
    type (node_ptr) :: fittedforest_ptrarr(numboots)

    integer :: treenum
    integer :: begidx, endidx


    !--- predict with inputted flattened forest ---
    ! unflatten forest
    begidx = 1
    endidx = numnodes(1)

    do treenum=1,numboots
        fittedforest_ptrarr(treenum)%t => flat2tree( &
            tag(begidx:endidx), &
            tagparent(begidx:endidx), tagleft(begidx:endidx), tagright(begidx:endidx), &
            is_topnode(begidx:endidx), &
            depth(begidx:endidx), majority(begidx:endidx), has_subnodes(begidx:endidx), &
            splitvarnum(begidx:endidx), splitvalue(begidx:endidx) &
            )

        if(treenum /= numboots) then
            begidx = endidx + 1
            endidx = begidx + numnodes(treenum+1) - 1
        endif
    enddo

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

    ! get forest prediction
    Ynew_pred = predict_forest(fittedforest, Xnew)

    
end subroutine
