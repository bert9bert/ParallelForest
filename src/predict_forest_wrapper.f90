!------------------------------------------------------------------------------
!   Fortran subroutine to be used as wrapper to predict forest in R.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------

subroutine predict_forest_wrapper(treenum, &
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

    integer, intent(in) :: treenum(sum(numnodes))
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
    type (node) :: forest(numboots)

    integer :: this_treenum
    integer :: begidx, endidx


    !--- predict with inputted flattened forest ---
    ! unflatten forest
    begidx = 1
    endidx = numnodes(1)

    do this_treenum=1,numboots
        forest(this_treenum) = flat2tree( &
            tag(begidx:endidx), &
            tagparent(begidx:endidx), tagleft(begidx:endidx), tagright(begidx:endidx), &
            is_topnode(begidx:endidx), &
            depth(begidx:endidx), majority(begidx:endidx), has_subnodes(begidx:endidx), &
            splitvarnum(begidx:endidx), splitvalue(begidx:endidx) &
            )

        if(this_treenum /= numboots) then
            begidx = endidx + 1
            endidx = begidx + numnodes(this_treenum+1) - 1
        endif
    enddo


    ! get forest prediction
    Ynew_pred = predict_forest(forest, Xnew)

    
end subroutine
