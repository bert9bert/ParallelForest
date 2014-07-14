!------------------------------------------------------------------------------
!   Fortran subroutine to be used as wrapper to predict tree in R.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


subroutine predict_tree_wrapper(tag, tagparent, tagleft, tagright, is_topnode, &
    depth, majority, has_subnodes, splitvarnum, splitvalue, numnodes, &
    N, P, Xnew, Ynew_pred)


    use utils
    use tree_utils
    use classification

    implicit none
    !--- variable declarations ---


    ! input variables
    integer, intent(in) :: numnodes, N, P

    integer, intent(in) :: tag(numnodes), tagparent(numnodes), tagleft(numnodes), tagright(numnodes)
    logical, intent(in) :: is_topnode(numnodes)
    integer, intent(in) :: depth(numnodes)
    integer, intent(in) :: majority(numnodes)
    logical, intent(in) :: has_subnodes(numnodes)
    integer, intent(in) :: splitvarnum(numnodes)
    real(dp), intent(in) :: splitvalue(numnodes)

    real(dp), intent(in) :: Xnew(N,P)

    ! output (flattened tree) variables
    integer, intent(out) :: Ynew_pred(N)

    ! private variables
    type (node), pointer :: tree



    !--- predict with inputted flattened tree ---
    tree => flat2tree(tag, tagparent, tagleft, tagright, is_topnode, &
            depth, majority, has_subnodes, splitvarnum, splitvalue)

    Ynew_pred = predict(tree, Xnew)

    
end subroutine
