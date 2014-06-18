subroutine grow_wrapper(n, p, xtrain, ytrain, min_node_obs, max_depth, &
	tag_padded, tagparent_padded, tagleft_padded, tagright_padded, is_topnode_padded, &
    depth_padded, majority_padded, has_subnodes_padded, splitvarnum_padded, splitvalue_padded)

	use utils
	use tree_utils
	use classification

	implicit none
	!--- variable declarations ---

	! DEBUGGING VARIABLES, DELETE WHEN NO LONGER NEEDED
	integer, parameter :: TMP = 25
	logical, parameter :: verbose = .true.
	integer :: i
	character(len=50) :: fmt

	! input variables
	integer, intent(in) :: n, p
	real(dp), intent(in) :: xtrain(n,p)
	integer, intent(in) :: ytrain(n)
	integer, intent(in) :: min_node_obs, max_depth

	! output (flattened tree) variables
	integer, intent(out)  :: tag_padded(TMP)
	integer, intent(out)  :: tagparent_padded(TMP), tagleft_padded(TMP), tagright_padded(TMP)
	logical, intent(out)  :: is_topnode_padded(TMP)
	integer, intent(out)  :: depth_padded(TMP)
	integer, intent(out)  :: majority_padded(TMP)
	logical, intent(out)  :: has_subnodes_padded(TMP)
	integer, intent(out)  :: splitvarnum_padded(TMP)
	real(dp), intent(out) :: splitvalue_padded(TMP)

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

    !--- flatten tree and pad ---
	call tree2flat(fittedtree, max_depth, &
		tag, tagparent, tagleft, tagright, is_topnode, &
    	depth, majority, has_subnodes, splitvarnum, splitvalue)

	tag_padded(1:size(tag)) = tag
	tagparent_padded(1:size(tag)) = tagparent
	tagleft_padded(1:size(tag)) = tagleft
	tagright_padded(1:size(tag)) = tagright
	is_topnode_padded(1:size(tag)) = is_topnode
	depth_padded(1:size(tag)) = depth
	majority_padded(1:size(tag)) = majority
	has_subnodes_padded(1:size(tag)) = has_subnodes
	splitvarnum_padded(1:size(tag)) = splitvarnum
	splitvalue_padded(1:size(tag)) = splitvalue

	if(verbose) then
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

! TODO and NOTES:
! 1. make take into account proper size of array when passing outputs from
! Fortran to R
! 2. look into using R logicals for returned Fortran logicals
! 3. why are there an unexpectedly high number of nodes (see Size = printed)?,
! probably need additional test in tree_utils.f90 to make sure erroneous nodes
! aren't created
