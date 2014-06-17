subroutine grow_predict_rwrapper(n, p, xtrain, ytrain, xtest, ytesthat)
	use classification

	implicit none
	! variable declarations
	integer, intent(in) :: n, p
	real(dp), intent(in) :: xtrain(n,p), xtest(n,p)
	integer, intent(in) :: ytrain(n)
	integer, intent(out) :: ytesthat(n)

	integer, parameter :: min_node_obs=2, max_depth=5

	type (node) :: fittedtree

	fittedtree = grow(ytrain, xtrain, min_node_obs, max_depth)

	ytesthat = predict(fittedtree, xtest)

end subroutine


! compile command
! gfortran -fPIC -shared -o grow_predict_rwrapper.dll classification.f90 grow_predict_rwrapper.f90
