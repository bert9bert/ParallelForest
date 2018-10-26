#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(grow_forest_wrapper)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(grow_tree_wrapper)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(predict_forest_wrapper)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(predict_tree_wrapper)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(fortran_unit_tests_wrapper)(void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"grow_forest_wrapper",    (DL_FUNC) &F77_NAME(grow_forest_wrapper),    21},
    {"grow_tree_wrapper",      (DL_FUNC) &F77_NAME(grow_tree_wrapper),      18},
    {"predict_forest_wrapper", (DL_FUNC) &F77_NAME(predict_forest_wrapper), 16},
    {"predict_tree_wrapper",   (DL_FUNC) &F77_NAME(predict_tree_wrapper),   15},
    {"fortran_unit_tests_wrapper",   (DL_FUNC) &F77_NAME(fortran_unit_tests_wrapper),   1},
    {NULL, NULL, 0}
};

void R_init_ParallelForest(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

