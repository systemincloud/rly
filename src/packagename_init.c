#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP id(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"id", (DL_FUNC) &id, 1},
    {NULL, NULL, 0}
};

void R_init_rly(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

