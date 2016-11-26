#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP id(SEXP x)
{
    char buffer[32];
    snprintf(buffer, 32, "%p", (void *)x);
    return(ScalarString(mkChar(buffer)));
}
