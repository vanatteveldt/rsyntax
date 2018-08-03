#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector local_ids_template( const Vector<RTYPE>& context) {
  IntegerVector out(context.size());
  int id = 1;
  
  for (int i = 0; i < context.size(); i++) {
    if (i > 0 && context[i] != context[i-1]) 
      id = 1;
    out[i] = id;
    id++;
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector local_ids(SEXP context) {
  switch( TYPEOF(context) ) {
  case INTSXP: return local_ids_template<INTSXP>(context);
  case REALSXP: return local_ids_template<REALSXP>(context);
  case STRSXP: return local_ids_template<STRSXP>(context);
  }
  return R_NilValue;
}


/*** R
local_ids(c(1,1,1,1,2,2,2,3,3))
local_ids(c('a','a','b','b','c','c','c','d'))
)*/
