get_children_i <- function(tokens, i) {
  tokens = as_tokenindex(tokens)
  select = tokens[i,c('doc_id','sentence','token_id'), with=FALSE]
  data.table::setnames(select, c('doc_id','sentence','parent'))
  children = tokens[select, on=c('doc_id','sentence','parent'), nomatch=0, which=TRUE]
  if (length(children) > 0) children = union(children, get_children_i(tokens, children)) 
  union(i, children)
}



#' Set number of threads to be used by rsyntax functions
#' 
#' rsyntax relies heavily on the data.table package, which supports multithreading. 
#' By default, the number of threads set by data.table are used, as you can see with \code{\link[data.table]{getDTthreads}}.
#' Here you can set the number of threads for rsyntax functions, without affecting the data.table settings.
#'
#' @param threads The number of threads to use. Cannot be higher than number of threads used by data.table, which you can change with \code{\link[data.table]{setDTthreads}}. If left empty (NULL), all data.table threads are used
#'
#' @return Does not return a value. Sets the global 'rsyntax_threads' option.
#' @export
#'
#' @examples
#' current_threads = rsyntax_threads()
#' 
#' set_rsyntax_threads(2)
#' 
#' ## undo change (necessary for CRAN checks)
#' set_rsyntax_threads(current_threads)
set_rsyntax_threads <- function(threads=NULL) {
  options(rsyntax_threads = min(threads, data.table::getDTthreads()))
}

#' Get the number of threads to be used by rsyntax functions
#' 
#' rsyntax relies heavily on the data.table package, which supports multithreading. 
#' By default, the number of threads set by data.table are used, as you can see with \code{\link[data.table]{getDTthreads}}.
#' With \code{\link{set_rsyntax_threads}} you can set the number of threads for rsyntax functions, without affecting the data.table settings.
#'
#' @return the setting for the number of threads used by rsyntax
#' @export
#'
#' @examples
#' rsyntax_threads()
rsyntax_threads <- function() {
  go = options('rsyntax_threads')
  if (is.null(go$rsyntax_threads)) data.table::getDTthreads() else min(go$rsyntax_threads, data.table::getDTthreads())
}

bquote_s <- function(expr, where=parent.frame()) {
  ## bquote, but for an expression that is already substituted
  unquote <- function(e) if (is.pairlist(e)) 
    as.pairlist(lapply(e, unquote))
  else if (length(e) <= 1L) 
    e
  else if (e[[1L]] == as.name(".")) 
    eval(e[[2L]], where)
  else as.call(lapply(e, unquote))
  unquote(expr)
}

rm_nodes <- function(nodes, ids) {
  if (ncol(nodes) > 1) {
    drop = rep(TRUE, nrow(nodes))
    for (j in 2:ncol(nodes)) {
      drop = drop & nodes[[j]] %in% ids
    }
    nodes = nodes[!drop,]
  }
  nodes
}


