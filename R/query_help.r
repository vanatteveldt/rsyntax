
#' Given a query, find all possible parents/children for a given node
#'
#' @param tokens    A tokenIndex
#' @param query     An rsyntax tquery
#' @param node      The name of a node (specified with 'label' in the tquery)
#' @param family    Either 'children', 'parents',  or 'family' (both children and parents)
#' @param depth     An integer for how deep to search. 1 is only children, 2 includes grandchilderen, 3 includes grandgrandchildren, etc.
#' @param group_cols Optionally, names of columns in tokens to group the results by. Groups are aggregated, and the number of times (and percentage) of each group is given. 
#'
#' @export
inspect_family <- function(tokens, query, node, family=c('family','children','parents'), depth=1, group_cols=NULL) {
  family = match.arg(family)
  ids = apply_queries(tokens, query)
  if (nrow(ids) == 0) {
    message('No nodes found')
    return(NULL)
  }
  if (!node %in% colnames(ids)) stop('node name does not exist (must be a name used in the label argument in tquery)')
  ids = subset(ids, select=c('doc_id','sentence',node))
  if (family == 'family') {
    if (!is.null(group_cols)) group_cols = union('family', group_cols)
    out = rbind(cbind(family='parent', token_family(tokens, ids, level = 'parents', depth=depth, show_level=T, replace=T)),
                cbind(family='child', token_family(tokens, ids, level = 'children', depth=depth, show_level=T, replace=T)))
  } else {
    out = token_family(tokens, ids, level = family, show_level=T, replace=T)
  }
  if (nrow(out) == 0) {
    message('no family found')
    return(NULL)
  } 
  data.table::setnames(out, '.FILL_LEVEL', 'depth')
  if (!is.null(group_cols)) {
    group_cols = union(c('family','depth'), group_cols)
    out = out[,list(.N),by=group_cols]
    out$pct = out$N / nrow(ids)
  }
  out
}

