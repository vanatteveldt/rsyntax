#' Annotate a tokenlist with the results of \link{find_nodes}
#'
#' Use the results of \link{find_nodes} (nodes) to add two columns to a tokenlist.
#' One column contains the ids for each match.
#' The other column contains the annotations, using the column names of 'nodes'.
#' 
#' @param tokens  A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param nodes      A data.table, as created with \link{find_nodes} or \link{apply_rules}. Can be a list of multiple data.tables.
#' @param column     The name of the column in which the annotations are added. The unique ids are added as [column]_id
#' @param use        Optionally, specify which columns from nodes to add. Other than convenient, this is slighly different 
#'                   from subsetting the columns in 'nodes' beforehand if fill is TRUE. When the children are collected,
#'                   the ids from the not-used columns are still blocked (see 'block')
#' @param fill       If TRUE, the children for each id are recursively (children of children etc.) added. If this leads to
#'                   duplicate ids (if an id in nodes is a child of another id in nodes), the most direct children are kept.
#'                   For example, if 1 -> 2 -> 3, and both 1 and 2 are in 'nodes', then 3 is only added as a child of 2. 
#' @param check      If TRUE, give a warning if there are duplicates in the data (in which case duplicates are deleted)
#' @param block      Optionally, another set of nodes, of which the .KEY values will be blocked for annotations
#'
#' @export
annotate <- function(tokens, nodes, column, use=NULL, fill=T, check=T, block=NULL) {
  tokens = as_tokenindex(tokens)
  .NODES = prepare_long_nodes(tokens, nodes, use=use, fill=fill, rm_dup=rm_dup, block=block)

  id_column = paste0(column, '_id')
  data.table::setnames(.NODES, c('.ROLE','.KEY'), c(column, id_column))
  
  if (column %in% colnames(tokens)) tokens[, (column) := NULL]
  if (id_column %in% colnames(tokens)) tokens[, (id_column) := NULL]

  tokens = merge(tokens, .NODES, by=c(cname('doc_id'),cname('token_id')), all.x=T)
  as_tokenindex(tokens)
}


#' Transform the nodes to long format and match with token data
#'
#' @param tokens     A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param nodes      A data.table, as created with \link{find_nodes} or \link{apply_rules}. Can be a list of multiple data.tables.
#' @param use        Optionally, specify which columns from nodes to add. Other than convenient, this is slighly different 
#'                   from subsetting the columns in 'nodes' beforehand if fill is TRUE. When the children are collected,
#'                   the ids from the not-used columns are still blocked (see 'block')
#' @param fill       If TRUE, the children for each id are recursively (children of children etc.) added. If this leads to
#'                   duplicate ids (if an id in nodes is a child of another id in nodes), the most direct children are kept.
#'                   For example, if 1 -> 2 -> 3, and both 1 and 2 are in 'nodes', then 3 is only added as a child of 2. 
#' @param token_cols A character vector, specifying which columns from tokens to include in the output
#' @param block      Optionally, another set of nodes, of which the .KEY values will be blocked for annotations
#'
#' @return A data.table with the nodes in long format, and the specified token_cols attached 
#' @export
get_nodes <- function(tokens, nodes, use=NULL, fill=T, token_cols=c('token'), block=NULL) {
  tokens = as_tokenindex(tokens)

  missing_col = setdiff(token_cols, colnames(tokens))
  if (length(missing_col) > 0) stop(sprintf('columns specified in token_cols arguments not found: %s', paste(missing_col, collapse=', ')))

  .NODES = prepare_long_nodes(tokens, nodes, use=use, fill=fill, rm_dup=F, check=F, block=block)

  out = merge(.NODES, tokens, by=c(cname('doc_id'),cname('token_id')))
  subset(out, select = c(cname('doc_id'),cname('token_id'),'.KEY','.ROLE', token_cols))
}

prepare_long_nodes <- function(tokens, nodes, use=NULL, fill=T, rm_dup=T, check=T, block=NULL) {
  use = if (is.null(use)) colnames(nodes) else union(c(cname('doc_id'), '.KEY'), use)
  if (!all(use %in% colnames(nodes))) stop('Invalid column names (for the nodes data.table) in the use argument')
  if (!is.null(block)) {
    if (is(block, 'data.table')) block = block_ids(block)
    nodes = nodes[!list(block[[1]], block[[2]]), on=c(cname('doc_id'),'.KEY')]
  }
  
  .NODES = subset(nodes, select=use) ## subset also prevents modifying by reference, even if all columns are used (so beware when changing this)
  
  .NODES = unique(data.table::melt(.NODES, id.vars=c(cname('doc_id'),'.KEY'), variable.name='.ROLE', value.name=cname('token_id'), na.rm=T))
  
  if (anyDuplicated(.NODES, by=c(cname('doc_id'),cname('token_id')))) {
    if (check) {
      mes = 'DUPLICATE NODES: Some tokens occur multiple times as nodes (either in different patterns or the same pattern). 
      Where possible, make the patterns more specific. Alternatively, and to ignore incidental duplicates, set rm_dup 
      (remove duplicates) to TRUE'
      if (!rm_dup) stop(mes) else warning(mes)
    } else {
      .NODES = nodes
      .NODES[, .PATH := 1:.N]
      .NODES = data.table::melt(.NODES, id.vars=c(cname('doc_id'),'.PATH','.KEY'), variable.name='.ROLE', value.name=cname('token_id'), na.rm=T)
      .NODES = unique(.NODES)
      .NODES = rm_duplicates(.NODES)
    }
    .NODES = unique(subset(.NODES, select = c(cname('doc_id'), '.KEY', '.ROLE', '.G_ID')))
  }
  #if (!is.null(block)) {
  #  if (is(block, 'data.table')) block = block_ids(block)
  #  .NODES = .NODES[!block, on=c(cname('doc_id'),cname('token_id'))]
  #}
  
  if (fill) {
    add = token_family(tokens, ids=.NODES[,c(cname('doc_id'),cname('token_id'))], rel=NULL, not_rel=NULL, level='children', depth=Inf, minimal=T, block=block, replace = F)
    add = merge(add, .NODES, by.x=c(cname('doc_id'),'.MATCH_ID'), by.y=c(cname('doc_id'),cname('token_id')), allow.cartesian = T)
    .NODES = rbind(.NODES, add[,colnames(.NODES), with=F])
  }
  data.table::setkeyv(.NODES, c(cname('doc_id'),cname('token_id')))
  .NODES
}

rm_duplicates <- function(nodes) {
  nodes = unique(nodes, by = c(cname('doc_id'),cname('token_id'))) ## sorted by name, for which the factor indices match the query order
  
  ## if there are incomplete .KEY's after removing duplicates, remove the entire pattern
  nodes[,complete := length(unique(.ROLE)) == nlevels(.ROLE), by=c(cname('doc_id'),'.KEY','.PATH')]
  nodes = subset(nodes, complete, select=setdiff(colnames(nodes), 'complete'))  
  
  nodes
}


