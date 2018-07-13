#' Annotate quotes and clauses
#' 
#' A convenience function that calls the \link{annotate} function for a list of quote and clause queries. 
#' 
#' This function assumes that sources (of quotes) cannot also be subjects (of clauses). Therefore, the quote queries are applied first,
#' and source nodes are blocked when applying the clause queries.
#'
#' @param tokens        A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param quote_queries   A list with queries for extracting quotes, as created with \link{tquery}. For example, see \link{alpino_quote_queries}.
#' @param clause_queries  A list with queries for extracting clauses, as created with \link{tquery}. For example, see \link{alpino_clause_queries}.
#' @param check         For testing queries. If TRUE, give a warning if there are duplicates in the data (in which case duplicates are deleted)
#' @param with_tquery     For testing queries. If TRUE, add a column that shows the name of the specific tquery that was used. This only works if the queries are passed as a named list.
#'
#' @export
annotate_qc <- function(tokens, quote_queries, clause_queries, check=F, with_tquery=F) {
  tokens = as_tokenindex(tokens)
  tokens = annotate(tokens, quote_queries, column='quote', check=check, with_tquery=with_tquery)
  tokens = annotate(tokens, clause_queries, column='clause', check=check, with_tquery=with_tquery, block = tokens$quote == 'source')
  tokens
}


#' Annotate a tokenlist based on rsyntax queries
#'
#' Apply queries to extract syntax patterns, and add the results as two columns to a tokenlist.
#' One column contains the ids for each hit. The other column contains the annotations.
#' Only nodes that are given a name in the tquery (using the 'save' parameter) will be added as annotation.
#' 
#' Note that while queries only find 1 node for each saved component of a pattern (e.g., quote queries have 1 node for "source" and 1 node for "quote"), 
#' all children of these nodes are also annotated (if fill is TRUE). If a child has multiple ancestors, only the most direct ancestors are used (see documentation for the fill argument).
#' 
#' @param tokens     A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param queries      A tquery or a list of queries, as created with \link{tquery}. 
#' @param column     The name of the column in which the annotations are added. The unique ids are added as [column]_id
#' @param fill       If TRUE, the children for each id are added recursively (children of children etc.). If this leads to
#'                   duplicate ids (if an id in nodes is a child of another id in nodes), the most direct children are kept.
#'                   For example, if 1 -> 2 -> 3, and both 1 and 2 are in 'nodes', then 3 is only added as a child of 2.
#' @param block      Optionally, a \link{subset} expression (e.g., quote == 'source') to block certain nodes from being assigned. For example, to prevent nodes to be extracted as clause subjects if they have already been extracted as quote sources. 
#' @param check      For testing queries. If TRUE, give a warning if there are duplicates in the data (in which case duplicates are deleted)
#' @param with_tquery  For testing queries. If TRUE, add a column that shows the name of the specific tquery that was used. This only works if 'queries' is a named list.
#'
#' @export
annotate <- function(tokens, queries, column, use=NULL, fill=T, block=NULL, check=T, with_tquery=F, show_fill=F) {
  tokens = as_tokenindex(tokens)
  
  if (!is.null(substitute(block))) {
    e = eval(block, tokens, parent.frame())
    e[is.na(e)] = F
    block = subset(tokens, e, select = cname('doc_id','token_id'))
  }
  
  nodes = apply_queries(tokens, queries, as_chain=T, block=block, check=F) ## check is F, because check in annotate_nodes is broader
  annotate_nodes(tokens, nodes, column=column, use=use, fill=fill, fill_block=block, check=check, with_tquery=with_tquery, show_fill=show_fill) 
}


#' Annotate a tokenlist based on rsyntax queries
#'
#' Apply queries to extract syntax patterns, and add the results as two columns to a tokenlist.
#' One column contains the ids for each hit. The other column contains the annotations.
#' Only nodes that are given a name in the tquery (using the 'save' parameter) will be added as annotation.
#' 
#' Note that while queries only find 1 node for each saved component of a pattern (e.g., quote queries have 1 node for "source" and 1 node for "quote"), 
#' all children of these nodes are also annotated (if fill is TRUE). If a child has multiple ancestors, only the most direct ancestors are used (see documentation for the fill argument).
#' 
#' @param tokens     A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param nodes      A data.table, as created with \link{find_nodes} or \link{apply_queries}. Can be a list of multiple data.tables.
#' @param column     The name of the column in which the annotations are added. The unique ids are added as [column]_id
#' @param fill       If TRUE, the children for each id are added recursively (children of children etc.). If this leads to
#'                   duplicate ids (if an id in nodes is a child of another id in nodes), the most direct children are kept.
#'                   For example, if 1 -> 2 -> 3, and both 1 and 2 are in 'nodes', then 3 is only added as a child of 2.
#' @param fill_block Optionally, another data.table of nodes (as created with \link{find_nodes}) or a list of data.tables, used to block the fill process. That is, the nodes in block and all their descendants are not used in fill.
#' @param check      For testing queries. If TRUE, give a warning if there are duplicates in the data (in which case duplicates are deleted)
#' @param with_tquery  For testing queries. If TRUE, add a column that shows the name of the specific tquery that was used. This only works if 'queries' is a named list.
#'
#' @export
annotate_nodes <- function(tokens, nodes, column, use=NULL, fill=T, fill_block=NULL, check=T, with_tquery=F, show_fill=F) {
  id_column = paste0(column, '_id')
  if (column %in% colnames(tokens)) tokens[, (column) := NULL]
  if (id_column %in% colnames(tokens)) tokens[, (id_column) := NULL]
  
  if (nrow(nodes) == 0) {
    tokens[,(column) := factor()]
    tokens[,(id_column) := numeric()]
    return(tokens)
  }
    
  tokens = as_tokenindex(tokens)
  .NODES = prepare_long_nodes(tokens, nodes, use=use, fill=fill, check=check, fill_block=fill_block, show_fill=show_fill)
  data.table::setnames(.NODES, c('.ROLE','.KEY'), c(column, id_column))
  
  if (with_tquery) {
    data.table::setnames(.NODES, c('.TQUERY'), paste0(column, '_tquery'))
  } else {
    .NODES[,.TQUERY := NULL]
  }
  
  if (show_fill) {
    data.table::setnames(.NODES, c('.FILL'), paste0(column, '_tquery'))
  } 
  
  tokens = merge(tokens, .NODES, by=c(cname('doc_id'),cname('sentence'),cname('token_id')), all.x=T)
  as_tokenindex(tokens)
 
}

#' Transform the nodes to long format and match with token data
#'
#' @param tokens     A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param nodes      A data.table, as created with \link{find_nodes} or \link{apply_queries}. Can be a list of multiple data.tables.
#' @param use        Optionally, specify which columns from nodes to add. Other than convenient, this is slighly different 
#'                   from subsetting the columns in 'nodes' beforehand if fill is TRUE. When the children are collected,
#'                   the ids from the not-used columns are still blocked (see 'block')
#' @param fill       If TRUE, the children for each id are recursively (children of children etc.) added. If this leads to
#'                   duplicate ids (if an id in nodes is a child of another id in nodes), the most direct children are kept.
#'                   For example, if 1 -> 2 -> 3, and both 1 and 2 are in 'nodes', then 3 is only added as a child of 2. 
#' @param token_cols A character vector, specifying which columns from tokens to include in the output
#' @param block      Optionally, another set of nodes, of which the .KEY values will be blocked for annotations
#' @param show_fill  If TRUE, add a .FILL column that indicates which tokens were added with fill
#'
#' @return A data.table with the nodes in long format, and the specified token_cols attached 
#' @export
get_nodes <- function(tokens, nodes, use=NULL, fill=T, token_cols=c('token'), block=NULL, show_fill=F) {
  tokens = as_tokenindex(tokens)

  missing_col = setdiff(token_cols, colnames(tokens))
  if (length(missing_col) > 0) stop(sprintf('columns specified in token_cols arguments not found: %s', paste(missing_col, collapse=', ')))
  .NODES = prepare_long_nodes(tokens, nodes, use=use, fill=fill, check=F, fill_block=block, show_fill=show_fill)
  
  out = merge(.NODES, tokens, by=c(cname('doc_id'),cname('sentence'),cname('token_id')))
  if (show_fill) {
    subset(out, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),'.KEY','.ROLE', '.FILL', token_cols))
  } else {
    subset(out, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),'.KEY','.ROLE', token_cols))
  }
}
  
prepare_long_nodes <- function(tokens, nodes, use=NULL, fill=T, rm_dup=T, check=T, fill_block=NULL, show_fill=F) {
  use = if (is.null(use)) colnames(nodes) else union(c(cname('doc_id'),cname('sentence'), '.KEY'), use)
  if (!all(use %in% colnames(nodes))) stop('Invalid column names (for the nodes data.table) in the use argument')

  .NODES = subset(nodes, select=use) ## subset also prevents modifying by reference, even if all columns are used (so beware when changing this)
  .NODES = unique(data.table::melt(.NODES, id.vars=c(cname('doc_id'),cname('sentence'),'.KEY','.TQUERY'), variable.name='.ROLE', value.name=cname('token_id'), na.rm=T))
  
  has_duplicates = anyDuplicated(.NODES, by=c(cname('doc_id'),cname('sentence'),cname('token_id')))
  if (has_duplicates & rm_dup) {
    if (check) {
      warning("DUPLICATE NODES: Some tokens occur multiple times as nodes (either in different patterns or the same pattern). 
      Duplicates have now been deleted, but it's better (less ambiguous) to prevent duplicates by making the queries more specific")
    }
    .NODES = nodes
    .NODES[, .PATH := 1:.N]
    .NODES = data.table::melt(.NODES, id.vars=c(cname('doc_id'),cname('sentence'),'.PATH','.KEY', '.TQUERY'), variable.name='.ROLE', value.name=cname('token_id'), na.rm=T)
    .NODES = unique(.NODES)
    .NODES = rm_duplicates(.NODES)
    .NODES = unique(subset(.NODES, select = c(cname('doc_id'),cname('sentence'),'.KEY', '.ROLE', cname('token_id'), '.TQUERY')))
  }
  
  if (fill) {
    add = token_family(tokens, ids=.NODES[,c(cname('doc_id'),cname('sentence'),cname('token_id'))], level='children', depth=Inf, minimal=T, block=fill_block, replace = F)
    add = merge(add, .NODES, by.x=c(cname('doc_id'),cname('sentence'),'.MATCH_ID'), by.y=c(cname('doc_id'),cname('sentence'),cname('token_id')), allow.cartesian = T)
    if (show_fill) {
      .NODES[,.FILL := F]
      add[,.FILL := T]
    } 
    .NODES = rbind(.NODES, add[,colnames(.NODES), with=F])
  }
  data.table::setkeyv(.NODES, c(cname('doc_id'),cname('sentence'),cname('token_id')))
  .NODES
}

rm_duplicates <- function(nodes) {
  nodes = unique(nodes, by = c(cname('doc_id'),cname('sentence'),cname('token_id'))) ## sorted by name, for which the factor indices match the query order
  
  ## if there are incomplete .KEY's after removing duplicates, remove the entire pattern
  nodes[,complete := length(unique(.ROLE)) == nlevels(.ROLE), by=c(cname('doc_id'),cname('sentence'),'.KEY','.PATH')]
  nodes = subset(nodes, complete, select=setdiff(colnames(nodes), 'complete'))  
  
  nodes
}


