#' Annotate a tokenlist based on rsyntax queries
#'
#' Apply queries to extract syntax patterns, and add the results as two columns to a tokenlist.
#' One column contains the ids for each hit. The other column contains the annotations.
#' Only nodes that are given a name in the tquery (using the 'label' parameter) will be added as annotation.
#' 
#' Note that while queries only find 1 node for each labeld component of a pattern (e.g., quote queries have 1 node for "source" and 1 node for "quote"), 
#' all children of these nodes can be annotated by settting fill to TRUE. If a child has multiple ancestors, only the most direct ancestors are used (see documentation for the fill argument).
#' 
#' @param tokens      A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param column      The name of the column in which the annotations are added. The unique ids are added as column_id
#' @param ...         One or multiple tqueries, or a list of queries, as created with \link{tquery}. Queries can be given a named by using a named argument, which will be used in the annotation_id to keep track of which query was used. 
#' @param block       Optionally, specify ids (doc_id - sentence - token_id triples) that are blocked from querying and filling (ignoring the id and recursive searches through the id). 
#' @param fill        Logical. If TRUE (default) also assign the fill nodes (as specified in the tquery). Otherwise these are ignored 
#' @param overwrite   If TRUE, existing column will be overwritten. Otherwise (default), the exsting annotations in the column will be blocked, and new annotations will be added. This is identical to using multiple queries.
#' @param block_fill  If TRUE (and overwrite is FALSE), the existing fill nodes will also be blocked. In other words, the new annotations will only be added if the 
#' @param copy        If TRUE (default), the data.table is copied. Otherwise, it is changed by reference. Changing by reference is faster and more memory efficient, but is not predictable R style, so is optional. 
#' 
#' @export
#' @return The tokenIndex with the annotation columns
#' @examples
#' ## spacy tokens for: Mary loves John, and Mary was loved by John
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("agent"), label = "subject"))
#' active =  tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("nsubj", "nsubjpass"), label = "subject"))
#' 
#' tokens = annotate(tokens, "clause", pas=passive, act=active)
#' 
#' tokens
#' plot_tree(tokens, annotation='clause')
annotate <- function(tokens, column, ..., block=NULL, fill=T, overwrite=F, block_fill=F, copy=T) {
  queries = list(...)
  is_tquery = sapply(queries, methods::is, 'tQuery')
  queries = c(queries[is_tquery], unlist(queries[!is_tquery], recursive = F))
  
  
  tokens = as_tokenindex(tokens)
  if (copy) tokens = data.table::copy(tokens)
  id_column = paste0(column, '_id')    
  fill_column = paste0(column, '_fill')
  
  
  #if (!is.null(bypass) || !is.null(isolate)) {
  #  tokens = simplify_tree(tokens, bypass=bypass, isolate=isolate, link_children = link_children)
  #}
  
  if (column %in% colnames(tokens)) {
    if (overwrite) {
      tokens[, (column) := NULL] 
      if (id_column %in% colnames(tokens)) tokens[, (id_column) := NULL]
      if (fill_column %in% colnames(tokens)) tokens[, (fill_column) := NULL]
    } else {
      if (!fill_column %in% colnames(tokens)) stop(sprintf('fill column (%s) is not available', fill_column))
      i = if (block_fill) which(!is.na(tokens[,get(fill_column)])) else which(tokens[,get(fill_column)] == 0)
      block = get_long_ids(block, tokens[i, c('doc_id','sentence','token_id')])
    }
  }
  nodes = apply_queries(tokens, queries, as_chain=T, block=block, fill=fill)
  
  if (nrow(nodes) == 0) {
    fill_column = paste0(column, '_fill')
    if (!column %in% colnames(tokens)) tokens[, (column) := factor()]
    if (!id_column %in% colnames(tokens)) tokens[, (id_column) := factor()]
    if (!fill_column %in% colnames(tokens)) tokens[, (fill_column) := double()]
    return(tokens[])
  }
  tokens = annotate_nodes(tokens, nodes, column=column)
  tokens[]
}

#' Annotate a tokenlist based on rsyntaxNodes
#' 
#' Use rsyntaxNodes, as created with \link{tquery} and \link{apply_queries}, to annotate a tokenlist.
#' Two columns will be added.
#' One column contains the ids for each hit. The other column contains the annotations.
#' Only nodes that are given a name in the tquery (using the 'label' parameter) will be added as annotation.
#' 
#' Note that you can also directly use \link{annotate}.
#' 
#' @param tokens  A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param nodes      A data.table, as created with \link{apply_queries}. Can be a list of multiple data.tables.
#' @param column     The name of the column in which the annotations are added. The unique ids are added as [column]_id
#'
#' @export
#' @return A data.table with nodes
#' 
#' @examples 
#' ## spacy tokens for: Mary loves John, and Mary was loved by John
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("agent"), label = "subject"))
#' active =  tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("nsubj", "nsubjpass"), label = "subject"))
#'
#' nodes = apply_queries(tokens, pas=passive, act=active)
#' annotate_nodes(tokens, nodes, 'clause')
annotate_nodes <- function(tokens, nodes, column) {
  .FILL_LEVEL = NULL
  
  tokens = as_tokenindex(tokens)
  if (nrow(nodes) == 0) stop('Cannot annotate nodes, because no nodes are provided')
  if (ncol(nodes) <= 3) stop('Cannot annotate nodes, because no nodes are specified (using the label parameter in find_nodes() or tquery())')
  id_column = paste0(column, '_id')
  fill_column = paste0(column, '_fill')

  #if (column %in% colnames(tokens)) tokens[, (column) := NULL]
  #if (id_column %in% colnames(tokens)) tokens[, (id_column) := NULL]
  #if (fill_column %in% colnames(tokens)) tokens[, (fill_column) := NULL]
  if (!column %in% colnames(tokens)) tokens[, (column) := factor()]
  if (!id_column %in% colnames(tokens)) tokens[, (id_column) := factor()]
  if (!fill_column %in% colnames(tokens)) tokens[, (fill_column) := double()]
  
  if (nrow(nodes) == 0) {
    #tokens[,(column) := factor()]
    #tokens[,(id_column) := numeric()]
    return(tokens)
  }
  
  .NODES = prepare_nodes(tokens, nodes) 
  i = tokens[.NODES, on=c('doc_id','sentence','token_id'), which=T]
  
  do_replace = .NODES[i, .FILL_LEVEL] < tokens[i, get(fill_column)]
  replace_row = which(do_replace | is.na(do_replace))
  i = i[replace_row]

  tokens[i, (column) := .NODES$.ROLE]
  tokens[i, (id_column) := .NODES$.ID]
  tokens[i, (fill_column) := .NODES$.FILL_LEVEL]
  
  #data.table::setnames(.NODES, c('.ROLE','.ID'), c(column, id_column))
  #if (show_fill) {
  #  data.table::setnames(.NODES, '.FILL_LEVEL', paste0(column, '_fill'))
  #} else {
  ##  .NODES[, .FILL_LEVEL := NULL]
  #}
  
  #tokens = merge(tokens, .NODES, by=c('doc_id','sentence','token_id'), all.x=T, allow.cartesian = T)
  
  
  #if (!is.factor(tokens[[column]])) tokens[[column]] = as.factor(tokens[[column]])
  #if (!is.factor(tokens[[id_column]])) tokens[[id_column]] = as.factor(tokens[[id_column]])
  as_tokenindex(tokens)
 
}




#' Transform the nodes to long format and match with token data
#'
#' @param tokens     A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param nodes      A data.table, as created with \link{apply_queries}. Can be a list of multiple data.tables.
#' @param use        Optionally, specify which columns from nodes to add. Other than convenient, this is slighly different 
#'                   from subsetting the columns in 'nodes' beforehand if fill is TRUE. When the children are collected,
#'                   the ids from the not-used columns are still blocked (see 'block')
#' @param token_cols A character vector, specifying which columns from tokens to include in the output
#'
#' @return A data.table with the nodes in long format, and the specified token_cols attached 
#' @export
#' @examples 
#' ## spacy tokens for: Mary loves John, and Mary was loved by John
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("agent"), label = "subject"))
#' active =  tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("nsubj", "nsubjpass"), label = "subject"))
#'
#' nodes = apply_queries(tokens, pas=passive, act=active)
#' get_nodes(tokens, nodes)
get_nodes <- function(tokens, nodes, use=NULL, token_cols=c('token')) {
  tokens = as_tokenindex(tokens)
  
  missing_col = setdiff(token_cols, colnames(tokens))
  if (length(missing_col) > 0) stop(sprintf('columns specified in token_cols arguments not found: %s', paste(missing_col, collapse=', ')))
  
  .NODES = prepare_nodes(tokens, nodes) 
  
  out = merge(.NODES, tokens, by=c('doc_id','sentence','token_id'))
  subset(out, select = c('doc_id','sentence','token_id','.ID','.ROLE', token_cols))
}
  

prepare_nodes <- function(tokens, nodes, use=NULL) {
  .ROLE = NULL
  #.NODES = data.table::copy(nodes)
  ##if (unique_fill) {
  ##  print('wtf')
  #  print(.NODES)
  #  dup_fill = duplicated(.NODES, by=c('doc_id','sentence','token_id')) & .NODES$.FILL_LEVEL > 0
  #  .NODES = subset(.NODES, !dup_fill)
  #}
  #print(.NODES)
  
  #still_dup = anyDuplicated(.NODES, by=c('doc_id','sentence','token_id'))
  #if (concat_dup && still_dup) {
  #  .SD=NULL
  #  .NODES = .NODES[,lapply(.SD, paste, collapse=','), by=eval(c('doc_id','sentence', 'token_id'))]
  #}
  .NODES = data.table::copy(unique(nodes, by = c('doc_id','sentence','token_id')))
  
  data.table::setkeyv(.NODES, c('doc_id','sentence','token_id'))
  if (!is.null(use)) .NODES = subset(.NODES, .ROLE %in% use)
  .NODES
}


rm_duplicates <- function(nodes) {
  dup = duplicated(nodes, by = c('doc_id','sentence','token_id'))
  dup_id = unique(nodes$.ID[dup])
  subset(nodes, !nodes$.ID %in% dup_id)
}
