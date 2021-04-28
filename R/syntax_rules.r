#' Apply queries created with \link{tquery}
#'
#' @param tokens  A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param ...      tqueries, as created with \link{tquery}. Can also be a list with tquery functions. It is recommended to use named arguments/lists, to name the tqueries. 
#' @param as_chain If TRUE, Nodes that have already been assigned assigned earlier in the chain will be ignored (see 'block' argument). 
#' @param block    Optionally, specify ids (doc_id - sentence - token_id triples) where find_nodes will stop (ignoring the id and recursive searches through the id). 
#'                 Can also be a data.table returned by (a previous) apply_queries, in which case all ids are blocked. 
#' @param check    If TRUE, return a warning if nodes occur in multiple patterns, which could indicate that the find_nodes query is not specific enough.
#' @param fill     If TRUE (default) the fill nodes are added. Otherwise these are ignored, even if the queries include fill()
#' @param return_wide If TRUE, return nodes in wide format.
#' @param verbose  If TRUE, report progress (only useful if multiple queries are used)
#'
#' @export
#' @return        A data.table in which each row is a node for which all conditions are satisfied, and each column is one of the linked nodes 
#'                (parents / children) with names as specified in the label argument.
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
#' nodes
apply_queries <- function(tokens, ..., as_chain=FALSE, block=NULL, check=FALSE, fill=TRUE, return_wide=FALSE, verbose=FALSE) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  tokens = as_tokenindex(tokens)
  r = list(...)
  
  is_tquery = sapply(r, methods::is, 'tQuery')
  r = c(r[is_tquery], unlist(r[!is_tquery], recursive = FALSE))
  
  out = vector('list', length(r))
  
  if (verbose) message('Applying queries:')
  for (i in 1:length(r)){
    if (!methods::is(r[[i]], 'tQuery')) next
    .TQUERY_NAME = names(r)[i]
    if (verbose) cat(paste0('\t', .TQUERY_NAME, '\n'))
    if (is.null(.TQUERY_NAME)) .TQUERY_NAME = ''
    if (grepl(',', .TQUERY_NAME)) stop('tquery name cannot contain a comma')
    .TQUERY_NAME = ifelse(.TQUERY_NAME == '', NA, as.character(.TQUERY_NAME))

    nodes = find_nodes(tokens, r[[i]], block=block, name=.TQUERY_NAME, fill=FALSE, melt = FALSE)
    
    if (!is.null(nodes)) {
      if (as_chain) block = get_long_ids(block, nodes)
      out[[i]] = nodes  
    }
  }
  
  
  if (fill && verbose) message('Adding fill nodes:')
  for (i in 1:length(r)) {
    if (is.null(out[[i]])) next
    if (fill) {
      if (verbose) cat(paste0('\t', names(r)[i], '\n'))
      out[[i]] = add_fill(tokens, out[[i]], r[[i]], block=block)
    }
    out[[i]] = if (return_wide) out[[i]] else melt_nodes_list(out[[i]])
  }
  
  nodes = data.table::rbindlist(out, fill=TRUE)
  #if (chain && !chain_fill && '.FILL_LEVEL' %in% colnames(d)) {
  #  data.table::setorder(d, '.FILL_LEVEL') 
  #  d = 
  #}
  class(nodes) = if (return_wide) c('rsyntaxNodesWide', class(nodes)) else c('rsyntaxNodes', class(nodes))
  nodes
} 
