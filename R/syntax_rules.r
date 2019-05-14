#' Apply queries created with \link{tquery}
#'
#' @param tokens  A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param ...      tqueries, as created with \link{tquery}. Can also be a list with tquery functions. It is recommended to use named arguments/lists, to name the tqueries. 
#' @param as_chain If TRUE, Nodes that have already been assigned assigned earlier in the chain will be ignored (see 'block' argument). 
#' @param block    Optionally, specify ids (doc_id - sentence - token_id triples) where find_nodes will stop (ignoring the id and recursive searches through the id). 
#'                 Can also be a data.table returned by (a previous) apply_queries, in which case all ids are blocked. 
#' @param check    If TRUE, return a warning if nodes occur in multiple patterns, which could indicate that the find_nodes query is not specific enough.
#' @param fill     If TRUE (default) the fill nodes are added. Otherwise these are ignored, even if the queries include fill()
#'
#' @return        A data.table in which each row is a node for which all conditions are satisfied, and each column is one of the linked nodes 
#'                (parents / children) with names as specified in the label argument.
#'                
#' @examples
#' ## it's convenient to first prepare vectors with relevant words/pos-tags/relations
#' .SAY_VERBS = c("tell", "show","say", "speak") ## etc.
#' .QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
#' .SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 
#' 
#' quotes_direct = tquery(lemma = .SAY_VERBS,
#'                          children(label = 'source', relation = .SUBJECT_RELS),
#'                          children(label = 'quote', relation = .QUOTE_RELS))
#' quotes_direct ## print shows tquery
#' 
#' tokens = subset(tokens_corenlp, sentence == 1)
#' 
#' nodes = apply_queries(tokens, quotes_direct)
#' nodes
#' annotate(tokens, nodes, column = 'example')
#' 
#' @export
apply_queries <- function(tokens, ..., as_chain=F, block=NULL, check=F, fill=T) {
  tokens = as_tokenindex(tokens)
  r = list(...)
  
  is_tquery = sapply(r, methods::is, 'tQuery')
  r = c(r[is_tquery], unlist(r[!is_tquery], recursive = F))
  
  out = vector('list', length(r))
  
  for (i in 1:length(r)){
    if (!methods::is(r[[i]], 'tQuery')) next
    .TQUERY_NAME = names(r)[i]
    if (is.null(.TQUERY_NAME)) .TQUERY_NAME = ''
    if (grepl(',', .TQUERY_NAME)) stop('tquery name cannot contain a comma')
    .TQUERY_NAME = ifelse(.TQUERY_NAME == '', NA, as.character(.TQUERY_NAME))
    
    nodes = find_nodes(tokens, r[[i]], block=block, name=.TQUERY_NAME, fill=F, melt = F)
   
    if (!is.null(nodes)) {
      if (as_chain) block = get_long_ids(block, nodes)
      out[[i]] = nodes  
    }
  }
  
  
  for (i in 1:length(r)) {
    if (is.null(out[[i]])) next
    if (fill) out[[i]] = add_fill(tokens, out[[i]], r[[i]], block=block)
    out[[i]] = melt_nodes_list(out[[i]])
  }
  

  nodes = data.table::rbindlist(out, fill=T)
  #if (chain && !chain_fill && '.FILL_LEVEL' %in% colnames(d)) {
  #  data.table::setorder(d, '.FILL_LEVEL') 
  #  d = 
  #}
  class(nodes) = c('rsyntaxNodes', class(nodes))
  nodes
} 
