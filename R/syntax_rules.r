#' Apply queries created with \link{tquery}
#'
#' @param tokens   A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param ...      tqueries, as created with \link{tquery}. Can also be a list with tquery functions. It is recommended to use named arguments/lists, to name the tqueries. 
#' @param as_chain If TRUE, Nodes that have already been assigned assigned earlier in the chain will be ignored (see 'block' argument). Note that duplicate nodes can also easily be ignored afterwards in annotate_nodes().
#' @param block    Optionally, specify ids (doc_id - token_id pairs) where find_nodes will stop (ignoring the id and recursive searches through the id). 
#'                 Can also be a data.table returned by (a previous) apply_queries, in which case all ids are blocked. 
#' @param check    If TRUE, return a warning if nodes occur in multiple patterns, which could indicate that the find_nodes query is not specific enough.
#'
#' @return        A data.table in which each row is a node for which all conditions are satisfied, and each column is one of the linked nodes 
#'                (parents / children) with names as specified in the save argument.
#'                
#' @examples
#' ## it's convenient to first prepare vectors with relevant words/pos-tags/relations
#' .SAY_VERBS = c("tell", "show","say", "speak") ## etc.
#' .QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
#' .SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 
#' 
#' quotes_direct = tquery(select = lemma %in% .SAY_VERBS,
#'                          children(save = 'source', p_rel = .SUBJECT_RELS),
#'                          children(save = 'quote', p_rel = .QUOTE_RELS))
#' quotes_direct ## print shows tquery
#' 
#' tokens = subset(tokens_corenlp, sentence == 1)
#' 
#' nodes = apply_queries(tokens, quotes_direct)
#' nodes
#' annotate(tokens, nodes, column = 'example')
#' 
#' @export
apply_queries <- function(tokens, ..., as_chain=F, block=NULL, check=T) {
  r = list(...)
  
  is_tquery = sapply(r, is, 'tQuery')
  r = c(r[is_tquery], unlist(r[!is_tquery], recursive = F))
  
  out = vector('list', length(r))
  
  for (i in 1:length(r)){
    .TQUERY_NAME = names(r)[i]
    if (grepl(',', .TQUERY_NAME)) stop('tquery name cannot contain a comma')
    .TQUERY_NAME = ifelse(is.null(.TQUERY_NAME), paste0('tq', i), as.character(.TQUERY_NAME))
    args = r[[i]]
    #nodes = find_nodes(tokens, )
    #nodes = do.call(r[[i]], args = list(tokens=tokens, block=block, check=check, e=parent.frame()))
    l = c(args$lookup, args$nested, list(tokens=tokens, select=args$select, g_id=args$g_id, save=args$save, block=block, check=check, name=.TQUERY_NAME, e=parent.frame()))
    nodes = do.call(find_nodes, args = l)
    if (!is.null(nodes)) {
      #nodes[,.TQUERY := .TQUERY_NAME]
      if (as_chain) block = block_ids(block, nodes)
      out[[i]] = nodes  
    }
  }
  nodes = data.table::rbindlist(out, fill=T)
  class(nodes) = c('rsyntaxNodes', class(nodes))
  nodes
} 


