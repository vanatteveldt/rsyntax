get_children_i <- function(tokens, i) {
  tokens = as_tokenindex(tokens)
  select = tokens[i,c('doc_id','sentence','token_id'), with=F]
  data.table::setnames(select, c('doc_id','sentence','parent'))
  children = tokens[select, on=c('doc_id','sentence','parent'), nomatch=0, which=T]
  if (length(children) > 0) children = union(children, get_children_i(tokens, children)) 
  union(i, children)
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
    drop = rep(T, nrow(nodes))
    for (j in 2:ncol(nodes)) {
      drop = drop & nodes[[j]] %in% ids
    }
    nodes = nodes[!drop,]
  }
  nodes
}

#' Construct a list of source/subject/object triples from a tokens list with clauses and quotes
#'
#' @param tokens a data frame with clause_id, clause_role, quote_id and quote_role columns
#' @param concept_column the name of the column in tokens that contains identified 'concepts'
#'
#' @return a data frame with clause_id and source, subject, object columns indicating concepts found in those positions
#' @export
construct_triples <- function(tokens, concept_column="concept") {
  sources = tokens[!is.na(tokens$quote_role) & tokens$quote_role == "source" & !is.na(tokens[[concept_column]]), c(concept_column, "quote_id")]
  sources = unique(merge(sources, tokens[!is.na(tokens$clause_id), c("clause_id", "quote_id")]))[c("clause_id", concept_column)]
  colnames(sources)[2] = "source"
  
  subjects = unique(tokens[!is.na(tokens$clause_id) & !is.na(tokens[[concept_column]]) & tokens$clause_role == "subject", c("clause_id", concept_column)])
  colnames(subjects)[2] = "subject"
  objects = unique(tokens[!is.na(tokens$clause_id) & !is.na(tokens[[concept_column]]) & tokens$clause_role == "predicate", c("clause_id", concept_column)])
  colnames(objects)[2] = "object"
  
  merge(sources, merge(subjects, objects, all=T), all=T)
}




