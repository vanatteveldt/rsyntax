get_children_i <- function(tokens, i) {
  tokens = as_tokenindex(tokens)
  select = tokens[i,cname('doc_id','token_id'), with=F]
  data.table::setnames(select, c('doc_id','parent'))
  children = tokens[select, on=cname('doc_id','parent'), nomatch=0, which=T]
  if (length(children) > 0) children = union(children, get_children_i(tokens, children)) 
  union(i, children)
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

#' Create a tokens dataframe from a CoreNLP annotation object
#'
#' @param a coreNLP annotation as returned by coreNLP::annotate*
#'
#' @return a data frame compatible with getclauses / getquotes
#' @export
tokens_from_coreNLP <- function(a) {
  tokens = coreNLP::getToken(a)
  deps = coreNLP::getDependency(a, type="CCprocessed")
  deps = data.frame(sentence=deps$sentence, id=deps$dependentIdx, parent=deps$governorIdx, relation=deps$type)
  tokens = merge(tokens, deps)
  tokens$pos1 = substr(tokens$POS, 1, 1)
  tokens = plyr::arrange(tokens, sentence, id)
  unique_ids(tokens, context=tokens$sentence)
}