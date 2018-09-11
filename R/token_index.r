#' Prepare a tokenIndex
#' 
#' Creates a tokenIndex data.table, that is required to use \link{find_nodes}. 
#' Accepts any data.frame given that the required columns (doc_id, sentence, token_id, parent) are present.
#' Alternative column names can be given.
#' 
#' The data will be sorted by the doc_id, sentence and token_id columns.
#' Accordingly, it is recommended to use numeric token_id's. 
#' Some parsers return token_id's as numbers with a prefix (t_1, w_1), in which case sorting is inconvenient (t_15 > t_100).
#'
#' The data in the data.frame will not be changed, with two exceptions. First, the columnnames will be changed if the default values are not used.
#' Second, if a token has itself as its parent (which in some parsers is used to indicate the root), the parent is set to NA (as used in other parsers) to prevent infinite cycles.
#'
#' @param tokens     A data.frame, data.table, or tokenindex. 
#' @param doc_id     The name of the document id columns
#' @param sentence   The name of the sentence (id/index) column
#' @param token_id   The name of the token id column
#' @param parent     The name of the parent id column
#'
#' @export
as_tokenindex <- function(tokens, doc_id='doc_id', sentence='sentence', token_id='token_id', parent='parent') {
  new_index = !is(tokens, 'tokenIndex')
  if (!is(tokens, 'data.table')) {
    tokens = data.table::data.table(tokens)
    data.table::setnames(tokens, old = c(doc_id, sentence, token_id, parent), new=c('doc_id','sentence','token_id','parent'))
  } else {
    ## if already a data.table, do not change by reference
    if (!all(c('doc_id','sentence','token_id','parent') %in% colnames(tokens))) {
      colnames(tokens)[match(c(doc_id,sentence,token_id,parent), colnames(tokens))] = c('doc_id','sentence','token_id','parent')
    }
  }

  has_keys = data.table::key(tokens)
  if (!identical(has_keys, c('doc_id','sentence','token_id'))) data.table::setkeyv(tokens, c('doc_id','sentence','token_id'))
  has_indices = data.table::indices(tokens)
  doc_id__sentence__parent = paste(c('doc_id','sentence','parent'), collapse='__')     ## paired doc_id__sentence__parent index
  if (!doc_id__sentence__parent %in% has_indices) data.table::setindexv(tokens, c('doc_id','sentence','parent'))
  
  if (new_index) {
    check_tokens(tokens)
    is_own_parent = tokens$parent == tokens$token_id
    is_own_parent[is.na(is_own_parent)] = F
    if (any(is_own_parent)) tokens$parent[is_own_parent] = NA
    data.table::setattr(tokens, name = 'class', c('tokenIndex', class(tokens)))
  }
  
  if (is(tokens$token_id, 'numeric') && is(tokens$parent, 'numeric')) {
    tokens$token_id = as.numeric(tokens$token_id)   ## token_id and parent need to be identical (not integer vs numeric)
    tokens$parent = as.numeric(tokens$parent)
  } else {
    tokens$token_id = as.factor(as.character(tokens$token_id))
    tokens$parent = as.factor(as.character(tokens$parent))
  }
  tokens
}

check_tokens <- function(tokens) {
  missing_parents = tokens[!tokens[,c('doc_id','sentence','token_id'), with=F], on=c('doc_id','sentence','token_id')]
  if (nrow(missing_parents) > 0) warning(sprintf('There are %s tokens with missing parents', nrow(missing_parents)))
}

