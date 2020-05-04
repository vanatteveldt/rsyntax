doc_level_token_ids <- function(tokens, keep_original) {
  token_id = parent = .TOKEN_DIFF = orig_token_id = orig_parent = NULL

  if (keep_original)  {
    .ORIGINAL_TOKEN_ID = tokens$token_id
    .ORIGINAL_PARENT_ID = tokens$parent
  }
  
  if (anyDuplicated(tokens[,c('doc_id','token_id')])) {
    .PARENT_INDEX = tokens[list(tokens$doc_id, tokens$sentence, tokens$parent),, on=c('doc_id','sentence','token_id'), which=T]
    
    # this would prevent token_ids that are globally uniuqe (across documents), but it would probably be overkill, and supposedly no parsers would do this.
    # tokens[, token_id := 1 + token_id - min(token_id), by='doc_id']
    
    tokens[, .TOKEN_DIFF := token_id - data.table::shift(token_id, fill = 0)]
    tokens[.TOKEN_DIFF < 1, .TOKEN_DIFF := 1]  ## keep higher token_diff to preserve gaps in the data (would affect word sequences and proximities)
    tokens[, token_id := cumsum(.TOKEN_DIFF), by='doc_id']
    
    tokens[, parent := token_id[.PARENT_INDEX]]
      
    tokens[, .TOKEN_DIFF := NULL]
  } else {
    if (keep_original) message('NOTE: keep_original is set to TRUE, but token_id was already unique within documents, so did not have to be changed. The orig_token_id and orig_parent have been added for consistency, but they are redundant')
  }
  
  if (keep_original) {
    tokens[, orig_token_id := .ORIGINAL_TOKEN_ID]
    tokens[, orig_parent := .ORIGINAL_PARENT_ID]
  }
  tokens[]
}


#' Create a tcorpus from a tokenindex
#' 
#' @description
#' This function creates a \code{\link[corpustools]{tCorpus}}. This is a corpus class for a tokenlist (and document meta data) that provides various functions for querying, analyzing
#' and visualizing text. 
#' 
#' In the tCorpus, token ids need to be locally unique within documents.
#' If this is not the case, the 'token_id' and 'parent' columns will be transformed. 
#' The new token_id and parent can still be used as normally within rsyntax, but there can be reasons to want to preserve the original token_id and parent.
#' Most importantly, the data might contain other columns that represent edges between tokens (e.g., coreference resolution). 
#' If copy_ids is TRUE, the original token_id and parent values will therefore be copied to the new columns orig_token_id and orig_parent.
#'
#' @param tokens     A tokenindex (\code{\link{as_tokenindex}})
#' @param meta       A data.frame with document meta data, where each row is a document, and columns contain the meta data per document. 
#'                   Needs to have a column named "doc_id", of which the values correspond to the doc_id in tokens. 
#' @param copy_ids   If TRUE, the original token_id and parent column will be copied to the columns orig_token_id and orig_parent. (see description for why)
#'
#' @export
tokenindex_tcorpus <- function(tokens, meta=NULL, copy_ids=F) {
  as_tokenindex(tokens)
  tokens = doc_level_token_ids(data.table::copy(tokens), keep_original=copy_ids)
  corpustools::tokens_to_tcorpus(tokens, meta=meta)
}

## the below can be used if we want to enable tcorpus as direct input to rsyntax functions (though it will be a bit messy, so probably best to wait until rsyntax is done for first release)

#if (methods::is(tokens, 'tCorpus')) {
#  tc = tokens
#  tokens = tcorpus_get_tokenindex(tokens)
#} else tc = NULL
#if (!is.null(tc)) tcorpus_set_tokenindex(tc, tokens) else tokens


tcorpus_get_tokenindex <- function(tc) {
  as_tokenindex(tc$tokens)
}

tcorpus_set_tokenindex <- function(tc, tokenindex) {
  ## this is only for the special case of updating the tokenindex after getting it with tcorpus_get_tokenindex
  data.table::setkeyv(tokenindex, c('doc_id','token_id'))
  tc$tokens = tokenindex
  tc
}
