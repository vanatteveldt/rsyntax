#' Prepare a tokenIndex
#' 
#' @description
#' Creates a tokenIndex data.table. 
#' Accepts any data.frame given that the required columns (doc_id, sentence, token_id, parent, relation) are present.
#' The names of these columns must be one of the values specified in the respective arguments.
#'
#' The data in the data.frame will not be changed, with three exceptions. First, the columnnames will be changed if the default values are not used.
#' Second, if a token has itself as its parent (which in some parsers is used to indicate the root), the parent is set to NA (as used in other parsers) to prevent infinite cycles.
#' Third, the data will be sorted by doc_id, sentence, token_id.
#'
#' @param tokens     A data.frame, data.table, or tokenindex. 
#' @param doc_id     candidate names for the document id columns
#' @param sentence   candidate names for sentence (id/index) column
#' @param token_id   candidate names for the  token id column. Has to be numeric (Some parsers return token_id's as numbers with a prefix (t_1, w_1))
#' @param parent     candidate names for the parent id column. Has to be numeric
#' @param relation   candidate names for the relation column
#' @param paragraph  Optionally, the name of a column with paragraph ids. This is only necessary if sentences are numbered per paragraph, and therefore not unique within documents. If given, sentences are re-indexed to be unique within documents.
#'
#' @return   a tokenIndex
#' @export
#' @examples
#' as_tokenindex(tokens_corenlp)
as_tokenindex <- function(tokens, doc_id=c('doc_id','document_id'), sentence=c('sentence', 'sentence_id'), token_id=c('token_id'), parent=c('parent','head_token_id'), relation=c('relation','dep_rel'), paragraph=NULL) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  new_index = !methods::is(tokens, 'tokenIndex')
    
  ## if we can confirm that this is udpipe input, do not give a warning for missing parents
  is_udpipe = all(c('doc_id','token_id','head_token_id','dep_rel') %in% colnames(tokens))
  warn = !is_udpipe

  for (cols_obj in c('doc_id','sentence','token_id','parent','relation')) {
    cols = get(cols_obj)
    in_tokens = cols %in% colnames(tokens)
    if (!any(in_tokens)) stop(sprintf('None of the default values in c(%s) is a valid column in tokens', paste(cols,collapse=', ')))
    col = cols[which(in_tokens)[1]]
    assign(cols_obj, value = col)
  }
  
  if (!methods::is(tokens, 'data.table')) {
    tokens = data.table::data.table(tokens)
    data.table::setnames(tokens, old = c(doc_id, sentence, token_id, parent, relation), new=c('doc_id','sentence','token_id','parent', 'relation'))
  } else {
    ## if already a data.table, do not change by reference
    if (!all(c('doc_id','sentence','token_id','parent','relation') %in% colnames(tokens))) {
      colnames(tokens)[match(c(doc_id,sentence,token_id,parent,relation), colnames(tokens))] = c('doc_id','sentence','token_id','parent','relation')
    }
  }
  
  ## token_id and parent need to be identical (not integer vs numeric)
  tokens$token_id = as.numeric(tokens$token_id)   
  tokens$parent = as.numeric(tokens$parent)
  
  ## in some cases (such as udpipe) sentence_id has the sentence index, and sentence is a text column
  if ('sentence_id' %in% colnames(tokens)) {
    tokens$sentence_txt = tokens$sentence
    tokens$sentence = tokens$sentence_id
    tokens$sentence_id = NULL
  }
  if (!methods::is(tokens$sentence, 'numeric')) {
    if (methods::is(tokens$sentence, 'factor')) 
      tokens$sentence = as.numeric(tokens$sentence)
    else {
      ## create a counter that increments for every new sentence within a document 
      tokens$sentence = tokens$sentence != data.table::shift(tokens$sentence, 1, fill=NA) 
      tokens$sentence[1] = TRUE
      tokens[, sentence := cumsum(sentence), by='doc_id']
    }
  }
  
  if (!is.null(paragraph)) {
    browser()
    
    .sentence = NULL; sentence = NULL
    data.table::setorderv(tokens, c('doc_id',paragraph, 'sentence','token_id'))
    sents = unique(tokens[,c('doc_id',paragraph,'sentence'), with=F], by = c('doc_id',paragraph, 'sentence'))
    sents[, .sentence := 1:length(sentence), by=c('doc_id')]
    tokens[sents, .sentence := .sentence, on=c('doc_id',paragraph,'sentence')]  
  }
  if (anyDuplicated(tokens[,c('doc_id','sentence','token_id')])) stop('tokenIndex has duplicate doc_id - sentence - token_id tripples. This can for instance happen if sentences are numbered within paragraphs (sentence 1 in par 1, sentence 1 in par 2, etc). If this is the cause, you might solve it with the "paragraph" argument.')

  if (new_index) {
    is_own_parent = tokens$parent == tokens$token_id
    is_own_parent[is.na(is_own_parent)] = FALSE
    if (any(is_own_parent)) tokens$parent[is_own_parent] = NA
    levels(tokens$relation) = union(levels(tokens$relation), 'ROOT')
    tokens$relation[is.na(tokens$parent)] = 'ROOT'
  }

  has_keys = data.table::key(tokens)
  if (!identical(has_keys, c('doc_id','sentence','token_id'))) data.table::setkeyv(tokens, c('doc_id','sentence','token_id'))
  has_indices = data.table::indices(tokens)
  if (!'doc_id__sentence__parent' %in% has_indices) data.table::setindexv(tokens, c('doc_id','sentence','parent'))
  if (!'relation' %in% has_indices) data.table::setindexv(tokens, 'relation')
  
  if (new_index) {
    tokens = fix_missing_parents(tokens, warn)
    data.table::setattr(tokens, name = 'class', c('tokenIndex', class(tokens)))
  }
  
  
  tokens[]
}

fix_missing_parents <- function(tokens, warn=TRUE) {
  parent = NULL; relation = NULL
  parent_ids = stats::na.omit(unique(tokens[,c('doc_id','sentence','parent')]))
  data.table::setnames(parent_ids, old='parent', new='token_id')
  missing_parents = parent_ids[!tokens, on=c('doc_id','sentence','token_id')]
  if (warn && nrow(missing_parents) > 0) warning(sprintf('There are %s tokens with missing parents. These have now been made roots (parent = NA, relation="ROOT")', nrow(missing_parents)))
  
  if (nrow(missing_parents) > 0) {
    data.table::setnames(missing_parents, old='token_id', new='parent')
    i = tokens[missing_parents, on=c('doc_id','sentence','parent'), which=TRUE]
    tokens[i, parent := NA]
    tokens[i, relation := "ROOT"]
  }
  tokens
}


