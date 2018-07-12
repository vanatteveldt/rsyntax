local_id <- function(group, i) {
  ## given global indices per group, make them locally unique
  ## has to be sorted on order(group, i)
  newgroup = which(!duplicated(group))
  repeat_add = c(newgroup[-1], length(group)+1) - newgroup
  group_start = rep(i[newgroup], repeat_add)
  (i - group_start) + 1
}

global_id <- function(group, i, window=NA) {
  ## given local indices per group, make them globally unique
  ## has to be sorted on order(group, i)
  if (!length(unique(group)) == 1) {
    newgroup = which(!duplicated(group)) # where does a new group start
    
    group.max = i[newgroup-1] # the highest value of each group
    if (!is.na(window)) group.max = group.max + window # increase the highest value of each group with max_window_size to make sure windows of different groups do not overlap.
    add_scores = cumsum(c(0,group.max)) # the amount that should be added to the i at the start of each group
    
    repeat_add = c(newgroup[-1], length(i)+1) - newgroup # the number of times the add scores need to be repeated to match the i vector
    i + rep(add_scores, repeat_add)
  } else {
    i
  }
}

local_position <- function(position, context, presorted=F){
  if (!presorted){
    ord = order(context, position)
    position = position[ord]
    context = context[ord]
  }
  position = local_id(context, position)
  if (!presorted) position = position[match(1:length(position), ord)]
  position
}

global_position <- function(position, context, max_window_size=NA, presorted=F, position_is_local=F){
  ## makes the (token) position counter global with dummy positions between contexts to prevent overlapping windows (so it can be used as an index).
  ## this way, overlapping token windows can be calculated for multiple documents within a single matrix.
  ## position and context need to be sorted on order(context,position). If this is already the case, presorted=T can be used for a speed up
  if (!presorted){
    ord = order(context, position)
    position = position[ord]
    context = context[ord]
  }
  
  ## first, make sure position is local and starts at 1 for each context (otherwise the global id can become absurdly high)
  if (!position_is_local) position = local_position(position, context, presorted=T)
  
  if (min(position) == 0) position = position + 1 ## position will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)
  
  position = global_id(context, position, max_window_size)
  if (!presorted) position = position[match(1:length(position), ord)]
  position
}


make_ids_local <- function(tokens, token_is_local, sent_is_local=NULL, sentence_col=NULL) {
  ndoc = nrow(unique(tokens, by='doc_id'))
  if (!is.null(sentence_col)){
    #if (sent_is_local) {
    #    #if (ndoc > 10) if (!anyDuplicated(unique(tokens, by=c('doc_id','sentence')), by='sentence') == 0) warning("Sentence positions (sentence) do not appear to be locally unique within documents (no duplicates in at least 10 documents). Unless you are sure they are, set sent_is_local to FALSE (and read documentation)")
    #}
    if (!sent_is_local) tokens[,'sentence' := local_position(tokens$sentence, tokens$doc_id, presorted = T)] ## make sure sentences are locally unique within documents (and not globally)
    if (!token_is_local) tokens[,'token_id' := global_position(tokens$token_id,
                                                               global_position(tokens$sentence, tokens$doc_id, presorted = T, position_is_local=T),
                                                               presorted = T)]  ## make token positions globally unique, taking sentence id into account (in case tokens are locally unique within sentences)
  }
  if (token_is_local) {
    if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) warning("Duplicate token ids (doc_id - token_id pairs) found. If token ids are not local at the document level, you can set token_is_local to False to use a token's position within a document as the token ids")
    #if (ndoc > 10) if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) warning("token positions (token_id) do not appear to be locally unique within documents (no duplicates in at least 10 documents). Unless you are sure they are, set token_is_local to FALSE (and read documentation)")
  } else {
    tokens[,'token_id' := local_position(tokens$token_id, tokens$doc_id, presorted=T)] ## make tokens locally unique within documents
  }
}