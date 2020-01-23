#' Parse text using udpipe
#'
#' Uses the cleanNLP package to parse text with the coreNLP parser.
#' To use, first download corenlp with \code{\link[cleanNLP]{cnlp_download_corenlp}}  
#' and initialize corenlp with \code{\link[cleanNLP]{cnlp_init_corenlp}}
#' 
#' If you want to use coreference resolution, make sure to initialize corenlp with anno_level = 3.
#'
#' @param text          A character vector
#' @param replace_coref If TRUE, replace coreference with main entity 
#'
#' @return A tokenindex
#' @export
#'
#' @examples
#' \donttest{
#' cnlp_download_corenlp()                ## first time only
#' 
#' cnlp_init_corenlp('en', anno_level=3)  ## initialize before use
#' 
#' corenlp_tokenindex('Bob said that he was tired')
#' }
corenlp_tokenindex <- function(text, replace_coref=F) {
  if (!cleanNLP:::volatiles$model_init_last == 'corenlp') {
    message('coreNLP has not yet been initialized. It will now be initialized for english with anno_level=3.')
    cleanNLP::cnlp_init_corenlp('en', anno_level = 3)
  }
    
  ann = cleanNLP::cnlp_annotate(text)
  tokens = cleanNLP::cnlp_get_token(ann, combine=F)
  
  tokens = adjusted_get_tokens(ann)
  
  conjs = tokens[tokens$relation == 'conj',]
  redundant = dplyr::semi_join(tokens, conjs, by=c('id','sid','tid'))
  redundant = dplyr::filter(redundant, !relation == 'conj')
  tokens = dplyr::anti_join(tokens, redundant, by=c('id','sid','tid','relation'))

  coref = cleanNLP::cnlp_get_coreference(ann)
  if (nrow(coref) > 0) {
    mainname = dplyr::filter(coref, mention_type=='PROPER')
    mainname = mainname[!duplicated(mainname[,c('rid')]),]
    mainname$rname = mainname$mention
    coref = dplyr::anti_join(coref, mainname, by=c('id','sid','tid'))
    
    tokens = dplyr::left_join(tokens, coref[,c('rid','id','sid','tid')], by=c('id','sid','tid'))
    tokens = dplyr::left_join(tokens, mainname[,c('rid','rname')], by='rid')
    if (!methods::is(tokens$rname, 'factor')) tokens$rname = as.factor(tokens$rname)
    levels(tokens$rname) = gsub(' ','_', levels(tokens$rname))
  } else {
    tokens$rid = NA
  }
  
  tokens$source[tokens$source == 0] = NA
  tokens = tokens[,!colnames(tokens) %in% c('word_source','lemma_source')]
  tokens = as_tokenindex(tokens, doc_id='id', sentence='sid', token_id='tid', parent='source', relation='relation')
  
  tokens = make_single_parent(tokens)
  
  data.table::setnames(tokens, c('word','pos'), c('token','POS'))
  if (replace_coref) {
    tokens[!is.na(tokens$rname),token := rname]
  }
  local_tokenid(tokens)
}

make_single_parent <- function(tokens) {
  ## right, soooo
  ## what we do here is say: if a node has more than one parent, only keep the parent that is
  ## highest in the tree, or if they are equally high the first in the sentence.
  
  ## Reason? coreNLP does not hold on to the single-parent property of dependency trees, but in
  ## rsyntax we do. Specifically, coreNLP gives a node multiple parents if it's parent has a child
  ## that is a conjunction, in which case the conjunction child is also made a parent. This is
  ## redundant, and in rsyntax we resolve these conjunction relations ourselves so that we can
  ## use some heuristics for deling with argument drop. 
  tokens = get_tree_level(tokens)
  
  dupl = tokens[duplicated(tokens, by=c('doc_id','sentence','token_id')), c('doc_id','sentence','token_id')]
  dupl = tokens[dupl, c('doc_id','sentence','parent','token_id')]
  data.table::setnames(dupl, c('parent','token_id'), c('token_id','real_token_id'))
  dupl_parents = tokens[dupl, c('doc_id','sentence','token_id','tree_level'), on=c('doc_id','sentence','token_id')]
  data.table::setnames(dupl_parents, 'tree_level','p_tree_level')
  dupl = merge(dupl, unique(dupl_parents), by=c('doc_id','sentence','token_id'), allow.cartesian=T)
  data.table::setorderv(dupl, c('p_tree_level','token_id'), c(1,1))
  remove = dupl[duplicated(dupl, by=c('doc_id','sentence','real_token_id')), c('doc_id','sentence','real_token_id','token_id')]
  data.table::setnames(remove, c('real_token_id','token_id'), c('token_id','parent'))
  tokens = tokens[!remove, on=c('doc_id','sentence','token_id','parent')]
  as_tokenindex(tokens)
}

get_tree_level <- function(tokens) {
  tokens[, tree_level := ifelse(is.na(tokens$parent), 1, NA)]
  
  i = 1
  while(TRUE) {
    select = tokens[list(tree_level=i), c('doc_id','sentence','token_id'), with=F, on='tree_level', nomatch=0]
    if (nrow(select) == 0) break
    data.table::setnames(select, c('doc_id','sentence','parent'))
    select[, tree_level := numeric()]
    tokens[select, tree_level := i+1, on=c('doc_id','sentence','parent','tree_level')]
    i = i + 1
  }
  tokens
}

adjusted_get_tokens <- function(ann) {
  #########   COPY PASTE FROM cleanNLP
  tokens = ann$token
  tid_target <- cid <- NULL
  dep <- cleanNLP::cnlp_get_dependency(ann)
  dep <- dplyr::left_join(dep, dplyr::select_(tokens, "id", 
                                              "sid", "tid", "word", "lemma"), by = c("id", "sid", "tid"))
  
  dep <- dplyr::select_(dep, "id", "sid", source = "tid", 
                        tid = "tid_target", "relation", word_source = "word", 
                        lemma_source = "lemma")
  tokens <- dplyr::left_join(tokens, dep, by = c("id", "sid", 
                                                 "tid"))
  tokens <- dplyr::left_join(tokens, cleanNLP::cnlp_get_sentence(ann), 
                             by = c("id", "sid"))
  
  ## increment sentence id (sid) by one
  ents = dplyr::select_(cleanNLP::cnlp_get_entity(ann), "-tid_end")
  ents$sid = ents$sid + 1
  tokens <- dplyr::left_join(tokens, ents, by = c("id", "sid", "tid"))
  tokens <- tokens[tokens$tid > 0, ]
  #################
  tokens
}

local_id <- function(group, i) {
  ## given global indices per group, make them locally unique
  ## has to be sorted on order(group, i)
  newgroup = which(!duplicated(group))
  repeat_add = c(newgroup[-1], length(group)+1) - newgroup
  group_start = rep(i[newgroup], repeat_add)
  (i - group_start) + 1
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

local_tokenid <- function(tokens) {
  tokens = data.table::as.data.table(tokens)
  parent_match = tokens[list(tokens$doc_id, tokens$sentence, tokens$parent),,on=c('doc_id','sentence', 'token_id'), which=T] ## efficient 3 column match that returns indices
  tokens$token_id = local_position(1:nrow(tokens), tokens$doc_id)   ## currently assuming that there are no gaps in token_ids. (otherwise need cumbersome solution from tokens_to_corpus function)
  tokens$parent = tokens$token_id[parent_match]
  tokens
} 


function() {
  "Bob said that John, who is an old guy, already left" %>%
    corenlp_tokenindex() %>%
    plot_tree(token, upos)
  
  
  "Bob said that John, who is an old guy, already left" %>%
    corenlp_tokenindex() %>%
    plot_tree(token, upos)
  
  
  "Bob said that John already left, and didn't stay" %>%
    corenlp_tokenindex() %>%
    plot_tree(token, upos)
}




