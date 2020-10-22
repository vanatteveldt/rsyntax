#' Isolate a branch in a dependency tree
#'
#' cuts of a branch at the nodes that match the lookup arguents (...).
#' A "branch_parent" column is added to the tokenindex, that indicates for the new roots
#' which node the parent was.  
#'
#' @param tokens   A tokenindex
#' @param ...      lookup arguments to find the node to split. For example, isolate_branch(tokens, relation='relcl') 
#'                 isolates branches of which the top node (the new root) has the relation "relcl". 
#' @param copy_parent If TRUE (default) copy the parent of the branch and include it in the isolated branch
#' @param copy_parent_fill If TRUE, also copy the parents fill nodes
#'
#' @return the tokenindex
#' @export
#'
#' @examples
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text4',]
#' tokens = as_tokenindex(tokens)
#' 
#' tokens2 = isolate_branch(tokens, relation = 'relcl', copy_parent = TRUE)
#' tokens2
#' \donttest{
#' if (interactive()) plot_tree(tokens2)
#' }
isolate_branch <- function(tokens, ..., copy_parent=TRUE, copy_parent_fill=TRUE) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  parent = .ISOLATED = NULL
  
  tokens = data.table::copy(tokens)
  if (!copy_parent) {
    ## in this case there can be no issues with nesting, so we can split everything in one go
    tq = tquery(label='parent',
                children(..., label='branch'))
    tokens = select_nodes(tokens, tq)
    tokens = mutate_nodes(tokens, 'branch', parent = NA, relation = 'ROOT', branch_parent=parent$token_id)
  } else {
    ## if we do copy the parent, we need to do it recursively from root to bottom 
    tokens[, .ISOLATED := FALSE]
    tq = tquery(label='parent', .ISOLATED=FALSE, fill=copy_parent_fill,
                        children(..., label='branch'))
    
    
    tokens = rec_isolate(tokens, tq)
    tokens[, .ISOLATED := NULL]
  }
  tokens
}

rec_isolate <- function(tokens, tq) {
  parent_copy = parent = NULL
  
  
  tokens = select_nodes(tokens, tq, fill_only_first=TRUE, .one_per_sentence = TRUE)
  if (nrow(selected_nodes(tokens)$nodes) == 0) return(tokens)
  tokens = copy_nodes(tokens, 'parent', 'parent_copy', only_new = F, copy_fill=TRUE)
  tokens = mutate_nodes(tokens, 'branch', parent = parent_copy$token_id)
  tokens = mutate_nodes(tokens, 'parent_copy', parent = NA, relation = 'ROOT', branch_parent=parent$parent, .ISOLATED=TRUE)
  rec_isolate(tokens, tq)
}

#' Add the branch id as a column to the tokenindex
#'
#' After splitting trees into branches 
#'
#' @param tokens  A tokenindex
#'
#' @return the tokenindex
#' @export
#'
#' @examples
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text4',]
#' tokens = as_tokenindex(tokens)
#' 
#' \donttest{
#' tokens2 = isolate_branch(tokens, relation = 'relcl', copy_parent = TRUE)
#' get_branch_id(tokens2)
#' }
get_branch_id <- function(tokens) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  branch_id = NULL
  
  tokens[, branch_id := ifelse(is.na(tokens$parent), tokens$token_id, NA)]
  tokens = fix_missing_parents(tokens)
  
  i = which(is.na(tokens$parent))
  safe_count = 1
  while(TRUE) {
    parents = tokens[i,c('doc_id','sentence','token_id','branch_id')]
    data.table::setnames(parents, 'token_id','parent')
    parents = merge(parents, tokens[,c('doc_id','sentence','token_id','parent')], by=c('doc_id','sentence','parent'))
    if (nrow(parents) == 0) break
    i = tokens[parents, on=c('doc_id','sentence','token_id'), which=TRUE]
    tokens[i, branch_id := parents$branch_id]
    
    if (safe_count == 200) {
      warning("stopped recursive loop at iteration 200. This is supposedly the depth of the tree, but
              since language is not THAT complex (unless you're working with German philosophers) it is
              most likely that something else went wrong. Please check your data or submit a bug report if its my fault")
    }
    safe_count = safe_count + 1
  }
  tokens
}

print_sentences <- function(tokens, sentence_i=1, token_col='token') {
  doc_id = sentence = branch_parent_id = NULL
  
  sentences = unique(tokens[,c('doc_id','sentence')])
  if (sentence_i > nrow(sentences)) stop('sentence_i is higher than number of sentences in tokens')
  sents = get_branch_id(tokens[sentences[1,], on=c('doc_id','sentence')])
  
  bp = sents[!is.na(sents$branch_parent),c('doc_id','sentence','branch_parent','token_id')]
  bp = merge(bp, sents[,c('doc_id','sentence','token_id','branch_id')], by.x=c('doc_id','sentence','branch_parent'), by.y=c('doc_id','sentence','token_id'), all.x=TRUE)
  sents[bp, branch_parent_id := bp$branch_id, on=c('doc_id','sentence','token_id')]
  
  get_bp <- function(x) if (any(!is.na(x))) first(stats::na.omit(x)) else numeric()
  sents = sents[,list(doc_id=unique(doc_id), sentence=unique(sentence), branch_parent=get_bp(branch_parent_id), text=paste(get(token_col), collapse=' ')), by='branch_id']
  
  for (i in which(is.na(sents$branch_parent))) {
    rec_print_sentences(sents, i)
    cat('\n')
  }
  tokens
}

rec_print_sentences <- function(sents, ivec, level=1) {
  if (length(ivec) == 0) return(NULL)
  for (i in ivec) {
    cat(rep('  ', level), gsub('\n', '', sents$text[i]), '\n')
    rec_print_sentences(sents, which(floor(sents$branch_parent) == floor(sents$branch_id[i])), level=level+1)
  }
}


