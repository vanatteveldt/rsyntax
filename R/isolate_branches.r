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
#' plot_tree(tokens)
#' tokens2 = isolate_branch(tokens, relation = 'relcl', copy_parent = TRUE)
#' plot_tree(tokens2)
isolate_branch <- function(tokens, ..., copy_parent=T, copy_parent_fill=T) {
  parent = .ISOLATED = NULL
  
  tokens = data.table::copy(tokens)
  if (!copy_parent) {
    ## this is simply, because there can be no issues with nesting, so we can split everything in one go
    tq = tquery(label='parent',
                children(..., label='branch'))
    tokens = select_nodes(tokens, tq)
    tokens = mutate_nodes(tokens, 'branch', parent = NA, relation = 'ROOT', branch_parent=parent$token_id)
    return(tokens)  
  }
  
  ## if we do copy the parent, we need to do it recursively from root to bottom 
  tokens[, .ISOLATED := F]
  tq = tquery(label='parent', .ISOLATED=F,
                      children(..., label='branch'))
  
  
  tokens = rec_isolate(tokens, tq)
  tokens[, .ISOLATED := NULL]
}

rec_isolate <- function(tokens, tq) {
  parent_copy = parent = NULL
  
  tokens = select_nodes(tokens, tq, fill_only_first=T, .one_per_sentence = T)
  if (nrow(selected_nodes(tokens)$nodes) == 0) return(tokens)
  tokens = copy_nodes(tokens, 'parent', 'parent_copy', copy_fill=T)
  tokens = mutate_nodes(tokens, 'branch', parent = parent_copy$token_id)
  tokens = mutate_nodes(tokens, 'parent_copy', parent = NA, relation = 'ROOT', branch_parent=parent$parent, .ISOLATED=T)
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
#' tokens2 = isolate_branch(tokens, relation = 'relcl', copy_parent = TRUE)
#' get_branch_id(tokens2)
get_branch_id <- function(tokens) {
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
    i = tokens[parents, on=c('doc_id','sentence','token_id'), which=T]
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

split_conjunctions <- function(tokens) {
  ## ignore most fill nodes for distant conjunctions (>= 3 words)
  tokens = split_tree(tokens, rel='conj', no_fill=c('acl:relcl', 'relcl', 'conj', 'cop', 'acl', 'dobj', 'advmod','advcl','xcomp','obl','ccomp','aux','det'), min_dist = 3)
  ## copy most fill nodes for close conjunctions
  tokens = split_tree(tokens, rel='conj', no_fill=c('acl:relcl', 'relcl', 'conj', 'cop'), max_dist=2)
  chop(tokens, relation='cc')
}

split_tree <- function(tokens, rel='conj', no_fill=NULL, min_dist=0, max_dist=Inf) {
  tq = tquery(label='target', NOT(relation = rel),
              children(relation = c('compound*', 'flat', 'amod'), label='ignore', req=F),
              children(NOT(relation=rel), max_window=c(0,Inf), label='ignore2', req=F, connected=T),
              fill(NOT(relation = no_fill), max_window = c(Inf,Inf), connected=T),
              children(relation = rel, label='origin', min_window=min_dist, max_window = max_dist,
                       fill(NOT(relation = no_fill), max_window=c(0,Inf), connected=T)))
  ## ok, this requires some explanation
  ## essentially the tquery looks for an 'origin' node with a specific relation, and 
  ## its parent 'target'. We want the origin to copy the position of the target node,
  ## and to adopt certain fill nodes from the target, but not all.
  ## The arguments to limit fill are not sufficient, so we add two children queries (that have priority over fill) for more control
  ## The first one, with label "ignore", prevents compounds of the target
  ## The second one, "ignore2", prevents all nodes between the origin and target by taking all nodes untill the relation.
  
  tokens = climb_tree(tokens, tq)
}

print_sentences <- function(tokens, sentence_i=1, token_col='token') {
  doc_id = sentence = branch_parent_id = NULL
  
  sentences = unique(tokens[,c('doc_id','sentence')])
  if (sentence_i > nrow(sentences)) stop('sentence_i is higher than number of sentences in tokens')
  sents = get_branch_id(tokens[sentences[1,], on=c('doc_id','sentence')])
  
  bp = sents[!is.na(sents$branch_parent),c('doc_id','sentence','branch_parent','token_id')]
  bp = merge(bp, sents[,c('doc_id','sentence','token_id','branch_id')], by.x=c('doc_id','sentence','branch_parent'), by.y=c('doc_id','sentence','token_id'), all.x=T)
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


