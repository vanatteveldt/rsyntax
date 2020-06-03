#' Split conjunctions in spacy tokens 
#'
#' The specific problem of splitting conjunctions is rather complicated because it requires recursion (for conjunctions in conjunctions) and needs to somehow deal with argument drop. 
#' In the sentence: "Bob ate bread and cheese", we cannot simply split the sentence into "Bob ate bread" and "cheese". We need to copy the implicit arguments to get "Bob ate bread" and "Bob ate cheese".
#' 
#' Note that this function is mainly provided for demonstration purposes.
#' The goal of the rsyntax package is to provide the tools to query and reshape dependency trees, and (at least for now) we want to keep applications such as this function separated.
#' This specific implementation is also not perfect, and for complex sentences other forms of text simplification would ideally be performed first (e.g., isolating relative clauses).
#'
#' @param tokens     a tokenIndex based on texts parsed with \code{\link[spacyr]{spacy_parse}} (with dependency=TRUE)
#'
#' @return  the tokenIndex with conjunctions split into separate isolated branches.
#' @export
#'
#' @examples
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text5',]
#' 
#' \donttest{
#' tokens %>%
#'    spacy_split_conjunctions() %>%
#'    plot_tree()
#'  }
spacy_split_conjunctions <- function(tokens) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  
  tokens = as_tokenindex(tokens)
  tokens = split_tree(tokens, rel='conj', 
                            no_fill=c('acl:relcl','acl','appos','relcl', 'cop', 
                                      'advmod','advcl','xcomp','obl','ccomp','aux','det'), 
                            min_dist = 3)
  tokens = split_tree(tokens, rel='conj', no_fill=c('acl:relcl','relcl', 'conj', 'cop'))
  tokens = chop(tokens, relation = 'cc')
  tokens
}

split_tree <- function(tokens, rel='conj', no_fill=NULL, min_dist=0, max_dist=Inf, compound = c('compound*','flat')) {
  tq = tquery(label='target', NOT(relation = rel),
              children(relation = compound, label='ignore', req=FALSE),
              fill(NOT(relation = no_fill), max_window = c(Inf,0), connected=TRUE),
              children(relation = rel, label='origin', min_window=c(min_dist,min_dist), max_window = c(max_dist,max_dist),
                       fill(NOT(relation = no_fill), max_window=c(0,Inf), connected=TRUE)))
  tokens = climb_tree(tokens, tq)
}

#' Have a node adopt its parent's position
#' 
#' given a tquery that identfies a node labeled "origin", that has a parent labeled "target", 
#' recursively have child adopt the parent's position (parent and relation column)
#' and adopt parents fill nodes. only_new restricts adding fill nodes to relations that child
#' does not already have. This seems to be a good heuristic for dealing with argument drop
#'
#' @param .tokens     A tokenIndex
#' @param tq          A tquery. Needs to have a node labeled "origin" that has a parent labeled "target" 
#' @param unpack      If TRUE (default), create separate branches for the parent and the node that inherits the parent position
#' @param isolate     If unpack is TRUE and isolate is TRUE (default is FALSE), isolate the new branch by recursively unpacking 
#' @param take_fill   If TRUE (default), give the node that will inherit the parent position a copy of the parent children (but only if it does not already have children with this relation; see only_new)
#' @param give_fill   If TRUE (default), copy the children of the node that will inherit the parent position to the parent (but only if it does not already have children with this relation; see only_new)
#' @param only_new    A characetr vector giving one or multiple column names that need to be unique for take_fill and give_fill
#' @param max_iter    The climb tree function repeatedly resolves the first conjunction it encounters in a sentence. This can lead to many iterations
#'                    for sentences with many (nested) conjunctions. It could be the case that in unforseen cases or with certain parsers
#'                    an infinite loop is reached, which is why we use a max_iter argument that breaks the loop and sends a warning if the max is reached.
#'
#' @return  The reshaped tokenIndex 
#' @export
#' @examples 
#' 
#' spacy_conjunctions <- function(tokens) {
#'   no_fill = c('compound*','case', 'relcl')
#'   tq = tquery(label='target', NOT(relation = 'conj'),
#'               rsyntax::fill(NOT(relation = no_fill), max_window = c(Inf,0)),
#'               children(relation = 'conj', label='origin',
#'                        rsyntax::fill(NOT(relation = no_fill), max_window=c(0,Inf))))
#'   tokens = climb_tree(tokens, tq)
#'   chop(tokens, relation = 'cc')
#' }
#' 
#' ## spacy tokens for "Bob and John ate bread and drank wine"
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text5',]
#'
#' tokens = spacy_conjunctions(tokens)
#' 
#' tokens
#' \donttest{
#' plot_tree(tokens)
#' }
climb_tree <- function(.tokens, tq, unpack=TRUE, isolate=TRUE, take_fill=TRUE, give_fill=TRUE, only_new='relation', max_iter=200) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  target = NULL; new_parent = NULL; branch_parent = NULL
  i = 1
  out = list()
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = FALSE, .one_per_sentence=TRUE)
  last_nodes = selected_nodes(.tokens)
  
  if (nrow(last_nodes$nodes) == 0) return(.tokens)

  ## split selected and unselected sentences and store unselected for output
  filt = unique(last_nodes$nodes[,c('doc_id','sentence')])
  out[[1]] = .tokens[!filt, on=c('doc_id','sentence')]
  .tokens = .tokens[filt, on=c('doc_id','sentence')]
  data.table::setattr(.tokens, '.nodes', value = last_nodes)  
  
  if (!'branch_parent' %in% colnames(.tokens)) .tokens[, branch_parent := numeric()]
  
  while (TRUE) {
    if (take_fill) .tokens = copy_fill(.tokens, 'target', 'origin', only_new=only_new)
    if (give_fill) .tokens = copy_fill(.tokens, 'origin', 'target', only_new=only_new)
    
    .tokens = mutate_nodes(.tokens, 'origin', parent=target$parent, relation=target$relation, branch_parent=target$branch_parent)
    
    if (unpack) {
      tq2 = tquery(label = 'child', g_id = last_nodes$nodes[,c('doc_id','sentence','origin')],
                   parents(label = 'parent'))
      .tokens = select_nodes(.tokens, tq2, fill_only_first = FALSE) 
      ## copy the parent 
      .tokens = copy_nodes(.tokens, 'parent', 'new_parent', copy_fill = FALSE)
      ## point the duplicate children towards new  copy
      .tokens = mutate_nodes(.tokens, 'child', parent=new_parent$token_id)
      ## and add the parent fill for which relation is not already in copy
      .tokens = copy_fill(.tokens, 'parent', 'new_parent', only_new = 'relation')
      if (isolate) .tokens = resolve_siblings(.tokens)
    }
    
    .tokens = select_nodes(.tokens, tq, fill_only_first = FALSE, .one_per_sentence=TRUE) 
    last_nodes = selected_nodes(.tokens)
    
    if (nrow(last_nodes$nodes) == 0) {
      out[[i+1]] = data.table::copy(.tokens)
      break
    }
    
    filt = unique(last_nodes$nodes[,c('doc_id','sentence')])
    out[[i+1]] = .tokens[!filt, on=c('doc_id','sentence')]
    .tokens = .tokens[filt, on=c('doc_id','sentence')]
    data.table::setattr(.tokens, '.nodes', value = last_nodes)  
    
    i = i + 1
    if (i > max_iter) {
      warning(sprintf('Stopped at iteration %s. See max_iter argument', max_iter))
      break ## this just shouldn't be possible
    }
    
  }
  #.tokens
  as_tokenindex(rbindlist(out, fill = TRUE))
}


resolve_siblings <- function(tokens, no_fill=NULL) {
  .SIBLING = target_copy = NULL
  
  ftok = data.table::data.table(doc_id=tokens$doc_id, 
                                sentence=tokens$sentence, 
                                floor_token_id=floor(tokens$token_id), 
                                token_id = tokens$token_id,
                                parent=tokens$parent)
  dupl = duplicated(ftok, by = c('doc_id','sentence','floor_token_id','parent')) &! is.na(ftok$parent)
  if (!any(dupl)) return(tokens)
  
  sib = tokens[dupl, c('doc_id','sentence','token_id')]
  
  dupl_tok = ftok[ftok[dupl,], on=c('doc_id','sentence','floor_token_id')]
  tokens[,.SIBLING := FALSE]
  tokens[dupl_tok, .SIBLING := TRUE, on=c('doc_id','sentence','token_id')]
  
  tq = tquery(label='target',
              fill(NOT(relation=no_fill), .SIBLING=FALSE, connected=TRUE),
              children(label='origin', g_id=sib[,c('doc_id','sentence','token_id')]))
  
  tokens = select_nodes(tokens, tq)
  tokens = copy_nodes(tokens, 'target', new = 'target_copy', copy_fill = TRUE)
  tokens = mutate_nodes(tokens, 'origin', parent = target_copy$token_id)
  
  ## repeat until no siblings remain
  tokens = resolve_siblings(tokens)
  
  ## backup plan
  #tokens = remove_duplicate_adds(tokens)
  #print(tokens)
  
  if ('.SIBLING' %in% colnames(tokens)) tokens[, .SIBLING := NULL]
  tokens
}

remove_duplicate_adds <- function(.tokens) {
  dupl = duplicated(data.table::data.table(doc_id=.tokens$doc_id, 
                                           sentence=.tokens$sentence, 
                                           token_id=floor(.tokens$token_id), 
                                           parent=.tokens$parent))
  dupl = dupl &! is.na(.tokens$parent)
  chop(.tokens, g_id = .tokens[dupl,])
}


one_per_sentence <- function(.tokens) {
  ## cannot unpack multiple pairs within the same branch, so force unique per sentence
  attr(.tokens, '.nodes')$nodes = attr(.tokens, '.nodes')$nodes[!duplicated(attr(.tokens, '.nodes')$nodes[,c('doc_id','sentence')]),]
  attr(.tokens, '.nodes')$fill = attr(.tokens, '.nodes')$fill[attr(.tokens, '.nodes')$fill$.ID %in% attr(.tokens, '.nodes')$nodes$.ID,]
  .tokens
}


#' Chop of a branch of the tree
#'
#' Using the query language for tquery, chop of the branch down from the node that is found
#'
#' @param .tokens A tokenIndex
#' @param ... Arguments passed to tquery. For instance, relation = 'punct' cuts off all punctuation dependencies (in universal dependencies)
#'
#' @export
#' @return A tokenIndex with the rows of the nodes in the selected branches removed
#' @examples 
#' 
#' spacy_conjunctions <- function(tokens) {
#'   no_fill = c('compound*','case', 'relcl')
#'   tq = tquery(label='target', NOT(relation = 'conj'),
#'               rsyntax::fill(NOT(relation = no_fill), max_window = c(Inf,0)),
#'               children(relation = 'conj', label='origin',
#'                        rsyntax::fill(NOT(relation = no_fill), max_window=c(0,Inf))))
#'   tokens = climb_tree(tokens, tq)
#'   chop(tokens, relation = 'cc')
#' }
#' 
#' ## spacy tokens for "Bob and John ate bread and drank wine"
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text5',]
#' 
#' tokens = spacy_conjunctions(tokens)
#' tokens
#' \donttest{
#' plot_tree(tokens)
#' }
chop <- function(.tokens, ...) {
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  tq = tquery(..., label = 'chop')
  .tokens = select_nodes(.tokens, tq)
  .tokens = remove_nodes(.tokens, 'chop')
  unselect_nodes(.tokens)
}

