## generic_reshape.r contains generic functions for mangling the parse tree
## applied_reshape.r uses these functions to do usefull stuff.

#' Have nodes with a certain relation inherit the position of their parent
#'
#' This is in particular usefull for removing conjunctions
#'
#' @param .tokens A tokenIndex
#' @param relation A character string specifying the relation
#' @param take_fill If TRUE, give the node that will inherit the parent position a copy of the parent children (but only if it does not already have children with this relation; see only_new)
#' @param give_fill If TRUE, copy the children of the node that will inherit the parent position to the parent (but only if it does not already have children with this relation; see only_new)
#' @param unpack If TRUE, create separate branches for the parent and the node that inherits the parent position
#' @param only_new A character vector giving one or multiple column names that need to be unique for take_fill and give_fill
inherit <- function(.tokens, relation, take_fill=fill(), give_fill=fill(), unpack=T, only_new='relation') {
  tq = tquery(label='target', NOT(relation = relation),
              take_fill,
              children(relation = relation, label='origin',
                       give_fill))
  
  .tokens = climb_tree(.tokens, tq, unpack=unpack, take_fill=T, give_fill=T, only_new=only_new)
  unselect_nodes(.tokens)
  .tokens
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
#' @return  A tokenIndex
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
#' plot_tree(tokens)
climb_tree <- function(.tokens, tq, unpack=T, isolate=T, take_fill=T, give_fill=T, only_new='relation', max_iter=200) {
  target = NULL; new_parent = NULL
  i = 1
  out = list()
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = F, .one_per_sentence=T)
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
      .tokens = select_nodes(.tokens, tq2, fill_only_first = F) 
      ## copy the parent 
      .tokens = copy_nodes(.tokens, 'parent', 'new_parent', copy_fill = F)
      ## point the duplicate children towards new  copy
      .tokens = mutate_nodes(.tokens, 'child', parent=new_parent$token_id)
      ## and add the parent fill for which relation is not already in copy
      .tokens = copy_fill(.tokens, 'parent', 'new_parent', only_new = 'relation')
      if (isolate) .tokens = resolve_siblings(.tokens)
    }
    
    .tokens = select_nodes(.tokens, tq, fill_only_first = F, .one_per_sentence=T) 
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
  as_tokenindex(rbindlist(out, fill = T))
}


resolve_siblings <- function(tokens, no_fill=NULL) {
  ftok = data.table::data.table(doc_id=tokens$doc_id, 
                                sentence=tokens$sentence, 
                                floor_token_id=floor(tokens$token_id), 
                                token_id = tokens$token_id,
                                parent=tokens$parent)
  dupl = duplicated(ftok, by = c('doc_id','sentence','floor_token_id','parent')) &! is.na(ftok$parent)
  if (!any(dupl)) return(tokens)
  
  sib = tokens[dupl, c('doc_id','sentence','token_id')]
  
  dupl_tok = ftok[ftok[dupl,], on=c('doc_id','sentence','floor_token_id')]
  tokens[,.SIBLING := F]
  tokens[dupl_tok, .SIBLING := T, on=c('doc_id','sentence','token_id')]
  
  tq = tquery(label='target',
              fill(NOT(relation=no_fill), .SIBLING=F, connected=T),
              children(label='origin', g_id=sib[,c('doc_id','sentence','token_id')]))
  
  tokens = select_nodes(tokens, tq)
  tokens = copy_nodes(tokens, 'target', new = 'target_copy', copy_fill = T)
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
#' @return A tokenIndex
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
#' plot_tree(tokens)
chop <- function(.tokens, ...) {
  tq = tquery(..., label = 'chop')
  .tokens = select_nodes(.tokens, tq)
  .tokens = remove_nodes(.tokens, 'chop')
  unselect_nodes(.tokens)
}

