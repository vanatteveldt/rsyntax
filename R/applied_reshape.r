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
#' This function is mainly used for removing conjunctions from dependency trees.
#' 
#' given a tquery that identfies a node labeled "origin", that has a parent labeled "target", 
#' recursively have child adopt the parent's position (parent and relation column)
#' and adopt parents fill nodes. only_new restricts adding fill nodes to relations that child
#' does not already have. This seems to be a good heuristic for dealing with argument drop
#'
#' @param .tokens     A tokenIndex
#' @param tq          A tquery. Needs to have a node labeled "origin" that has a parent labeled "target" 
#' @param unpack      If TRUE, create separate branches for the parent and the node that inherits the parent position
#' @param take_fill   If TRUE, give the node that will inherit the parent position a copy of the parent children (but only if it does not already have children with this relation; see only_new)
#' @param give_fill   If TRUE, copy the children of the node that will inherit the parent position to the parent (but only if it does not already have children with this relation; see only_new)
#' @param only_new    A characetr vector giving one or multiple column names that need to be unique for take_fill and give_fill
#'
#' @return  A tokenIndex
#' @export
climb_tree <- function(.tokens, tq, unpack=T, take_fill=T, give_fill=T, only_new='relation') {
  target = NULL; new_parent = NULL
  i = 1
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = F, .one_per_sentence=T)
  #if (nrow(attr(.tokens, '.nodes')$nodes) == 0) return(.tokens)
  #.tokens = one_per_sentence(.tokens)
  
  last_nodes = .nodes_from_attr(.tokens)
  while (TRUE) {
    if (nrow(last_nodes$nodes) == 0) return(.tokens)
    
    if (take_fill) {
      .tokens = copy_fill(.tokens, 'target', 'origin', only_new=only_new)
    }
    if (give_fill) {
      .tokens = copy_fill(.tokens, 'origin', 'target', only_new=only_new)
    }
    
    .tokens = mutate_nodes(.tokens, 'origin', parent=target$parent, relation=target$relation)
    
    if (unpack) {
      tq2 = tquery(label = 'child', g_id = last_nodes$nodes[,c('doc_id','sentence','origin')],
                   parents(label = 'parent'))
      .tokens = select_nodes(.tokens, tq2, fill_only_first = F)
      ## copy the parent 
      .tokens = copy_nodes(.tokens, 'parent', 'new_parent', copy_fill = F)
      ## point the duplicate childen towards new  copy
      .tokens = mutate_nodes(.tokens, 'child', parent=new_parent$token_id)
      ## and add the parent fill for which relation is not already in copy
      .tokens = copy_fill(.tokens, 'parent', 'new_parent', only_new = 'relation')
    }
    
    .tokens = select_nodes(.tokens, tq, fill_only_first = F, .one_per_sentence=T) 
    #if (nrow(attr(.tokens, '.nodes')$nodes) == 0) return(.tokens)
    #.tokens = one_per_sentence(.tokens)
    
    if (identical(.nodes_from_attr(.tokens)$nodes, last_nodes$nodes)) {
      break
    } else last_nodes = .nodes_from_attr(.tokens)
    i = i + 1
    if (i > 100) break ## this just shouldn't be possible
    
  }
  .tokens
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
chop <- function(.tokens, ...) {
  tq = tquery(..., label = 'chop')
  .tokens = select_nodes(.tokens, tq)
  .tokens = remove_nodes(.tokens, 'chop')
  unselect_nodes(.tokens)
}

