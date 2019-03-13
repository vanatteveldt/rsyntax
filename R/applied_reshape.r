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
#' @param depth Certain relations can be recursively nested (e.g. conjunction within conjunction). Depth specifies how far to inherit (by default infinite)
#'
#' @export
inherit <- function(.tokens, relation, take_fill=fill(), give_fill=fill(), unpack=T, only_new='relation', depth=Inf) {
  tq = tquery(label='target', NOT(relation = relation),
              take_fill,
              children(relation = relation, label='origin',
                       give_fill))
  
  .tokens = climb_tree(.tokens, tq, unpack=unpack, take_fill=T, give_fill=T, only_new=only_new, depth=depth)
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
#' @param only_new    A character vector giving one or multiple column names that need to be unique for take_fill and give_fill
#' @param depth       Certain relations can be recursively nested (e.g. conjunction within conjunction). Depth specifies how far to inherit (by default infinite)
#'
#' @return
#' @export
#'
#' @examples
climb_tree <- function(.tokens, tq, unpack=T, take_fill=T, give_fill=T, only_new='relation', depth=Inf) {
  i = 1
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = F)
  if (nrow(attr(.tokens, '.nodes')$nodes) == 0) return(.tokens)
  .tokens = one_per_sentence(.tokens)
  
  last_nodes = .nodes_from_attr(.tokens)
  while (i < depth) {
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
    
    .tokens = select_nodes(.tokens, tq, fill_only_first = F) 
    if (nrow(attr(.tokens, '.nodes')$nodes) == 0) return(.tokens)
    .tokens = one_per_sentence(.tokens)
    
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
#' @param .tokens 
#' @param ... Arguments passed to tquery. For instance, relation = 'punct' cuts off all punctuation dependencies (in universal dependencies)
#'
#' @export
chop <- function(.tokens, ...) {
  tq = tquery(..., label = 'chop')
  .tokens = .tokens %>%
    select_nodes(tq) %>%
    remove_nodes('chop')
  unselect_nodes(.tokens)
}

function(){
  library(spacyr)
  tokens = spacy_parse("Bob likes dogs but hates tiny cats", dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, lemma, use_color = T)
  
  
  library(spacyr)
  spacy_initialize()
  tokens = spacy_parse('China may meet Russia for war games, but that doesn’t make them allies.', dependency=T)
  plot_tree(tokens, token, lemma, pos, use_color = T)
  
  
  tokens = spacy_parse('China may meet Russia for war games, and that doesn’t make them allies.', dependency=T)
  plot_tree(tokens, token, lemma, pos, use_color = F)
  
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  
  tokens = spacy_parse('Conjunctions are usefull but annoying', dependency=T)
  plot_tree(tokens, token, lemma, pos, use_color = F)
  
  
  tq = tquery(label='target', 
              children(relation = 'conj', label='conj',
                       not_children(lemma = c('that','this','which'), pos='nsubj')),
              children(lemma = c('and'), relation = 'cc', label='cc', req = F))
  
  tokens %>%
    flatten_conjunctions(tq=tq) %>%
    unpack_tree(relations=c('nsubj','nobj','nmod','pobj','prep')) %>%
    plot_tree(token, lemma, use_color = F)
  
  tokens = spacy_parse('Bob, who shot John, went home.', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, lemma)
  
  tokens %>%
    flatten_conjunctions('conj', cc='cc') %>%
    unpack_tree(relations=c('nsubj','nobj','nmod','dobj')) %>%
    plot_tree(token, lemma)
  
  
  
  tq = tquery(label='parent', 
              children(relation = 'conj', label='child'),
              chlldren(relation = 'cc', label='cc'))
  tokens = select_nodes(tokens, tq)
  
  find_nodes(tokens, tq)
  
  select_nodes(tokens, tq) %>%
    plot_tree(token)
  
  .nodes_from_attr(select_nodes(tokens, tq, fill_only_first = F))$fill
  .nodes_from_attr(select_nodes(tokens, tq, fill_only_first = F))$nodes
  
  select_nodes(tokens, tq, fill_only_first = F) %>%
    climb_tree('child', 'parent') %>%
    unpack_tree() %>%
    plot_tree(token)
  
  
  
  
  select_nodes(tokens, tq) %>%
    climb_tree('child', 'parent', adopt_fill = T, subset_fill = !relation == 'cc') %>%
    unpack_tree(subset_fill = !relation == 'cc') %>%
    plot_tree(token)
  
  
  tokens = spacy_parse('Those who would give up essential Liberty, to purchase a little temporary Safety, deserve neither Liberty nor Safety', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, use_color = T)
  
  
  
  tq = tquery(label='parent', 
              children(relation = 'conj', label='child'))
  tokens = select_nodes(tokens, tq)
  
  select_nodes(tokens, tq) %>%
    climb_tree('child', 'parent', adopt_fill = T, subset_fill = !relation == 'cc') %>%
    unpack_tree() %>%
    plot_tree(token)
  
  tokens
  
  
  mutate_nodes(tokens, 'child', subset = dup)
  
  select_nodes(tokens, tq) %>%
    mutate_nodes('child', token = parent$relation, subset = child$lemma == 'banana')  
  
  select_nodes(tokens, tq) %>%
    copy_nodes('child', token = parent$relation, subset = child$lemma == 'banana')  
  
  
  select_nodes(tokens, tq) %>%
    copy_nodes('parent', 'parent_copy') %>%
    copy_fill('parent', 'parent_copy')
  
  select_nodes(tokens, tq) %>%
    copy_nodes('parent', 'parent_copy') %>%
    copy_fill('parent', 'parent_copy') %>%
    copy_fill('parent', 'parent_copy')
  
}

