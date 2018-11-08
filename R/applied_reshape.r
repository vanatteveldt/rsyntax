## generic_reshape.r contains generic functions for mangling the parse tree
## applied_reshape.r uses these functions to do usefull stuff.

inherit <- function(.tokens, relation, take_fill=T, give_fill=F, unpack=T, subset=NULL, subset_fill=NULL, only_new='relation', depth=Inf) {
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset))
  if (!is_deparsed_call(subset_fill)) subset_fill = deparse(substitute(subset_fill))
  
  tq = tquery(label='target', 
              children(relation = relation, label='origin'))
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = F) 
  .nodes = .nodes_from_attr(.tokens)
  if (nrow(.nodes$nodes) > 0) {
    if (unpack) target_vars = get_node_vars(.tokens, .nodes, 'target')
    .tokens = climb_tree(.tokens, node='origin', take_fill=take_fill, give_fill=give_fill, subset_fill=subset_fill, only_new=only_new, subset=subset, depth=depth)
    if (unpack) .tokens = unpack_tree(.tokens, relations = na.omit(unique(target_vars$relation)))
  }
  unselect_nodes(.tokens)
  .tokens
}

chop <- function(.tokens, ...) {
  tq = tquery(..., label = 'chop')
  .tokens = .tokens %>%
    select_nodes(tq) %>%
    remove_nodes('chop')
  unselect_nodes(.tokens)
}

isolate_relcl <- function(.tokens, relation='relcl', who_lemma=c('who','that')) { 
  tq = tquery(relation = relation, label='relcl',
              children(lemma = who_lemma, label='who'),
              parents(relation = 'dobj', label='obj'))

  .tokens = select_nodes(.tokens, tq)
  .tokens = copy_nodes(.tokens, 'obj', new = 'who_obj', copy_fill = T)
  .tokens = mutate_nodes(.tokens, 'who_obj', parent = who$parent, relation = who$relation) 
  .tokens = remove_nodes(.tokens, 'who', with_fill = T) 
  .tokens = mutate_nodes(.tokens, 'relcl', parent = NA, relation = 'ROOT')
  .tokens = unselect_nodes(.tokens)
  .tokens[]
}

flatten_conjunctions <- function(.tokens, conj=NULL, cc=NULL, tq=NULL, target_is_cc=F, rm_cc=T, take_fill=T, give_fill=T, subset=NULL, subset_fill=NULL, only_new='relation', depth=Inf) {
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset))
  if (!is_deparsed_call(subset_fill)) subset_fill = deparse(substitute(subset_fill))
  
  if (is.null(tq)) {
    if (is.null(conj)) stop('either conj or tq has to be given (currently both are NULL)')
    tq = tquery(label='target', 
              children(relation = conj, label='conj'),
              if (is.null(cc)) NULL else children(relation = cc, label='cc', req = F))
  }
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = F) 
  .nodes = .nodes_from_attr(.tokens)
  has_cc = 'cc' %in% colnames(.nodes$nodes)
  if (has_cc && rm_cc) .tokens = remove_nodes(.tokens, 'cc', with_fill = T)
  .tokens = climb_tree(.tokens, node='conj', take_fill=take_fill, give_fill=give_fill, subset_fill=subset_fill, only_new=only_new, subset=subset, depth=depth)

  if (target_is_cc) {
    tq = tquery(label='cc', g_id = .nodes$nodes[,c('doc_id','sentence','target')])
    .tokens = select_nodes(.tokens, tq) 
    .tokens = remove_nodes(.tokens, 'cc', with_fill = T)
  }
  unselect_nodes(.tokens)
  .tokens
}



climb_tree <- function(.tokens, node='conj', take_fill=T, give_fill=T, subset_fill=NULL, only_new='relation', subset=NULL, depth=Inf) {
  ## given a node selection that identifies pairs of 'child' and 'parent', 
  ## recursively have child adopt parent relation (parent and relation column)
  ## and adopt parents fill nodes. only_new restricts adding fill nodes to relations that child
  ## does not already have. This seems to be a good heuristic for dealing with argument drop
  i = 1
  
  last_nodes = .nodes_from_attr(.tokens)
  while (i < depth) {
    if (nrow(last_nodes$nodes) == 0) return(.tokens)
    
    if (take_fill) {
      .tokens = copy_fill(.tokens, 'target', node, subset_fill = subset_fill, subset = subset, only_new=only_new)
    }
    if (give_fill) {
      .tokens = copy_fill(.tokens, node, 'target', subset_fill = subset_fill, subset = subset, only_new=only_new)
    }
    
    .tokens = mutate_nodes(.tokens, node, subset=subset, parent=target$parent, relation=target$relation)
    
    .tokens = reselect_nodes(.tokens)
    
    if (identical(.nodes_from_attr(.tokens)$nodes, last_nodes$nodes)) {
      break
    } else last_nodes = .nodes_from_attr(.tokens)
    i = i + 1
    if (i > 100) break ## this just shouldn't be possible
    
  }
  .tokens
}

unpack_tree <- function(.tokens, relations, subset_fill=NULL) {
  if (!is_deparsed_call(subset_fill)) subset_fill = deparse(substitute(subset_fill))
  #dup = duplicated(.tokens, by = c('doc_id','sentence','parent','relation'))
  #dup_rel = unique(.tokens$relation[dup])
  #dup_rel = setdiff(dup_rel, 'ROOT')
  #for (rel in dup_rel) {
  n = nrow(.tokens)
  for (rel in relations) {
    .tokens = do_unpack_tree(.tokens, rel, subset_fill)
  }
  
  ## it can be that unpacking some relations duplicates others, so repeat until no changes occr
  if (n < nrow(.tokens)) .tokens = unpack_tree(.tokens, relations, subset_fill)
  unselect_nodes(.tokens)
}

do_unpack_tree <- function(.tokens, relation, subset_fill) {
  i = 1
  while (TRUE) {
    dup = duplicated(.tokens, by = c('doc_id','sentence','parent','relation'))
    #dup = dup & !is.na(.tokens$parent)
    dup = dup & .tokens$relation == relation
    if (!any(dup)) break
    
    ## select duplicate nodes and their parents
    tq = tquery(label = 'child', g_id = .tokens[dup,c('doc_id','sentence','token_id')],
                parents(label = 'parent'))
    
    .tokens = select_nodes(.tokens, tq)
    ## copy the parent 
    .tokens = copy_nodes(.tokens, 'parent', 'new_parent', copy_fill = F)
    ## point the duplicate childen towards new  copy
    .tokens = mutate_nodes(.tokens, 'child', parent=new_parent$token_id)
    ## and add the parent fill for which relation is not already in copy
    .tokens = copy_fill(.tokens, 'parent', 'new_parent', only_new = 'relation', subset_fill=subset_fill)
    i = i + 1
    if (i > 20) break ## just shouldn't happen
    ## and repeat until no duplicates remain. (for each loop, 1 duplicate is resolved for each sentence, so this should take a few iterations at most)
  }
  .tokens
}



function(){
  library(spacyr)
  tokens = spacy_parse("Bob likes dogs but hates tiny cats", dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, lemma, use_color = T)
  
  
  library(spacyr)
  spacy_initialize()
  tokens = spacy_parse('China may meet Russia for war games, and that doesn’t make them allies.', dependency=T)
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

