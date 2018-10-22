## generic_reshape.r contains generic functions for mangling the parse tree
## applied_reshape.r uses these functions to do usefull stuff.

flatten_conjunctions <- function(.tokens, conj=NULL, cc=NULL, tq=NULL, target_is_cc=F, rm_cc=T, adopt_fill=T, subset=NULL, subset_fill=NULL, only_new='relation', depth=Inf) {
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset))
  if (!is_deparsed_call(subset_fill)) subset_fill = deparse(substitute(subset_fill))
  
  if (is.null(tq)) {
    if (is.null(conj)) stop('either conj or tq has to be given (currently both are NULL)')
    tq = tquery(save='target', 
              children(relation = conj, save='conj'),
              if (is.null(cc)) NULL else children(relation = cc, save='cc', req = F))
  }
  
  .tokens = select_nodes(.tokens, tq, fill_only_first = F) 
  .nodes = .nodes_from_attr(.tokens)
  has_cc = 'cc' %in% colnames(.nodes$nodes)
  if (has_cc && rm_cc) .tokens = remove_nodes(.tokens, 'cc', with_fill = T)
  .tokens = climb_tree(.tokens, node='conj', adopt_fill=adopt_fill, subset_fill=subset_fill, only_new=only_new, subset=subset, depth=depth)

  if (target_is_cc) {
    tq = tquery(save='cc', g_id = .nodes$nodes[,c('doc_id','sentence','target')])
    .tokens = select_nodes(.tokens, tq) 
    .tokens = remove_nodes(.tokens, 'cc', with_fill = T)
  }
  unselect_nodes(.tokens)
  .tokens
}



climb_tree <- function(.tokens, node='conj', adopt_fill=T, subset_fill=NULL, only_new='relation', subset=NULL, depth=Inf) {
  ## given a node selection that identifies pairs of 'child' and 'parent', 
  ## recursively have child adopt parent relation (parent and relation column)
  ## and adopt parents fill nodes. only_new restricts adding fill nodes to relations that child
  ## does not already have. This seems to be a good heuristic for dealing with argument drop
  i = 1
  
  last_nodes = .nodes_from_attr(.tokens)
  while (i < depth) {
    if (nrow(last_nodes$nodes) == 0) return(.tokens)
    
    if (adopt_fill) {
      .tokens = copy_fill(.tokens, 'target', node, subset_fill = subset_fill, subset = subset, only_new=only_new)
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
  for (rel in relations) {
    .tokens = do_unpack_tree(.tokens, rel, subset_fill)
  }
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
    tq = tquery(save = 'child', g_id = .tokens[dup,c('doc_id','sentence','token_id')],
                parents(save = 'parent'))
    
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
  tokens = spacy_parse('Steve and Bob love pizza but hate cheese.', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, lemma)
  
  tokens %>%
    flatten_conjunctions('conj', cc='cc') %>%
    unpack_tree(relations=c('nsubj','nobj','nmod')) %>%
    plot_tree(token, lemma)
  
  tokens = spacy_parse('In this campaign season, establishment GOP candidates have accepted his help and endorsement and, in many cases, mimicked his style and themes.', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, lemma)
  
  tokens %>%
    flatten_conjunctions('conj', cc='cc') %>%
    unpack_tree(relations=c('nsubj','nobj','nmod','dobj')) %>%
    plot_tree(token, lemma)
  
  
  
  tq = tquery(save='parent', 
              children(relation = 'conj', save='child'),
              chlldren(relation = 'cc', save='cc'))
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
  
  
  tokens = spacy_parse('Steve and Bob ate a pie, banana and cake', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, token, use_color = T)
  
  
  
  tq = tquery(save='parent', 
              children(relation = 'conj', save='child'))
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

