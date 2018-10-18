treshape <- function(tokens, tq, fill=T, only_first_fill=T){
  nodes = find_nodes(tokens, tq, fill = F, melt = F)
  if (fill) ids = add_fill(tokens, nodes, tq, block=nodes)
  ids = melt_nodes_list(ids, only_first_fill=only_first_fill)
  
  is_fill = ids$.FILL_LEVEL > 0
  #fill_table = subset(ids, is_fill, select=c('doc_id','sentence','.ID','.ROLE','token_id'))
  fill_table = subset(ids, is_fill)
  data.table::setindexv(fill_table, '.ROLE')
  
  #rtokens = tokens[unique(ids[,c('doc_id','sentence','token_id')]), on=c('doc_id','sentence','token_id')]
  #rtokens = as_tokenindex(rtokens)

  rnodes = list(tokens=tokens,        ## The tokenindex 
                #rm_tokens=list(),     ## a list with global token ids (data.tables with doc_id, sentence and token_id) that are to be removed
                nodes=nodes,          ## the nodes found with the tquery
                fill = fill_table)    ## a table with all fill values is first created 
  
  rnodes
}

mutate_nodes <- function(.treshape, node, ..., subset=NULL, copy=T) {
  if (is(substitute(node), 'name')) node = as.character(substitute(node))
  
  l = tidyselect::quos(...)
  if (length(l) == 0) return(.treshape) 
  
  .treshape = data.table::copy(.treshape)
  linked_node_vars = get_linked_node_vars(.treshape)
  
  token_i = get_node_position(.treshape, node)
  
  subset = substitute(subset)
  if (!is.null(subset)) {
    subset_l = eval(subset, envir = linked_node_vars, parent.frame())
    token_i[subset_l]
  }
  
  for (i in seq_along(l)) {
    col = names(l)[i]
    if (is.character(l[[i]][[2]])) l[[i]][[2]] = parse(text=l[[i]][[2]])
    val = eval(l[[i]][[2]], linked_node_vars, parent.frame())
    if (!is.null(subset)) val = val[subset_l]
    .treshape$tokens[token_i, (col) := val]
  }
  
  if (copy) .treshape else invisible(.treshape)
}

remove_nodes <- function(.treshape, node, rm_subset=NULL, with_fill=T) {
  if (is(substitute(node), 'name')) node = as.character(substitute(node))
  
  if (!is.null(rm_subset)) {
    if (!is.logical(rm_subset)) rm_subset = treshape_eval(.treshape, substitute(rm_subset))
    if (!any(rm_subset)) return(.treshape) ## if there are no nodes that meet the rm_subset condition, nothing is removed
  } else rm_subset = rep(T, nrow(.treshape$nodes))
  
  ## remove fill first, because this uses .treshape$nodes (but after rm_subset, so that it only has to be evaluated once)
  if (with_fill) remove_fill(.treshape, node, rm_subset, rm_subset_fill)
  
  drop_ids = .treshape$nodes[rm_subset, c('doc_id','sentence',node), with=F]
  drop_ids = na.omit(drop_ids)
  if (nrow(drop_ids) > 0) {
    data.table::setnames(drop_ids, old=node, new='token_id')
    .treshape$tokens = .treshape$tokens[!drop_ids, on=c('doc_id','sentence','token_id')]
    .treshape$nodes[[node]] = ifelse(rm_subset, NA, .treshape$nodes[[node]])
  }
  .treshape$tokens = fix_missing_parents(.treshape$tokens, warn = F)  ## (function in token_index.r)
  .treshape
}

remove_fill <- function(.treshape, node, rm_subset=NULL, rm_subset_fill=NULL, keep_shared=F) {
  fill_nodes = get_fill_nodes(.treshape, node)
  
  node_ids = .treshape$nodes$.ID

  if (!is.null(rm_subset)) {
    if (!is.logical(rm_subset)) rm_subset = treshape_eval(.treshape, substitute(rm_subset))
    if (!any(rm_subset)) return(.treshape) ## if there are no nodes that meet the rm_subset condition, nothing is removed
    node_ids = na.omit(node_ids[rm_subset])
    fill_nodes = fill_nodes[list(node_ids), on='.ID', nomatch=0]
  }
  if (nrow(fill_nodes) == 0) return(.treshape)
  .treshape$fill = .treshape$fill[!fill_nodes, on=c('.ID','.ROLE')]
  
  if (keep_shared) {
    fill_nodes = fill_nodes[!.treshape$fill, on=c('doc_id','sentence','token_id')]  
    if (nrow(fill_nodes) == 0) return(.treshape)
  } 
  .treshape$tokens = .treshape$tokens[!fill_nodes, on=c('doc_id','sentence','token_id')]
  .treshape$tokens = fix_missing_parents(.treshape$tokens, warn=F)  ## (function in token_index.r)
  .treshape
}

copy_nodes <- function(.treshape, node, new, subset=NULL, keep_relation=T, copy_fill=F, subset_fill=NULL, only_new=NULL) {
  if (is(substitute(node), 'name')) node = as.character(substitute(node))
  if (is(substitute(new), 'name')) new = as.character(substitute(new))
  if (!node %in% names(.treshape$nodes)) stop(sprintf('node (%s) is not a valid node in the tquery results (use the save= argument)', node))
  
  if (!is.null(subset)) {
    if (!is.logical(subset)) subset = treshape_eval(.treshape, substitute(subset))
  } else subset = rep(T, nrow(.treshape$nodes)) 

  node_ids = .treshape$nodes[,c('doc_id','sentence',node),with=F]
  node_ids = add_sub_id(.treshape, node_ids[subset]) ## beware of the subset being used here
  .treshape$nodes[[new]] = double(nrow(.treshape$nodes))
  .treshape$nodes[subset, (new) := node_ids]
  
  node_vars = get_node_vars(.treshape, node)
  node_vars = node_vars[subset]
  node_vars$token_id = node_ids
  if (!keep_relation) {
    node_vars[, parent := NA]
    node_vars[, relation := 'ROOT']
  }
  .treshape = add_to_tokens(.treshape, node_vars)
  if (copy_fill) .treshape = copy_fill(.treshape, node, new, subset_fill=subset_fill, only_new=only_new)
  .treshape
}

copy_fill <- function(.treshape, from_node, to_node, subset=NULL, subset_fill=NULL, only_new=NULL) {
  if (is(substitute(from_node), 'name')) from_node = as.character(substitute(from_node))
  if (is(substitute(to_node), 'name')) to_node = as.character(substitute(to_node))
  if (!from_node %in% names(.treshape$nodes)) stop(sprintf('from_node (%s) is not a valid node in the tquery results (use the save= argument)', node))
  if (!to_node %in% names(.treshape$nodes)) stop(sprintf('to_node (%s) is not a valid node in the tquery results (use the save= argument)', node))
  #.treshape = create_fill_table(.treshape, only_first_fill)  ## creates .treshape$fill, if it does not already exist

  
  fill_nodes = get_fill_nodes(.treshape, from_node)
  
  if (!is.null(subset_fill)) {
    subset_fill = eval(substitute(subset_fill), envir = fill_nodes, parent.frame())
    fill_nodes = fill_nodes[subset_fill]
  }
  
  if (!is.null(only_new)) {
    current_fill = get_fill_nodes(.treshape, to_node)
    if (any(!only_new %in% colnames(current_fill))) stop('some values in only_new are not valid token columns')
    remove_fill = current_fill[,c('.ID',only_new),with=F]
    fill_nodes = fill_nodes[!remove_fill, on=c('.ID',only_new)]
  }
  
  if (!is.null(subset)) {
    if (!is.logical(subset)) subset = treshape_eval(.treshape, substitute(subset))
    node_ids = .treshape$nodes$.ID[subset]
    fill_nodes = fill_nodes[list(node_ids), on='.ID', nomatch=0]
  }
  
  id_index = match(fill_nodes$.ID, .treshape$nodes$.ID)
  from_node_ids = .treshape$nodes[[from_node]][id_index]
  to_node_ids = .treshape$nodes[[to_node]][id_index]
  
  .NEW_ID = add_sub_id(.treshape, fill_nodes[,c('doc_id','sentence','token_id')])
  parent_index = fill_nodes[,c('doc_id','sentence','token_id','parent')]
  parent_index[, .I := 1:nrow(parent_index)]
  parent_index = parent_index[list(parent_index$doc_id, parent_index$sentence, parent_index$parent),
                            on = c('doc_id','sentence','token_id')]
  .NEW_PARENT = .NEW_ID[parent_index$parent]

  fill_nodes[, token_id := .NEW_ID]
  ## if parent is the from_node id (i.e. fill is a direct child that connects the fill tree to the node), replace parent with node id, otherwise create the sub id
  is_direct_child = fill_nodes$parent == from_node_ids
  if (any(is_direct_child)) fill_nodes[is_direct_child, parent := to_node_ids[is_direct_child]]
  if (any(!is_direct_child)) fill_nodes[!is_direct_child, parent := .NEW_PARENT[!is_direct_child]]
  
  fill_nodes[, .ROLE := to_node]  
  .treshape = add_to_tokens(.treshape, fill_nodes)  
  .treshape = add_to_fill(.treshape, fill_nodes)  
  .treshape
}

add_to_tokens <- function(.treshape, new_tokens) {
  if (!identical(colnames(.treshape$tokens), colnames(new_tokens))) new_tokens = subset(new_tokens, select = colnames(.treshape$tokens))
  .treshape$tokens = unique(rbind(.treshape$tokens, new_tokens))
  .treshape$tokens = as_tokenindex(.treshape$tokens)
  .treshape
}

add_to_fill <- function(.treshape, new_fill) {
  if (!identical(colnames(.treshape$fill), colnames(new_fill))) new_fill = subset(new_fill, select = colnames(.treshape$fill))
  .treshape$fill = unique(rbind(.treshape$fill, new_fill))
  data.table::setindexv(.treshape$fill, '.ROLE')
  .treshape
}

treshape_eval <- function(.treshape, x) {
  if (is.null(x)) return(NULL)
  linked_node_vars = get_linked_node_vars(.treshape)
  eval(x, envir = linked_node_vars, parent.frame())
}


apply_treshape <- function(.treshape, tokens) {
  exists = tokens[.treshape$tokens[,c('doc_id','sentence','token_id')], on=c('doc_id','sentence','token_id'), which=T]
}


get_node_vars <- function(.treshape, node) {
  node_ids = .treshape$nodes[,c('doc_id','sentence',node),with=F]
  data.table::setnames(node_ids, old=node, new='token_id')
  .treshape$tokens[node_ids, on=c('doc_id','sentence','token_id')]
}
  
get_linked_node_vars <- function(.treshape) {
  node_names = setdiff(colnames(.treshape$nodes), c('.ID','doc_id','sentence'))
  node_vars = lapply(node_names, function(node) get_node_vars(.treshape, node))
  names(node_vars) = node_names
  node_vars
}

get_node_position <- function(.treshape, node) {
  node_ids = .treshape$nodes[,c('doc_id','sentence',node), with=F]
  data.table::setnames(node_ids, old=node, new='token_id')
  .treshape$tokens[node_ids,on=c('doc_id','sentence','token_id'),which=T]
}

get_fill_nodes <- function(.treshape, node) {
  #.treshape = create_fill_table(.treshape, only_first_fill)  ## creates .treshape$fill, if it does not already exist (should be done before calling get_fill_nodes for efficiency)
  fill_table = .treshape$fill[list(node), on='.ROLE', nomatch=0]
  data.table::setkeyv(fill_table, c('doc_id','sentence','token_id'))
  merge(fill_table, .treshape$tokens, by=c('doc_id','sentence','token_id'))
}

#new_sub_id <- function(.treshape) {
#  max_sub = max(.treshape$tokens$token_id - floor(.treshape$tokens$token_id))
#  max_sub = round(max_sub, 6) ## necessary due to weird accuracy errors in adding the sub int, that create huge strings
#  max_sub_int = as.numeric(gsub('0\\.', '', max_sub))
#  n = nchar(max_sub_int)
#  (max_sub_int + 1) / 10^n
#}

## much more efficient, but less intuitive ids (increment counts from the highest sub_id)
#add_sub_id <- function(.treshape, x, sub_id=NULL) {   ## find more elegant solution
#  if (is.null(sub_id)) sub_id = new_sub_id(.treshape)
#  floor(x) + sub_id
#}

#add_sub_id2 <- function(x, sub_id) {
#  n = nchar(gsub('\\.', '', sub_id))
##  x[x == 0] = 0.01 ## in case the token is zero, since 0.0 would mess things up
#  ##x / 10^(ceiling(log10(highest)))
#  x / 10^(nchar(highest) + 1)
#}

increment_sub_id <- function(x) {
  ## use decimals as sub_id by mirroring increment. so increments 3.99 to 3.001 (mirroring 99 to 100), 3.001 to 3.101, etc. 
  int_x = floor(x) 
  dec = x - int_x
  n = stringi::stri_count_regex(dec, pattern = '[0-9]')
  new_dec = (dec * 10^(n-1)) + 1
  n = stringi::stri_length(new_dec)
  sub_id = new_dec / 10^(n*2-1)
  int_x + sub_id
}


add_sub_id <- function(.treshape, ids) {
  ## ids should be a data.table with the first three columns being: doc_id, sentence, token_id
  ids = data.table::copy(ids)
  data.table::setnames(ids, c('doc_id','sentence','token_id'))
  data.table::setkeyv(ids, c('doc_id','sentence','token_id'))
  
  matched = rep(1, nrow(ids)) ## ids should initially exists
  exists = rep(T, nrow(ids))
  while (any(exists)) {
    ids[exists, token_id := increment_sub_id(ids$token_id[exists])]
    matched[exists] = .treshape$token[ids[exists], on=c('doc_id','sentence','token_id'), which=T]
    exists = !is.na(matched)
  }
  
  dup = duplicated(ids, by = c('doc_id','sentence','token_id'))
  while (any(dup)) {
    ids[dup, token_id := increment_sub_id(ids$token_id[dup])]
    dup = duplicated(ids, by = c('doc_id','sentence','token_id'))
  }
  ids$token_id
}


#add_sub_id <- function(.treshape, ids, id_col='token_id') {
#  colnames(ids) = c('doc_id','sentence',id_col)
#  data.table::setkeyv(ids, c('doc_id','sentence',id_col))
#  i = 0L
#  ex = 1:nrow(ids)
#  while (length(ex) > 0) {
#    i = i + 1
#    n = ceiling(log10(i+1))
#    sub_id = i / 10^n
#    ids[ex, (id_col) := floor(ids[[id_col]]) + sub_id]
#    sub_ex = ids[ex,][.treshape$tokens, on=c('doc_id','sentence',id_col), which=T, nomatch=0]
#    ex = ex[sub_ex]
#  }
#  
#  add_i = ave(ids[[id_col]]==ids[[id_col]], ids[[id_col]], FUN=cumsum) 
#  is_dup = add_i > 1
#  if (any(is_dup)) {
#    add_i = add_i[is_dup] - 1
#    n = ceiling(log10(add_i+1))
#    sub_id = add_i / 10^n
#    ids[is_dup, (id_col) := ids[[id_col]] + sub_id] 
#  }
#  ids[[id_col]]
#}




function(){
library(spacyr)
tokens = spacy_parse('Steve ate a pie, banana and cake', dependency=T)
tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
plot_tree(tokens, token, use_color = T)
  
tokens = spacy_parse('Steve ate a pie and banana and bought a cake', dependency=T)
tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
plot_tree(tokens, token)
  
tq = tquery(save='parent', 
            children(relation = 'conj', save='child'))

tq = tquery(save='parent',  
            children(relation = 'nsubj', save='child'))


treshape(tokens, tq) %>%
  mutate_nodes(child, token = parent$relation)  

treshape(tokens, tq) %>%
  copy_nodes(parent, parent_copy) %>%
  copy_fill(parent, parent_copy)

treshape(tokens, tq) %>%
  copy_nodes(parent, parent_copy) %>%
  copy_fill(parent, parent_copy) %>%
  copy_fill(parent, parent_copy)


treshape(tokens, tq) %>%
  copy_fill(parent, child)
  
treshape(tokens, tq) %>%
  copy_fill(parent, child) %>%
  copy_fill(parent, child)

.treshape = copy_nodes(.treshape, 'parent', 'parent_copy', keep_parent=T, with_fill = T)
.treshape = set_relation(.treshape, 'child', parent_copy$parent, parent_copy$relation)
.treshape
}












