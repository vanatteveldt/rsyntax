#' Apply tquery to initiate reshape operations
#'
#' @param tokens            A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param tquery            A \link{tquery} that selects and labels the nodes that are used in the reshape operations
#' @param fill              Logical, should fill be used?
#' @param fill_only_first   Logical, should a node only be filled once, with the nearest (first) labeled node?
#' @param .one_per_sentence If true, only the first match in each sentence is used. This is currently used internally in the climb_tree function.
#'                        
#' @return A tokenIndex with a .nodes attribute, that enables the use of reshape operations on the selected nodes
#' @export
select_nodes <- function(tokens, tquery, fill=T, fill_only_first=T, .one_per_sentence=F){
  ## fill mode: all, only_first, 
  tokens = as_tokenindex(tokens)
  nodes = find_nodes(tokens, tquery, fill = F, melt = F)

  if (!is.null(nodes)) {
    if (.one_per_sentence) nodes = nodes[!duplicated(nodes, by=c('doc_id','sentence')),]
    if (fill) {
      if (fill_only_first) {
        ids = unique(add_fill(tokens, nodes, tquery, block=nodes))
      } else {
        ids = unique(add_fill(tokens, nodes, tquery, block=NULL))
      }
    } else ids = NULL
    if (!is.null(ids)) {
      ids = melt_nodes_list(ids, fill_only_first=fill_only_first)
    
      is_fill = ids$.FILL_LEVEL > 0
      fill_table = subset(ids, is_fill)
      data.table::setindexv(fill_table, '.ROLE')
    }
    l = list(nodes=nodes, fill=fill_table, prov = list(tquery=tquery, fill=fill, fill_only_first=fill_only_first))
  } else {
    l = list(nodes =data.table::data.table(), fill=data.table::data.table(), prov = list(tquery=tquery, fill=fill, fill_only_first=fill_only_first))
  }
  data.table::setattr(tokens, '.nodes', value = l)  ## .nodes is assigned to attribute. This way it will automatically be deleted
                                                    ## if tokens is changes (which in this case is convenient)
  tokens[]
}

#' Within a chain of reshape operations, reapply the tquery
#'
#' @param .tokens A tokenIndex in which nodes are selected with \link{select_nodes}.  
#'
#' @export
reselect_nodes <- function(.tokens) {
  .nodes = .nodes_from_attr(.tokens)
  select_nodes(.tokens, tquery = .nodes$prov$tquery, fill = .nodes$prov$fill, fill_only_first = .nodes$prov$fill_only_first)
}

#' Undo select_nodes
#'
#' Not strictly required. Only available for elegance and minor memory efficiency
#'
#' @param .tokens A tokenIndex in which nodes are selected with \link{select_nodes}.  
#'
#' @export
unselect_nodes <- function(.tokens) {
  data.table::setattr(.tokens, '.nodes', NULL)
  .tokens
}

.nodes_from_attr <- function(.tokens) {
  if (is.null(attr(.tokens, '.nodes'))) stop('.tokens data.table does not have selected nodes. Please use select_nodes(tokens, tquery, ...) before using this function. See ?select_nodes documentation for details')
  attr(.tokens, '.nodes')
}

#' Subset a select_nodes selection 
#' 
#' Enables more control in reshape operations
#'
#' @param .tokens A tokenIndex in which nodes are selected with \link{select_nodes}.  
#' @param subset  A subset expression (that evaluates to a logical vector). The token column for each labeled node in the tquery can be referred to as label$column.
#' @param copy    If TRUE, make a deep copy of .tokens. Use if output does not overwrite .tokens
#'
#' @export
subset_nodes <- function(.tokens, subset, copy=T) {
  copy=T ## make optional if tested
  .nodes = .nodes_from_attr(.tokens)
  if (nrow(.nodes$nodes) == 0) return(.tokens)
  if (copy) .tokens = data.table::copy(.tokens)
  
  .tokens = data.table::copy(.tokens)
  linked_node_vars = get_linked_node_vars(.tokens, .nodes)
  
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset)) 
  subset = if (subset == 'NULL') NULL else eval(parse(text=subset), envir=linked_node_vars, parent.frame())
  
  if (is.null(subset)) if (copy) .tokens[] else invisible(.tokens)
  if (all(subset)) if (copy) .tokens[] else invisible(.tokens)
  
  .nodes$nodes = .nodes$nodes[subset,]
  node_ids = unique(as.character(.nodes$nodes$.ID))
  .nodes$fill = .nodes$fill[list(.ID=node_ids), on='.ID', nomatch=0]
  
  data.table::setattr(.tokens, '.nodes', value = .nodes)
  if (copy) .tokens[] else invisible(.tokens)
}

#' Mutate nodes
#'
#' @param .tokens A tokenIndex in which nodes are selected with \link{select_nodes}.  
#' @param node    The name of the node that is to be mutated
#' @param ...     named arguments. The name should be a column in tokens
#' @param subset  A subset expression (that evaluates to a logical vector). The token column for each labeled node in the tquery can be referred to as label$column.
#'
#' @export
mutate_nodes <- function(.tokens, node, ..., subset=NULL) {
  copy = T ## make optional if tested
  .nodes = .nodes_from_attr(.tokens)
  if (nrow(.nodes$nodes) == 0) return(.tokens)
  if (!is.character(node)) stop('"node" argument has to be a character value')
    
  l = tidyselect::quos(...)
  if (length(l) == 0) return(.tokens) 
  if (copy) .tokens = data.table::copy(.tokens)
  
  linked_node_vars = get_linked_node_vars(.tokens, .nodes)
  
  token_i = get_node_position(.tokens, .nodes, node)
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset)) 
  subset = if (subset == 'NULL') NULL else eval(parse(text=subset), envir=linked_node_vars, parent.frame())
  for (i in seq_along(l)) {
    col = names(l)[i]
    if (is.character(l[[i]][[2]])) l[[i]][[2]] = parse(text=l[[i]][[2]])
    if (is.expression(l[[i]][[2]])) {
      .VAL = as.character(l[[i]][[2]][[1]])
    } else {
      .VAL = eval(l[[i]][[2]], linked_node_vars, parent.frame())
    }
    if (!is.null(subset)) {
      .tokens[token_i[subset], (col) := .VAL[subset]]
    } else {
      .tokens[token_i, (col) := .VAL]
    }
  }

  data.table::setattr(.tokens, '.nodes', value = .nodes)
  if (copy) .tokens[] else invisible(.tokens)
}


#' Remove nodes
#'
#' @param .tokens          A tokenIndex in which nodes are selected with \link{select_nodes}.  
#' @param node             The name of the node that is to be mutated
#' @param rm_subset        A subset expression (that evaluates to a logical vector) to more specifically specify which nodes to remove. The token column for each labeled node in the tquery can be referred to as label$column.
#' @param with_fill        If TRUE, also remove the fill nodes
#' @param rm_subset_fill   A subset on the fill nodes. Can only directly use token column. For example, use pos == 'VERB' to remove only verbs
#' @param keep_shared      If there is another node that has the same fill nodes, should the fill nodes that are shared also be removed?
#'
#' @export
remove_nodes <- function(.tokens, node, rm_subset=NULL, with_fill=T, rm_subset_fill=NULL, keep_shared=F) {
  .nodes = .nodes_from_attr(.tokens)
  if (nrow(.nodes$nodes) == 0) return(.tokens)
  
  if (!is.character(node)) stop('"node" argument has to be a character value')
  
  if (!is_deparsed_call(rm_subset)) rm_subset = deparse(substitute(rm_subset)) 
  rm_subset = if (rm_subset == 'NULL') NULL else .nodes_eval(.tokens, .nodes, rm_subset)
  if (!is.null(rm_subset)) {
    if (!any(rm_subset)) return(.tokens) ## if there are no nodes that meet the rm_subset condition, nothing is removed
  } else rm_subset = rep(T, nrow(.nodes$nodes))
  
  ## remove fill first because nodes are used to find fill nodes
  if (with_fill) {
    fill_nodes = get_fill_nodes(.tokens, .nodes, node)
    if (!is_deparsed_call(rm_subset_fill)) rm_subset_fill = deparse(substitute(rm_subset_fill)) 
    rm_subset_fill = if (rm_subset_fill == 'NULL') NULL else eval(parse(text=rm_subset_fill), envir = fill_nodes, parent.frame())
    .tokens = do_remove_fill(.tokens, .nodes, fill_nodes, node, rm_subset_fill, rm_subset, keep_shared)
    .nodes = .nodes_from_attr(.tokens)
  }
  
  drop_ids = .nodes$nodes[rm_subset, c('doc_id','sentence',node), with=F]
  drop_ids = stats::na.omit(drop_ids)
  if (nrow(drop_ids) > 0) {
    data.table::setnames(drop_ids, old=node, new='token_id')
    .tokens = .tokens[!drop_ids, on=c('doc_id','sentence','token_id')]
    .nodes$nodes[[node]] = ifelse(rm_subset, NA, .nodes$nodes[[node]])
  }
  .tokens = fix_missing_parents(.tokens, warn = F)  ## (function in token_index.r)
  
  data.table::setattr(.tokens, '.nodes', value = .nodes)
  .tokens[]
}

#' Remove fill
#' 
#' Like remove_nodes, but only removing the fill nodes
#' 
#' @param .tokens          A tokenIndex in which nodes are selected with \link{select_nodes}.  
#' @param node             The name of the node that is to be mutated
#' @param rm_subset_fill   A subset on the fill nodes. Can only directly use token column. For example, use pos == 'VERB' to remove only verbs
#' @param rm_subset        A subset expression (that evaluates to a logical vector) to more specifically specify which nodes to remove. The token column for each labeled node in the tquery can be referred to as label$column.
#' @param keep_shared      If there is another node that has the same fill nodes, should the fill nodes that are shared also be removed?
#'
#' @export
remove_fill <- function(.tokens, node, rm_subset_fill=NULL, rm_subset=NULL, keep_shared=F) {
  .nodes = .nodes_from_attr(.tokens)
  if (nrow(.nodes$nodes) == 0) return(.tokens)
  
  if (!is.character(node)) stop('"node" argument has to be a character value')
  
  if (!is_deparsed_call(rm_subset)) rm_subset = deparse(substitute(rm_subset)) 
  rm_subset = if (rm_subset == 'NULL') NULL else .nodes_eval(.tokens, .nodes, rm_subset)
  
  fill_nodes = get_fill_nodes(.tokens, .nodes, node)
  if (!is_deparsed_call(rm_subset_fill)) rm_subset_fill = deparse(substitute(rm_subset_fill)) 
  rm_subset_fill = if (rm_subset_fill == 'NULL') NULL else eval(parse(text=rm_subset_fill), envir = fill_nodes, parent.frame())
  
  do_remove_fill(.tokens, .nodes, fill_nodes, node, rm_subset_fill, rm_subset, keep_shared)
}

do_remove_fill <- function(.tokens, .nodes, fill_nodes, node, rm_subset_fill, rm_subset, keep_shared=F) {
  ## here node must already be a character, and rm_subset and rm_subset_fill must already be logical vectors (or NULL)
  node_ids = .nodes$nodes$.ID

  if (!is.null(rm_subset_fill)) {
    fill_nodes = fill_nodes[rm_subset_fill,] 
  }
  if (nrow(fill_nodes) == 0) return(.tokens)
  
  if (!is.null(rm_subset)) {
    if (!any(rm_subset)) return(.nodes) ## if there are no nodes that meet the rm_subset condition, nothing is removed
    node_ids = unique(stats::na.omit(node_ids[rm_subset]))
    fill_nodes = fill_nodes[list(node_ids), on='.ID', nomatch=0, allow.cartesian=T]
  }
  if (nrow(fill_nodes) == 0) return(.tokens)
  
  .nodes$fill = .nodes$fill[!fill_nodes, on=c('.ID','.ROLE')]
  
  if (keep_shared) {
    fill_nodes = fill_nodes[!.nodes$fill, on=c('doc_id','sentence','token_id')]  
    if (nrow(fill_nodes) == 0) return(.tokens)
  } 
  
  .tokens = .tokens[!fill_nodes, on=c('doc_id','sentence','token_id')]
  .tokens = fix_missing_parents(.tokens, warn=F)  ## (function in token_index.r)
  
  data.table::setattr(.tokens, '.nodes', value = .nodes)
  .tokens[]
}

#' Copy nodes
#'
#' @param .tokens          A tokenIndex in which nodes are selected with \link{select_nodes}.  
#' @param node             The name of the node that is to be copied
#' @param new              The name given to the copy
#' @param subset           A subset expression (that evaluates to a logical vector). The token column for each labeled node in the tquery can be referred to as label$column.
#' @param keep_relation    If FALSE, remove relation (making node a root)
#' @param copy_fill        If TRUE, also copy the fill
#' @param subset_fill      A subset on the fill nodes. Can only directly use token column. For example, use pos == 'VERB' to copy only verbs
#' @param only_new         If TRUE, direct fill children will only be copied to to_node if it does not already have nodes of this relation. This is a good heuristic for dealing with argument drop. 
#'
#' @export
copy_nodes <- function(.tokens, node, new, subset=NULL, keep_relation=T, copy_fill=F, subset_fill=NULL, only_new=NULL) {
  parent = NULL; relation = NULL
  
  .nodes = .nodes_from_attr(.tokens)  
  if (nrow(.nodes$nodes) == 0) return(.tokens)
  
  if (!is.character(node)) stop('"node" argument has to be a character value')
  if (!is.character(new)) stop('"new" argument has to be a character value')
  if (!node %in% colnames(.nodes$nodes)) stop(sprintf('node (%s) is not a valid node in the tquery results (use the label= argument)', node))
  
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset)) 
  subset = if (subset == 'NULL') NULL else .nodes_eval(.tokens, .nodes, subset)
  if (is.null(subset)) subset = rep(T, nrow(.nodes$nodes))
  
  node_ids = .nodes$nodes[,c('doc_id','sentence',node),with=F]
  node_ids = add_sub_id(.tokens, node_ids[subset])$token_id ## beware of the subset being used here
  .nodes$nodes[[new]] = double(nrow(.nodes$nodes))
  .nodes$nodes[subset, (new) := node_ids]
  
  node_vars = get_node_vars(.tokens, .nodes, node)
  if (nrow(node_vars) > nrow(.nodes$nodes)) browser()
  #.tokens[list(doc_id='text1042', sentence=1, token_id=7.3),]
  
  node_vars = node_vars[subset]
  node_vars$token_id = node_ids
  if (!keep_relation) {
    node_vars[, parent := NA]
    node_vars[, relation := 'ROOT']
  }
  .tokens = add_to_tokens(.tokens, node_vars)
  
  data.table::setattr(.tokens, '.nodes', value = .nodes)
  
  if (copy_fill) {
    fill_nodes = get_fill_nodes(.tokens, .nodes, node)
    if (!is_deparsed_call(subset_fill)) subset_fill = deparse(substitute(subset_fill)) 
    subset_fill = if (subset_fill == 'NULL') NULL else eval(parse(text=subset_fill), envir = fill_nodes, parent.frame())
    .tokens = do_copy_fill(.tokens, .nodes, fill_nodes, node, new, subset, subset_fill, only_new=NULL)
  }
  .tokens[]
}

#' Copy nodes
#'
#' @param .tokens          A tokenIndex in which nodes are selected with \link{select_nodes}.  
#' @param from_node        The name of the node from which fill is copied
#' @param to_node              The name of the node to which fill is copied
#' @param subset           A subset expression (that evaluates to a logical vector). The token column for each labeled node in the tquery can be referred to as label$column.
#' @param subset_fill      A subset on the fill nodes. Can only directly use token column. For example, use pos == 'VERB' to copy only verbs
#' @param only_new         If TRUE, direct fill children will only be copied to to_node if it does not already have nodes of this relation. This is a good heuristic for dealing with argument drop. 
#'
#' @export
copy_fill <- function(.tokens, from_node, to_node, subset=NULL, subset_fill=NULL, only_new=NULL) {
  .nodes = .nodes_from_attr(.tokens)
  if (nrow(.nodes$nodes) == 0) return(.tokens)
  
  if (!is.character(from_node)) stop('"from_node" argument has to be a character value')
  if (!is.character(to_node)) stop('"to_node" argument has to be a character value')
  if (!from_node %in% colnames(.nodes$nodes)) stop(sprintf('from_node (%s) is not a valid node in the tquery results (use the label= argument)', from_node))
  if (!to_node %in% colnames(.nodes$nodes)) stop(sprintf('to_node (%s) is not a valid node in the tquery results (use the label= argument)', to_node))
  
  if (!is_deparsed_call(subset)) subset = deparse(substitute(subset)) 
  subset = if (subset == 'NULL') NULL else .nodes_eval(.tokens, .nodes, subset)
  
  fill_nodes = get_fill_nodes(.tokens, .nodes, from_node)
  
  if (!is_deparsed_call(subset_fill)) subset_fill = deparse(substitute(subset_fill)) 
  subset_fill = if (subset_fill == 'NULL') NULL else eval(parse(text=subset_fill), envir = fill_nodes, parent.frame())
  
  do_copy_fill(.tokens, .nodes, fill_nodes, from_node, to_node, subset, subset_fill, only_new)
}

do_copy_fill <- function(.tokens, .nodes, fill_nodes, from_node, to_node, subset=NULL, subset_fill=NULL, only_new=NULL) {
  .NEW_ID = NULL; token_id = NULL; parent = NULL; .ROLE = NULL
  ## here from_node and to_node must already be characters, and subset and subset_fill must already be logical vectors (or NULL)
  
  if (nrow(fill_nodes) == 0) return(.tokens)
  
  if (!is.null(subset_fill)) {
    fill_nodes = fill_nodes[subset_fill]
  }
  
  if (nrow(fill_nodes) == 0) return(.tokens)
  
  if (!is.null(only_new)) {
    only_new = 'relation' ## could be any column, but think only this makes sense
    first_children_parents = .nodes$nodes[,c('doc_id','sentence',to_node,'.ID'), with=F]
    first_children = rbind(
      merge(.tokens, first_children_parents, by.x=c('doc_id','sentence','token_id'), by.y=c('doc_id','sentence',to_node)),
      merge(.tokens, first_children_parents, by.x=c('doc_id','sentence','parent'), by.y=c('doc_id','sentence',to_node))
    )
    first_children$.FILL_LEVEL = 1
    if (any(!only_new %in% colnames(first_children))) stop('some values in only_new are not valid token columns')
    remove_fill = first_children[,c('.FILL_LEVEL','.ID',only_new),with=F]
    fill_nodes = fill_nodes[!remove_fill, on=c('.FILL_LEVEL','.ID',only_new)]
  }
  if (nrow(fill_nodes) == 0) return(.tokens)
  
  
  if (!is.null(subset)) {
    if (!all(subset)) {
      node_ids = unique(.nodes$nodes$.ID[subset])
      fill_nodes = fill_nodes[list(.ID=node_ids), on='.ID', nomatch=0, allow.cartesian=TRUE]
    }
  }
  if (nrow(fill_nodes) == 0) return(.tokens)
  
  id_index = match(fill_nodes$.ID, .nodes$nodes$.ID)
  from_node_ids = .nodes$nodes[[from_node]][id_index]
  to_node_ids = .nodes$nodes[[to_node]][id_index]
  
  fill_nodes[,.NEW_ID := add_sub_id(.tokens, fill_nodes[,c('doc_id','sentence','token_id')])$token_id]
  
  parent_index = fill_nodes[,c('doc_id','sentence','token_id','parent','.ID','token')]
  parent_index[, .I := 1:nrow(parent_index)]
  
  match_id = fill_nodes[,c('doc_id','sentence','token_id','.ID','.NEW_ID')]
  parent_index = merge(parent_index, match_id, by.x=c('doc_id','sentence','parent','.ID'), by.y=c('doc_id','sentence','token_id','.ID'), allow.cartesian=T)
  .NEW_PARENT = rep(NA, nrow(fill_nodes))
  parent_index = stats::na.omit(parent_index)
  .NEW_PARENT[parent_index$.I] = parent_index$.NEW_ID
  
  fill_nodes[, token_id := .NEW_ID]
  ## if parent is the from_node id (i.e. fill is a direct child that connects the fill tree to the node), replace parent with node id, otherwise create the sub id
  is_direct_child = fill_nodes$parent == from_node_ids
  if (any(is_direct_child)) {
    fill_nodes[is_direct_child, parent := to_node_ids[is_direct_child]]
  }
  if (any(!is_direct_child)) {
    fill_nodes[!is_direct_child, parent := as.double(.NEW_PARENT[!is_direct_child])]
    fill_nodes = remove_isolate_fill(fill_nodes)
  }
  fill_nodes[, .ROLE := to_node]  
  .tokens = add_to_tokens(.tokens, fill_nodes)  
  .nodes = add_to_fill(.nodes, fill_nodes)  
  
  data.table::setattr(.tokens, '.nodes', value = .nodes)
  .tokens[]
}

remove_isolate_fill <- function(fill_nodes) {
  parent = NULL
  
  iso = fill_nodes[is.na(parent),] ## removes isolates, which can result from subsetting or using only_new
  fill_nodes = fill_nodes[!is.na(parent),]
  while (nrow(iso) > 0) {
    iso_i = fill_nodes[list(iso$doc_id, iso$sentence, iso$token_id), on=c('doc_id','sentence','parent'), nomatch=0, which=T]
    if (length(iso_i) > 0) {
      iso = fill_nodes[iso_i,]
      fill_nodes = fill_nodes[!iso_i,]
    } else break
  }
  fill_nodes
}

is_deparsed_call <- function(x) {
  ## for use in functions to pass on deparsed calls, in particular for subset arguments
  ## determine whether x is a deparsed call (call as a string)
  tryCatch(is.character(eval(x)), error = function(e) F, finally = T)
}



add_to_tokens <- function(.tokens, new_tokens) {
  if (!identical(colnames(.tokens), colnames(new_tokens))) new_tokens = subset(new_tokens, select = colnames(.tokens))
  .tokens = unique(rbind(.tokens, new_tokens))
  .tokens = as_tokenindex(.tokens)
  .tokens
}

add_to_fill <- function(.nodes, new_fill) {
  if (!identical(colnames(.nodes$fill), colnames(new_fill))) new_fill = subset(new_fill, select = colnames(.nodes$fill))
  .nodes$fill = unique(rbind(.nodes$fill, new_fill))
  data.table::setindexv(.nodes$fill, '.ROLE')
  .nodes
}

.nodes_eval <- function(.tokens, .nodes, x) {
  ## x has to be an expressions given as a character value (deparse(substitute(x)))
  if (is.null(x)) return(NULL)
  linked_node_vars = get_linked_node_vars(.tokens, .nodes)
  eval(parse(text=x), envir = linked_node_vars, parent.frame())
}

get_node_vars <- function(.tokens, .nodes, node) {
  node_ids = .nodes$nodes[,c('doc_id','sentence',node),with=F]
  data.table::setnames(node_ids, old=node, new='token_id')
  node_vars = .tokens[node_ids, on=c('doc_id','sentence','token_id')]
  #unique(node_vars, by=c('doc_id','sentence','token_id'))
  node_vars
}
  
get_linked_node_vars <- function(.tokens, .nodes) {
  node_names = setdiff(colnames(.nodes$nodes), c('.ID','doc_id','sentence'))
  node_vars = lapply(node_names, function(node) get_node_vars(.tokens, .nodes, node))
  names(node_vars) = node_names
  node_vars
}

get_node_position <- function(.tokens, .nodes, node) {
  node_ids = .nodes$nodes[,c('doc_id','sentence',node), with=F]
  data.table::setnames(node_ids, old=node, new='token_id')
  .tokens[node_ids,on=c('doc_id','sentence','token_id'),which=T]
}

get_fill_nodes <- function(.tokens, .nodes, node) {
  fill_table = .nodes$fill[list(node), on='.ROLE', nomatch=0]
  data.table::setkeyv(fill_table, c('doc_id','sentence','token_id'))
  merge(fill_table, .tokens, by=c('doc_id','sentence','token_id'))
}

increment_sub_id <- function(x) {
  ## use decimals as sub_id by mirroring increment. so increments 3.99 to 3.001 (mirroring 99 to 100), 3.001 to 3.101, etc. 
  int_x = floor(x) 
  dec = round(x - int_x, 6) ## see round at end, for some reason it has to go here as well
  dec = ifelse(dec > 0, stringi::stri_extract_first_regex(x, '[0-9]+$'), '0')
  dec = as.numeric(stringi::stri_reverse(dec)) + 1
  sub_id = as.numeric(stringi::stri_reverse(sprintf('%.1f', dec)))
  round(int_x + sub_id, 6) ## round is necessary, because sub_id is sliiiiiigly more than what you see, which messes up the stri_count_regex for number of digits 
}

add_sub_id <- function(.tokens, ids) {
  token_id = NULL
  
  .EXISTS = NULL
  ## ids should be a data.table with the first three columns being: doc_id, sentence, token_id
  ids = data.table::copy(ids)
  data.table::setnames(ids, c('doc_id','sentence','token_id'))
  data.table::setkeyv(ids, c('doc_id','sentence','token_id'))
  
  #matched = rep(1, nrow(ids)) ## ids should initially exists
  #exists = rep(T, nrow(ids))
  ids[, .EXISTS := T]
  while (any(ids$.EXISTS)) {
    ids[ids$.EXISTS, token_id := increment_sub_id(ids$token_id[ids$.EXISTS])]
    e = .tokens[unique(ids[ids$.EXISTS]), on=c('doc_id','sentence','token_id'), which=F, nomatch=0]
    if (nrow(e) == 0) break
    ids[, .EXISTS := F]
    ids[e, on=c('doc_id','sentence','token_id'), .EXISTS := T]
  }
  
  dup = duplicated(ids, by = c('doc_id','sentence','token_id'))
  while (any(dup)) {
    ids[dup, token_id := increment_sub_id(ids$token_id[dup])]
    dup = duplicated(ids, by = c('doc_id','sentence','token_id'))
  }
  
  e = .tokens[unique(ids), on=c('doc_id','sentence','token_id'), which=F, nomatch=0]
  if (nrow(e) > 0) {
    ids[, .EXISTS := NULL]
    #stop('en nog een keer!!')
    ids = add_sub_id(.tokens, ids)
  }
  
  ids
}






