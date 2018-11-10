find_nodes <- function(tokens, tquery, block=NULL, use_index=T, name=NA, fill=T, melt=T) {
  .MATCH_ID = NULL; .DROP = NULL ## declare data.table bindings
  tokens = as_tokenindex(tokens)  
  block = get_long_ids(block)
  nodes = filter_tokens(tokens, lookup=tquery$lookup, .G_ID=tquery$g_id, .BLOCK=block, use_index=use_index)
  if (nrow(nodes) == 0) return(NULL)
  nodes = subset(nodes, select = c('doc_id','sentence','token_id'))
  
  any_req_nested = any(sapply(tquery$nested, function(x) x$req))
  if (any_req_nested) {
    nodes = find_nested(tokens, nodes, tquery, block, fill=F)
  } else {
    data.table::setnames(nodes, old = 'token_id', new='.ID')
    if (!is.na(tquery$label)) nodes[,(tquery$label) := .ID]
  } 
  if (is.null(nodes)) return(NULL)  
  if (nrow(nodes) == 0) return(NULL)

  if (fill) nodes = add_fill(tokens, nodes, tquery, block=nodes)
  nodes = create_unique_key(nodes, name)
  if (melt) {
    nodes = melt_nodes_list(nodes)
  }
  nodes[]
}

find_nested <- function(tokens, nodes, tquery, block, fill) {
  nodes = rec_find(tokens, ids=nodes, ql=tquery$nested, block=block, fill=fill)
  if (nrow(nodes) == 0) return(NULL)
  nodes[, .ID := .MATCH_ID]
  data.table::setcolorder(nodes, c('.ID', setdiff(colnames(nodes), '.ID')))
  
  if (is.na(tquery$label)) {
    nodes[,.MATCH_ID := NULL]
  } else {
    data.table::setnames(nodes, '.MATCH_ID', tquery$label)
  }
  
  dropcols = grep('.DROP.*', colnames(nodes), value=T)
  if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
  
  unique(nodes)
}

add_fill <- function(tokens, nodes, tquery, block, level=1) {
  #is_req = sapply(tquery$nested, function(x) x$req)
  is_fill = sapply(tquery$nested, is, 'tQueryFill')
  
  if (any(!is_fill)) {
    for (tq in tquery$nested[!is_fill]) {
      nodes = add_fill(tokens, nodes, tq, block, level+1)
    }
  } 

  if (any(is_fill)) {
    if (is.na(tquery$label)) {
      if (level == 1) match_id = '.ID' else return(nodes)
    } else match_id = tquery$label
    if (!match_id %in% colnames(nodes)) return(nodes)
    ids = subset(nodes, select = c('doc_id','sentence',match_id))
    ids = unique(na.omit(ids))
    add = rec_find(tokens, ids, tquery$nested[is_fill], block = block, fill=T)
    if (nrow(add) > 0) {
      setkeyv(nodes, c('doc_id','sentence',match_id))
      
      nodes = merge(nodes, add, by.x=c('doc_id','sentence',match_id), by.y=c('doc_id','sentence','.MATCH_ID'), all.x=T, allow.cartesian=T)
      dropcols = grep('.DROP.*', colnames(nodes), value=T)
      if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
    }
  }
  unique(nodes)
}

create_unique_key <- function(nodes, name){
  #if (ncol(nodes) > 3) {
  #  key = paste0(name, '(', nodes$.ID, ':', do.call(paste, args = c(nodes[,-(1:3)], sep='.')), ')')
  #} else {
  #  key = paste0(name, '(', nodes$.ID, ')')
  #}      
  id_col = setdiff(colnames(nodes), c('doc_id','sentence','.ID'))[1]
  #key = paste0(name, '#', nodes$doc_id, '.', nodes$sentence, '.', match(nodes$.ID, unique(nodes$.ID)))
  if (!is.na(name)) {
    key = paste0(name, '#', nodes$doc_id, '.', nodes$sentence, '.', nodes[[id_col]])
  } else {
    key = paste0(nodes$doc_id, '.', nodes$sentence, '.', nodes[[id_col]])
  }


  nodes$.ID = paste0(nodes$doc_id, '...', nodes$sentence, '...', nodes$.ID) ## quick fix for matching on 3 columns
  key = key[match(nodes$.ID, nodes$.ID)] ## give same id to nodes with same .ID
  #key = paste0(name, '#', 1:nrow(nodes))
  nodes$.ID = key
  return(nodes)
}



