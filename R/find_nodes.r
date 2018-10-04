find_nodes <- function(tokens, tquery, block=NULL, use_index=T, name=NULL) {
  .MATCH_ID = NULL; .DROP = NULL ## declare data.table bindings
  safe_save_name(tquery$save)
  tokens = as_tokenindex(tokens)  
  block = get_long_ids(block)
  
  nodes = filter_tokens(tokens, lookup=tquery$lookup, .G_ID=tquery$g_id, .BLOCK=block, use_index=use_index)
  if (nrow(nodes) == 0) return(NULL)
  nodes = subset(nodes, select = c('doc_id','sentence','token_id'))
  
  any_req_nested = any(sapply(tquery$nested, function(x) x$req))
  #any_req_nested = length(tquery$nested) > 0
  if (any_req_nested) {
    nodes = find_nested(tokens, nodes, tquery, block, only_req=T)
  } else {
    data.table::setnames(nodes, old = 'token_id', new='.ID')
    if (!is.na(tquery$save)) nodes[,(tquery$save) := .ID]
  } 
  if (is.null(nodes)) return(NULL)
  if (nrow(nodes) == 0) return(NULL)
  
  nodes = add_unrequired(tokens, nodes, tquery, block=nodes)
  nodes = create_unique_key(nodes, name)
  nodes = melt_nodes_list(nodes)
  nodes[]
}

find_nested <- function(tokens, nodes, tquery, block, only_req) {
  nodes = rec_find(tokens, ids=nodes, ql=tquery$nested, block=block, only_req=only_req)
  if (nrow(nodes) == 0) return(NULL)
  nodes[, .ID := .MATCH_ID]
  data.table::setcolorder(nodes, c('.ID', setdiff(colnames(nodes), '.ID')))
  
  if (is.na(tquery$save)) {
    nodes[,.MATCH_ID := NULL]
  } else {
    data.table::setnames(nodes, '.MATCH_ID', tquery$save)
  }
  
  dropcols = grep('.DROP.*', colnames(nodes), value=T)
  if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
  
  unique(nodes)
}

add_unrequired <- function(tokens, nodes, tquery, block) {
  is_req = sapply(tquery$nested, function(x) x$req)
  if (any(is_req)) {
    for (tq in tquery$nested[is_req]) {
      nodes = add_unrequired(tokens, nodes, tq, block)
    }
  } 
  if (any(!is_req)) {
    if (is.na(tquery$save)) return(nodes)
    if (!tquery$save %in% colnames(nodes)) return(nodes)
    ids = subset(nodes, select = c('doc_id','sentence',tquery$save))
    add = rec_find(tokens, ids, tquery$nested[!is_req], block = block, only_req=F)
    if (nrow(add) > 0) {
      setkeyv(nodes, c('doc_id','sentence',tquery$save))
      nodes = merge(nodes, add, by.x=c('doc_id','sentence',tquery$save), by.y=c('doc_id','sentence','.MATCH_ID'), all.x=T, allow.cartesian=T)
      dropcols = grep('.DROP.*', colnames(nodes), value=T)
      if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
    }
  }
  unique(nodes)
}

function(){
  tokens = tokens_dutchquotes
  tokens = as_tokenindex(tokens)
  tquery = alpino_quote_queries()[[1]]
  #tquery = tquery(POS='VB*', save='test', children(save='testing'))
  nodes = find_nodes(tokens, tquery)
  
  add_unrequired(tokens, nodes, tquery, block)
  
}

create_unique_key <- function(nodes, name){
  #if (ncol(nodes) > 3) {
  #  key = paste0(name, '(', nodes$.ID, ':', do.call(paste, args = c(nodes[,-(1:3)], sep='.')), ')')
  #} else {
  #  key = paste0(name, '(', nodes$.ID, ')')
  #}        
  key = paste0(name, '#', match(nodes$.ID, unique(nodes$.ID)))
  #key = paste0(name, '#', 1:nrow(nodes))
  nodes$.ID = key
  return(nodes)
}



safe_save_name <- function(name) {
  if(grepl('\\.[A-Z]', name)) stop(sprintf('save name cannot be all-caps and starting with a dot'))
  if(grepl(',', name)) stop(sprintf('save name cannot contain comma'))
  
  special_names = c('doc_id','token_id','sentence')
  if (name %in% special_names) stop(sprintf('save name (%s) cannot be the same as the special tokenIndex column names (%s)', name, paste(special_names, collapse=', ')))
}



