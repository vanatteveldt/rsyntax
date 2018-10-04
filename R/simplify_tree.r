simplify_tree <- function(tokens, bypass=NULL, isolate=NULL, link_children=NULL, only_lowest_level=T, copy=T) {
  tokens = as_tokenindex(tokens)
  if (copy) tokens = data.table::copy(tokens)
  if (!is.null(bypass)) tokens = reshape_tree(tokens, relation=bypass, mode='bypass', link_children = link_children, only_lowest_level=only_lowest_level, link_children_block = isolate, copy=F)
  if (!is.null(isolate)) tokens = reshape_tree(tokens, relation=isolate, mode='isolate', link_children = link_children, only_lowest_level=only_lowest_level, copy=F)
  tokens
}

reshape_tree <- function(tokens, relation, mode=c('isolate','bypass'), link_children=NULL, link_children_block=NULL, only_lowest_level=T, copy=T) {
  mode = match.arg(mode)
  nodes = find_path(tokens, relation=relation)
  
  parent = NULL; relation = NULL; .REL_LEVEL = NULL; .MATCH_ID = NULL; .ADDED = NULL
  
  
  if (is.null(nodes)) return(tokens)
  
  ## replace for the original node the parent and relation with those of the head node
  if (copy) tokens = data.table::copy(tokens) else tokens = tokens
  i = tokens[list(nodes$doc_id, nodes$sentence, nodes$token_id),on=c('doc_id','sentence','token_id'),which=T]
  
  if (mode == 'bypass') {
    tokens[i, parent := nodes$parent]
    tokens[i, relation := nodes$relation]
  }
  if (mode == 'isolate') {
    #print('kankerr')
    tokens[i, parent := NA]
    tokens[i, relation := 'ROOT']
  }
  tokens[i, .REL_LEVEL := nodes$.REL_LEVEL]
  
  if (!is.null(link_children)) {
    tokens = add_link_children(tokens, nodes, link_children, link_children_block, only_lowest_level=only_lowest_level)
  }
  as_tokenindex(tokens)
}

find_path <- function(tokens, ...) {
  d = filter_tokens(tokens, lookup=list(...))
  
  if (nrow(d) == 0) return(d)
  d$.MATCH_ID = d$token_id
  fam = d
  
  ## also add bypass with token_id_head to itself, so that in add
  out = list() 
  out[['']] = data.table::data.table(doc_id=fam$doc_id, sentence=fam$sentence, token_id=fam$.MATCH_ID, token_id_head=fam$.MATCH_ID, .REL_LEVEL=0, parent=fam$parent, relation=fam$relation)
  
  rematch = NULL ## for deeper than one level, carry on the previous .MATCH_ID
  level = 0L
  while (TRUE) {
    level = level + 1
    fam = token_family(tokens, depth = 1, ids=fam[,c('doc_id','sentence','token_id')], level = 'parents', show_level = T, replace = T)
    if (nrow(fam) == 0) break
    if (!is.null(rematch)) fam$.MATCH_ID = rematch$match_id[match(fam$.MATCH_ID, rematch$new_match_id)]
    out[['']] = data.table::data.table(doc_id=fam$doc_id, sentence=fam$sentence, token_id=fam$.MATCH_ID, token_id_head=fam$token_id, .REL_LEVEL=level, parent=fam$parent, relation=fam$relation)
    
    #is_bypass = fam$relation %in% bypass
    #fam = fam[list(d$doc_id, d$sentence, d$token_id), on=c('doc_id','sentence','token_id'),nomatch=0]
    fam = filter_tokens(fam, .G_ID = d[,c('doc_id','sentence','token_id')])
    if (nrow(fam) > 0) {
      #fam = fam[list(bypass),,on='relation', nomatch=0]
      rematch = data.table::data.table(match_id = fam$.MATCH_ID, new_match_id = fam$token_id)
    } else break
  }
  if (length(out) == 0) return(NULL) 
  out = data.table::rbindlist(out)
  data.table::setkeyv(out, c('doc_id','sentence','token_id'))
  data.table::setindexv(out, c('doc_id','sentence','token_id_head'))
  out
}

add_link_children <- function(tokens, nodes, link_children, link_children_block, only_lowest_level=T) {
  ids = unique(rbind(data.table::data.table(doc_id=nodes$doc_id, sentence=nodes$sentence, token_id=nodes$token_id),
                     data.table::data.table(doc_id=nodes$doc_id, sentence=nodes$sentence, token_id=nodes$token_id_head)))
  add = token_family(tokens, depth = 1, ids=ids, level = 'children', replace = T)
  add[, .REL_LEVEL := NULL]
  
  if (!is.null(link_children) && is.character(link_children)) {
    add = subset(add, add$relation %in% link_children)
  }
  
  ## link_children_block is used to block given relations from being filled. (in token_family below)
  ## this is used to skip the isolate relations
  fam_lookup = if(is.null(link_children_block)) NULL else list(relation__N = link_children_block)
  
  
  if (!is.null(add)) {
    add = unique(add, by=c('doc_id','sentence','token_id','parent'))
    add = merge(add, data.table::data.table(new_parent=nodes$token_id, parent=nodes$token_id_head, .REL_LEVEL=nodes$.REL_LEVEL), by='parent', allow.cartesian=T)
    
    if (only_lowest_level) {
      ## sort by bypass level, look for the non-duplicates, and from the non duplicates return all with the same bypass level (we allow duplicates on the same level)
      data.table::setorder(add, '.REL_LEVEL')
      not_dup = !duplicated(add, by = c('doc_id','sentence','new_parent'))
      add = add[add[not_dup,c('doc_id','sentence','new_parent','.REL_LEVEL')],on=c('doc_id','sentence','new_parent','.REL_LEVEL')]
    }
    add$parent = add$new_parent
    add$.MATCH_ID = add$new_parent
    add = subset(add, subset=add$.REL_LEVEL > 0, select=colnames(tokens))
    
    add = as_tokenindex(add)
    
    fam = token_family(tokens, id = unique(add[,c('doc_id','sentence','token_id')]), minimal=F, 
                       level='children', depth = Inf, lookup=fam_lookup)
    
    
    largest_id = max(nchar(unique(tokens$token_id)))
    fam = merge(fam, data.table::data.table(.LINK_PARENT = add$parent, .LINK_BYPASS_LEVEL=add$.REL_LEVEL, token_id=add$token_id),
                by.x='.MATCH_ID', by.y='token_id', allow.cartesian=T)
    add$token_id = (add$parent) + token_sub_id(add$token_id, largest_id)
    
    if (nrow(fam) > 0) {
      fam[, .REL_LEVEL := fam$.LINK_BYPASS_LEVEL,]
      fam$token_id = fam$.LINK_PARENT + token_sub_id(fam$token_id, largest_id)
      fam$parent = fam$.LINK_PARENT + token_sub_id(fam$parent, largest_id)
      add = rbind(add, subset(fam, select=colnames(add)))
    }
  }
  tokens[,.ADDED := F]
  add[,.ADDED := T]
  rbind(tokens,add)
}

token_sub_id <- function(x, highest=x) {
  x[x == 0] = 0.01 ## in case the token is zero, since 0.0 would mess things up
  ##x / 10^(ceiling(log10(highest)))
  x / 10^(nchar(highest) + 1)
}
