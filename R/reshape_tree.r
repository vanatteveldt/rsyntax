treshape_link <- function(..., link_children, link_fill=fill(), add_left=T) {
  if (is.character(link_children)) link_children = children(relation = link_children)
  l = list(tquery = treshape_query(...), mode='link_children', link_children=link_children, link_fill=link_fill, add_left=add_left)
  class(l) = c('tReshape', class(l))
  l
}
treshape_bypass <- function(...) {
  l = list(tquery = treshape_query(...), mode='bypass')
  class(l) = c('tReshape', class(l))
  l
}
treshape_isolate <- function(..., copy_parent=T, parent_fill=fill()) {
  l = list(tquery = treshape_query(...), mode='isolate', copy_parent=copy_parent, parent_fill=parent_fill)
  class(l) = c('tReshape', class(l))
  l
}
treshape_remove <- function(...) {
  l = list(tquery = treshape_query(...), mode='remove')
  class(l) = c('tReshape', class(l))
  l
}


treshape_query <- function(...){
  l = list(...)
  save = '.RESHAPE_KEY'
  
  if (length(l) > 0) {
    l = l[!sapply(l, is, 'tQueryFill')]
  }
  if (length(l) > 0) {
    is_nested = sapply(l, is, 'tQueryParent') | sapply(l, is, 'tQueryChild') 
    q = list(g_id=NULL, save=save, lookup = l[!is_nested], nested=l[is_nested])
  } else {
    q = list(g_id=NULL, save=save, lookup =NULL, nested=NULL)
  }
  q = safe_names(q)
  class(q) = c('tQuery', class(q))
  q
}


apply_reshapes <- function(tokens, ..., copy=T) {
  tokens = as_tokenindex(tokens)
  
  r = list(...)
  is_treshape = sapply(r, is, 'tReshape')
  r = c(r[is_treshape], unlist(r[!is_treshape], recursive = F))

  if (!any(sapply(r, is, 'tReshape'))){
    ## to prevent needless copy if copy is true but tReshape queries are not used
    return(tokens)
  }
  
  if (copy) tokens = data.table::copy(tokens)
      
  for (i in 1:length(r)){
    if (!is(r[[i]], 'tReshape')) next
    tokens = reshape_tree(tokens, r[[i]], copy=F)
   }
  tokens
}


reshape_tree <- function(tokens, tr, copy=T) {
  nodes = find_nodes(tokens, tr$tquery, block=NULL, name='treshape', add_unreq = F, melt = F)
  if (is.null(nodes)) return(tokens)
  
  parent = NULL; relation = NULL; .REL_LEVEL = NULL; .MATCH_ID = NULL; .ADDED = NULL
  
  ## replace for the original node the parent and relation with those of the head node
  if (copy) tokens = data.table::copy(tokens) else tokens = tokens
  
  if (tr$mode == 'link_children') {
    path = find_path(tokens, nodes)
    if (is.null(path)) return(tokens)
    tokens = add_link_children(tokens, path, tr$link_children, tr$link_fill, only_lowest_level=T, add_left=tr$add_left)
  }
  
  if (tr$mode == 'bypass') {
    path = find_path(tokens, nodes)
    if (is.null(path)) return(tokens)
    i = tokens[list(path$doc_id, path$sentence, path$token_id),on=c('doc_id','sentence','token_id'),which=T]
    tokens[i, parent := path$parent]
    tokens[i, relation := path$relation]
  }
  
  if (tr$mode == 'isolate') {
    i = tokens[list(nodes$doc_id, nodes$sentence, nodes$.RESHAPE_KEY),on=c('doc_id','sentence','token_id'),which=T]
    if (length(i) > 0) {
      if (tr$copy_parent) {
        tokens = add_isolate_parents(tokens, i, tr$parent_fill)
      } else {
        tokens[i, parent := NA]
        tokens[i, relation := 'ROOT']
      }
    }
  }
  
  if (tr$mode == 'remove') {
    ids = nodes[,c('doc_id','sentence','.RESHAPE_KEY')]
    data.table::setnames(ids, old='.RESHAPE_KEY', new = 'token_id')
    rm = token_family(tokens, ids, level = 'children', depth = Inf, replace=F, minimal = T)
    if (nrow(rm) > 0) ids = unique(rbind(ids, rm[,c('doc_id','sentence','token_id')]))
    tokens = tokens[!ids, on=c('doc_id','sentence','token_id')]
    return(as_tokenindex(tokens))
  }
  
  as_tokenindex(tokens)
}

find_path <- function(tokens, nodes) {
  d = filter_tokens(tokens, .G_ID = list(nodes$doc_id, nodes$sentence, nodes$.RESHAPE_KEY))
  #d = filter_tokens(tokens, lookup=list(...))
  
  if (nrow(d) == 0) return(d)
  d$.MATCH_ID = d$token_id
  fam = d
  
  ## also add bypass with token_id_head to itself, so that in link_children the direct children come first
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

add_isolate_parents <- function(tokens, i, parent_fill, add_left=T) {
  isolated = tokens[i,]
  add = tokens[list(isolated$doc_id, isolated$sentence, isolated$parent), on=c('doc_id','sentence','token_id')]
  tokens[i, parent := NA]
  
  #add$.LINK_PARENT = isolated$token_id
  if (add_left) {
    first_child = tokens[list(isolated$doc_id, isolated$sentence, isolated$token_id), on=c('doc_id','sentence','parent')]
    first_child = unique(first_child, by=c('doc_id','sentence','parent'))
    add$.LINK_PARENT = first_child$token_id - 1
  } else {
    add$.LINK_PARENT = isolated$token_id
  }
  add$parent = NA
  add$relation = 'ROOT'
  
  
  fam = select_token_family(tokens, unique(add[,c('doc_id','sentence','token_id')]), q = parent_fill, block = NULL)
  fam = merge(fam, data.table::data.table(.LINK_PARENT = add$.LINK_PARENT, token_id=add$token_id),
              by.x='.MATCH_ID', by.y='token_id', allow.cartesian=T)
  
  largest_id = max(nchar(unique(tokens$token_id)))
  add$token_id = (add$.LINK_PARENT) + token_sub_id(add$token_id, largest_id)
  tokens[i, parent := add$token_id]
  
  if (nrow(fam) > 0) {
    fam$token_id = fam$.LINK_PARENT + token_sub_id(fam$token_id, largest_id)
    fam$parent = fam$.LINK_PARENT + token_sub_id(fam$parent, largest_id)
    add = rbind(add, subset(fam, select=colnames(add)))
  }
  
  tokens[,.ADDED := F]
  add[,.ADDED := T]
  add[, .LINK_PARENT := NULL]
  rbind(tokens,add)
}

add_link_children <- function(tokens, path, link_children, link_fill, only_lowest_level=T, add_left=T) {
  if (is.null(path)) return(tokens)
  i = tokens[list(path$doc_id, path$sentence, path$token_id),on=c('doc_id','sentence','token_id'),which=T]
  tokens[i, .REL_LEVEL := path$.REL_LEVEL]
  
  ids = unique(rbind(data.table::data.table(doc_id=path$doc_id, sentence=path$sentence, token_id=path$token_id),
                     data.table::data.table(doc_id=path$doc_id, sentence=path$sentence, token_id=path$token_id_head)))
  link_children$save = '.LINK_KEY'
  link_ids = rec_find(tokens, ids=ids, ql = list(link_children))
  add = tokens[list(link_ids$doc_id, link_ids$sentence, link_ids$.LINK_KEY), on=c('doc_id','sentence', 'token_id')]
  
  if (!is.null(add)) {
    add = unique(add, by=c('doc_id','sentence','token_id','parent'))
    add[, .REL_LEVEL := NULL]
    add = merge(add, data.table::data.table(new_parent=path$token_id, parent=path$token_id_head, .REL_LEVEL=path$.REL_LEVEL), by='parent', allow.cartesian=T)
    
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
    
    if (add_left) {
      first_child = tokens[list(add$doc_id, add$sentence, add$parent), on=c('doc_id','sentence','parent')]
      first_child = unique(first_child, by=c('doc_id','sentence','parent'))
      add$.LINK_PARENT = first_child$token_id - 1
    } else {
      add$.LINK_PARENT = add$parent
    }
    
    fam = select_token_family(tokens, unique(add[,c('doc_id','sentence','token_id')]), q = link_fill, block = NULL)

    largest_id = max(nchar(unique(tokens$token_id)))
    fam = merge(fam, data.table::data.table(.LINK_PARENT = add$.LINK_PARENT, .LINK_BYPASS_LEVEL=add$.REL_LEVEL, token_id=add$token_id),
                by.x='.MATCH_ID', by.y='token_id', allow.cartesian=T)
    add$token_id = (add$.LINK_PARENT) + token_sub_id(add$token_id, largest_id)
    
    if (nrow(fam) > 0) {
      fam[, .REL_LEVEL := fam$.LINK_BYPASS_LEVEL,]
      fam$token_id = fam$.LINK_PARENT + token_sub_id(fam$token_id, largest_id)
      fam$parent = fam$.LINK_PARENT + token_sub_id(fam$parent, largest_id)
      add = rbind(add, subset(fam, select=colnames(add)))
    }
  }
  tokens[,.ADDED := F]
  add[,.ADDED := T]
  add[, .LINK_PARENT := NULL]
  rbind(tokens,add)
}

token_sub_id <- function(x, highest=x) {
  x[x == 0] = 0.01 ## in case the token is zero, since 0.0 would mess things up
  ##x / 10^(ceiling(log10(highest)))
  x / 10^(nchar(highest) + 1)
}
