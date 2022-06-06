find_nodes <- function(tokens, tquery, block=NULL, use_index=TRUE, name=NA, fill=TRUE, melt=TRUE, root_dist=FALSE) {
  .MATCH_ID = NULL; .DROP = NULL; .ID = NULL ## declare data.table bindings
  tokens = as_tokenindex(tokens)  
  block = get_long_ids(block)
  
  nodes = filter_tokens(tokens, lookup=tquery$lookup, .G_ID=tquery$g_id, .BLOCK=block, use_index=use_index)
  if (nrow(nodes) == 0) return(NULL)
  nodes = subset(nodes, select = c('doc_id','sentence','token_id'))

  any_nonfill_nested = any(sapply(tquery$nested, function(x) !methods::is(x, 'tQueryFill')))
  if (any_nonfill_nested) {
    nodes = find_nested(tokens, nodes, tquery, block, fill=FALSE, block_loop=F)
  } else {
    data.table::setnames(nodes, old = 'token_id', new='.ID')
    if (!is.na(tquery$label)) nodes[,(tquery$label) := .ID]
  } 

  if (is.null(nodes)) return(NULL)  
  if (nrow(nodes) == 0) return(NULL)

  ### possible solution for removing block within rec_search
  nodes = get_root_dist(tokens, nodes)
  nodes = get_unique_patterns(nodes)
  if (!root_dist) nodes$.ROOT_DIST = NULL
  
  if (fill) nodes = add_fill(tokens, nodes, tquery, block=nodes)

  
  nodes = create_unique_key(nodes, name, tquery)
  if (melt) {
    nodes = melt_nodes_list(nodes)
  }
  nodes[]
}

find_nested <- function(tokens, nodes, tquery, block, fill, block_loop) {
  .ID = NULL; .MATCH_ID = NULL

  nodes = rec_find(tokens, ids=nodes, ql=tquery$nested, block=block, fill=fill, block_loop=block_loop)
  
  if (nrow(nodes) == 0) return(NULL)
  nodes[, .ID := .MATCH_ID]
  data.table::setcolorder(nodes, c('.ID', setdiff(colnames(nodes), '.ID')))
  
  if (is.na(tquery$label)) {
    nodes[,.MATCH_ID := NULL]
  } else {
    data.table::setnames(nodes, '.MATCH_ID', tquery$label)
  }
  
  dropcols = grep('.DROP.*', colnames(nodes), value=TRUE)
  if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
  
  unique(nodes)
}

add_fill <- function(tokens, nodes, tquery, block, level=1) {
  is_fill = sapply(tquery$nested, methods::is, 'tQueryFill')

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
    ids = unique(stats::na.omit(ids))
    add = rec_find(tokens, ids, tquery$nested[is_fill], block = block, fill=TRUE, block_loop=T)
    
    if (grepl('#', tquery$label)) {
      label = gsub('#.*', '', tquery$label)
      label = paste0('^', label, '\\_')
      colnames(add) = gsub(label, tquery$label, colnames(add))
    }

    if (nrow(add) > 0) {
      setkeyv(nodes, c('doc_id','sentence',match_id))
      nodes = merge(nodes, add, by.x=c('doc_id','sentence',match_id), by.y=c('doc_id','sentence','.MATCH_ID'), all.x=TRUE, allow.cartesian=TRUE)
      dropcols = grep('.DROP.*', colnames(nodes), value=TRUE)
      if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
    }
  }
  unique(nodes)
}

get_top_label <- function(tquery) {
  ## get the first label in a tquery.
  if (!is.na(tquery$label)) return(tquery$label)
  if (!is.null(tquery$nested)) {
    for (nested in tquery$nested) {
      label = get_top_label(nested)
      if (!is.na(label)) return(label)
    }
  }
  return(NA)
}

create_unique_key <- function(nodes, name, tquery){
  id_col = get_top_label(tquery)
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

get_root_dist <- function(tokens, nodes) {
  .ROOT_DIST = NULL
  tf = token_family(tokens, unique(data.table(doc_id=nodes$doc_id, sentence=nodes$sentence, token_id=nodes$.ID)), 
                    depth=Inf, level='parents', minimal=TRUE, show_level=TRUE, replace=TRUE)
  tf = data.table::setorderv(tf, cols = '.FILL_LEVEL', order = -1)
  tf = unique(tf, by=c('doc_id','sentence','.MATCH_ID'))
  data.table::setnames(tf, c('.FILL_LEVEL', '.MATCH_ID'), c('.ROOT_DIST', '.ID'))
  tf = subset(tf, select=c('doc_id','sentence','.ID','.ROOT_DIST'))
  nodes = merge(nodes, tf, by = c('doc_id','sentence','.ID'), all.x=TRUE)
  #nodes = nodes[list(tf$doc_id, tf$sentence, tf$.MATCH_ID), .ROOT_DIST := tf$.FILL_LEVEL, on=c('doc_id','sentence','.ID')]
  nodes[is.na(nodes$.ROOT_DIST), .ROOT_DIST := 0]
  nodes
}



get_unique_patterns <- function(nodes) {
  ln = nodes
  ln$i = 1:nrow(ln)
  ln = data.table::melt(ln, id.vars=c('doc_id','sentence','.ID','i','.ROOT_DIST'))
  ln = ln[!is.na(ln$value),]
  data.table::setorderv(ln, c('doc_id','sentence','.ID','i'))

  ## rm duplicate i-value pairs
  rm_i = unique(ln$i[duplicated(ln[,c('i','value')])])
  if (length(rm_i > 0)) ln = ln[-ln[list(i=rm_i), on='i', which=T]]
  
  ## If nodes are matched multiple times, remove the ones where the root dist is higher
  ## (these are most often nested in the other pattern, unless very compliated tqueries are used)
  possible_dupl = unique(ln$value[duplicated(ln[,c('doc_id','sentence','value')])])
  possible_dupl = unique(ln[list(value=possible_dupl),,on='value']$i)
  dup = get_duplicates(ln[list(i=possible_dupl),,on='i'])
  
  ## A complication is that once we remove a duplicate/nested pattern, 
  ## It might also solve another duplicate. So we can't just remove
  ## all duplicates. We can see which duplicates certainly need to be removed
  ## by looking which duplicates are not 'solved' by removing other duplicates
  ## We then repeat this until no remain
  ## this loop is guaranteed to remove one pattern per sentence per iteration (so it's fairly short) 
  rm_j = rep(F, nrow(nodes))
  while (TRUE) {
    dupl_pat = unique(dup$i.x)    ## what are patterns (by index i in nodes) that seem to be duplicates?
    possible_nondupl = dup[list(i.y = dupl_pat),,on='i.y',which=T,nomatch=0] ## get all duplicates where the matched pattern is a duplicate in another pattern 
    if (length(possible_nondupl) > 0) {
      certain_dupl = dup[-possible_nondupl]    ## if the matched pattern is not a duplicate in another pattern, we can be sure it's a definitive duplicate
      rm_j[certain_dupl$i.x] = T
    } else {
      rm_j[dup$i.x] = T
      break
    }
    possible_dupl = setdiff(possible_nondupl, unique(certain_dupl$i.x))  ## now repeat for remaining possible duplicates
    dup = get_duplicates(ln[list(i=possible_dupl),,on='i'])
  }
  rm_j = which(rm_j)

  nodes
  if (length(rm_i) > 0 || length(rm_j) > 0)
    nodes = nodes[-unique(c(rm_i, rm_j)),]
  
  nodes
  
}  

get_duplicates <- function(ln, priority='higher') {
  ln_m = merge(ln[,!colnames(ln) == 'variable', with=F], 
               ln[,c('doc_id','sentence','.ID','.ROOT_DIST','value','i')], 
               by=c('doc_id','sentence','value'), allow.cartesian = T)
  ln_m = ln_m[ln_m$.ID.x != ln_m$.ID.y,]
  
  if (priority == 'higher') {
    dupl = ifelse(ln_m$.ROOT_DIST.x != ln_m$.ROOT_DIST.y, 
                  ln_m$.ROOT_DIST.x > ln_m$.ROOT_DIST.y,       ## remove x if x lower in tree
                  ln_m$.ID.x > ln_m$.ID.y)                     ## and if same height remove x if to the left in sentence
  } else {
    dupl = ifelse(ln_m$.ROOT_DIST.x != ln_m$.ROOT_DIST.y, 
                  ln_m$.ROOT_DIST.x < ln_m$.ROOT_DIST.y,       ## remove x if x higher in tree
                  ln_m$.ID.x > ln_m$.ID.y)                     ## and if same height remove x if to the left in sentence
  }
  ui = unique(ln_m$i.x[dupl])
  ln_m[list(i.x=ui),,on='i.x']
}