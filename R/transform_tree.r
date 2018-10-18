follow_bypass <- function(tokens, conj) {
  d = tokens[list(get(conj)),,on='relation', nomatch=0]
  if (nrow(d) == 0) return(d)
  d$.MATCH_ID = d$token_id
  fam = d
  
  ## also add conj with token_id_head to itself, so that in add
  out = list() 
  out[['']] = data.table::data.table(doc_id=fam$doc_id, sentence=fam$sentence, token_id=fam$.MATCH_ID, token_id_head=fam$.MATCH_ID, .CONJ_LEVEL=0, parent=fam$parent, relation=fam$relation)
  
  rematch = NULL ## for deeper than one level, carry on the previous .MATCH_ID
  level = 0L
  while (TRUE) {
    level = level + 1
    fam = token_family(tokens, depth = 1, ids=fam[,c('doc_id','sentence','token_id')], level = 'parents', show_level = T, replace = T)
    
    if (!is.null(rematch)) fam$.MATCH_ID = rematch$match_id[match(fam$.MATCH_ID, rematch$new_match_id)]
    
    out[['']] = data.table::data.table(doc_id=fam$doc_id, sentence=fam$sentence, token_id=fam$.MATCH_ID, token_id_head=fam$token_id, .CONJ_LEVEL=level, parent=fam$parent, relation=fam$relation)

    is_conj = fam$relation %in% conj
    if (any(is_conj)) {
      fam = fam[list(get(conj)),,on='relation', nomatch=0]
      rematch = data.table::data.table(match_id = fam$.MATCH_ID, new_match_id = fam$token_id)
    } else break
  }
  if (length(out) == 0) return(NULL) 
  out = data.table::rbindlist(out)
  data.table::setkeyv(out, c('doc_id','sentence','token_id'))
  data.table::setindexv(out, c('doc_id','sentence','token_id_head'))
  out
}

unpack_bypass <- function(tokens, bypass, link_children, only_first_level=T, copy=T) {
  parent = NULL; relation = NULL; .CONJ_LEVEL = NULL; .MATCH_ID = NULL; .ADDED = NULL
  
  conj = follow_bypass(tokens, bypass)
  if (nrow(conj) > 0) {
    ## replace for the original node the parent and relation with those of the head node
    if (copy) tokens = data.table::copy(tokens) else tokens = tokens
    i = tokens[list(conj$doc_id, conj$sentence, conj$token_id),on=c('doc_id','sentence','token_id'),which=T]
    tokens[i, parent := conj$parent]
    tokens[i, relation := conj$relation]
    tokens[i, .CONJ_LEVEL := conj$.CONJ_LEVEL]
    
    
    if (!is.null(link_children)) {
      ## duplicate children of head node, now pointing to original node
      conj_ids = unique(rbind(data.table::data.table(doc_id=conj$doc_id, sentence=conj$sentence, token_id=conj$token_id),
                              data.table::data.table(doc_id=conj$doc_id, sentence=conj$sentence, token_id=conj$token_id_head)))
      add = token_family(tokens, depth = 1, ids=conj_ids, level = 'children', replace = T)
      add[, .CONJ_LEVEL := NULL]
      #also look in ids, which have the updated parents/relations
      #add2 = token_family(ids, depth = 1, ids=conj_ids, level = 'children', replace = T)
      #add2[, .CONJ_LEVEL := NULL]
      #add2[, .MATCH_ID := NULL]
      #add = rbind(add, add2, fill=T)
      
      if (is.logical(link_children) && !link_children) {
        add = NULL
      } else {
        if (is.character(link_children)) {
          add = subset(add, add$relation %in% link_children)
        }
      }

      if (!is.null(add)) {
        add = unique(add, by=c('doc_id','sentence','token_id','parent'))
        add = merge(add, data.table::data.table(new_parent=conj$token_id, parent=conj$token_id_head, .CONJ_LEVEL=conj$.CONJ_LEVEL), by='parent', allow.cartesian=T)
        
        if (only_first_level) {
          ## sort by conj level, look for the non-duplicates, and from the non duplicates return all with the same conj level (we allow duplicates on the same level)
          data.table::setorder(add, '.CONJ_LEVEL')
          not_dup = !duplicated(add, by = c('doc_id','sentence','new_parent'))
          add = add[add[not_dup,c('doc_id','sentence','new_parent','.CONJ_LEVEL')],on=c('doc_id','sentence','new_parent','.CONJ_LEVEL')]
        }
        add$parent = add$new_parent
        add$.MATCH_ID = add$new_parent
        add = subset(add, subset=add$.CONJ_LEVEL > 0, select=colnames(tokens))
      
        add = as_tokenindex(add)
        fam = token_family(tokens, id = unique(add[,c('doc_id','sentence','token_id')]), minimal=F, 
                          level='children', depth = Inf)

        largest_id = max(nchar(unique(tokens$token_id)))
        fam = merge(fam, data.table(.LINK_PARENT = add$parent, .LINK_CONJ_LEVEL=add$.CONJ_LEVEL, token_id=add$token_id),
                    by.x='.MATCH_ID', by.y='token_id', allow.cartesian=T)
        # fam_match = match(add$token_id, fam$.MATCH_ID)
        # print(fam_match)
        # fam = fam[na.omit(fam_match),]
        # fam_uberparent = add$parent[!is.na(fam_match)]
        add$token_id = (add$parent) + token_sub_id(add$token_id, largest_id)
        
        if (nrow(fam) > 0) {
          # fam[, .CONJ_LEVEL := add$.CONJ_LEVEL[!is.na(fam_match)]]
          fam[, .CONJ_LEVEL := fam$.LINK_CONJ_LEVEL,]
          # fam$token_id = (fam_uberparent) + token_sub_id(fam$token_id, largest_id)
          # fam$parent = (fam_uberparent) + token_sub_id(fam$parent, largest_id)
          fam$token_id = fam$.LINK_PARENT + token_sub_id(fam$token_id, largest_id)
          fam$parent = fam$.LINK_PARENT + token_sub_id(fam$parent, largest_id)
          add = rbind(add, subset(fam, select=colnames(add)))
        }
        tokens[,.ADDED := F]
        add[,.ADDED := T]
        tokens = rbind(tokens,add)
      }
    }
  }
  as_tokenindex(tokens)
}

token_sub_id <- function(x, highest=x) {
  x[x == 0] = 0.01 ## in case the token is zero, since 0.0 would mess things up
  ##x / 10^(ceiling(log10(highest)))
  x / 10^(nchar(highest) + 1)
}
