#' Query the token index
#' 
#' @description 
#' To find nodes you can use named arguments, where the names are column names (in the data.table on which the
#' queries will be used) and the values are vectors with lookup values. 
#' 
#' Children or parents of nodes can be queried by passing the \link{childen} or \link{parents} function as (named or unnamed) arguments.
#' These functions use the same query format as the tquery function, and children and parents can be nested recursively to find children of children etc. 
#' 
#' Please look at the examples below for a recommended syntactic style for using the find_nodes function and these nested functions.
#'
#' @param tokens  A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param ...     Accepts two types of arguments: name-value pairs for finding nodes (i.e. rows), and functions to look for parents/children of these nodes.
#'                
#'                The name in the name-value pairs need to match a column in the data.table, and the value needs to be a vector of the same data type as the column.
#'                By default, search uses case sensitive matching, with the option of using common wildcards (* for any number of characters, and ? for a single character).
#'                Alternatively, flags can be used to to change this behavior to 'fixed' (__F), 'igoring case' (__I) or 'regex' (__R). See details for more information. 
#'                
#'                If multiple name-value pairs are given, they are considered as AND statements, but see details for syntax on using OR statements, and combinations.
#'                
#'                To look for parents and children of the nodes that are found, you can use the \link{parents} and \link{children} functions as (named or unnamed) arguments. 
#'                These functions have the same query arguments as tquery, but with some additional arguments. 
#' @param g_id    Find nodes by global id, which is the combination of the doc_id, sentence and token_id. Passed as a data.frame or data.table with 2 columns: (1) doc_id, (2) sentence and (3) token_id. 
#' @param save    A character vector, specifying the column name under which the selected tokens are returned. 
#' @param block   Optionally, specify ids (like g_id) where find_nodes will stop (ignoring the id and recursive searches through the id). 
#'                Can also be a data.table returned by (a previous) find_nodes, in which case all ids are blocked. 
#' @param check   If TRUE, return a warning if nodes occur in multiple patterns, which could indicate that the find_nodes query is not specific enough.
#' @param use_index if TRUE, index column before lookup
#' @param only_key If TRUE, only return the ids of the 
#'
#' @return        A data.table in which each row is a node for which all conditions are satisfied, and each column is one of the linked nodes 
#'                (parents / children) with names as specified in the save argument.
#'
#' @details 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' If the name is given the suffix __N, only rows without an exact match are found. (so, lemma__N = "fish" look for all rows in which the lemma is not "fish").
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__NRI, or lemma_IRN  (order of flags is irrelevant)
#' 
#'                
#' @export
find_nodes <- function(tokens, ..., g_id=NULL, save=NA, block=NULL, check=T, use_index=T, name=NULL, only_key=F) {
  .MATCH_ID = NULL; .DROP = NULL ## declare data.table bindings
  safe_save_name(save)
  tokens = as_tokenindex(tokens)  
  block = get_long_ids(block)
  
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, is, 'tQueryParent') | sapply(l, is, 'tQueryChild')
    lookup = l[!is_nested]
    nested = l[is_nested]
  } else {
    lookup = NULL
    nested = NULL
  }
  
  #if (!class(substitute(select)) %in% c('name','character')) select = deparse(substitute(select))
  
  ids = filter_tokens(tokens, lookup, .G_ID=g_id, .BLOCK=block, use_index = use_index)

  ids = subset(ids, select = c('doc_id','sentence','token_id'))
  if (length(ids) == 0) return(NULL)
  if (length(nested) > 0) {
    nodes = rec_find(tokens, ids=ids, ql=nested, block=block)
  } else {
    data.table::setnames(ids, old = 'token_id', new='.ID')
    if (!is.na(save)) ids[,(save) := .ID]
    ids = create_unique_key(ids, name)
    data.table::setattr(ids, 'class', c('rsyntaxNodes', class(ids)))
    return(ids[])
  }
  if (nrow(nodes) == 0) return(NULL)
  
  ## always remember the node from which the search starts as .ID, for identifying unique matches
  nodes[, .ID := .MATCH_ID]
  data.table::setcolorder(nodes, c('.ID', setdiff(colnames(nodes), '.ID')))
  
  
  if (is.na(save)) {
    nodes[,.MATCH_ID := NULL]
    data.table::setcolorder(nodes, c('doc_id','sentence','.ID', setdiff(colnames(nodes), c('.ID', 'doc_id','sentence'))))
  } else {
    if (save %in% colnames(nodes)) {
      data.table::setnames(nodes, save, paste0(save,'.y'))
      save = paste0(save,'.x')
    }
    data.table::setnames(nodes, '.MATCH_ID', save)
    data.table::setcolorder(nodes, c('doc_id','sentence','.ID', save, setdiff(colnames(nodes), c('.ID','doc_id','sentence',save))))
  }
  
  dropcols = grep('.DROP.*', colnames(nodes), value=T)
  if (length(dropcols) > 0) nodes[, (dropcols) := NULL]
  
  nodes = unique(nodes)
  if (check && ncol(nodes) > 3) {
    lnodes = unique(melt(nodes, id.vars=c('doc_id','sentence','.ID'), variable.name = '.ROLE', value.name = 'token_id'))
    if (anyDuplicated(lnodes, by=c('doc_id','sentence','token_id'))) {
      warning('DUPLICATE NODES: Some tokens occur multiple times as nodes (either in different patterns or the same pattern). 
              This should be preventable by making patterns more specific. You can turn off this duplicate check by setting check to FALSE')
    }
  }

  data.table::setnames(nodes, colnames(nodes), gsub('\\.[xy]$', '', colnames(nodes)))
  nodes = create_unique_key(nodes, name)
  data.table::setattr(ids, 'class', c('rsyntaxNodes', class(ids)))
  nodes[]
}

create_unique_key <- function(nodes, name){
  #if (ncol(nodes) > 3) {
  #  key = paste0(name, '(', nodes$.ID, ':', do.call(paste, args = c(nodes[,-(1:3)], sep='.')), ')')
  #} else {
  #  key = paste0(name, '(', nodes$.ID, ')')
  #}        
  #key = paste0(name, '#', match(nodes$.ID, unique(nodes$.ID)))
  key = paste0(name, '#', 1:nrow(nodes))
  nodes$.ID = key
  return(nodes)
}

#' Get ids in various forms to extract token_ids
#'
#' @param ... Either a data.table with the columns doc_id, sentence and token_id, or the output of \link{find_nodes}
#'
#' @return A data.table with the columns doc_id, sentence and token_id
#' @export
get_long_ids <- function(..., names=NULL) {
  l = list(...)

  len = length(l)
  out = vector('list', len)
  for (i in 1:len) {
    d = l[[i]]
    if (is.null(d)) next
    if (is(d, 'data.table')) {
      if (!'token_id' %in% colnames(d)) {
        if (!is.null(names)) {
          names = setdiff(names, '.TQUERY')
          d = subset(d, select = colnames(d) %in% union('doc_id', 'sentence', names))
        } else {
          d = subset(d, select = colnames(d) %in% setdiff(colnames(d), '.TQUERY'))
        }
        d = safe_melt(d, id.vars = c('doc_id', 'sentence'), 
                             measure.vars=setdiff(colnames(d), c('doc_id','sentence','.ID')),
                             variable.name = '.VARIABLE', value.name='token_id')
        #if ('.VARIABLE' in colnames(d)) d[,.VARIABLE := NULL]
      } 
      if (!'token_id' %in% colnames(d)) next
      out[[i]] = d[,c('doc_id','sentence','token_id')]
      next
    }
    if (is(d, 'list')) {
      out[[i]] = get_long_ids(d)
    }
    stop('Not a valid input for get_long_ids')
  }
  out = unique(data.table::rbindlist(out))
  if (ncol(out) == 0) NULL else out
}


#' Recursive search tokens
#'
#' @param tokens   The tokenIndex
#' @param ids      A data.table with global ids (doc_id,sentence,token_id). 
#' @param ql       a list of queriers (possibly the list nested in another query, or containing nested queries)
#' @param block    A data.table with global ids (doc_id,sentence,token_id) for excluding nodes from the search
rec_find <- function(tokens, ids, ql, block=NULL) {
  .DROP = NULL
  out_req = list()
  out_not_req = list()
  for (i in seq_along(ql)) {
    q = ql[[i]]

    if (is.na(q$save)) {
      q$save = paste('.DROP', i)
    } else {
      safe_save_name(q$save)
    }
    
    selection = select_tokens(tokens, ids=ids, q=q, block=block)
    if (length(q$nested) > 0 & length(selection) > 0) {
      nested = rec_find(tokens, ids=selection[,c('doc_id','sentence',q$save),with=F], ql=q$nested, block=block)  
      ## The match_id column in 'nested' is used to match nested results to the current level
      if (nrow(nested) > 0) {
        selection = merge(selection, nested, by.x=c('doc_id','sentence',q$save), by.y=c('doc_id','sentence','.MATCH_ID'), allow.cartesian=T) 
      } else {
        selection = data.table::data.table(.MATCH_ID = numeric(), doc_id=numeric(), sentence=numeric(), .DROP = numeric())
        data.table::setnames(selection, 'doc_id', 'doc_id')
        data.table::setnames(selection, 'sentence','sentence')
      }
    } 
    data.table::setkeyv(selection, c('doc_id','sentence','.MATCH_ID'))
    
    
    if (q$NOT) {
      if (nrow(selection) > 0) {
        selection = data.table::fsetdiff(data.table::data.table(ids[,1],ids[,2], .MATCH_ID=ids[[3]]), selection[,c('doc_id','sentence','.MATCH_ID')])
      } else selection = data.table::data.table(ids[,1], ids[,2], .MATCH_ID=ids[[3]])
      selection[,.DROP := NA]
    }

    if (q$req) {
      if (nrow(selection) == 0) return(selection)
      out_req[['']] = selection
      if ('.DROP' %in% colnames(selection)) selection[,.DROP := NULL]
      block = get_long_ids(block, selection)
    } else {
      out_not_req[['']] = selection
    }
  }
  
  
  out = data.table::data.table()
  for (selection in out_req) {
    out = if (nrow(out) == 0) selection else merge(out, selection, by=c('doc_id','sentence','.MATCH_ID'), allow.cartesian=T)
  }

  #out_add = data.table::data.table()
  for (selection in out_not_req) {
    out = if (nrow(out) == 0) selection else merge(out, selection, by=c('doc_id','sentence','.MATCH_ID'), allow.cartesian=T, all.x=T)
  }
  
  out
}


#' Select which tokens to add
#' 
#' Given ids, look for their parents/children (specified in query q) and filter on criteria (specified in query q)
#'
#' @param tokens   The tokenIndex
#' @param ids      A data.table with global ids (doc_id,sentence,token_id). 
#' @param q        a query (possibly nested in another query, or containing nested queries)
#' @param block    A data.table with global ids (doc_id,sentence,token_id) for excluding nodes from the search
select_tokens <- function(tokens, ids, q, block=NULL) {
  .MATCH_ID = NULL ## bindings for data.table
  
  ##
  if (q$connected) {
    selection = token_family(tokens, ids=ids, level=q$level, depth=q$depth, block=block, replace=T, show_level = T, lookup=q$lookup, g_id=q$g_id)
  } else {
    selection = token_family(tokens, ids=ids, level=q$level, depth=q$depth, block=block, replace=T, show_level = T)
    if (!data.table::haskey(selection)) data.table::setkeyv(selection, c('doc_id','sentence','token_id'))
    selection = filter_tokens(selection, q$lookup, .G_ID = q$g_id)
  }

  selection = subset(selection, select=c('.MATCH_ID', 'doc_id','sentence','token_id'))
  data.table::setnames(selection, 'token_id', q$save)
  selection
}

#' Get the parents or children of a set of ids
#'
#' @param tokens   The tokenIndex
#' @param ids      A data.table with global ids (doc_id,sentence,token_id). 
#' @param level    either 'children' or 'parents'
#' @param depth    How deep to search. eg. children -> grandchildren -> grandgrand etc.
#' @param minimal  If TRUE, only return doc_id, sentence, token_id and parent
#' @param block    A data.table with global ids (doc_id,sentence,token_id) for excluding nodes from the search
#' @param replace  If TRUE, re-use nodes in deep_family() 
#' @param show_level 
#' @param lookup   filter tokens by lookup values. Will be applied at each level (if depth > 1) 
#' @param g_id     filter tokens by id. See lookup
token_family <- function(tokens, ids, level='children', depth=Inf, minimal=F, block=NULL, replace=F, show_level=F, lookup=NULL, g_id=NULL, filter_each=F) {
  .MATCH_ID = NULL

  
  if (!replace) block = get_long_ids(ids, block)
  
  if ('.MATCH_ID' %in% colnames(tokens)) tokens[, .MATCH_ID := NULL]

  if (level == 'children') {
    id = tokens[list(ids[[1]], ids[[2]], ids[[3]]), on=c('doc_id','sentence','parent'), nomatch=0, allow.cartesian=T]
    id = filter_tokens(id, .BLOCK=block, lookup=lookup, .G_ID = g_id)
    if (minimal) id = subset(id, select = c('doc_id','sentence','token_id','parent'))
    data.table::set(id, j = '.MATCH_ID', value = id[['parent']])
  }
  if (level == 'parents') {
    .NODE = filter_tokens(tokens, .G_ID = ids)
    .NODE = subset(.NODE, select=c('doc_id','sentence','parent','token_id'))

    data.table::setnames(.NODE, old='token_id', new='.MATCH_ID')
    id = filter_tokens(tokens, .G_ID = .NODE[,c('doc_id','sentence','parent')], .BLOCK=block)
    id = filter_tokens(id, .G_ID = g_id, lookup=lookup)
    
    if (minimal) id = subset(id, select = c('doc_id','sentence','token_id','parent'))
    id = merge(id, .NODE, by.x=c('doc_id','sentence','token_id'), by.y=c('doc_id','sentence','parent'), allow.cartesian=T)
  }
  
  if (depth > 1) id = deep_family(tokens, id, level, depth, minimal=minimal, block=block, replace=replace, show_level=show_level, lookup=lookup, g_id=g_id) 
  if (depth <= 1 && show_level) id = cbind(.FILL_LEVEL=rep(1,nrow(id)), id)
  
  id
}

#' Get the parents or children of a set of ids
#'
#' @param tokens   The tokenIndex
#' @param id       rows in the tokenIndex to use as the ID (for whom to get the family)
#' @param level    either 'children' or 'parents'
#' @param depth    How deep to search. eg. children -> grandchildren -> grandgrand etc.
#' @param minimal  If TRUE, only return doc_id, sentence, token_id and parent
#' @param block    A data.table with global ids (doc_id,sentence,token_id) for excluding nodes from the search
#' @param replace  If TRUE, re-use nodes 
#' @param show_level If TRUE, return a column with the level at which the node was found (e.g., as a parent, grantparent, etc.)
#' @param only_new If TRUE, only return new found family. Otherwise, the id input is included as well.
deep_family <- function(tokens, id, level, depth, minimal=F, block=NULL, replace=F, show_level=F, only_new=F, lookup=NULL, g_id=NULL) {
  id_list = vector('list', 10) ## 10 is just for reserving (more than sufficient) items. R will automatically add more if needed (don't think this actually requires reallocation).
  id_list[[1]] = id
  i = 2
  safety_depth = max(tokens$token_id) + 1
  while (i <= depth) {
    if (i == safety_depth) {
      warning(sprintf('Safety depth threshold was reached (max token_id), which probably indicates an infinite loop in deep_family(), which shouldnt happen. Please make a GitHub issue if you see this'))
      break
    }
    .NODE = id_list[[i-1]]
    
    if (!replace) block = get_long_ids(block, .NODE[,c('doc_id','sentence','token_id'), with=F])
    #print(.NODE)
    #.NODE = subset(.NODE, .NODE$token_id %in% unique(.NODE$parent)) ## prevent infinite loops
    
    if (level == 'children') {
      id = filter_tokens(tokens, .G_PARENT = .NODE[,c('doc_id','sentence','token_id')], .BLOCK=block, lookup=lookup, .G_ID=g_id)
      id = merge(id, subset(.NODE, select = c('doc_id','sentence','token_id','.MATCH_ID')), by.x=c('doc_id','sentence','parent'), by.y=c('doc_id','sentence','token_id'), allow.cartesian=T)
      id_list[[i]] = if (minimal) subset(id, select = c('doc_id','sentence','token_id','parent','.MATCH_ID')) else id
    }
  
    if (level == 'parents') {
      id = filter_tokens(tokens, .G_ID = .NODE[,c('doc_id','sentence','parent')], .BLOCK=block)
      id = filter_tokens(id, .G_ID = g_id, lookup=lookup)
      id = merge(id, subset(.NODE, select = c('doc_id','sentence','parent', '.MATCH_ID')), by.x=c('doc_id','sentence','token_id'), by.y=c('doc_id','sentence','parent'), allow.cartesian=T)
      id_list[[i]] = if (minimal) subset(id, select = c('doc_id','sentence','token_id','parent','.MATCH_ID')) else id
    }
    
    
    #print(nrow(id_list[[i]]))
    if (nrow(id_list[[i]]) == 0) break
    i = i + 1
  }
  
  if (only_new) id_list[[1]] = NULL
  if (show_level) {
    id_list = id_list[!sapply(id_list, is.null)]   ## in older version of data.table R breaks (badly) if idcol is used in rbindlist with NULL values in list
    return(data.table::rbindlist(id_list, use.names = T, idcol = '.FILL_LEVEL'))
  } else {
    return(data.table::rbindlist(id_list, use.names = T))
  }
}

safe_save_name <- function(name) {
  if(grepl('\\.[A-Z]', name)) stop(sprintf('save name cannot be all-caps and starting with a dot'))
  if(grepl(',', name)) stop(sprintf('save name cannot contain comma'))
  
  special_names = c('doc_id','token_id','sentence')
  if (name %in% special_names) stop(sprintf('save name (%s) cannot be the same as the special tokenIndex column names (%s)', name, paste(special_names, collapse=', ')))
}

#(annotate(tokens, copula, column='test'))

