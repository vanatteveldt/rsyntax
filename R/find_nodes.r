#' There are two ways to query nodes (i.e. rows). Firstly, you can use named arguments, where the names are column names (in the data.table on which the
#' queries will be used) and the values are vectors with lookup values. Secondly, you can use the select arguments to use logical expressions.   
#' The select argument is more versatile (but see the parameter details for limitations), whereas the named argument approach is more explicit and uses binary search (which is much faster).
#' 
#' Children or parents of nodes can be queried by passing the \link{childen} or \link{parents} function as (named or unnamed) arguments.
#' These functions use the same query format as the tquery function, and children and parents can be nested recursively to find children of children etc. 
#' 
#' Please look at the examples below for a recommended syntactic style for using the find_nodes function and these nested functions.
#'
#' @param tokens  A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
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
#' @param select  An expression to select specific parents/children, which can use any columns in the token data (similar to the subset argument in \link{subset.data.frame}).
#'                Note, however, that select should not rely on absolute positions (a logical vector or indices). 
#' @param g_id    Find nodes by global id, which is the combination of the doc_id, sentence and token_id. Passed as a data.frame or data.table with 2 columns: (1) doc_id, (2) sentence and (3) token_id. 
#' @param save    A character vector, specifying the column name under which the selected tokens are returned. 
#' @param block   Optionally, specify ids (like g_id) where find_nodes will stop (ignoring the id and recursive searches through the id). 
#'                Can also be a data.table returned by (a previous) find_nodes, in which case all ids are blocked. 
#' @param check   If TRUE, return a warning if nodes occur in multiple patterns, which could indicate that the find_nodes query is not specific enough.
#' @param use_index if TRUE, index column before lookup
#' @param e       environment used for evaluating select.
#'
#' @return        A data.table in which each row is a node for which all conditions are satisfied, and each column is one of the linked nodes 
#'                (parents / children) with names as specified in the save argument.
#'
#' @details 
#' 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' If the name is given the suffix __N, only rows without an exact match are found. (so, lemma__N = "fish" look for all rows in which the lemma is not "fish").
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__NRI, or lemma_IRN  (order of flags is irrelevant)
#' 
#'                
#' @export
find_nodes <- function(tokens, ..., select=NULL, g_id=NULL, save=NA, block=NULL, check=T, use_index=T, e=parent.frame()) {
  .MATCH_ID = NULL; .DROP = NULL ## declare data.table bindings
  safe_save_name(save)
  tokens = as_tokenindex(tokens)  
  block = block_ids(block)
  
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, is, 'tQueryParent') | sapply(l, is, 'tQueryChild')
    lookup = l[!is_nested]
    nested = l[is_nested]
  } else {
    lookup = NULL
    nested = NULL
  }
  
  if (!class(substitute(select)) %in% c('name','character')) select = deparse(substitute(select))
  
  ids = filter_tokens(tokens, lookup, select=select, .G_ID=g_id, .BLOCK=block, e=e, use_index = use_index)
  ids = subset(ids, select = c(cname('doc_id'),cname('sentence'),cname('token_id')))
  if (length(ids) == 0) return(NULL)
  if (length(nested) > 0) {
    nodes = rec_find(tokens, ids=ids, ql=nested, e=e, block=block)
  } else {
    data.table::setnames(ids, old = cname('token_id'), new='.KEY')
    if (!is.na(save)) ids[,(save) := .KEY]
    return(ids)
  }
  if (nrow(nodes) == 0) return(NULL)
  
  ## always remember the node from which the search starts as .ID, for identifying unique matches
  nodes[, .KEY := .MATCH_ID]
  data.table::setcolorder(nodes, c('.KEY', setdiff(colnames(nodes), '.KEY')))
  
  if (is.na(save)) {
    nodes[,.MATCH_ID := NULL]
    data.table::setcolorder(nodes, c(cname('doc_id'),cname('sentence'),'.KEY', setdiff(colnames(nodes), c('.KEY',cname('doc_id'),cname('sentence')))))
  } else {
    if (save %in% colnames(nodes)) {
      data.table::setnames(nodes, save, paste0(save,'.y'))
      save = paste0(save,'.x')
    }
    data.table::setnames(nodes, '.MATCH_ID', save)
    data.table::setcolorder(nodes, c(cname('doc_id'),cname('sentence'),'.KEY', save, setdiff(colnames(nodes), c('.KEY',cname('doc_id'),cname('sentence'),save))))
  }
  if ('.DROP' %in% colnames(nodes)) {
    nodes[, .DROP := NULL]
  }
  
  nodes = unique(nodes)
  
  if (check) {
    lnodes = unique(melt(nodes, id.vars=c(cname('doc_id'),cname('sentence'),'.KEY'), variable.name = '.ROLE', value.name = cname('token_id')))
    if (anyDuplicated(lnodes, by=c(cname('doc_id'),cname('sentence'),cname('token_id')))) {
      warning('DUPLICATE NODES: Some tokens occur multiple times as nodes (either in different patterns or the same pattern). 
              This should be preventable by making patterns more specific. You can turn off this duplicate check by setting check to FALSE')
    }
  }
  
  data.table::setnames(nodes, colnames(nodes), gsub('\\.[xy]$', '', colnames(nodes)))
  nodes[]
}


#' Get and/or merge ids for the block argument in \link{find_nodes}
#'
#' @param ... Either a data.table with the columns doc_id, sentence and token_id, or the output of \link{find_nodes}
#'
#' @return A data.table with the columns doc_id, sentence and token_id
#' @export
block_ids <- function(..., names=NULL) {
  l = list(...)

  len = length(l)
  out = vector('list', len)
  for (i in 1:len) {
    d = l[[i]]
    if (is.null(d)) next
    if (is(d, 'data.table')) {
      if (!cname('token_id') %in% colnames(d)) {
        if (!is.null(names)) {
          names = setdiff(names, '.TQUERY')
          d = subset(d, select = union(cname('doc_id'), cname('sentence'), names))
        } else {
          d = subset(d, select = setdiff(colnames(d), '.TQUERY'))
        }
        d = data.table::melt(d, id.vars = c(cname('doc_id'), cname('sentence')), value.name=cname('token_id'))
        d[,variable := NULL]
      } 
     
      out[[i]] = d[,c(cname('doc_id'),cname('sentence'),cname('token_id'))]
      next
    }
    if (is(d, 'list')) {
      out[[i]] = block_ids(d)
    }
    stop('Not a valid input for block_ids')
  }
  out = unique(data.table::rbindlist(out))
  if (ncol(out) == 0) NULL else out
}


rec_find <- function(tokens, ids, ql, e=parent.frame(), block=NULL) {
  out = data.table::data.table()
  for (i in seq_along(ql)) {
    q = ql[[i]]
    
    if (is.na(q$save)) {
      q$save = '.DROP'
    } else {
      safe_save_name(q$save)
    }
    
    selection = select_tokens(tokens, ids=ids, q=q, e=e, block=block)
    if (length(q$nested) > 0 & length(selection) > 0) {
      nested = rec_find(tokens, ids=selection[,c(cname('doc_id'),cname('sentence'),q$save),with=F], ql=q$nested, e=e, block=block)  
      ## The match_id column in 'nested' is used to match nested results to the current level
      if (nrow(nested) > 0) {
        selection = merge(selection, nested, by.x=c(cname('doc_id'),cname('sentence'),q$save), by.y=c(cname('doc_id'),cname('sentence'),'.MATCH_ID'), allow.cartesian=T) 
      } else {
        selection = data.table::data.table(.MATCH_ID = numeric(), doc_id=numeric(), sentence=numeric(), .DROP = numeric())
        data.table::setnames(selection, 'doc_id', cname('doc_id'))
        data.table::setnames(selection, 'sentence', cname('sentence'))
      }
    } 
    data.table::setkeyv(selection, c(cname('doc_id'),cname('sentence'),'.MATCH_ID'))
  
    if (q$NOT) {
      if (nrow(selection) > 0) {
        selection = data.table::fsetdiff(data.table::data.table(ids[,1],ids[,2], .MATCH_ID=ids[[3]]), selection[,c(cname('doc_id'),cname('sentence'),'.MATCH_ID')])
      } else selection = data.table::data.table(ids[,1], ids[,2], .MATCH_ID=ids[[3]])
      selection[,.DROP := NA]
    }
    if (nrow(selection) == 0) return(selection)
    out = if(nrow(out) > 0) merge(out, selection, by=c(cname('doc_id'),cname('sentence'),'.MATCH_ID'), allow.cartesian=T) else selection
  }
  out
}


select_tokens <- function(tokens, ids, q, e, block=NULL) {
  .MATCH_ID = NULL ## bindings for data.table
  
  selection = token_family(tokens, ids=ids, level=q$level, depth=q$depth, block=block, replace=T)
  if (!data.table::haskey(selection)) data.table::setkeyv(selection, c(cname('doc_id'),cname('sentence'),cname('token_id')))
  
  ## for the indexed columns, use the full tokens data, because in selection the indices are gone.
  #filter_ids = filter_tokens(tokens, q$lookup, .G_ID = q$g_id)
  #if (nrow(filter_ids) < nrow(tokens)) {
  #  selection = selection[filter_ids, on=cname('doc_id','sentence','token_id'), nomatch=0]
  #}
  ## for 'select', only use the selected and filtered tokens, because this is a vector search
  #if (!is.null(q$select)) selection = filter_tokens(selection, select=q$select, e=e)
  
  selection = filter_tokens(selection, q$lookup, select = q$select, .G_ID = q$g_id)

  selection = subset(selection, select=c('.MATCH_ID', cname('doc_id'),cname('sentence'),cname('token_id')))
  data.table::setnames(selection, cname('token_id'), q$save)
  selection
}


token_family <- function(tokens, ids, level='children', depth=Inf, minimal=F, block=NULL, replace=F) {
  .MATCH_ID = NULL

  if (!replace) block = block_ids(ids, block)
  
  if ('.MATCH_ID' %in% colnames(tokens)) tokens[, .MATCH_ID := NULL]

  if (level == 'children') {
    id = tokens[list(ids[[1]], ids[[2]], ids[[3]]), on=c(cname('doc_id'),cname('sentence'),cname('parent')), nomatch=0]
    id = filter_tokens(id, .BLOCK=block)
    if (minimal) id = subset(id, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),cname('parent')))
    data.table::set(id, j = '.MATCH_ID', value = id[[cname('parent')]])
  }
  if (level == 'parents') {
    .NODE = filter_tokens(tokens, .G_ID = ids)
    .NODE = subset(.NODE, select=c(cname('doc_id'),cname('sentence'),cname('parent'),cname('token_id')))

    data.table::setnames(.NODE, old=cname('token_id'), new='.MATCH_ID')
    id = filter_tokens(tokens, .G_ID = .NODE[,c(cname('doc_id'),cname('sentence'),cname('parent'))], .BLOCK=block)
    if (minimal) id = subset(id, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),cname('parent')))
    id = merge(id, .NODE, by.x=c(cname('doc_id'),cname('sentence'),cname('token_id')), by.y=c(cname('doc_id'),cname('sentence'),cname('parent')), allow.cartesian=T)
  }
  
  if (depth > 1) id = deep_family(tokens, id, level, depth, minimal=minimal, block=block, replace=replace) 
  id
}

deep_family <- function(tokens, id, level, depth, minimal=F, block=NULL, replace=F) {
  id_list = vector('list', 10) ## 10 is just for reserving (more than sufficient) items. R will automatically add more if needed.
  id_list[[1]] = id
  i = 2
  while (i <= depth) {
    .NODE = id_list[[i-1]]
    if (!replace) block = block_ids(block, .NODE[,cname('doc_id','sentence','token_id'), with=F])
    
    if (level == 'children') {
      id = filter_tokens(tokens, .G_PARENT = .NODE[,c(cname('doc_id'),cname('sentence'),cname('token_id'))], .BLOCK=block)
      id = merge(id, subset(.NODE, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),'.MATCH_ID')), by.x=c(cname('doc_id'),cname('sentence'),cname('parent')), by.y=c(cname('doc_id'),cname('sentence'),cname('token_id')), allow.cartesian=T)
      id_list[[i]] = if (minimal) subset(id, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),cname('parent'),'.MATCH_ID')) else id
    }
  
    if (level == 'parents') {
      id = filter_tokens(tokens, .G_ID = .NODE[,c(cname('doc_id'),cname('sentence'),cname('parent'))], .BLOCK=block)
      id = merge(id, subset(.NODE, select = c(cname('doc_id'),cname('sentence'),cname('parent'), '.MATCH_ID')), by.x=c(cname('doc_id'),cname('sentence'),cname('token_id')), by.y=c(cname('doc_id'),cname('sentence'),cname('parent')), allow.cartesian=T)
      id_list[[i]] = if (minimal) subset(id, select = c(cname('doc_id'),cname('sentence'),cname('token_id'),cname('parent'),'.MATCH_ID')) else id
    }
    if (nrow(id_list[[i]]) == 0) break
    i = i + 1
  }
  data.table::rbindlist(id_list)
}

safe_save_name <- function(name) {
  if(grepl('\\.[A-Z]', name)) stop(sprintf('save name cannot be all-caps and starting with a dot'))
  special_names = cname('doc_id','token_id')
  if (name %in% special_names) stop(sprintf('save name (%s) cannot be the same as the special tokenIndex column names (%s)', name, paste(special_names, collapse=', ')))
}


