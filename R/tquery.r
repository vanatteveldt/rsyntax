
#' Create a query for dependency based parse trees in a data.table (CoNLL-U or similar format).
#'
#' @description
#' To find nodes you can use named arguments, where the names are column names (in the data.table on which the
#' queries will be used) and the values are vectors with lookup values. 
#' 
#' Children or parents of nodes can be queried by passing the \link{children} or \link{parents} function as (named or unnamed) arguments.
#' These functions use the same query format as the tquery function, and children and parents can be nested recursively to find children of children etc. 
#'
#' The fill() function (also see fill argument) can be nested to include the children of a 'labeld' node. It can only be nested in a query if the label argument is not NULL,
#' and by default will include all children of the node that have not been assigned to another node. If two nodes have a shared child, the child will be
#' assigned to the closest node. 
#'   
#' Please look at the examples below for a recommended syntactic style for using the find_nodes function and these nested functions.
#'
#'
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
#' @param g_id    Find nodes by global id, which is the combination of the doc_id, sentence and token_id. Passed as a data.frame or data.table with 3 columns: (1) doc_id, (2) sentence and (3) token_id. 
#' @param label    A character vector, specifying the column name under which the selected tokens are returned. 
#'                If NA, the column is not returned.
#' @param fill    Logical. If TRUE (default), the default fill() will be used (this is identical to nesting fill(); see description). To more specifically controll fill, you can nest the \link{fill} 
#'                function (a special version of the children function). 
#' @param block   Logical. If TRUE, the node will be blocked from being assigned (labeld). This is mainly usefull if you have a node that you do not want to be assigned by fill,
#'                but also don't want to 'label' it. Essentially, block is shorthand for using label and then removing the node afterwards. If block is TRUE, label has to be NA.
#'                
#'                
#' @return        A tQuery object, that can be used with the \link{apply_queries} function.
#' 
#' @details 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__RI, or lemma_IR  (order of flags is irrelevant)
#' 
#' @examples
#' ## it is convenient to first prepare vectors with relevant words/pos-tags/relations
#' .SAY_VERBS = c("tell", "show","say", "speak") ## etc.
#' .QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
#' .SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 
#' 
#' quotes_direct = tquery(lemma = .SAY_VERBS,
#'                          children(label = 'source', p_rel = .SUBJECT_RELS),
#'                          children(label = 'quote', p_rel = .QUOTE_RELS))
#' quotes_direct 
#' @export
tquery <- function(..., g_id=NULL, label=NA, fill=TRUE, block=FALSE) {
  #select = deparse(bquote_s(substitute(select), where = parent.frame()))
  
  l = list(...)
  validate_label_name(label)
  
  if (block) {
    if (!is.na(label)) stop('block cannot be TRUE if label is not NA. Note that block is used to block nodes from being labeld or filled (see ?tquery documentation)')
    label = 'BLOCK'
  }
  
  is_fill = if (length(l) > 0) sapply(l, methods::is, 'tQueryFill') else c()
  if (sum(is_fill) > 1) stop('cannot nest more than one fill()')
  if (any(is_fill)) {
    if (!fill) stop('fill cannot be FALSE if a nested fill() query is given')
    if (is.na(label)) {
      warning('fill() is used in a query where label = NA. This will be ignored, because the children of a node that is not labeld would be forgotten anyway')
      l = l[!is_fill]
    }
  } else {
    if (fill && !is.na(label)) l[['']] = fill() 
  }

  
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild') | sapply(l, methods::is, 'tQueryFill') 
    for (fill_i in which(sapply(l, methods::is, 'tQueryFill'))) {
      if (!is.na(label)) {
        l[[fill_i]]$label = paste(label, 'FILL', sep='_')
      } else {
        is_nested = is_nested[-fill_i]
        l = l[-fill_i]
      }
    }
    q = list(g_id=g_id, label=label, lookup = l[!is_nested], nested=l[is_nested])
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL)
  }
  q = safe_names(q)
  class(q) = c('tQuery', class(q))
  q
}

validate_label_name <- function(label) {
  if (!is.na(label)) {
    if (grepl('#', label)) stop('label name cannot contain a hashtag')
    if (grepl('\\_FILL', label)) stop('label name cannot contain _FILL')
    if (grepl('BLOCK', label)) stop('label name cannot contain BLOCK')
    if (grepl('\\.[A-Z]', label)) stop(sprintf('label name cannot be all-caps and starting with a dot'))
    if (grepl(',', label)) stop(sprintf('label name cannot contain comma'))
    forbidden = c('doc_id','sentence','token_id','.ID','.MATCH_ID','.FILL_LEVEL')
    if (label %in% forbidden) stop(sprintf('label name cannot be one of: %s', paste(forbidden, collapse=', ')))
  }
}

safe_names <- function(tq, label_names=c()) {
  if (!is.na(tq$label)) {
    already_used = sum(tq$label == label_names)
    label_names = c(label_names, tq$label)
    if (already_used > 0) tq$label = paste(tq$label, already_used+1, sep='#')
  }
  for (i in seq_along(tq$nested)) tq$nested[[i]] = safe_names(tq$nested[[i]], label_names)
  tq
}


#' Search for parents or children in tquery
#'
#' Should only be used inside of the \link{tquery} function.
#' Enables searching for parents or children, either direct (depth = 1) or until a given depth (depth 2 for children and grandchildren, Inf (infinite) for all).
#' 
#' Searching for parents/children within find_nodes works as an AND condition: if it is used, the node must have these parents/children.
#' The label argument is used to remember the global token ids (.G_ID) of the parents/children under a given column name.
#' 
#' the not_children and not_parents functions will make the matched children/parents a NOT condition. 
#' 
#' The fill() function is used to include the children of a 'labeld' node. It can only be nested in a query if the label argument is not NULL,
#' and by default will include all children of the node that have not been assigned to another node. If two nodes have a shared child, the child will be
#' assigned to the closest node.
#'   
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
#' @param g_id    Find nodes by global id, which is the combination of the doc_id, sentence and token_id. Passed as a data.frame or data.table with 3 columns: (1) doc_id, (2) sentence and (3) token_id. 
#' @param label    A character vector, specifying the column name under which the selected tokens are returned. 
#'                If NA, the column is not returned.
#' @param req     Can be set to false to not make a node 'required'. This can be used to include optional nodes in queries. For instance, in a query for finding subject - verb - object triples, 
#'                make the object optional.
#' @param depth   A positive integer, determining how deep parents/children are sought. 1 
#'                means that only direct parents and children of the node are retrieved. 2 means children and grandchildren, etc.
#'                All parents/children must meet the filtering conditions (... or g_id)
#' @param connected controlls behaviour if depth > 1 and filters are used. If FALSE, all parents/children to the given depth are retrieved, and then filtered. 
#'                  This way, grandchilden that satisfy the filter conditions are retrieved even if their parents do not satisfy the conditions.
#'                  If TRUE, the filter is applied at each level of depth, so that only fully connected branches of nodes that satisfy the conditions are retrieved. 
#' @param fill    Logical. If TRUE (default), the default fill() will be used (this is identical to nesting fill(); see description). To more specifically controll fill, you can nest the \link{fill} 
#'                function (a special version of the children function). 
#' @param block   Logical. If TRUE, the node will be blocked from being assigned (labeld). This is mainly usefull if you have a node that you do not want to be assigned by fill,
#'                but also don't want to 'label' it. Essentially, block is shorthand for using label and then removing the node afterwards. If block is TRUE, label has to be NA.
#' @param max_window  Set the max token distance of the children/parents to the node. Has to be either a numerical vector of length 1 for distance in both directions, or a 
#'                vector of length 2, where the first value is the max distance to the left, and the second value the max distance to the right. Default is c(Inf, Inf) meaning that no max distance is used.
#' @param min_window  Like max_window, but for the min distance. Default is c(0,0) meaning that no min is used.
#'
#' @details 
#' Having nested queries can be confusing, so we tried to develop the find_nodes function and the accompanying functions in a way
#' that clearly shows the different levels. As shown in the examples, the idea is that each line is a node, and to look for parents
#' or children, we put them on the next line with indentation (in RStudio, it should automatically allign correctly when you press enter inside
#' of the children() or parents() functions). 
#' 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__RI, or lemma__IR  (order of flags is irrelevant)
#' 
#' @return Should not be used outside of \link{tquery}
#' @name nested_nodes
#' @rdname nested_nodes
NULL

#' @rdname nested_nodes
#' @export
children <- function(..., g_id=NULL, label=NA, req=TRUE, depth=1, connected=FALSE, fill=TRUE, block=FALSE, max_window=c(Inf,Inf), min_window=c(0,0)) {
  if (length(min_window) == 1) min_window = c(min_window,min_window)
  if (length(max_window) == 1) max_window = c(max_window,max_window)
  
  NOT = FALSE
  if (NOT && !req) stop('cannot combine NOT=TRUE and req=FALSE')
  validate_label_name(label)
  
  l = list(...)
  
  if (block) {
    if (!is.na(label)) stop('block cannot be TRUE if label is not NA. Note that block is used to block nodes from being labeld or filled (see ?tquery documentation)')
    label = 'BLOCK'
  }
  
  is_fill = if (length(l) > 0) sapply(l, methods::is, 'tQueryFill') else c()
  if (sum(is_fill) > 1) stop('cannot nest more than one fill()')
  if (any(is_fill)) {
    if (!fill) stop('fill cannot be FALSE if a nested fill() query is given')
    if (is.na(label)) {
      warning('fill() is used in a query where label = NA. This will be ignored, because the children of a node that is not labeld would be forgotten anyway')
      l = l[!is_fill]
    }
  } else {
    if (fill && !is.na(label)) l[['']] = fill() 
  }
 
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill') 
    for (fill_i in which(sapply(l, methods::is, 'tQueryFill'))) {
      if (!is.na(label)) {
        l[[fill_i]]$label = paste(label, 'FILL', sep='_')
      } else {
        is_nested = is_nested[-fill_i]
        l = l[-fill_i]
      }
    }
    q = list(g_id=g_id, label=label, lookup = l[!is_nested], nested=l[is_nested], level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, recursive=FALSE, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, recursive=FALSE, max_window=max_window, min_window=min_window)
  }
  
  
  class(q) = c('tQueryChild', class(q))
  q
}


#' @rdname nested_nodes
#' @export
not_children <- function(..., g_id=NULL, depth=1, connected=FALSE, max_window=c(Inf,Inf), min_window=c(0,0)) {
  if (length(min_window) == 1) min_window = c(min_window,min_window)
  if (length(max_window) == 1) max_window = c(max_window,max_window)
  
  label=NA
  req = TRUE
  NOT = TRUE
  if (NOT && !req) stop('cannot combine NOT=TRUE and req=FALSE')
  
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill')
    if (any(sapply(l, methods::is, 'tQueryFill'))) stop('fill() cannot be used in not_ queries (not_children, not_parents)')
    q = list(g_id=g_id, label=label, lookup = l[!is_nested], nested=l[is_nested], level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  }
  
  
  class(q) = c('tQueryChild', class(q))
  q
}


#' @rdname nested_nodes
#' @export
parents <- function(..., g_id=NULL, label=NA, req=TRUE, depth=1, connected=FALSE, fill=TRUE, block=FALSE, max_window=c(Inf,Inf), min_window=c(0,0)) {
  if (length(min_window) == 1) min_window = c(min_window,min_window)
  if (length(max_window) == 1) max_window = c(max_window,max_window)
  
  NOT = FALSE
  if (NOT && !req) stop('cannot combine NOT=TRUE and req=FALSE')
  validate_label_name(label)
  
  
  l = list(...)
  
  if (block) {
    if (!is.na(label)) stop('block cannot be TRUE if label is not NA. Note that block is used to block nodes from being labeld or filled (see ?tquery documentation)')
    label = 'BLOCK'
  }

  
  is_fill = if (length(l) > 0) sapply(l, methods::is, 'tQueryFill') else c()
  if (sum(is_fill) > 1) stop('cannot nest more than one fill()')
  if (any(is_fill)) {
    if (!fill) stop('fill cannot be FALSE if a nested fill() query is given')
    if (is.na(label)) {
      warning('fill() is used in a query where label = NA. This will be ignored, because the children of a node that is not labeld would be forgotten anyway')
      l = l[!is_fill]
    }
  } else {
    if (fill && !is.na(label)) l[['']] = fill() 
  }
  

  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill')
    for (fill_i in which(sapply(l, methods::is, 'tQueryFill'))) {
      if (!is.na(label)) {
        l[[fill_i]]$label = paste(label, 'FILL', sep='_')
      } else {
        is_nested = is_nested[-fill_i]
        l = l[-fill_i]
      }
    }
    q = list(g_id=g_id, label=label, lookup = l[!is_nested], nested=l[is_nested], level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  }
  
  class(q) = c('tQueryParent', class(q))
  q
}

#' @rdname nested_nodes
#' @export
not_parents <- function(..., g_id=NULL, depth=1, connected=FALSE, max_window=c(Inf,Inf), min_window=c(0,0)) {
  if (length(min_window) == 1) min_window = c(min_window,min_window)
  if (length(max_window) == 1) max_window = c(max_window,max_window)
  label=NA
  req = TRUE
  NOT = TRUE
  if (NOT && !req) stop('cannot combine NOT=TRUE and req=FALSE')
  
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill')
    if (any(sapply(l, methods::is, 'tQueryFill'))) stop('fill() cannot be used in not_ queries (not_children, not_parents)')
    q = list(g_id=g_id, label=label, lookup = l[!is_nested], nested=l[is_nested], level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  }
  
  class(q) = c('tQueryParent', class(q))
  q
}


#' @rdname nested_nodes
#' @export
fill <- function(..., g_id=NULL, depth=Inf, connected=FALSE, max_window=c(Inf,Inf), min_window=c(0,0)) {
  if (length(min_window) == 1) min_window = c(min_window,min_window)
  if (length(max_window) == 1) max_window = c(max_window,max_window)
  #select = deparse(bquote_s(substitute(select)))
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill')
    if (any(is_nested)) stop('Cannot use nested queries (children(), parents(), etc.) in fill()')
    q = list(g_id=g_id, label='fill', lookup = l[!is_nested], nested=l[is_nested], level = 'children', req=FALSE, NOT=FALSE, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label='fill', lookup =NULL, nested=NULL, level = 'children', req=FALSE, NOT=FALSE, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  }
  
  class(q) = c('tQueryFill', class(q))
  q
}


#' Use OR search in tquery
#' 
#' @param ... name-value pairs for lookup terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(OR(lemma = 'walk', POS='Noun'))
OR <- function(...) {
  l = list(lookup = list(...), boolean='OR')
  class(l) = c(class(l), 'tokenLookup')
  l
}

#' Use AND search in tquery
#' 
#' @param ... name-value pairs for lookup terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(AND(lemma = 'walk', POS='Noun'))   ## is also the default
AND <- function(...) {
  l = list(lookup = list(...), boolean='AND')
  class(l) = c(class(l), 'tokenLookup')
  l
}

#' Use NOT search in tquery
#' 
#' @param ... name-value pairs for lookup terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(NOT(POS='Noun'))   
NOT <- function(...) {
  l = list(lookup = list(...), boolean='NOT')
  class(l) = c(class(l), 'tokenLookup')
  l
}


