
#' Create a query for dependency based parse trees in a data.table (CoNLL-U or similar format).
#'
#' @description
#' To find nodes you can use named arguments, where the names are column names (in the data.table on which the
#' queries will be used) and the values are vectors with look-up values. 
#'
#' Children or parents of nodes can be queried by passing the \link{children} or \link{parents} function as (named or unnamed) arguments.
#' These functions use the same query format as the tquery function, and children and parents can be nested recursively to find children of children etc. 
#'
#' The custom_fill() function (also see fill argument) can be nested to customize which children of a 'labeled' node need to be matched. It can only be nested in a query if the label argument is not NULL,
#' and by default will include all children of the node that have not been assigned to another node. If two nodes have a shared child, the child will be
#' assigned to the closest node. 
#'   
#' Please look at the examples below for a recommended syntactic style for using the find_nodes function and these nested functions.
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
#' @param fill    Logical. If TRUE (default), the default custom_fill() will be used. To more specifically control fill, you can nest the \link{custom_fill} 
#'                function (a special version of the children function).
#' @param block   Logical. If TRUE, the node will be blocked from being assigned (labeled). This is mainly useful if you have a node that you do not want to be assigned by fill,
#'                but also don't want to 'label' it. Essentially, block is shorthand for using label and then removing the node afterwards. If block is TRUE, label has to be NA.
#'                
#'                
#' @return        A tQuery object, that can be used with the \link{apply_queries} function.
#' 
#' @details 
#' Multiple values in a name-value pair operate as OR conditions.
#' For example, tquery(relation = c('nsubj','dobj')) means that the relation column should have the value 'nsubj' OR 'dobj'. 
#' 
#' If multiple named arguments are given they operate as AND conditions. 
#' For example, tquery(relation = 'nsubj', pos = 'PROPN') means that the relation should be 'nsubj' AND the pos should be 'PROPN'.
#' 
#' This easily combines for the most common use case, which is to select on multiple conditions (relation AND pos), but allowing different (similar) values ('PROPN' OR 'NOUN').
#' For example: tquery(relation = 'nsubj', pos = c('PROPN','NOUN')) means that the node should have the 'nsubj' relation, but pos can be either 'PROPN' or 'NOUN'.
#' 
#' For more specific behavior, the AND(), OR() and NOT() functions can be used for boolean style conditions.
#' 
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
  if (sum(is_fill) > 1) stop('cannot nest more than one custom_fill()')
  if (any(is_fill)) {
    if (!fill) stop('fill cannot be FALSE if a nested custom_fill() query is given')
    if (is.na(label)) {
      warning('custom_fill() is used in a query where label = NA. This will be ignored, because the children of a node that is not labeld would be forgotten anyway')
      l = l[!is_fill]
    }
  } else {
    if (fill && !is.na(label)) l[['']] = custom_fill() 
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
#' @description 
#' Enables searching for parents or children.
#' Should only be used inside of the \link{tquery} function, or within other children/parents functions.
#' Look-up conditions are specified in the same way as in the tquery function.
#'  
#' Multiple children() or parents() functions can be nested side by side.
#' This works as an AND condition: the node must have all these parents/children (unless the req [required] argument is set to FALSE).
#' 
#' 
#' The custom_fill() function is used to include the children of a 'labeled' node. It can only be nested in a query if the label argument is not NULL,
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
#' @param connected Controls behavior if depth > 1 and filters are used. If FALSE, all parents/children to the given depth are retrieved, and then filtered. 
#'                  This way, grandchildren that satisfy the filter conditions are retrieved even if their parents do not satisfy the conditions.
#'                  If TRUE, the filter is applied at each level of depth, so that only fully connected branches of nodes that satisfy the conditions are retrieved. 
#' @param fill    Logical. If TRUE (default), the default custom_fill() will be used. To more specifically control fill, you can nest the \link{custom_fill} 
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
#' or children, we put them on the next line with indentation (in RStudio, it should automatically align correctly when you press enter inside
#' of the children() or parents() functions). 
#' 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__RI, or lemma__IR  (order of flags is irrelevant)
#' 
#' The not_children and not_parents functions will make the matched children/parents a NOT condition. Note that this is different from using the NOT() look-up function.
#' NOT operates at the node level, so you specify that a node should NOT be matched if certain conditions are met. the not_parents and not_children functions operate 
#' at the pattern level, so you can specify that a pattern is invalid if these parents/children are matched.
#' 
#' Next to the OR, AND, and NOT functions, children/parents functions can have the special BREAK function for cases where depth > 1.
#' If depth > 1 in the children, parents or fill function, the children/parents will
#' be retrieved recursively (i.e. children, children of children, etc.).
#' If the look-up conditions (e.g., relation = 'nsubj') are not satisfied, a node 
#' will not be matched by the query, but the search will still continue for it's
#' parents/children. The special BREAK look-up function allows you to specify a condition
#' for breaking the recursive loop (lending it's name from the `break` in a for loop).
#' An example is that you might want to stop the recursive loop in a custom_fill() once it encounters
#' a nested sentence, such as a relative clause: custom_fill(BREAK(relation = 'relcl')). 
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
  if (sum(is_fill) > 1) stop('cannot nest more than one custom_fill()')
  if (any(is_fill)) {
    if (!fill) stop('fill cannot be FALSE if a nested custom_fill() query is given')
    if (is.na(label)) {
      warning('custom_fill() is used in a query where label = NA. This will be ignored, because the children of a node that is not labeld would be forgotten anyway')
      l = l[!is_fill]
    }
  } else {
    if (fill && !is.na(label)) l[['']] = custom_fill() 
  }
 
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill') 
    is_break = sapply(l, methods::is, 'tokenLookupBreak')
    if (any(is_break) & connected) stop('Cannot use BREAK if connected = TRUE (note that this is also redundant')
    
    for (fill_i in which(sapply(l, methods::is, 'tQueryFill'))) {
      if (!is.na(label)) {
        l[[fill_i]]$label = paste(label, 'FILL', sep='_')
      } else {
        is_nested = is_nested[-fill_i]
        l = l[-fill_i]
      }
    }
    q = list(g_id=g_id, label=label, lookup = l[!(is_nested | is_break)], nested=l[is_nested], BREAK=l[is_break], level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, recursive=FALSE, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, BREAK=list(), level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, recursive=FALSE, max_window=max_window, min_window=min_window)
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
    is_break = sapply(l, methods::is, 'tokenLookupBreak')
    if (any(is_break) & connected) stop('Cannot use BREAK if connected = TRUE (note that this is also redundant')
    if (any(sapply(l, methods::is, 'tQueryFill'))) stop('custom_fill() cannot be used in not_ queries (not_children, not_parents)')
    q = list(g_id=g_id, label=label, lookup = l[!(is_nested | is_break)], nested=l[is_nested], BREAK=l[is_break], level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, BREAK=list(), level = 'children', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
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
  if (sum(is_fill) > 1) stop('cannot nest more than one custom_fill()')
  if (any(is_fill)) {
    if (!fill) stop('fill cannot be FALSE if a nested custom_fill() query is given')
    if (is.na(label)) {
      warning('custom_fill() is used in a query where label = NA. This will be ignored, because the children of a node that is not labeld would be forgotten anyway')
      l = l[!is_fill]
    }
  } else {
    if (fill && !is.na(label)) l[['']] = custom_fill() 
  }
  

  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill')
    is_break = sapply(l, methods::is, 'tokenLookupBreak')
    if (any(is_break) & connected) stop('Cannot use BREAK if connected = TRUE (note that this is also redundant')
    
    for (fill_i in which(sapply(l, methods::is, 'tQueryFill'))) {
      if (!is.na(label)) {
        l[[fill_i]]$label = paste(label, 'FILL', sep='_')
      } else {
        is_nested = is_nested[-fill_i]
        l = l[-fill_i]
      }
    }
    q = list(g_id=g_id, label=label, lookup = l[!(is_nested | is_break)], nested=l[is_nested], BREAK=l[is_break], level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, BREAK=list(), level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
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
    is_break = sapply(l, methods::is, 'tokenLookupBreak')
    if (any(is_break) & connected) stop('Cannot use BREAK if connected = TRUE (note that this is also redundant')
    
    if (any(sapply(l, methods::is, 'tQueryFill'))) stop('custom_fill() cannot be used in not_ queries (not_children, not_parents)')
    q = list(g_id=g_id, label=label, lookup = l[!(is_nested | is_break)], nested=l[is_nested], level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label=label, lookup =NULL, nested=NULL, BREAK=list(), level = 'parents', req=req, NOT=NOT, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  }
  
  class(q) = c('tQueryParent', class(q))
  q
}



#' Specify custom fill behavior
#'
#' @description 
#' If a tquery(), parents() or children() function has set a label, all children of the matched node (that are not matched by another query) will also be given this label.
#' This is called the 'fill' heuristic.
#' The custom_fill() function can be used to give more specific conditions for which children need to be labeled.
#' 
#' The function can be used almost identically to the children() function. The specification of the look-up conditions works in the same way.
#' NOTE that custom_fill, just like the children() function, should be passed as an unnamed argument, and NOT to the 'fill' argument 
#' (which is the boolean argument for whether fill should be used)
#' 
#' For the custom_fill function, the special BREAK() look-up function is particularly powerful.
#' custom_fill will recursively search for children, children of children, etc.
#' The look-up conditions in custom_fill determine which of all these direct and indirect children to label.
#' Often, however, you would want to the recursive loop to 'break' when certain conditions are met.
#' For instance, to ignore children in a relative clause: custom_fill(BREAK(relation = 'relcl'))
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
#' @param depth   A positive integer, determining how deep parents/children are sought. 1 
#'                means that only direct parents and children of the node are retrieved. 2 means children and grandchildren, etc.
#'                All parents/children must meet the filtering conditions (... or g_id)
#' @param connected Controls behavior if depth > 1 and filters are used. If FALSE, all parents/children to the given depth are retrieved, and then filtered. 
#'                  This way, grandchildren that satisfy the filter conditions are retrieved even if their parents do not satisfy the conditions.
#'                  If TRUE, the filter is applied at each level of depth, so that only fully connected branches of nodes that satisfy the conditions are retrieved. 
#' @param max_window  Set the max token distance of the children/parents to the node. Has to be either a numerical vector of length 1 for distance in both directions, or a 
#'                vector of length 2, where the first value is the max distance to the left, and the second value the max distance to the right. Default is c(Inf, Inf) meaning that no max distance is used.
#' @param min_window  Like max_window, but for the min distance. Default is c(0,0) meaning that no min is used.
#' 
#' @return Should not be used outside of \link{tquery}
#' @examples 
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text4',]
#' 
#' ## custom fill rule that ignores relative clauses
#' no_relcl_fill = custom_fill(BREAK(relation='relcl'))
#' 
#' ## add custom fill as argument in children(). NOTE that it should be
#' ## passed as an unnamed argument (and not to the fill boolean argument)
#' tq = tquery(label = 'verb', pos='VERB', fill=FALSE,
#'          children(label = 'subject', relation = 'nsubj', no_relcl_fill),
#'          children(label = 'object', relation = 'dobj', no_relcl_fill))
#'          
#' tokens = annotate_tqueries(tokens, "clause", tq)
#' tokens
#' @export
custom_fill <- function(..., g_id=NULL, depth=Inf, connected=FALSE, max_window=c(Inf,Inf), min_window=c(0,0)) {
  if (length(min_window) == 1) min_window = c(min_window,min_window)
  if (length(max_window) == 1) max_window = c(max_window,max_window)
  #select = deparse(bquote_s(substitute(select)))
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, methods::is, 'tQueryParent') | sapply(l, methods::is, 'tQueryChild')  | sapply(l, methods::is, 'tQueryFill')
    is_break = sapply(l, methods::is, 'tokenLookupBreak')
    
    if (any(is_nested)) stop('Cannot use nested queries (children(), parents(), etc.) in custom_fill()')
    q = list(g_id=g_id, label='fill', lookup = l[!(is_nested | is_break)], nested=l[is_nested], BREAK=l[is_break], level = 'children', req=FALSE, NOT=FALSE, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  } else {
    q = list(g_id=g_id, label='fill', lookup =NULL, nested=NULL, BREAK=list(), level = 'children', req=FALSE, NOT=FALSE, depth=depth, connected=connected, max_window=max_window, min_window=min_window)
  }
  
  class(q) = c('tQueryFill', class(q))
  q
}

#' Specify custom fill behavior
#'
#' @description 
#' 
#' This is soft deprecated, with the new preferred function being custom_fill to avoid namespace conflicts with tidyr::fill() and data.table::fill()
#' 
#' @param ...     passes to custom_fill
#' 
#' @return Should not be used outside of \link{tquery}
#' @export
fill <- function(...) {
  custom_fill(...)
}

#' Use OR search in tquery
#' 
#' @param ... name-value pairs for look-up terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(OR(lemma = 'walk', POS='Noun'))
OR <- function(...) {
  l = list(lookup = list(...), boolean='OR')
  for (cl in sapply(l$lookup, class, simplify=F)) {
    if (any(grepl('tQuery', cl))) stop('Boolean function AND cannot contain nested tqueries/parents/children')
    if (any(grepl('tokenLookupBreak', cl))) stop('Special Boolean function BREAK cannot be nested within other boolean functions')
  }
  class(l) = c(class(l), 'tokenLookup')
  l
}

#' Use AND search in tquery
#' 
#' @param ... name-value pairs for look-up terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(AND(lemma = 'walk', POS='Noun'))   ## is also the default
AND <- function(...) {
  l = list(lookup = list(...), boolean='AND')
  for (cl in sapply(l$lookup, class, simplify=F)) {
    if (any(grepl('tQuery', cl))) stop('Boolean function AND cannot contain nested tqueries/parents/children')
    if (any(grepl('tokenLookupBreak', cl))) stop('Special Boolean function BREAK cannot be nested within other boolean functions')
  }
  class(l) = c(class(l), 'tokenLookup')
  l
}

#' Use NOT search in tquery
#' 
#' @param ... name-value pairs for look-up terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(NOT(POS='Noun'))  
NOT <- function(...) {
  l = list(lookup = list(...), boolean='NOT')
  for (cl in sapply(l$lookup, class, simplify=F)) {
    if (any(grepl('tQuery', cl))) stop('Boolean function NOT cannot contain nested tqueries/parents/children')
    if (any(grepl('tokenLookupBreak', cl))) stop('Special Boolean function BREAK cannot be nested within other boolean functions')
  }
  class(l) = c(class(l), 'tokenLookup')
  l
}



#' A special NOT condition if depth > 1
#' 
#' If depth > 1 in the children, parents or fill function, the children/parents will
#' be retrieved recursively (i.e. children, children of children, etc.).
#' If the look-up conditions (e.g., relation = 'nsubj') are not satisfied, a node 
#' will not be matched by the query, but the search will still continue for it's
#' parents/children. The special BREAK look-up function allows you to specify a condition
#' for breaking the recursive loop (lending it's name from the `break` in a for loop).
#' An example is that you might want to stop the recursive loop in a custom_fill() once it encounters
#' a nested sentence, such as a relative clause: custom_fill(BREAK(relation = 'relcl')). 
#' 
#' @param ... name-value pairs for look-up terms. see ?query.
#'
#' @return A list, to be used as input to \link{tquery}
#' @export
#'
#' @examples
#' tquery(NOT(POS='Noun'))  
BREAK <- function(...) {
  l = list(lookup = list(...), boolean='NOT')
  for (cl in sapply(l$lookup, class, simplify=F)) {
    if (any(grepl('tQuery', cl))) stop('Special Boolean function BREAK cannot contain nested tqueries/parents/children')
    if (any(grepl('tokenLookupBreak', cl))) stop('Special Boolean function BREAK cannot be nested within other boolean functions')
  }
  class(l) = c(class(l), 'tokenLookupBreak')
  l
}





