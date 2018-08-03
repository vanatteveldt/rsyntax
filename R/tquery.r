
#' Create a query for dependency based parse trees in a data.table (CoNLL-U or similar format).
#'
#' @description
#' There are two ways to query nodes (i.e. rows). Firstly, you can use named arguments, where the names are column names (in the data.table on which the
#' queries will be used) and the values are vectors with lookup values. Secondly, you can use the select arguments to use logical expressions.   
#' The select argument is more versatile (but see the parameter details for limitations), whereas the named argument approach is more explicit and uses binary search (which is much faster).
#' 
#' Children or parents of nodes can be queried by passing the \link{childen} or \link{parents} function as (named or unnamed) arguments.
#' These functions use the same query format as the tquery function, and children and parents can be nested recursively to find children of children etc. 
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
#' @param select  An expression to select specific parents/children, which can use any columns in the token data (similar to the subset argument in \link{subset.data.frame}).
#'                WARNING!! since the expression will only be evaluated when the query is performed, any names used in the expression that are not columns in the data.table 
#'                need to be in the environment from where the query is performed. Alternatively, you can ask to evaluate specific names when making the tquery, by marking them 
#'                as .(name). For example, if the name VERBS refers to a character vector of verbs, using 'lemma \%in\% .(VERBS)' will replace .(VERBS) with the actual vector.
#' @param g_id    Find nodes by global id, which is the combination of the doc_id, sentence and token_id. Passed as a data.frame or data.table with 3 columns: (1) doc_id, (2) sentence and (3) token_id. 
#' @param save    A character vector, specifying the column name under which the selected tokens are returned. 
#'                If NA, the column is not returned.
#'                
#'                
#' @return        A tQuery object, that can be used with the \link{apply_queries} function.
#' 
#' @details 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' If the name is given the suffix __N, only rows without an exact match are found. (so, lemma__N = "fish" look for all rows in which the lemma is not "fish").
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__NRI, or lemma_IRN  (order of flags is irrelevant)
#' 
#' @examples
#' ## it is convenient to first prepare vectors with relevant words/pos-tags/relations
#' .SAY_VERBS = c("tell", "show","say", "speak") ## etc.
#' .QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
#' .SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 
#' 
#' quotes_direct = tquery(lemma = .SAY_VERBS,
#'                          children(save = 'source', p_rel = .SUBJECT_RELS),
#'                          children(save = 'quote', p_rel = .QUOTE_RELS))
#' quotes_direct ## print shows tquery
#' @export
tquery <- function(..., select=NULL, NOT=NULL, g_id=NULL, save=NA) {
  select = deparse(bquote_s(substitute(select), where = parent.frame()))
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, is, 'tQueryParent') | sapply(l, is, 'tQueryChild') 
    q = list(select = select, g_id=g_id, save=save, lookup = l[!is_nested], nested=l[is_nested])
  } else {
    q = list(select = select, g_id=g_id, save=save, lookup =NULL, nested=NULL)
  }
  
  class(q) = c('tQuery', class(q))
  q
}

#' Search for parents or children in tquery
#'
#' Should only be used inside of the \link{tquery} function.
#' Enables searching for parents or children, either direct (depth = 1) or until a given depth (depth 2 for children and grandchildren, Inf (infinite) for all).
#' 
#' Searching for parents/children within find_nodes works as an AND condition: if it is used, the node must have parents/children.
#' If select is used to pass an expression, the node must have parents/children for which the expression is TRUE.
#' The save argument can be used to remember the global token ids (.G_ID) of the parents/children under a given column name.
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
#' @param select  An expression to select specific parents/children, which can use any columns in the token data (similar to the subset argument in \link{subset.data.frame}).
#'                However, this has two limitations. Firstly, select should not rely on absolute positions (a logical vector or indices). Secondly, since the expression will only be evaluated
#'                when the query is performed, any names used in the expression that are not columns in the data.table need to be in the environment when the search is performed.
#'                A solution for the second limitation is to explicitly tell tquery to evalute these names immediately, by marking them as .(name). For example, if the name VERBS refers 
#'                to a character vector of verbs, using 'lemma \%in\% .(VERBS)' will replace .(VERBS) with the actual vector.
#' @param g_id    Find nodes by global id, which is the combination of the doc_id, sentence and token_id. Passed as a data.frame or data.table with 3 columns: (1) doc_id, (2) sentence and (3) token_id. 
#' @param save    A character vector, specifying the column name under which the selected tokens are returned. 
#'                If NA, the column is not returned.
#' @param NOT     If TRUE, make having these parents/children a NOT condition.          
#' @param depth   A positive integer, determining how deep parents/children are sought. The default, 1, 
#'                means that only direct parents and children of the node are retrieved. 2 means children and grandchildren, etc.
#'
#' @details 
#' Having nested queries can be confusing, so we tried to develop the find_nodes function and the accompanying functions in a way
#' that clearly shows the different levels. As shown in the examples, the idea is that each line is a node, and to look for parents
#' or children, we put them on the next line with indentation (in RStudio, it should automatically allign correctly when you press enter inside
#' of the children() or parents() functions). 
#' 
#' There are several flags that can be used to change search condition. To specify flags, add a double underscore and the flag character to the name in the name value pairs (...).
#' If the name is given the suffix __N, only rows without an exact match are found. (so, lemma__N = "fish" look for all rows in which the lemma is not "fish").
#' By adding the suffix __R, query terms are considered to be regular expressions, and the suffix __I uses case insensitive search (for normal or regex search).
#' If the suffix __F is used, only exact matches are valid (case sensitive, and no wildcards).
#' Multiple flags can be combined, such as lemma__NRI, or lemma_IRN  (order of flags is irrelevant)
#' 
#' @return Should not be used outside of \link{find_nodes}
#' @name find_nodes_functions
#' @rdname find_nodes_functions
NULL

#' @rdname find_nodes_functions
#' @export
children <- function(..., select=NULL, g_id=NULL, save=NA, NOT=F, depth=1) {
  select = deparse(bquote_s(substitute(select)))
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, is, 'tQueryParent') | sapply(l, is, 'tQueryChild') 
    q = list(select = select, g_id=g_id, save=save, lookup = l[!is_nested], nested=l[is_nested], level = 'children', NOT=NOT, depth=depth)
  } else {
    q = list(select = select, g_id=g_id, save=save, lookup =NULL, nested=NULL, level = 'children', NOT=NOT, depth=depth)
  }
  
  
  class(q) = c('tQueryChild', class(q))
  q
}

#' @rdname find_nodes_functions
#' @export
parents <- function(..., select=NULL, g_id=NULL, save=NA, NOT=F, depth=1) {
  select = deparse(bquote_s(substitute(select)))
  l = list(...)
  if (length(l) > 0) {
    is_nested = sapply(l, is, 'tQueryParent') | sapply(l, is, 'tQueryChild') 
    q = list(select = select, g_id=g_id, save=save, lookup = l[!is_nested], nested=l[is_nested], level = 'parents', NOT=NOT, depth=depth)
  } else {
    q = list(select = select, g_id=g_id, save=save, lookup =NULL, nested=NULL, level = 'parents', NOT=NOT, depth=depth)
  }
  
  class(q) = c('tQueryParent', class(q))
  q
}

