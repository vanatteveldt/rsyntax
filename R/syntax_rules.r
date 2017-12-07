#' Apply rules created with \link{create_rule}
#'
#' @param tokens   A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param ...      rule functions, as created with \link{create_rule}. Can also be a list with rule functions.
#' @param as_chain If TRUE, Nodes that have already been assigned assigned earlier in the chain will be ignored (see 'block' argument).
#' @param block    Optionally, specify ids (doc_id - token_id pairs) where find_nodes will stop (ignoring the id and recursive searches through the id). 
#'                 Can also be a data.table returned by (a previous) apply_rules, in which case all ids are blocked. 
#' @param check    If TRUE, return a warning if nodes occur in multiple patterns, which could indicate that the find_nodes query is not specific enough.
#'
#' @return        A data.table in which each row is a node for which all conditions are satisfied, and each column is one of the linked nodes 
#'                (parents / children) with names as specified in the save argument.
#'                
#' @examples
#' ## it is convenient to first prepare vectors with relevant words/pos-tags/relations
#' .SAY_VERBS = c("tell", "show","say", "speak") ## etc.
#' .QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
#' .SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 
#' 
#' quotes_direct = create_rule(select = lemma %in% .SAY_VERBS,
#'                          children(save = 'source', rel = .SUBJECT_RELS),
#'                          children(save = 'quote', rel = .QUOTE_RELS))
#' quotes_direct ## print shows rule
#' 
#' tokens = subset(tokens_corenlp, sentence == 1)
#' 
#' nodes = apply_rules(tokens, quotes_direct)
#' nodes
#' annotate(tokens, nodes, column = 'example')
#' 
#' @export
apply_rules <- function(tokens, ..., as_chain=T, block=NULL, check=T) {
  r = list(...)
  r = unlist(sapply(r, unlist)) ## account for nested lists of functions

  out = vector('list', length(r))
  for (i in 1:length(r)){
    nodes = do.call(r[[i]], args = list(tokens=tokens, block=block, check=check, e=parent.frame()))
    if (!is.null(nodes)) {
      if (as_chain) block = block_ids(block, nodes)
      out[[i]] = nodes  
    }
  }
  data.table::rbindlist(out, fill=T)
} 

#' Write rules for extracting syntactic elements from tokens with dependency relations.
#'
#' This is the primary workhorse for writing rules for quote and clause extraction.
#' Specific nodes can be selected using standard R expressions, such as: 
#' \itemize{
#' \item pos == "verb" & lemma \%in\% .SAY_VERBS
#' }
#' Then, from the position of these nodes, you can lookup children, optionally with 
#' another select expression. This can be done recursively to find children of children etc. 
#' 
#' To look for parents or children, use the \link{parents} and \link{children} functions (and optionally \link{all_parents} or \link{all_children}).
#' Please look at the examples below for a recommended syntactic style for using the find_nodes function and these nested functions.
#'
#' @param ...     used to nest the \link{parents} and \link{children} functions as unnamed arguments. See the documentation of these
#'                functions for details.
#' @param save    A character vector, specifying the column name under which the selected tokens are returned. 
#'                If NA, the column is not returned.
#' @param rel     A character vector, specifying the relation of the node to its parent.
#' @param not_rel Like rel, but for excluding relations
#' @param select  An expression to select specific parents/children. All columns in the tokens input for \link{find_nodes} can be used, 
#'                as well as any column in the environment from which find_nodes is called. (similar to \link{subset.data.frame}).
#'                Note (!!) that select should not rely on absolute positions (i.e. choosing rows with a logical vector or a numeric vector with indices).
#' @param g_id    A data.frame or data.table with 2 columns: (1) doc_id and (2) token_id, indicating the global id. While this can also be done using 'select', this alternative uses fast binary search.
#' 
#' @return        A function for performing the specified rule, using the \link{apply_rules} function.
#' 
#' @examples
#' ## it is convenient to first prepare vectors with relevant words/pos-tags/relations
#' .SAY_VERBS = c("tell", "show","say", "speak") ## etc.
#' .QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
#' .SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 
#' 
#' quotes_direct = create_rule(select = lemma %in% .SAY_VERBS,
#'                          children(save = 'source', rel = .SUBJECT_RELS),
#'                          children(save = 'quote', rel = .QUOTE_RELS))
#' quotes_direct ## print shows rule
#' @export
create_rule <- function(..., save=NA, rel=NULL, not_rel=NULL, select=NULL, g_id=NULL) {
  select = deparse(substitute(select))
  f <- function(tokens, block=NULL, check=T, e=parent.frame()) {
    find_nodes(tokens, ..., save=save, rel=rel, not_rel=not_rel, select=select, g_id=g_id, block=block, check=check, e=e)
  }
  class(f) = c('rsyntaxRule', class(f))
  attr(f, 'print') = format_syscall(sys.call()) 
  f
}

format_syscall <- function(sc) {
  ## given the output of sys.call(), create a string with suitable indentation
  sc = deparse(sc)                               ## create string
  sc = paste(sc, collapse='')                    ## unsplit width based linebreaks
  sc = gsub('create_rule\\(', '', sc)            ## remove create_rule()
  sc = gsub('\\)$', '', sc)
  sc = gsub(' +', ' ', sc)                       ## drop previous indentation
  sc = gsub('(children\\()', '\n\\1', sc)        ## add level based linebreaks
  sc = gsub('(parents\\()', '\n\\1', sc)         ## ...
  sc = stringi::stri_split(sc, regex='\n')[[1]]  ## split into liness
  
  lpar = stringi::stri_count(sc, regex='\\(')    ## count left parentheses
  rpar = stringi::stri_count(sc, regex='\\)')    ## count right parentheses
  indent = cumsum(lpar - rpar) +  1              ## nesting based indentation
  prefix = sapply(indent, function(x)            ## indentation as string (tabs)
    paste(rep('\t', x), collapse='')) 
  sc = paste(sc, '\n', prefix, sep='')           ## add new linebreaks and indentation
  paste(sc, collapse='')                         ## voila
}

#' S3 print for rsyntaxRule class
#'
#' @param x an rsyntaxRule function
#' @param ... not used
#'
#' @method print rsyntaxRule
#' @examples
#' rule = create_rule(select = lemma %in% .VIND_VERBS, 
#'                    children(save = 'source', rel=.SUBJECT_REL),
#'                    children(rel='vc', select = POS %in% c('C', 'comp'),
#'                             children(save='quote', rel=.SUBJECT_BODY)))
#' rule 
#' @export
print.rsyntaxRule <- function(x, ...) cat(attr(x, 'print'))

