
filter_tokens <- function(tokens, lookup=list(), select='NULL', .G_ID=NULL, .G_PARENT=NULL, .BLOCK=NULL, e=parent.frame(), use_index=T) {
  ## we need the ridiculous .UPPERCASE because if the name happens to be a column in data.table it messes up (it will use its own column for the binary search)
  
  i = NULL
  null_intersect <- function(x, y) if (is.null(x)) y else intersect(x,y) 
  if (!is.null(.G_ID)) i = null_intersect(i, tokens[list(.G_ID[[1]], .G_ID[[2]], .G_ID[[3]]), on=c(cname('doc_id'),cname('sentence'),cname('token_id')), which=T])
  if (!is.null(.G_PARENT)) i = null_intersect(i, tokens[list(.G_PARENT[[1]], .G_PARENT[[2]], .G_PARENT[[3]]), on=c(cname('doc_id'),cname('sentence'),cname('parent')), which=T])
  .BLOCK = block_ids(.BLOCK)
  if (!is.null(.BLOCK)) i = null_intersect(i, tokens[!list(.BLOCK[[1]], .BLOCK[[2]], .BLOCK[[3]]), on=c(cname('doc_id'),cname('sentence'),cname('token_id')), which=T])
  if (!is.null(i)) {
    i = na.omit(i)
    tokens = tokens[as.numeric(i),]  
  }
  
  i = lookup_tokens(tokens, lookup, use_index=use_index)
  if (!is.null(i)) {
    i = na.omit(i)
    tokens = tokens[as.numeric(i),]  
  }
  
  if (!select == 'NULL' & !is.null(select)) tokens = tokens[eval(parse(text=select), tokens, e),]
  tokens
}


lookup_tokens <- function(tokens, lookup=list(), boolean='AND', use_index=T) {
  i = NULL
  for (lookup_i in seq_along(lookup)) {
    .N = names(lookup)[lookup_i]
    .V = lookup[[lookup_i]]
    
    if (is(.V, 'tokenLookup')) {
      result = lookup_tokens(tokens, .V$lookup, boolean=.V$boolean)
    } else {
      .COLNAME = gsub('__.*', '', .N)
      if (!.COLNAME %in% colnames(tokens)) stop(sprintf('%s is not a valid column name in tokens', .N))
      if (use_index) if (!.N %in% data.table::indices(tokens)) data.table::setindexv(tokens, .COLNAME)
      .V = prepare_terms(.V, tokens[[.COLNAME]], 
                         ignore_case = grepl('__N?R?F?I', .N), 
                         regex = grepl('__N?I?F?R', .N),
                         fixed = grepl('__N?R?I?F', .N))
      if (!grepl('__R?I?L?N', .N)) {
        result = tokens[list(.V), on=(.COLNAME), which=T]
      } else {
        result = tokens[!list(.V), on=(.N), which=T]
      }
    }
    if (is.null(i)) {
      i = result
    } else {
      if (boolean == 'AND') i = intersect(i, result)
      if (boolean == 'OR') i = union(i, result)
    }
  }
  i
}

get_full_terms <- function(x, terms, batchsize=25, ignore_case=T) {
  terms = if (is(terms, 'factor')) levels(terms) else unique(terms)
  if (length(x) > 1) { ## if there are multiple terms, make batches of terms and turn each batch into a single regex
    x = split(as.character(x), ceiling(seq_along(x)/batchsize))
    x = sapply(x, stringi::stri_paste, collapse='|')
    out = rep(F, length(x))
    for(xbatch in x){
      out = out | grepl(xbatch, terms, ignore.case=ignore_case)
    }
  } else {
    out = grepl(as.character(x), terms, ignore.case=ignore_case)
  }
  terms[out]
}

search_term_regex <- function(patterns) {
  patterns = gsub("([^0-9a-zA-Z])", '\\\\\\1', x=patterns)  # escape special characters
  patterns = gsub('\\\\(\\*)|\\\\(\\?)', '.\\1', patterns)  # process wildcards
  paste0('\\b',patterns,'\\b')                              # set word boundaries
}

prepare_terms <- function(x, terms, ignore_case=T, regex=F, fixed=F) {
  if (ignore_case && fixed) warning('ignore_case (__I) is not used, because fixed (__F) is also used')
  if (regex && fixed) warning('regex (__R) is not used, because fixed (__F) is also used')
  if (fixed) return(x)
  
  if (regex) {
    return(get_full_terms(x, terms, ignore_case = ignore_case)) 
  } else {
    if (ignore_case) {
      x = search_term_regex(x)
      return(get_full_terms(x, terms, ignore_case = T)) 
    } else {
      has_wildcard = grepl('[*?]', x)
      if (!any(has_wildcard)) return(x)
      x_full = get_full_terms(search_term_regex(x[has_wildcard]), terms, ignore_case=F)
      x = c(x[!has_wildcard], x_full)
      return(unique(x))
    }
  }
}

#' Use OR search in tquery
#' 
#' @param ... 
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
#' @param ... 
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

