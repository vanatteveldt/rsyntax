pipe_tr <- function() intToUtf8(9492)
pipe_tb <- function() intToUtf8(9474)
pipe_trb <- function() intToUtf8(9500)

abbrev_str <- function(string, maxlen) {
  if (nchar(string) > maxlen) string = paste(stringi::stri_sub(string, 0, maxlen-3), '...', sep='')
  string
}

recprint <- function(x, pd, level=1, connector=pipe_tr(), pipe_level=c(), max_char=getOption('tQuery_print_max_char', default=30), ...) {
  #cat(level, ': ', sep='')
  if (level > 0) {
    type = if('level' %in% names(x)) ifelse(x$level == 'children', ' c', ' p') else '  n'
    
    level_space = rep('  ', level-1)
    level_space[pipe_level] = sprintf('%s ', pipe_tb())
    text = paste(paste(level_space, collapse=''), connector, type, ' ', sep='')
    if (nchar(text) < pd[1]) text = paste(text, paste(rep(' ', pd[1] - nchar(text)), collapse=''), sep='')
    cat(text, sep='')
    
    if (!is.na(x$label))
      cat('  ', x$label, rep(' ', pd[2] - nchar(x$label)), '  ', sep='') else cat('  ', rep(' ', pd[2]), '  ', sep='')

    first = TRUE    
    if ('NOT' %in% names(x) && x$NOT) {
      if (!first) cat(', ') else cat(' ')
      cat('NOT=T')
      first = FALSE
    }
    if ('req' %in% names(x) && !x$req) {
      if (!first) cat(', ') else cat(' ')
      cat('req=F')
      first = FALSE
    }  
    #if (!x$select == 'NULL') {
    #  if (!first) cat(', ') else cat(' ')
    #  cat('select=', abbrev_str(x$select, max_char))
    #  first = FALSE
    #}  
    if ('depth' %in% names(x) && x$depth > 1) {
      if (!first) cat(', ') else cat(' ')
      cat('depth=', x$depth, sep='')
      first = FALSE
    }  
    
    l = x$lookup
    rec_lookup_print(l, first, max_char)
    cat('\n', sep='')
  }
  for (i in seq_along(x$nested)) {
    pipe_level = if (level < 2) pipe_level else c(pipe_level, level)
    if (i == length(x$nested))
      recprint(x$nested[[i]], pd, level+1, pipe_tr(), pipe_level=pipe_level, max_char=max_char)
    else
      recprint(x$nested[[i]], pd, level+1, pipe_trb(), pipe_level=pipe_level, max_char=max_char)
  }
}

rec_lookup_print <- function(l, first, max_char, op = 'AND', level=1) {
  if (op %in% c('OR','NOT')) {
    if (level > 1) cat(', ') else cat(' ')
    cat(op, '(', sep='')
  }
  for (i in seq_along(l)) {
    if (is.null(l[[i]])) next
    if (methods::is(l[[i]], 'tokenLookup')) {
      rec_lookup_print(l[[i]]$lookup, first=TRUE, max_char, l[[i]]$boolean, level=level+1)
      if (!first) cat(', ') else cat(' ')
      first = FALSE
      next
    }
    n = names(l)[[i]]
    v = if (class(l[[n]]) %in% c('factor','character')) paste0(l[[n]]) else l[[n]]
    if (length(v) > 1) v = paste0('(', abbrev_str(paste(v, collapse=','), max_char), ')')
    if (!first) cat(', ') else if (level==1) cat(' ')
    first = FALSE
    cat(n, '=', v, sep='')
  }
  if (op %in% c('OR','NOT')) cat(')')
}

get_print_data <- function(x, d=c(0,0)) {
  d[2] = max(d[2], nchar(x$label), na.rm = TRUE)
  for (i in seq_along(x$nested)) {
    d = get_print_data(x$nested[[i]], c(d[1]+1, d[2]))
  }
  d
}

#' S3 print for tQuery class
#'
#' @param x a tQuery
#' @param ... not used
#'
#' @method print tQuery
#' @examples
#' q = tquery(label='quote',
#'            children(relation='nmod:according_to', label='source',
#'                     children(label='verb')))
#' q 
#' @export
print.tQuery <- function(x, ...) {
  pd = get_print_data(x, c(0,10))
  pd[1] = (pd[1]*3)
  if (pd[1] < 12) pd[1] = 10
  #cat('LEVEL ', rep(' ', pd[1]-3), '  NAME', rep(' ', pd[2]-4), '   FILTER\n', sep='')
  #if (!is.na(x$label) &! x$label == '') cat(x$label, '\n', sep = '') else cat('...', sep='')
  recprint(x, pd, connector='', ...)
}
