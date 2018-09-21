abbrev_str <- function(string, maxlen) {
  if (nchar(string) > maxlen) string = paste(stringi::stri_sub(string, 0, maxlen-3), '...', sep='')
  string
}



recprint <- function(x, pd, level=1, connector='└', max_char=getOption('tQuery_print_max_char', default=30), ...) {
  cat(level, ': ', sep='')
  if (level > 0) {
    type = if('level' %in% names(x)) ifelse(x$level == 'children', ' c', ' p') else 'root'
    text = paste(paste(rep('  ', level-1), collapse=''), connector, type, ' ', sep='')
    if (nchar(text) < pd[1]) text = paste(text, paste(rep(' ', pd[1] - nchar(text)), collapse=''), sep='')
    cat(text, sep='')
    
    if (!is.na(x$save))
      cat('  ', x$save, rep(' ', pd[2] - nchar(x$save)), '  ', sep='') else cat('  ', rep(' ', pd[2]), '  ', sep='')

    first = T    
    if (x$NOT) {
      if (!first) cat(', ') else cat(' ')
      cat('NOT=T')
      first = F
    }
    if (!x$req) {
      if (!first) cat(', ') else cat(' ')
      cat('req=F')
      first = F
    }  
    if (!x$select == 'NULL') {
      if (!first) cat(', ') else cat(' ')
      cat('select=', abbrev_str(x$select, max_char))
      first = F
    }  
    if (x$depth > 1) {
      if (!first) cat(', ') else cat(' ')
      cat('depth=', x$depth)
      first = F
    }  
    
    l = x$lookup
    for (n in names(l)) {
      if (is.null(l[[n]])) next
      v = if (class(l[[n]]) %in% c('factor','character')) paste0(l[[n]]) else l[[n]]
      if (length(v) > 1) v = paste0('(', abbrev_str(paste(v, collapse=','), max_char), ')')
      if (!first) cat(', ') else cat(' ')
      first = F
      cat(n, '=', v, sep='')
    }
    cat('\n', sep='')
  }
  for (i in seq_along(x$nested)) {
    if (i == length(x$nested))
      recprint(x$nested[[i]], pd, level+1, '└', max_char=max_char)
    else
      recprint(x$nested[[i]], pd, level+1, '├', max_char=max_char)
  }
}

get_print_data <- function(x, d=c(0,0)) {
  d[2] = max(d[2], nchar(x$save), na.rm = T)
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
#' q = tquery(select = lemma %in% .VIND_VERBS, 
#'                    children(save = 'source', p_rel=.SUBJECT_REL),
#'                    children(p_rel='vc', select = POS %in% c('C', 'comp'),
#'                             children(save='quote', p_rel=.SUBJECT_BODY)))
#' q 
#' @export
print.tQuery <- function(x, ...) {
  pd = get_print_data(x, c(0,10))
  pd[1] = (pd[1]*3)
  if (pd[1] < 12) pd[1] = 10
  cat('LEVEL ', rep(' ', pd[1]-3), '  NAME', rep(' ', pd[2]-4), '   FILTER\n', sep='')
  #if (!is.na(x$save) &! x$save == '') cat(x$save, '\n', sep = '') else cat('...', sep='')
  recprint(x, pd, connector='', ...)
}


