#' Reshape tokens based on rsyntax annotations
#'
#' @param tokens 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
reshape_tokens <- function(tokens, by) {
  
}

get_token_group <- function(tokens, column, group, rows, group_values, mode) {
  if (!mode %in% c('collapse', 'each')) stop('not a valid mode')
  sub = tokens[[column]] %in% group
  column_id = paste0(column, '_id')
  #if (!is.null(filter)) sub = sub & filter
  
  d = subset(tokens, subset = tokens[[column]] %in% group, select = c(group_values, column_id))
  if (mode == 'collapse') {
    ids = tapply(d[[group_values]], d[[column_id]], FUN=paste, collapse=' ')
    ids = data.table::data.table(id = names(ids), value=as.factor(as.character(ids)))
  }
  if (mode == 'each') {
    d = unique(subset(d, subset=!is.na(d[[group_values]]), select = c(column_id, group_values)))
    ids = data.table::data.table(id = d[[column_id]], value=as.factor(as.character(d[[group_values]])))
  }
  
  data.table::setnames(ids, 'value', paste(column, paste(group, collapse='_'), sep='_'))
  out = if (is.null(rows)) subset(tokens, subset=!tokens[[column]] %in% group) else subset(tokens, subset=tokens[[column]] %in% rows)
  merge(out, ids, by.x = column_id, by.y = 'id')
}

group_tokens <- function(column, group, group_values, values=NA, mode=c('collapse','each')) {
  mode = match.arg(mode)
  out = list(column=column, group=group, group_values=group_values, mode=mode, values=values)
}


function() {
group = group_tokens(column='quotes', group='source', group_values='lemma', mode='collapse')


ids = get_token_group(tokens, column=group$column, group=group$group, rows=NULL, group_values=group$group_values, mode=group$mode)
}

