#' Cast tokens into sub-sentences based on rsyntax annotations
#' 
#' @param tokens       A tokenIndex with rsyntax annotations
#' @param by           A list or named vector to specify the grouping elements, where names are columns and values are values in the columns. For example, if there are 
#'                     "quotes" and "clauses" columns, that have "source" and "subject" respectively, use c(quotes = "source", clauses = "subject"). 
#' @param id           The name of the column the contains the ids/codes of the groups. By default, each unique code is casted to a subsentence. Alternatively,
#'                     if the id column contains text instead of unique codes, the collapse_id argument can be used to collapse the text into an id.
#' @param collapse_id  see id
#' @param columns      Optionally, specify which column from 'tokens' will be included in the output. If NULL, all columns are returned, but it is recommended 
#'                     to specify only the columns that you want to use for clarity.
#'
#' @return
#' @export
#'
#' @examples
cast_tokens <- function(tokens, by, id, collapse_id=F, columns=NULL, rm.na=T){
  mode = if (collapse_id) 'collapse' else 'each'
  out = NULL
  for (i in seq_along(by)) {
    column = names(by)[i]
    group = by[[i]]
    if (!column %in% colnames(tokens)) stop(sprintf('%s is not a valid column in tokens', column))
    if (!group %in% levels(tokens[[column]])) stop(sprintf('%s is not a valid value in tokens$%s', group, column))
    tg = get_token_group(tokens, column=column, group=group, rows=NULL, group_values=id, mode=mode)
    out = if (is.null(out)) tg else merge(out, tg, by=c('doc_id','sentence','token_id'), all=!rm.na, allow.cartesian=T)
  }
  
  id_cols = paste(names(by), 'id', sep='_')
  ids = do.call(paste, args = c(as.list(subset(out, select=id_cols)), sep='|'))
  out$subsent_id = match(ids, unique(ids))
  out[,(id_cols) := NULL]
  
  if (!is.null(columns)) {
    out = merge(out, subset(tokens, select=unique(c('doc_id','sentence','token_id',columns))), by=c('doc_id','sentence','token_id'))
  } else {
    out = merge(out, tokens, by=c('doc_id','sentence','token_id'))
  }
  data.table::setcolorder(out, c('doc_id','sentence','subsent_id'))
  out
}

#' Like cast_tokens, but instead returns the tokens as a quanteda document-feature matrix
#' 
#' @param tokens       A tokenIndex with rsyntax annotations
#' @param x            The name of the column that will be aggregated (!not the vector itself)
#' @param FUN          The function used to aggregate.
#' @param by           A list or named vector to specify the grouping elements, where names are columns and values are values in the columns. For example, if there are 
#'                     "quotes" and "clauses" columns, that have "source" and "subject" respectively, use c(quotes = "source", clauses = "subject"). 
#' @param id           The name of the column the contains the ids/codes of the groups. By default, each unique code is casted to a subsentence. Alternatively,
#'                     if the id column contains text instead of unique codes, the collapse_id argument can be used to collapse the text into an id.
#' @param collapse_id  see id
#' @param ...          Additional arguments passed to FUN
#'
#' @return
#' @export
#'
#' @examples
cast_tokens_dfm <- function(tokens, x, FUN, by, id, collapse_id=F, ...){
  out = cast_tokens(tokens, by=by, id=id, collapse_id=collapse_id, columns=x)
  docs = as.factor(out$subsent_id)
  m = Matrix::spMatrix(length(levels(i)), length(levels(feature)),
                       as.numeric(i)[notNA], as.numeric(feature)[notNA],
                       rep(1, sum(notNA)))
  m = quanteda::as.dfm(methods::as(m, 'dgCMatrix'))
  dvars = tc$get_meta(copy=T)
  dvars = dvars[match(rownames(m), dvars$doc_id),]  ## in case of subsetting
  for (dvar in colnames(dvars)) {
    if (dvar == 'doc_id') next
    quanteda::docvars(m, field = dvar) = dvars[[dvar]]
  }
  
}

#' Like cast_tokens, but instead of returning the tokens, aggregate one column
#' 
#' @param tokens       A tokenIndex with rsyntax annotations
#' @param x            The name of the column that will be aggregated (!not the vector itself)
#' @param FUN          The function used to aggregate.
#' @param by           A list or named vector to specify the grouping elements, where names are columns and values are values in the columns. For example, if there are 
#'                     "quotes" and "clauses" columns, that have "source" and "subject" respectively, use c(quotes = "source", clauses = "subject"). 
#' @param id           The name of the column the contains the ids/codes of the groups. By default, each unique code is casted to a subsentence. Alternatively,
#'                     if the id column contains text instead of unique codes, the collapse_id argument can be used to collapse the text into an id.
#' @param collapse_id  see id
#' @param ...          Additional arguments passed to FUN
#'
#' @return
#' @export
#'
#' @examples
cast_tokens_aggregate <- function(tokens, x, FUN, by, id, collapse_id=F, ...){
  out = cast_tokens(tokens, by=by, id=id, collapse_id=collapse_id, columns=x)
  cols = colnames(out)
  cols = setdiff(cols, c('token_id',x))
  out = out[,list(.AGG_COL= FUN(get(x))), by=cols]
  data.table::setnames(out, '.AGG_COL', x)
  out
}

#' Like cast_tokens, but prints for easier inspection
#' 
#' @param tokens       A tokenIndex with rsyntax annotations
#' @param text         THe name of the column with token texts (often token or lemma)
#' @param by           A list or named vector to specify the grouping elements, where names are columns and values are values in the columns. For example, if there are 
#'                     "quotes" and "clauses" columns, that have "source" and "subject" respectively, use c(quotes = "source", clauses = "subject"). 
#' @param id           The name of the column the contains the ids/codes of the groups. By default, each unique code is casted to a subsentence. Alternatively,
#'                     if the id column contains text instead of unique codes, the collapse_id argument can be used to collapse the text into an id.
#' @param collapse_id  see id
#' 
#' @return
#' @export
#'
#' @examples
cast_tokens_text <- function(tokens, text, by, id, collapse_id=F){
  out = cast_tokens(tokens, by=by, id=id, collapse_id=collapse_id, columns=text)
  cols = colnames(out)
  cols = setdiff(cols, c('token_id',text))
  out = out[,list(.TEXT=paste(get(text), collapse=' ')), by=cols]
  data.table::setnames(out, '.TEXT', text)
  out
}



expand_nodes <- function(tokens, column) {
  id_column = paste(column, 'id', sep='_')
  id = stringi::stri_split(tokens[[id_column]], fixed=',')
  group = stringi::stri_split(tokens[[column]], fixed = ',')
  len = sapply(id, length)
  tokens = tokens[rep(1:nrow(tokens),len),]
  tokens[[column]] = unlist(group)
  tokens[[id_column]] = unlist(id)
  tokens
}

get_token_group <- function(tokens, column, group, rows, group_values, mode) {
  if (any(grepl(',', levels(tokens[[column]])))) {
    tokens = expand_nodes(tokens, column)
  }
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
  
  data.table::setnames(ids, 'value', paste(column, paste(group, collapse='_'), sep='.'))
  if (is.null(rows)) {
    out = subset(tokens, subset=!tokens[[column]] %in% group, select = c('doc_id','sentence','token_id',column_id)) 
  }  else {
    out = subset(tokens, subset=tokens[[column]] %in% rows, select = c('doc_id','sentence','token_id',column_id))
  }
  merge(out, ids, by.x = column_id, by.y = 'id', allow.cartesian=T)
}


