melt_nodes_list <- function(nodes, fill_only_first=TRUE) {
  .ROLE = NULL; token_id = NULL; .FILL_LEVEL = NULL
  ## the nodes created in find_nodes have a weird structure, which is only usefull for efficiently merging data.tables
  ## here we melt the nodes to a more convenient format
  
  for (.RM in grep('^BLOCK', colnames(nodes), value=TRUE)) 
    nodes[, (.RM) := NULL]
  
  if (nrow(nodes) == 0 || ncol(nodes) <= 3) return(data.table::data.table())
  nodes_list = list()
  fill_cols = grep('_FILL', colnames(nodes), fixed=TRUE, value=TRUE)
  
  #browser()
  
  
  cols = setdiff(colnames(nodes), c('doc_id','sentence','.ID'))
  cols = setdiff(cols, grep('\\_LEVEL$', cols, value=TRUE))
  level = paste0(cols, '_LEVEL')
  
  missing = setdiff(c(level), colnames(nodes))
  nodes[, (missing) := double()]
  
  nodes = melt(nodes, id.vars=c('doc_id','sentence','.ID'), 
                      measure.vars=list(cols,level), 
                      value.name=c('token_id','.FILL_LEVEL'),
                      variable.name='.ROLE', variable.factor=TRUE)
  nodes[, .ROLE := factor(.ROLE, labels=cols)]
  nodes = subset(nodes, !is.na(token_id))
  
  ## remove duplicate label name tags (#2, etc)
  data.table::setattr(nodes$.ROLE, 'levels', gsub('#.*', '', levels(nodes$.ROLE)))
  
  ## fill should be 0 if not fill (NA), and _FILL should be removed from .ROLE 
  nodes[is.na(nodes$.FILL_LEVEL), .FILL_LEVEL := 0]
  data.table::setattr(nodes$.ROLE, 'levels', gsub('\\_FILL', '', levels(nodes$.ROLE)))

  if (fill_only_first) {
    data.table::setorder(nodes, '.FILL_LEVEL')
    nodes = unique(nodes, by=c('doc_id','sentence','token_id'))
  } else {
    nodes = unique(nodes, by=c('doc_id','sentence','token_id','.ROLE','.ID'))
  }
  nodes
}

#' Get ids in various forms to extract token_ids
#'
#' @param ... Either a data.table with the columns doc_id, sentence and token_id, or the output of \link{apply_queries}
#' @param select    If not null, a character vector for selecting column names
#' @param with_fill If TRUE, include the ids of the fill nodes
#'
#' @return A data.table with the columns doc_id, sentence and token_id
#' @export
get_long_ids <- function(..., select=NULL, with_fill=FALSE) {
  l = list(...)
  
  len = length(l)
  out = vector('list', len)
  for (i in 1:len) {
    d = l[[i]]
    if (is.null(d)) next
    if (methods::is(d, 'data.table')) {
      if (!'token_id' %in% colnames(d)) {
        if (!is.null(select)) {
          select = setdiff(select, '.TQUERY')
          d = subset(d, select = colnames(d) %in% union(c('doc_id', 'sentence'), select))
        } else {
          d = subset(d, select = colnames(d) %in% setdiff(colnames(d), '.TQUERY'))
        }
        
        if (with_fill) {
          ignore_vars = grep('\\FILL\\_LEVEL', colnames(d), value=TRUE)  ## exclude only level vars from measure vars
        } else {
          ignore_vars = grep('\\_FILL', colnames(d), value=TRUE)   ## also exclude fill vars from measure vars
        }
        ignore_vars = union(ignore_vars, grep('\\_PARENT', colnames(d), value=TRUE))

        d = melt(d, id.vars = c('doc_id', 'sentence'), 
                    measure.vars=setdiff(colnames(d), c('doc_id','sentence','.ID',ignore_vars)),
                    variable.name = '.VARIABLE', value.name='token_id')
      } else {
        if (!with_fill && '.FILL_LEVEL' %in% colnames(d)) 
          d = d[d$.FILL_LEVEL == 0,,drop=FALSE]
      } 
      if (!'token_id' %in% colnames(d)) next
      out[[i]] = d[,c('doc_id','sentence','token_id')]
      next
    }
    if (methods::is(d, 'list')) {
      out[[i]] = get_long_ids(d)
    }
    stop('Not a valid input for get_long_ids')
  }
  out = unique(data.table::rbindlist(out))
  if (ncol(out) == 0) NULL else out
}