melt_nodes_list <- function(nodes, only_first_fill=T) {
  ## the nodes created in find_nodes have a weird structure, which is only usefull for efficiently merging data.tables
  ## here we melt the nodes to a more convenient format
  
  for (.RM in grep('^BLOCK', colnames(nodes), value=T)) 
    nodes[, (.RM) := NULL]
  
  if (nrow(nodes) == 0 || ncol(nodes) <= 3) return(data.table::data.table())
  nodes_list = list()
  fill_cols = grep('_FILL', colnames(nodes), fixed=T, value=T)
  
  #browser()
  
  cols = setdiff(colnames(nodes), c('doc_id','sentence','.ID'))
  cols = setdiff(cols, grep('\\_LEVEL$', cols, value=T))
  level = paste0(cols, '_LEVEL')
  
  missing = setdiff(c(level), colnames(nodes))
  nodes[, (missing) := double()]
  
  nodes = melt(nodes, id.vars=c('doc_id','sentence','.ID'), 
                      measure.vars=list(cols,level), 
                      value.name=c('token_id','.FILL_LEVEL'),
                      variable.name='.ROLE', variable.factor=T)
  nodes[, .ROLE := factor(.ROLE, labels=cols)]
  nodes = subset(nodes, !is.na(token_id))
  
  ## remove duplicate save name tags (#2, etc)
  data.table::setattr(nodes$.ROLE, 'levels', gsub('#.*', '', levels(nodes$.ROLE)))
  
  ## fill should be 0 if not fill (NA), and _FILL should be removed from .ROLE 
  nodes[is.na(nodes$.FILL_LEVEL), .FILL_LEVEL := 0]
  data.table::setattr(nodes$.ROLE, 'levels', gsub('\\_FILL', '', levels(nodes$.ROLE)))


  if (only_first_fill) {
    data.table::setorder(nodes, '.FILL_LEVEL')
    nodes = unique(nodes, by=c('doc_id','sentence','token_id','.ID'))
  }
  nodes
}

#' Get ids in various forms to extract token_ids
#'
#' @param ... Either a data.table with the columns doc_id, sentence and token_id, or the output of \link{find_nodes}
#' @param with_fill If TRUE, include the ids of the fill nodes
#' @param with_parents if TRUE, include the ids of the parent nodes
#'
#' @return A data.table with the columns doc_id, sentence and token_id
#' @export
get_long_ids <- function(..., names=NULL, with_fill=F) {
  l = list(...)
  
  len = length(l)
  out = vector('list', len)
  for (i in 1:len) {
    d = l[[i]]
    if (is.null(d)) next
    if (is(d, 'data.table')) {
      if (!'token_id' %in% colnames(d)) {
        if (!is.null(names)) {
          names = setdiff(names, '.TQUERY')
          d = subset(d, select = colnames(d) %in% union('doc_id', 'sentence', names))
        } else {
          d = subset(d, select = colnames(d) %in% setdiff(colnames(d), '.TQUERY'))
        }
        
        if (with_fill) {
          ignore_vars = grep('\\FILL\\_LEVEL', colnames(d), value=T)  ## exclude only level vars from measure vars
        } else {
          ignore_vars = grep('\\_FILL', colnames(d), value=T)   ## also exclude fill vars from measure vars
        }
        ignore_vars = union(ignore_vars, grep('\\_PARENT', colnames(d), value=T))

        d = melt(d, id.vars = c('doc_id', 'sentence'), 
                    measure.vars=setdiff(colnames(d), c('doc_id','sentence','.ID',ignore_vars)),
                    variable.name = '.VARIABLE', value.name='token_id')
      } else {
        if (!with_fill && '.FILL_LEVEL' %in% colnames(d)) 
          d = d[d$.FILL_LEVEL == 0,,drop=F]
      } 
      if (!'token_id' %in% colnames(d)) next
      out[[i]] = d[,c('doc_id','sentence','token_id')]
      next
    }
    if (is(d, 'list')) {
      out[[i]] = get_long_ids(d)
    }
    stop('Not a valid input for get_long_ids')
  }
  out = unique(data.table::rbindlist(out))
  if (ncol(out) == 0) NULL else out
}