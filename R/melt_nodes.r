melt_nodes_list <- function(nodes) {
  ## the nodes created in find_nodes have a weird structure, which is only usefull for efficiently merging data.tables
  ## here we melt the nodes to a more convenient format
  
  only_first_fill = T ## now a constant, but noted here in case there might be reasons not to do this
  
  if (nrow(nodes) == 0 || ncol(nodes) <= 3) return(data.table::data.table())
  nodes_list = list()
  fill_cols = grep('_FILL', colnames(nodes), fixed=T, value=T)
  
  if (length(fill_cols) > 0) {
    fill_level_cols = grep('_FILL_LEVEL', colnames(nodes), value=T)
    measure_vars = setdiff(colnames(nodes), c('doc_id','sentence','.ID', fill_cols))
    fill_measure_vars = setdiff(colnames(nodes), c('doc_id','sentence','.ID', measure_vars, fill_level_cols))
    
    nodes_list[['']] = unique(safe_melt(nodes, id.vars=c('doc_id','sentence','.ID'), measure.vars = measure_vars, variable.name='.ROLE', value.name='token_id', na.rm=T))
    nodes_list[[1]][, .FILL_LEVEL := 0] ## add .FILL_LEVEL column
    
    fill_list = list()
    
    for (flc in fill_level_cols) {
      fc = gsub('\\_LEVEL','',flc) ## get fill column
      fills = safe_melt(nodes, id.vars=c('doc_id','sentence','.ID',flc), measure.vars = fc, variable.name='.ROLE', value.name='token_id', na.rm=T)
      data.table::setnames(fills, flc, '.FILL_LEVEL')
      nodes_list[['']] = unique(fills)
    }
    
    nodes = data.table::rbindlist(nodes_list, fill=T)
  } else {
    nodes = safe_melt(nodes, id.vars=c('doc_id','sentence','.ID'), variable.name='.ROLE', value.name='token_id', na.rm=T)
    nodes[, .FILL_LEVEL := 0] ## add .FILL_LEVEL column
  }
  if (!is.factor(nodes$.ROLE)) nodes.ROLE = as.factor(nodes.ROLE)
  levels(nodes$.ROLE) = gsub('\\_FILL', '', levels(nodes$.ROLE))
  
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
          fill_vars = grep('\\FILL\\_LEVEL', colnames(d), value=T)  ## exclude only level vars from measure vars
        } else {
          fill_vars = grep('\\_FILL', colnames(d), value=T)   ## also exclude fill vars from measure vars
        }
        
        d = safe_melt(d, id.vars = c('doc_id', 'sentence'), 
                      measure.vars=setdiff(colnames(d), c('doc_id','sentence','.ID',fill_vars)),
                      variable.name = '.VARIABLE', value.name='token_id')
        #if ('.VARIABLE' in colnames(d)) d[,.VARIABLE := NULL]
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