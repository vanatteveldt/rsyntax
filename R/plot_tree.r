get_children_i <- function(tokens, i) {
  tokens = as_tokenindex(tokens)
  select = tokens[i,cname('doc_id','token_id'), with=F]
  data.table::setnames(select, c('doc_id','parent'))
  children = tokens[select, on=cname('doc_id','parent'), nomatch=0, which=T]
  if (length(children) > 0) children = union(children, get_children_i(tokens, children)) 
  union(i, children)
}

#' Create an igraph tree from a sentence
#' 
#' Create an igraph tree from a token_index (\link{as_tokenindex}) or a data.frame that can be coerced to a tokenindex.
#' 
#' @param tokens  A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param root_i By default, plot_tree uses the first root (i.e. token without parent) in the data. Change root_i to use the second (2), third (3), etc.
#' @param label_vars A character vector, specifying the columns that will be used to label the nodes
#' @param quote_var The name of the column with quote annotations (will be ignored if column is not available)
#' @param clause_var The name of the column with quote annotations (will be ignored if column is not available)
#' 
#' @return an igraph graph
#' @export
plot_tree <-function(tokens, root_i=1, label_vars = c('token'), quote_var='quote', clause_var='clause') {  
  tokens = as_tokenindex(tokens)  
  
  if (length(label_vars) < 1) label_vars = cname('token_id')
  missing_vars = setdiff(label_vars, colnames(tokens))
  if (length(missing_vars) > 0) stop(sprintf('label_vars are not column names: (%s)', paste(missing_vars, collapse=', ')))
  
  roots = which(is.na(tokens[[cname('parent')]]))
  if (root_i > length(roots)) stop(sprintf('Cannot select root_i %s: only %s roots in data', root_i, length(roots)))
  tree = tokens[get_children_i(tokens, roots[root_i]),]
  
  # reorder columns and split to edges and nodes, keep only nodes that appear in an edge:
  edges = tree[!is.na(tree[[cname('parent')]]), cname('parent', 'token_id', 'relation'), with=F]
  
  if (!quote_var %in% colnames(tokens)) quote_var = NULL
  if (!clause_var %in% colnames(tokens)) clause_var = NULL
  nodes = subset(tree, select = c(cname('token_id'),label_vars,quote_var,clause_var))
  nodes$label = apply(nodes[,label_vars,with=F], 1, paste, collapse='\n')      
  
  g = igraph::graph.data.frame(edges, vertices=nodes, directed = T)
  root = tree[[cname('token_id')]][is.na(tree[['parent']])]
  g$layout = igraph::layout_as_tree(g)
  
  
  ## make childen line out in circle, preventing (most) label overlap
  ei = get.edgelist(g, names = F)
  parent.x = g$layout[ei[match(1:nrow(g$layout), ei[,2]),1],1]
  parent.x[is.na(parent.x)] = 0
  dif = abs(parent.x - g$layout[,1])
  dif = (dif - min(dif)) / (max(dif) - min(dif))
  g$layout[,2] = g$layout[,2] + (0.4*dif)
  
  ## adjust size based on width    
  lsize = strwidth(V(g)$label, cex=1)
  lsize[lsize < 0.4] = 0
  igraph::V(g)$label.cex = 1 - (lsize^2.2)
  
  # style defaults
  igraph::E(g)$label = igraph::get.edge.attribute(g, cname('relation'))
  igraph::E(g)$label.cex=.7
  igraph::E(g)$color = 'grey'
  igraph::E(g)$label.color = 'blue'
  igraph::E(g)$arrow.size=.3
  
  igraph::V(g)$label.color = 'black'
  igraph::V(g)$size = 20
  igraph::V(g)$color = "white"
  igraph::V(g)$shape = 'none'
  igraph::V(g)$frame.size=20
  
  if (!is.null(quote_var)) {
    quote = igraph::get.vertex.attribute(g, quote_var)
    is_source = quote == 'source' & !is.na(quote)
    is_quote = quote == 'quote' & !is.na(quote)
    V(g)$shape[is_source] = 'rectangle'
    V(g)$shape[is_quote] = 'circle'
    V(g)$frame.color[is_source | is_quote] = 'tomato'
    V(g)$color[is_source] = 'tomato1'
    V(g)$color[is_quote] = 'tomato3'
    
  } else {
    is_source = rep(F, vcount(g))
    is_quote = rep(F, vcount(g))
  }
  
  if (!is.null(clause_var)) {
    clause = igraph::get.vertex.attribute(g, clause_var)
    is_subject = clause == 'subject' & !is.na(clause)
    is_predicate = clause == 'predicate' & !is.na(clause)
    V(g)$shape[is_subject] = 'rectangle'
    V(g)$shape[is_predicate] = 'circle'
    V(g)$color[is_subject] = 'lightblue1'
    V(g)$color[is_predicate] = 'lightblue3'
    
  }
  
  par(mar=c(0,0,0,0))
  plot(g)
  par(mar=c(4,4,4,4))
  invisible(g)
}




function() {
  tokens = as_tokenindex(tokens_dutchquotes)
  tokens = annotate_alpino(tokens)
  g =  plot_tree(tokens, label_vars=c('token'), root_i = 2)
}

