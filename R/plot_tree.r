get_sentence <- function(tokens, .DOC_ID=NULL, .SENTENCE=NULL, sentence_i=1) {
  if (!length(sentence_i) == 1) stop('Can only select one sentence_i') 
  if (!is.null(.DOC_ID)) {
    if (!length(.DOC_ID) == 1) stop('Can only select one doc_id') 
    sent = tokens[list(.DOC_ID), on=cname('doc_id'), nomatch=0]
    if (nrow(sent) == 0) return(sent)
    if (is.null(.SENTENCE)) {
      sentences = unique(sent[[cname('sentence')]])
      if (length(sentences) < sentence_i) stop(sprintf('Cannot select sentence_i = %s, only %s sentences available', sentence_i, length(sentences)))
      .SENTENCE = sentences[sentence_i]
    }
    if (!length(.SENTENCE) == 1) stop('Can only select one sentence') 
    sent = sent[list(.SENTENCE), on=cname('sentence'), nomatch=0]
  } else {
    if (!is.null(.SENTENCE)) stop('Cannot specificy "sentence" without specifying "doc_id"')
    .DOC_SENT = unique(subset(tokens, select = cname('doc_id','sentence')))
    if (nrow(.DOC_SENT) < sentence_i) stop(sprintf('Cannot select sentence_i = %s, only %s sentences available', sentence_i, nrow(.DOC_SENT)))
    .DOC_SENT = .DOC_SENT[sentence_i,]
    sent = tokens[.DOC_SENT, on=cname('doc_id','sentence'), nomatch=0]
  }
  sent
}


#' Create an igraph tree from a sentence
#' 
#' Create an igraph tree from a token_index (\link{as_tokenindex}) or a data.frame that can be coerced to a tokenindex.
#' 
#' @param tokens      A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param sentence_i  By default, plot_tree uses the first sentence (sentence_i = 1) in the data. sentence_i can be changed to select other sentences by position (the i-th unique sentence in the data). Note that sentence_i does not refer to the values in the sentence column (for this use the sentence argument together with doc_id)
#' @param doc_id      Optionally, the document id can be specified. If so, sentence_i refers to the i-th sentence within the given document. 
#' @param sentence    Optionally, the sentence id can be specified (note that sentence_i refers to the position). If sentence is given, doc_id has to be given as well. 
#' @param label_var   The name of the column with the token label (word or lemma). Will be ignored if column is not available.
#' @param pos_var     The name of the column with the part-of-speech tag. Will be ignored if column is not available.
#' @param quote_var   The name of the column with quote annotations. Will be ignored if column is not available.
#' @param clause_var  The name of the column with quote annotations. Will be ignored if column is not available.
#' 
#' @return an igraph graph
#' @export
plot_tree <-function(tokens, sentence_i=1, doc_id=NULL, sentence=NULL, quote_var='quote', clause_var='clause', coref='coref_id', label_size=0.9, node_size=30, edge_label_size=0.8) {  
  tokens = as_tokenindex(tokens)  
  if (!quote_var %in% colnames(tokens)) quote_var = NULL
  if (!clause_var %in% colnames(tokens)) clause_var = NULL
  
  nodes = get_sentence(tokens, doc_id, sentence, sentence_i)
  data.table::setcolorder(nodes, union(cname('token_id'), colnames(nodes))) ## set token_id first for matching with edges
  
  # reorder columns and split to edges and nodes, keep only nodes that appear in an edge:
  edges = nodes[!is.na(nodes[[cname('parent')]]), cname('parent', 'token_id', 'relation'), with=F]
  
  label = nodes[[cname('token_id')]]
  if (coref %in% colnames(tokens)) {
    has_coref = !is.na(nodes[[coref]]) & !nodes[[coref]] == ''
    label = ifelse(has_coref, paste0(label, ' - ', nodes[[coref]]), label)
  }
  label = paste0(label, '\n', nodes[[cname('lemma')]])
  label = paste0(label, '\n', '(', nodes[[cname('POS')]])
  label = paste0(label, ')')
  
  nodes$label = label 
  
  g = igraph::graph.data.frame(edges, vertices=nodes, directed = T)
  root = nodes[[cname('token_id')]][is.na(nodes[['parent']])]
  
  plot.new()
  par(mar=c(0,0,0,0))

  root = find_roots(g)
  g$layout = igraph::layout_as_tree(g, root = root)

  ## make childen line out in circle, preventing (most) label overlap
  ei = get.edgelist(g, names = F)
  parent.x = g$layout[ei[match(1:nrow(g$layout), ei[,2]),1],1]
  parent.x[is.na(parent.x)] = 0
  dif = abs(parent.x - g$layout[,1])
  dif = (dif - min(dif)) / (max(dif) - min(dif))
  g$layout[,2] = g$layout[,2] + (0.5*label_size*dif)
  
  ## adjust size based on width    
  lsize = strwidth(V(g)$label, cex=label_size)
  lsize[lsize < (0.5*label_size)] = 0
  igraph::V(g)$label.cex = label_size - (lsize^2.2)
  
  # style defaults
  igraph::E(g)$label = igraph::get.edge.attribute(g, cname('relation'))
  igraph::E(g)$label.cex= edge_label_size
  igraph::E(g)$color = 'grey'
  igraph::E(g)$label.color = 'blue'
  igraph::E(g)$arrow.size=.3
  
  igraph::V(g)$label.color = 'black'
  igraph::V(g)$size = node_size
  igraph::V(g)$size2 = node_size*0.66
  igraph::V(g)$color = "white"
  igraph::V(g)$shape = 'none'
  igraph::V(g)$frame.size=20
  
  quote = igraph::get.vertex.attribute(g, quote_var)
  if (!is.null(quote)) {
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

  clause = igraph::get.vertex.attribute(g, clause_var)
  if (!is.null(clause)) {
    is_subject = clause == 'subject' & !is.na(clause)
    is_predicate = clause == 'predicate' & !is.na(clause)
    V(g)$shape[is_subject] = 'rectangle'
    V(g)$shape[is_predicate] = 'circle'
    V(g)$color[is_subject] = 'lightblue1'
    V(g)$color[is_predicate] = 'lightblue3'
  }
  plot(g)
  par(mar=c(4,4,4,4))
  invisible(g)
}

function() {
  tokens = as_tokenindex(tokens)
  tokens = as_tokenindex(tokens_dutchquotes)
  tokens = annotate_alpino(tokens)
  g =  plot_tree(tokens, label_vars=c('token'), sentence_i = 2)
}

find_roots <- function(g) {
  comps = igraph::decompose(g)
  roots = c()
  for (i in 1:length(comps)) {
    comp = comps[[i]]
    root = names(which.min(igraph::degree(comp, mode = 'in')))
    if (length(root) > 1) {
      out = igraph::degree(comp, mode='out')
      out = out[match(root, names(out))]
      root = names(out)[which.max(out)]
    } 
    if (length(root) > 1) {
      root = root[1]
    }
    roots = union(roots, root)
  }
  roots
}
