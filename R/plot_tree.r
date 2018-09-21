get_sentence <- function(tokens, .DOC_ID=NULL, .SENTENCE=NULL, sentence_i=1) {
  if (!length(sentence_i) == 1) stop('Can only select one sentence_i') 
  if (!is.null(.DOC_ID)) {
    if (!length(.DOC_ID) == 1) stop('Can only select one doc_id') 
    sent = tokens[list(.DOC_ID), on='doc_id', nomatch=0]
    if (nrow(sent) == 0) return(sent)
    if (is.null(.SENTENCE)) {
      sentences = unique(sent[['sentence']])
      if (length(sentences) < sentence_i) stop(sprintf('Cannot select sentence_i = %s, only %s sentences available', sentence_i, length(sentences)))
      .SENTENCE = sentences[sentence_i]
    }
    if (!length(.SENTENCE) == 1) stop('Can only select one sentence') 
    sent = sent[list(.SENTENCE), on='sentence', nomatch=0]
  } else {
    if (!is.null(.SENTENCE)) stop('Cannot specificy "sentence" without specifying "doc_id"')
    .DOC_SENT = unique(subset(tokens, select = c('doc_id','sentence')))
    if (nrow(.DOC_SENT) < sentence_i) stop(sprintf('Cannot select sentence_i = %s, only %s sentences available', sentence_i, nrow(.DOC_SENT)))
    .DOC_SENT = .DOC_SENT[sentence_i,]
    sent = tokens[.DOC_SENT, on=c('doc_id','sentence'), nomatch=0]
  }
  sent
}


#' Create an igraph tree from a sentence
#' 
#' Create an igraph tree from a token_index (\link{as_tokenindex}) or a data.frame that can be coerced to a tokenindex.
#' 
#' @param tokens      A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}.
#' @param text        A character vector with column names, to be printed as text and labels (e.g., c('token','lemma','POS'))
#' @param sentence_i  By default, plot_tree uses the first sentence (sentence_i = 1) in the data. sentence_i can be changed to select other sentences by position (the i-th unique sentence in the data). Note that sentence_i does not refer to the values in the sentence column (for this use the sentence argument together with doc_id)
#' @param doc_id      Optionally, the document id can be specified. If so, sentence_i refers to the i-th sentence within the given document. 
#' @param sentence    Optionally, the sentence id can be specified (note that sentence_i refers to the position). If sentence is given, doc_id has to be given as well. 
#' @param ignore_rel  Optionally, a character vector with relation names that will not be shown in the tree
#' @param all_lower   If TRUE (default) make all text lowercase
#' @param bypass      Optionally, a character vector specifying one or multiple relations. If a node has this relation to its parent, it will bypass the parent by adopting the parent's parent id and relation. Using bypass and link_children makes it possible to reshape the tree to deal with the pesky recursive nature of language.
#' @param link_children Optionally, a character vector specifying one or multiple relations. If bypass is used, children of the parent nodes with this relation will adopted. 
#' @param textsize    A number to manually change the textsize. The function tries to set a suitable textsize for the plotting device, but if this goes wrong and now everything is broken and sad, you can multiply the textsize with the given number. 
#' @param abbrev      Optionally, a number for abbreviating text, with the number being the target number of characters. 
#' @param spacing     A number for scaling the distance between words (between 0 and infinity) 
#' @param use_color   If true, use colors
#' @param max_curve   A number for controlling the allowed amount of curve in the edges. 
#' @param palette     A function for creating a vector of n contiguous colors. See ?terrain.colors for standard functions and documentation
#'   
#' @return an igraph graph
#' @export
plot_tree <-function(tokens, text=c('token_id'), sentence_i=1, doc_id=NULL, sentence=NULL, ignore_rel=NULL, all_lower=T, bypass=NULL, link_children=NULL, textsize=1, abbrev=NULL, spacing=1, use_color=T, max_curve=0.3, palette=terrain.colors) {  
  tokens = as_tokenindex(tokens)  
  
  nodes = get_sentence(tokens, doc_id, sentence, sentence_i)
  if (!is.null(bypass)) {
    nodes = unpack_conjunctions(nodes, bypass, link_children = link_children)
  }
  
  data.table::setcolorder(nodes, union('token_id', colnames(nodes))) ## set token_id first for matching with edges
  
  # reorder columns and split to edges and nodes, keep only nodes that appear in an edge:
  edges = nodes[!is.na(nodes[['parent']]), c('parent', 'token_id', 'relation'), with=F]
  edges = edges[!edges$relation %in% bypass,]
  
  text_cols = text
  text = NULL
  for (tc in text_cols) {
    textval = if (!is.null(abbrev)) abbreviate(nodes[[tc]], minlength = abbrev) else nodes[[tc]]
    text = if (is.null(text)) textval else paste(text, textval, sep='\n')
  }
  nodes$label = if (!is.null(abbrev)) abbreviate(nodes[['relation']], abbrev) else nodes[['relation']]
  if (!is.null(ignore_rel)) nodes$label[nodes$label %in% ignore_rel] = ''
  
  if (all_lower) {
    text = tolower(text)
    nodes$label = tolower(nodes$label)
  }
  
  g = igraph::graph.data.frame(edges, vertices=nodes, directed = T)

  root = find_roots(g)
  g$layout = igraph::layout_as_tree(g, root = root)
  if (!is.null(ignore_rel)) g = delete.edges(g, which(get.edge.attribute(g, 'relation') %in% ignore_rel))
  co = g$layout ## vertex coordinates
  
  ## arrange horizontal positions
  textwidth = strwidth(text)
  relwidth = strwidth(V(g)$label)
  width = ifelse(textwidth > relwidth, textwidth, relwidth)
  co[,1] = 1:nrow(co) / (nrow(co) / 2) - 1
  right_allign = cumsum(width)
  left_allign = c(0,right_allign[-length(right_allign)])
  middle = right_allign + left_allign / 2
  co[,1] = rescale_var(middle, new_min = -1, new_max = 1)
  
  ## format edges
  e = get.edges(g, E(g))
  vdist = (e[,2] - e[,1])
  maxcurve = 1 / (1 + exp(-max(abs(vdist))*0.05))
  maxcurve = min(maxcurve, max_curve) # max_curve, with underscore, is a parameter
  curve = rescale_var(abs(vdist)^2, 0, maxcurve) * sign(vdist)
  E(g)$curved = curve
  E(g)$arrow.size = 1
  E(g)$color = 'darkgrey'

  
  ## arrange vertical positions
  levels = max(co[,2]) - min(co[,2])
  maxheight = if (levels > 10) 1 else -0.5 + levels*0.15
  co[,2] = rescale_var(co[,2], new_min = -0.5, new_max = maxheight)
  #co[,2] = co[,2] + 1
  E(g)$arrow.size=0.3
  E(g)$arrow.mode=1
  
  ## make empty plot to get positions in current plot device
  par(mar=c(0,0,0,0))
  plot(0, type="n", ann=FALSE, axes=FALSE, xlim=extendrange(co[,1]),ylim=extendrange(c(-1,1)))
  
  need_width = strwidth(paste(V(g)$label, collapse=paste(rep(' ', 5*spacing), collapse='')), units='inches')
  max_width = dev.size(units = 'in')[1]
  cex = if (max_width < need_width) max_width / need_width else 1
  cex = textsize * cex
    
  width = (strwidth(V(g)$label, cex=cex) + strwidth('', cex=cex)) * 100
  height = strheight('I', cex=cex) * 1.2 * 100
  
  V(g)$label.cex = cex
  V(g)$label.color = 'black'
  V(g)$shape = 'rectangle'
  V(g)$size = width
  V(g)$size2 = height
  V(g)$color = 'white'
  V(g)$border.color = 'white'
  V(g)$frame.color = 'white'
  V(g)$label.font=2

  drop = if (is.null(ignore_rel)) rep(F, vcount(g)) else V(g)$relation %in% ignore_rel
  V(g)$size[drop] = 0
  V(g)$size2[drop] = 0
  V(g)$label[drop] = ''

  hl = if ('.CONJ_LEVEL' %in% vertex.attributes(g)) !is.na(V(g)$.CONJ_LEVEL) else rep(F, vcount(g))
  if (use_color) {
    V(g)$color = festival(V(g)$label, palette)
    E(g)$color = V(g)$color[e[,2]]
    V(g)$frame.color[hl] = 'red'
    E(g)$lty = ifelse(hl[e[,2]], 2, 1)
  } else {
    V(g)$frame.color[hl] =  'grey'
    V(g)$color[hl] =  'grey'
    E(g)$lty = ifelse(hl[e[,2]], 2, 1)
  }
  
    
  plot(g, layout=co, rescale=FALSE, add=TRUE)
  ## add text and lines
  if ('.ADDED' %in% vertex.attributes(g)) {
    col = ifelse(V(g)$.ADDED, ifelse(use_color, 'red', 'darkgrey'),'black')
  } else col = 'black'
  text(co[,1], min(co[,2]), labels=text, 
       col = col, 
       cex=cex, adj=c(0.5,1.5))
  segments(co[,1], min(co[,2]), co[,1], co[,2]-0.05, lwd = ifelse(drop, NA, 0.5), lty=2, col='grey')
}

festival <- function(labels, palette=palette){
  pal = palette(10000)
  color = NA
  for (label in labels) {
    if (label == '') next
    labelint = sum(utf8ToInt(label)^(nchar(label)))
    labelint = labelint %% 10000
    mean(utf8ToInt(paste(c(letters,LETTERS), collapse='')))
    color[labels == label] = pal[labelint]
  }
  color
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



rescale_var <- function(x, new_min=0, new_max=1, x_min=min(x), x_max=max(x)){
  if (x_min == x_max) return(x)
  x = (x - x_min) / (x_max - x_min) # normalize
  x = x * (new_max-new_min)
  return(x + new_min)
}

#plot_tree(tokens, text=c('token','lemma','pos'))

#plot_tree(tokens, text=c('token','lemma','pos'), bypass='conj', link_children = 'nsubj')


function() {
  tokens = spacy_parse('"Kenny said: "Steve and Patrick hit John, kissed Mary, and were kicked by Bob."', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, text=c('token_id','token'))
  plot_tree(tokens, text=c('token_id','token'), bypass='conj', link_children='nsubj')
  
  
  tokens = spacy_parse('"Kenny said: "Steve and Patrick hit John, and Bob kissed Mary and punched Ben."', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, text=c('token_id','token'))
  plot_tree(tokens, text=c('token_id','token'), bypass='conj', link_children = 'nsubj')
  plot_tree(tokens, text=c('token_id','token'), bypass='conj', link_children = 'nsubj', ignore_rel = 'punct')
  
  tokens = spacy_parse('Steve and Patrick were kicked by Bob.', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, text=c('token_id','token'))
  plot_tree(tokens, text=c('token','lemma','pos'), bypass='conj', link_children = 'nsubj')
  
  
  tokens = spacy_parse('Charges would have been filed by the woman, Christine Blasey Ford, or her parents.', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, text=c('token_id','token'))
  plot_tree(tokens, text=c('token_id','token','pos'), bypass='conj', link_children = 'nsubj', ignore_rel = 'punct')
  tokens
  
  tokens = spacy_parse('Steve said that Bob loved Mary and John', dependency=T)
  tokens = as_tokenindex(tokens, sentence='sentence_id', parent='head_token_id', relation='dep_rel')
  plot_tree(tokens, text=c('token_id','token'))
  plot_tree(tokens, text=c('token_id','token'), bypass='conj', link_children=c('nsubj'))
  
  
  plot_tree(tokens, text='token', labels='POS')
  
  tokens = tokens2
  tokens = as_tokenindex(tokens_corenlp)
  tokens = tokens[7:14,]
  plot_tree(tokens, text='token', labels = c('POS'))
  
  tokens
  
  text = tokens$token
  width = strwidth(paste0(text, '  '))
  if (sum(width) > 1) {
    cex = 1 / sum(width) 
    maxwidth = 1
  } else {
    cex = 1 
    maxwidth = sum(width)
  }
  
  cumwidth = cumsum(width)
  cumwidth = rescale_var(cumwidth, new_max = maxwidth)
  startpos = c(0, cumwidth[-length(cumwidth)])
  centerpos = (startpos + cumwidth) / 2
  
  d = data.frame(text = text,
                 startpos = startpos,
                 centerpos = centerpos)
   
  
  plot.new()
  for(i in 1:length(text)) {
    text(startpos[i], 0, text[i], cex=cex, adj=c(0,0))
  }
  
  plot.new()
  ?text
  text(0,0,'test', adj=c(0,0), family='mono')
  text(0.1,0,'test', adj=c(0,0))
  text(0.2,0,'John', adj=c(0,0))
  text(0.3,0,'testing', adj=c(0,0))
  text(0.4,0,'WHAT', adj=c(0,0))
  text(0.5,0,'test', adj=c(0,0))
  

  ?text()
  strwidth('testing', font = 'monospace')
  text(0,0,'test testint twwe')
  text(0,0,'test')
  
  
  cumlen
  plot.new()
  text(0,0,'test')
  
  plot(0:0, -1:1, type = "n", xlab = "Re", ylab = "Im")
  text('test')
  label = vertex.attributes(g)
  g$layout
}
