#' Create an igraph tree from a sentence
#' 
#' Create an igraph tree from a data frame with token information, e.g, as donwloaded with amcat.get.tokens.
#' This assumes that id, parent, and relation are presente in the data frame. 
#' Any other columns will be available as vertex and edge attributes (edge attributes are taken from the child token).
#' If available, the 'word' column will be used to label the vertices.
#' The data frame should contain a single sentence with a single root node (with no parent).
#' 
#' 
#' @param tokens a data frame of tokens containing id, parent, and relation columns
#' @param sentence an optional sentence to filter the tokens (which should then contain a sentence column)
#' @return an igraph graph
#' @export
graph_from_sentence <- function(tokens, sentence=NULL) {  
  if (!is.null(sentence)) tokens = tokens[tokens$sentence == sentence,]
  # reorder columns and split to edges and nodes, keep only nodes that appear in an edge:
  edges = tokens[!is.na(tokens$parent), c("parent", "id", setdiff(colnames(tokens), c("parent", "id")))]
  nodes = tokens[tokens$id %in% c(edges$parent, edges$id), c("id", setdiff(colnames(tokens), c("id")))]
  # check single root
  root = as.character(setdiff(nodes$id, edges$id))
  if (length(root) == 0) stop("Cannot find root in ", nrow(tokens), " tokens (sentence=",sentence,")")
  if (length(root) > 1) stop("Multiple roots (", length(root), ") in ", nrow(tokens), " tokens (sentence=",sentence,")")
  
  g = graph.data.frame(edges, vertices=nodes, directed = T)
  g$layout = layout.reingold.tilford(g, root=as.character(root))
  if (!is.null(V(g)$lemma)) V(g)$label = V(g)$lemma
  E(g)$label = E(g)$relation
  V(g)$shape = "none"
  return(g)
}

#' Get the text from a tokens data frame 
#' 
#' The data is sorted by aid, sentence, and offset, and the words are pasted together to a single character value.
#' The word.column parameter specifies which column to use for the words.
#' If multiple columns arse given (e.g. lemma and POS) they are pasted together with a slash ("Like/IN this/DT")
#' 
#' @param tokens a data frame of tokens containing id, parent, and relation columns
#' @param word.column the column(s) containing the words to use. 
#' @return the text as a single character value
#' @export
get_text <- function(tokens, word.column="word") {
    words = tokens[order(tokens$aid,tokens$sentence,tokens$offset), word.column, drop=F]
    args = as.list(words)
    args$sep = "/"
    paste(do.call(paste, args), collapse=" ")
}
