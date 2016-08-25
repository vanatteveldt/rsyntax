#' Create an igraph tree from a sentence
#' 
#' Create an igraph tree from a data frame with token information, e.g, as donwloaded with amcat.get.tokens.
#' This assumes that id, parent, and relation are presente in the data frame. 
#' Any other columns will be available as vertex and edge attributes (edge attributes are taken from the child token).
#' If available, the 'word' column will be used to label the vertices.
#' The data frame should contain a single sentence with a single root node (with no parent).
#' 
#' 
#' @param tokens a data frame of tokens containing id, parent, and relation columns. If there are also quote and clause columns
#' (as output by annotate_tokens), the graph will be annotated with these relations. 
#' @return an igraph graph
#' @export
graph_from_sentence <-function(tokens) {  
    # reorder columns and split to edges and nodes, keep only nodes that appear in an edge:
    edges = tokens[!is.na(tokens$parent), c("parent", "id", setdiff(colnames(tokens), c("parent", "id")))]
    nodes = tokens[tokens$id %in% c(edges$parent, edges$id), c("id", setdiff(colnames(tokens), c("id")))]
    # check single root
    root = as.character(setdiff(nodes$id, edges$id))

    if (length(root) == 0) stop("Cannot find root in ", nrow(tokens), " tokens (sentence=",sentence,")")
    if (length(root) > 1) stop("Multiple roots (", length(root), ") in ", nrow(tokens), " tokens (sentence=",sentence,")")
    
    g = graph.data.frame(edges, vertices=nodes, directed = T)
    g$layout = layout.reingold.tilford(g, root=as.character(root))
    if (!is.null(V(g)$lemma)) V(g)$label = paste(V(g)$name, paste(V(g)$lemma, V(g)$pos1), sep="\n")
    E(g)$label = E(g)$relation
    
    
    # style defaults
    E(g)$label.cex=.7
    V(g)$label.cex=.7
    V(g)$size = 20
    E(g)$arrow.size=.3
    E(g)$arrow.color="#333333"
    V(g)$color = "white"
    V(g)$shape = "none"
    
    # process quotes/clauses  
    if (!is.null(tokens$quote_id)) {
    quote_ids = unique(na.omit(tokens$quote_id))
    colors =rainbow(length(quote_ids))
    for (i in seq_along(quote_ids)) {
      qtokens = tokens$id[!is.na(tokens$quote_id) & tokens$quote_id == quote_ids[i]]
      srctokens = intersect(qtokens, tokens$id[tokens$quote_role == "source"])
      V(g)$shape[V(g)$name %in% qtokens] = "rectangle"
      V(g)$frame.color[V(g)$name %in% qtokens] =colors[i]
      V(g)$color[V(g)$name %in% srctokens] = colors[i]
      }
    }  
    
    if (!is.null(tokens$clause_id)) {
      clause_ids = unique(na.omit(tokens$clause_id))
      colors =rainbow_hcl(length(clause_ids), s=.2)
      for (i in seq_along(clause_ids)) {
        ctokens = tokens$id[!is.na(tokens$clause_id) & tokens$clause_id == clause_ids[i]]
        subjtokens = intersect(ctokens, tokens$id[tokens$clause_role == "subject"])
        V(g)$color[V(g)$name %in% ctokens] = colors[i]  
        V(g)$shape[V(g)$name %in% ctokens] = "rectangle"
        V(g)$shape[V(g)$name %in% subjtokens] = "circle"    
      }
      
    }
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
