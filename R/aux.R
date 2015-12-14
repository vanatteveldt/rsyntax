
#' Fill out the ids to all their descendants, until they hit one of the ids in the block list
#' @param id a vector of ids to fill out
#' @param tokens a data frame of tokens containing child and id columns
#' @return a data frame with the original ids and all descendants
fill <- function(id, tokens, block=unique(id)) {
  parents = na.omit(data.frame(parent=tokens$parent, child=tokens$id))
  parents = unique(rbind(parents, data.frame(parent=tokens$id, child=tokens$id)))
  f = unique(data.frame(id=id, filled=id))  
  while (T) {
    f2 = merge(f, parents, by.x="filled", by.y="parent")
    f2 = unique(data.frame(id=f2$id, filled=f2$child))
    f2 = f2[f2$id == f2$filled | !(f2$filled %in% block),]
    if (nrow(f) == nrow(f2)) return(f2)
    f = f2
  }
}   

#' Make tokens ids globally unique (and update parents accordingly)
#' @param tokens a data frame of tokens
#' @return the data frame with id and parent made globally unique
#' @export
unique_ids <- function(tokens) {
  ids = interaction(tokens$aid, tokens$id) # aid.id
  parents = interaction(tokens$aid, tokens$parent) # aid.parent
  
  tokens$id = match(ids, na.omit(unique(ids))) # generate new unique id for each aid.id
  tokens$parent = tokens$id[match(parents, ids)] # match aid.parent with aid.id and get new id
  tokens
}

#' get all child-parent pairs with the given relation
#' @param tokens a df of tokens
#' @param rel the relation of child with parent (optional)
#' @param rename optionally rename the output column, defaults to re
#' @param ... other filters
#' @return a df with the ids of (parent) id and child (id)
get_children <- function(tokens, relation=NULL, rename=relation, ...) {
  filters = c(list(...), if (is.null(relation)) NULL else list(relation=relation))
  tokens = do.call(function(...) find_nodes(tokens, ..., columns="parent"), filters)
  #for (name in names(filters))
  #  tokens = tokens[tokens[[name]] == filters[[name]], ]
  tokens = data.frame(id=tokens$parent, child=tokens$id)
  if (!is.null(rename)) colnames(tokens)[2] = rename
  tokens
}

#' Search for nodes matching specific criteria
#' 
#' You can search on node attributes (lemma, pos) and on attributes of children.
#' The function will return a data frame with the found nodes and a column for each matched child relation.
#' The node attributes allow __i (e.g. lemma_i="bush") to match case insensitive, 
#' and __in (e.g. id__in=ids) to match multiple values.
#' The child attributes can be either a single relation name, or a list of node attributes.
#' The return column for children is named after the explicit rename= attribute, defaulting to the relation pattern, if given.
#' 
#' @param tokens a df of tokens
#' @param ... node attribute filters
#' @param children a list of children, each a list of child attribute filters
#' @param columns vector of column names to return from the node
#' @return a df with (parent) id and a column for each rel with the respective child id
#' @export
find_nodes <- function(tokens, children=NULL, columns=NULL, ...) {
  filters = list(...)
  result = tokens
  for (name in names(filters)) {
    filter_value = filters[[name]]
    filter_column = sub("__.*$", "", name)
    match_values = result[[filter_column]]
    if (grepl("__in$", name)) {
      result = result[match_values %in% filter_value,]
    } else {
      if (grepl("__i$", name)) {match_values = tolower(match_values); filter_value = tolower(filter_value)}
      result = result[match_values == filter_value,]
    }
  }
  result = result[c("id", columns)]
  if (!is.null(names(children))) children = list(children)
  
  for (i in seq_along(children)) {
    t = do.call(function(...) get_children(tokens, ...), as.list(children[[i]]))
    t = rename(t, c(child=paste("rel", i, sep = "_")), warn_missing = F) # in case no rel is given
    result = merge(result, t)
  }
  result  
}
