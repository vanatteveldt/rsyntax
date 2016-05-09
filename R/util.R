
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
    f2 = unique(data.frame(id=as.character(f2$id), filled=as.character(f2$child), stringsAsFactors = F))
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

#' get all children with the given relation
#' @param tokens a df of tokens
#' @param rel the relation of child with parent (optional)
#' @param rename optionally rename the output column, defaults to re
#' @param ... other filters
#' @return a df with the ids of (parent) id and child (id)
get_children <- function(tokens, relation=NULL, rename=relation, ...) {
  filters = c(list(...), if (is.null(relation)) NULL else list(relation=relation))
  ctokens = do.call(find_nodes, c(list(tokens, columns="parent"), filters))
  # rename parent to id, id to `rename`, keep the rest and return with id as first column
  ctokens = plyr::rename(ctokens, c(parent="id", id=rename))
  cbind(ctokens[2], ctokens[-2])
}

#' get all parents with the given relation, 
#' @param tokens a df of tokens
#' @param rel the relation of child with parent (optional)
#' @param rename optionally rename the output column, defaults to re
#' @param ... other filters
#' @return a df with the ids of (parent) id and child (id)
get_parents <- function(tokens, relation=NULL, rename=relation, ...) {
  filters = c(list(...), if (is.null(relation)) NULL else list(relation=relation))
  ctokens = do.call(find_nodes, c(list(tokens=tokens, child=list(rename='___child___')), filters))
  ctokens = plyr::rename(ctokens, c('id'=rename, '___child___'='id'))
  cbind(ctokens[2], ctokens[-2])
  #do.call(find_nodes, c(list(tokens=tokens, child_filter, child=parent_filter)))
}

#' Search for nodes matching specific criteria
#' 
#' You can search on node attributes (lemma, pos) and on attributes of children or parents.
#' The function will return a data frame with the found nodes and a column for each matched child/parent relation.
#' The node attributes allow the extension __in (e.g. id__in=ids) to match multiple values, __not to not match a value, and __not_in to not match multiple values.
#' The child/parent attributes can be either a single relation name, or a list of node attributes.
#' The return column for children is named after the explicit rename= attribute, defaulting to the relation pattern, if given.
#' 
#' @param tokens a df of tokens
#' @param ... node attribute filters
#' @param child a list of child attribute filters. If multiple children are sought, use the children parameter instead.
#' @param parent a list of parent attribute filters. If multiple parents are sought, use the parents parameter instead.
#' @param children a list of children, each a list of child attribute filters
#' @param parents a list of parents, each a list of parent attribute filters
#' @param parents a list of parents, each a list of parent attribute filters
#' @param columns vector of column names to return from the node
#' @return a df with (parent) id and a column for each rel with the respective child id
#' @export
find_nodes <- function(tokens, child=NULL, parent=NULL, children=NULL, parents=NULL, columns=NULL, ...) {
  children[['']] = child
  parents[['']] = parent
  
  filters = list(...)
  result = tokens
  for (name in names(filters)) {
    filter_value = filters[[name]]
    filter_column = sub("__.*$", "", name)
    match_values = result[[filter_column]]
    
    ## case insensitive search
    filter_value = tolower(filter_value)
    match_values = tolower(match_values)
    
    if (!grepl("__in$|__not_in$|__not", name)) {
      result = result[!is.na(match_values) & match_values == filter_value,]
    }
    if (grepl("__in$", name)) {
      result = result[match_values %in% filter_value,]
    } 
    if (grepl("__not_in$", name)) {
      result = result[!match_values %in% filter_value,]
    }
    if (grepl("__not$", name)) {
      result = result[!match_values == filter_value,]
    }
  }
  result = result[c("id", columns)]

  ## Find children
  for (i in seq_along(children)) {
    children[[i]] = prepFilterList(children, i)
    t = do.call(get_children, c(list(tokens=tokens), as.list(children[[i]])))
    result = merge(result, t)
  }
  ## Find parents
  for (i in seq_along(parents)) {
    parents[[i]] = prepFilterList(parents, i)
    t = do.call(get_parents, c(list(tokens=tokens), as.list(parents[[i]])))
    result = merge(result, t)
  }
  
  result  
}

prepFilterList <- function(filters, filter_i){
  flist = as.list(filters[[filter_i]])
  if (is.null(flist[['rename']]) && !is.null(names(filters))) flist[['rename']] = names(filters)[filter_i]
  if (is.null(flist[['rename']]) || flist[['rename']] == '') flist[['rename']] = paste("rel", filter_i, sep="_")  
  flist
}

match_tokens <- function(ids, tokens, block) {
  filled = merge(ids, fill(ids$id, tokens, block = block))
  filled$id <- NULL
  plyr::rename(filled, c(filled="id"))
}



#' Annotate a tokenlist with clauses
#'
#' @param tokens a df of tokens
#' @param quotes the output of get_quotes
#' @param clauses the output of get_clauses
#'
#' @return a df of tokens with columns for clause_id and clause_role
#' @export
tokenClauseAnnotation <- function(tokens, quotes, clauses){
    quotes$quote_id = 1:nrow(quotes)
    block = c(quotes$source, quotes$quote)
    
    x = match_tokens(data.frame(quote_id=quotes$quote_id, id=quotes$source, quote_role="source"), tokens, block)
    
    quote_add = rbind(match_tokens(data.frame(quote_id=quotes$quote_id, id=quotes$source, quote_role="source"), tokens, block),
                      match_tokens(data.frame(quote_id=quotes$quote_id, id=quotes$quote, quote_role="quote"), tokens, block))
    
    block = c(block, clauses$subject, clauses$predicate)
    pred_add = rbind(match_tokens(data.frame(clause_id=clauses$clause_id, id=clauses$subject, clause_role="subject"), tokens, block),
                     match_tokens(data.frame(clause_id=clauses$clause_id, id=clauses$predicate, clause_role="predicate"), tokens, block))
    
    merge(merge(tokens, quote_add, all.x=T), pred_add, all.x=T)
}
  
#' Construct a list of source/subject/object triples from a tokens list with clauses and quotes
#'
#' @param tokens a data frame with clause_id, clause_role, quote_id and quote_role columns
#' @param concept_column the name of the column in tokens that contains identified 'concepts'
#'
#' @return a data frame with clause_id and source, subject, object columns indicating concepts found in those positions
#' @export
construct_triples <- function(tokens, concept_column="concept") {
  sources = tokens[!is.na(tokens$quote_role) & tokens$quote_role == "source" & !is.na(tokens[[concept_column]]), c(concept_column, "quote_id")]
  sources = unique(merge(sources, tokens[!is.na(tokens$clause_id), c("clause_id", "quote_id")]))[c("clause_id", concept_column)]
  colnames(sources)[2] = "source"
  
  subjects = unique(tokens[!is.na(tokens$clause_id) & !is.na(tokens[[concept_column]]) & tokens$clause_role == "subject", c("clause_id", concept_column)])
  colnames(subjects)[2] = "subject"
  objects = unique(tokens[!is.na(tokens$clause_id) & !is.na(tokens[[concept_column]]) & tokens$clause_role == "predicate", c("clause_id", concept_column)])
  colnames(objects)[2] = "object"
  
  merge(sources, merge(subjects, objects, all=T), all=T)
}