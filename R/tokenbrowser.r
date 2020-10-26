#' Create a full text browser with highlighted rsyntax annotations
#'
#' @param tokens      A tokenIndex
#' @param annotation  The name of the column that contains the rsyntax annotation
#' @param value       Optionally, a character vector with values in annotation. If used, only these values are fully colored, and the other (non NA) values only have border colors.
#' @param value2      Optionally, a character vector with values in annotation other than those specified in 'value'. If used, only these values have border colors.
#' @param meta        Optionally, a data.frame with document meta data. Has to have a column named doc_id of which the values match with the doc_id column in tokens 
#' @param token_col   The name of the column in tokens with the token text
#' @param filename    Optionally, a filename to directly save the file. If not specified, a temporary file is created
#' @param view        If TRUE, the browser will immediatly be viewed in the viewer panel
#' @param random_seed If a number is given, it is used as a seed to randomize the order of documents. This is usefull for
#'                    validations purposes, because the doc_id in the tokenindex is sorted.
#' @param ...         Arguments passed to \link[tokenbrowser]{create_browser}
#'
#' @return  The url for the file
#' @export
#'
#' @examples
#' tokens = tokens_spacy
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "predicate",
#'                 children(relation = c("agent"), label = "subject"))
#' active =  tquery(pos = "VERB*", label = "predicate",
#'                 children(relation = c("nsubj", "nsubjpass"), label = "subject"))
#' 
#' 
#' \donttest{
#' tokens = annotate_tqueries(tokens, 'clause', pas=passive, act=active)
#' syntax_reader(tokens, annotation = 'clause', value = 'subject')
#' }
syntax_reader <- function(tokens, annotation, value=NULL, value2=NULL, meta=NULL, token_col='token', filename=NULL, view=TRUE, random_seed=NA, ...){
  #if (!methods::is(tokens, 'tokenIndex')) stop('tokens has to be a tokenIndex')
  if (!is.na(random_seed)) {
    tokens = data.table::copy(tokens)
    doc_ids = unique(tokens$doc_id)
    set.seed(random_seed)
    doc_ids = doc_ids[sample(1:length(doc_ids))]
    tokens = tokens[doc_ids, on=c('doc_id')]
  }
  
  #tokens = as_tokenindex(tokens)

  ann_id = paste0(annotation, '_id')
  id = match(tokens[[ann_id]], unique(tokens[[ann_id]]))
  
  if (is.null(value)) {
    if (!is.null(value2)) stop("value2 can only be used (not-NULL) if value is used")
    value = unique(as.character(tokens[[annotation]]))
  }
  value = ifelse(!is.na(tokens[[annotation]]) & tokens[[annotation]] %in% value, id, NA)
  
  if (is.null(value2)) {
    value2 = ifelse(!is.na(tokens[[annotation]]) & is.na(value), id, NA)
  } else {
    value2 = ifelse(!is.na(tokens[[annotation]]) & tokens[[annotation]] %in% value2, id, NA)
  }
  
  value_label = paste(stats::na.omit(unique(as.character(tokens[[annotation]][value]))), collapse=', ')
  value2_label = paste(stats::na.omit(unique(as.character(tokens[[annotation]][value2]))), collapse=', ')
  
  tokens[[token_col]] = syntax_highlight_tokens(tokens$doc_id, tokens[[token_col]], tokens[[ann_id]], value, value2, value_label, value2_label)

  url = tokenbrowser::create_browser(tokens, meta, 'doc_id', token_col, filename= filename, ...)
  if (view) tokenbrowser::view_browser(url)
  invisible(url)
}

syntax_highlight_tokens <- function(doc_id, tokens, ann_id, value, value2, value_label, value2_label) {
  doc_i = match(doc_id, stats::na.omit(unique(doc_id)))
  ann_i = match(ann_id, stats::na.omit(unique(ann_id)))
  colindex = tapply(ann_i, doc_i, function(x) if (all(is.na(x))) rep(NA, length(x)) else (x - min(x, na.rm = TRUE)) + 1)
  colindex = as.numeric(unlist(colindex))
  
  ncolors = 8   ## repeat x colors over and over, so different colors are used for different annotation_ids, but we don't start a carnival
  colindex_mod = colindex %% ncolors + 1
  
  colors = grDevices::palette()
  #colors = grDevices::terrain.colors(ncolors+1)
  
  tcolor = colors[ifelse(is.na(value), NA, colindex_mod)]
  
  alpha = rep(0.2, length(value))
  alpha[is.na(tcolor)] = NA
  
  col = tokenbrowser::highlight_col(alpha, col=tcolor)
  tokens = tokenbrowser::tag_tokens(tokens,
                      title = ifelse(!is.na(value), sprintf('%s; %s', value_label, ann_id), NA),
                      style = tokenbrowser::attr_style(`background-color` = col, `border` = stringi::stri_paste('3px solid ', col)),
                      span_adjacent = TRUE, doc_id=doc_id)
  
  alpha = rep(0.8, length(value2))
  boxcolor = colors[ifelse(is.na(value2), NA, colindex_mod)]
  alpha[is.na(boxcolor)] = NA
  
  col = tokenbrowser::highlight_col(alpha, col=boxcolor)
  tokens = tokenbrowser::tag_tokens(tokens,
                      title = ifelse(!is.na(value2), sprintf('%s; %s', value2_label, ann_id), NA),
                      style = tokenbrowser::attr_style(`border` = stringi::stri_paste('3px solid ', col)),
                      span_adjacent = TRUE, doc_id=doc_id)
  
  non_na_ann_i = match(ann_id, unique(ann_id))
  non_na_ann_i[is.na(ann_id)] = NA
  tokens = tokenbrowser::tag_tokens(tokens, 'a', tokenbrowser::tag_attr(name = stringi::stri_paste('nav', non_na_ann_i, sep='')),
                      span_adjacent = TRUE, doc_id=doc_id)
  
  tokens
  
}


