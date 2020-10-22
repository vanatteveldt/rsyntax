#' Cast annotations to text
#' 
#' Cast labeled tokens to sentences.  
#'
#' @param tokens       A tokenIndex
#' @param annotation   The name of annotations (the "column" argument in annotate_tqueries)
#' @param ...          Optionally, group annotations together. Named arguments can be given
#'                     where the name is the new group, and the value is a character vector with
#'                     values in the annotation column. For example, text = c('verb','predicate') would 
#'                     group the 'verb' and 'predicate' nodes together under the name 'text'.
#' @param text_col     The name of the column in tokens with the text. Usually this is "token",
#'                     but some parsers use alternatives such as 'word'.
#' @param na.rm        If true (default), drop tokens where annotation id is NA (i.e. tokens without labels)
#'
#' @return             a data.table
#' @export
#'
#' @examples
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "verb", fill=FALSE,
#'                  children(relation = "agent",
#'                           children(label="subject")),
#'                  children(relation = "nsubjpass", label="object"))
#' active =  tquery(pos = "VERB*", label = "verb", fill=FALSE,
#'                  children(relation = c("nsubj", "nsubjpass"), label = "subject"),
#'                  children(relation = "dobj", label="object"))
#' 
#' tokens = annotate_tqueries(tokens, "clause", pas=passive, act=active, overwrite=T)
#' 
#' cast_text(tokens, 'clause')
#' 
#' ## group annotations
#' cast_text(tokens, 'clause', text = c('verb','object'))
#' 
#' ## use grouping to sort
#' cast_text(tokens, 'clause', subject = 'subject', 
#'                             verb = 'verb', object = 'object')
cast_text <- function(tokens, annotation, ..., text_col='token', na.rm=T) {
  ann_id = NULL; ann = NULL
  cols = list(...)
  
  d = subset(tokens, select = c('doc_id',text_col, paste0(annotation, c('','_id'))))
  data.table::setnames(d, new=c('doc_id','text','ann','ann_id'))
  if (na.rm) d = subset(d, !is.na(ann_id))
  
  for (col in names(cols)) {
    d[list(cols[[col]]), ann := col, on='ann']
  }
  
  d = data.table::dcast(doc_id + ann_id ~ ann, fun.aggregate=paste, collapse=' ', value.var='text', data=d)
  if (length(cols) > 0) data.table::setcolorder(d, c('doc_id','ann_id',names(cols)))
  d
}