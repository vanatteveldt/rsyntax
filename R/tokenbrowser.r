#To be added once tokenbrowser is on cran


#syntax_reader <- function(tokens, annotation, from, to, meta=NULL, doc_col='doc_id', token_col='token', filename=NULL, ...){
#  ann_id = paste0(annotation, '_id')
#  id = match(tokens[[ann_id]], unique(tokens[[ann_id]]))
#  from = ifelse(!is.na(tokens[[annotation]]) & tokens[[annotation]] == from, id, NA)
#  to = ifelse(!is.na(tokens[[annotation]]) & tokens[[annotation]] == to, id, NA)
#  
#  tokens[[token_col]] = syntax_highlight_tokens(tokens[[token_col]], tokens[[ann_id]], from, to)
#  tokenbrowser::create_reader(tokens, meta, doc_col, token_col, nav=unique(tokens[[ann_id]]), filename= filename, ...)
#}

#syntax_highlight_tokens <- function(tokens, ann_id, from, to, labels=labels) {
#  nids = length(unique(ann_id))
#  colors = grDevices::rainbow(nids)
#  
#  alpha = rep(0.4, length(from))
#  tcolor = colors[from]
#  alpha[is.na(tcolor)] = NA
#  
#  col = tokenbrowser::highlight_col(alpha, col=tcolor)
#  tokens = tokenbrowser::tag_tokens(tokens,
#                      title = ifelse(!is.na(from), as.character(ann_id), NA),
#                      style = attr_style(`background-color` = col, `border` = stringi::stri_paste('3px solid ', col)),
#                      span_adjacent = T)
#  
#  alpha = rep(0.8, length(to))
#  boxcolor = colors[to]
#  alpha[is.na(boxcolor)] = NA
#  
#  col = tokenbrowser::highlight_col(alpha, col=boxcolor)
#  tokens = tokenbrowser::tag_tokens(tokens,
#                      title = ifelse(!is.na(to), as.character(ann_id), NA),
#                      style = attr_style(`border` = stringi::stri_paste('3px solid ', col)),
#                      span_adjacent = T)
#  
#  non_na_ann_i = match(ann_id, unique(ann_id))
#  non_na_ann_i[is.na(ann_id)] = NA
#  tokens = tokenbrowser::tag_tokens(tokens, 'a', tag_attr(name = stringi::stri_paste('nav', non_na_ann_i, sep='')),
#                      span_adjacent = T)
#  
#  tokens
#}
