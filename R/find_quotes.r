#' Add span quotes to a source-quote annotations
#'
#' @description
#' Quotes can span across sentences, which makes it impossible to find them based on dependency tree quories. 
#' This function can be used as post-processing, AFTER using tqueries to find 'source' and 'quote' nodes, to add some of these quotes. 
#' 
#' The quotes themselves are often easy to detect due to the use of quotation marks. There are two common ways of indicating the sources.
#' 
#' Firstly, the source might be used before the start of the quote (Steve said: "hey a quote!". "I like quotes!").
#' Secondly, the source might be implied in the sentence where the quotes starts, or the sentence before that (Steve was mad. "What a stupid way of quoting me!"). 
#'
#' In the first case, the source can be found with a tquery. If there is a source (source_val) in the quote_col that is linked to a part of the quote (quote_val), this function will add the rest of the quote. 
#' 
#' In the second case, we can look for candidates near the beginning of the quote. The candidate criteria can be specified as tqueries
#'
#' @param tokens        A tokenIndex with rsyntax annotations for 'sources' and 'quotes'
#' @param text_col      The column with the text (often 'token' or 'word')
#' @param quote_col     The column that contains the quote annotations
#' @param source_val    The value in quote_col that indicates the source
#' @param quote_val     The value in quote_col that indicates the quote
#' @param tqueries      A list of tqueries, that will be performed to find source candidates. The order of the queries determines which source candidates are preferred. It would make sense to use the same value as in source_val in the 'label' argument for the tquery.
#' @param par_col       If available in the parser output, the column with the paragraph id. We can assume that quotes do not span across paragraphs. By using this argument, quotes that are not properly closed (uneven number of quotes) will stop at the end of the paragraph 
#' @param space_col     If par_col is not used, paragraphs will be identified based on hard enters in the text_col. In some parsers, there is an additional "space" column that hold the whitespace and linebreaks, which can be included here. 
#' @param lag_sentences The max number of sentences looked backwards to find source candidates. Default is 1, which means the source candidates have to occur in the sentence where the quote begins (lag = 0) or the sentence before that (lag = 1) 
#' @param add_quote_symbols   Optionally, add additional punctuation symbols for finding quotation marks. 
#'                      In some contexts and languages it makes sense to add single quotes, but in that case it is oftne necessary to 
#'                      also use the quote_subset argument. For instance, in Spacy (and probably other UD based annotations), single quotes in posessives (e.g., Bob's, scholars') have a
#'                      PART POS tag, whereas quotation symbols have PUNCT, NOUN, VERB, or ADJ (for some reason).   
#' @param quote_subset  Optionally, an expression to be evaluated on the columns of 'tokens' for selecting/deselecting tokens that can/cant be quotation marks. For example,
#'                      pos != "PART" can be used for the example mentioned in add_quote_symbols. 
#' @param copy          If TRUE, deep copy the data.table (use if output tokens do not overwrite input tokens)
#'
#' @return the tokenIndex
#' @export
#' @examples 
#' ## This function is best used after first annotating regular quotes
#' ## Here we first apply 3 tqueries for annotating quotes in spacy tokens
#' 
#' \donttest{
#' 
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text6',]
#' 
## a list of verbs that indicate communication acts is required. This is an example:
#' verbs = c("tell", "show", "acknowledge", "admit", "affirm", "allege", 
#'   "announce", "assert", "attest", "avow", "call", "claim", "comment", 
#'   "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", 
#'   "note", "post","predict", "proclaim", "promise", "reply", "remark", 
#'   "report", "say", "speak", "state", "suggest", "talk", "tell", "think",
#'   "warn","write", "add")
#' 
#' direct = tquery(lemma = verbs, label='verb',
#'    children(req=FALSE, relation = c('npadvmod'), block=TRUE),
#'    children(relation=c('su','nsubj','agent','nmod:agent'), label='source'),
#'    children(label='quote'))
#' 
#' nosrc = tquery(pos='VERB*',
#'    children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
#'    children(lemma = verbs, relation='xcomp', label='verb',
#'      children(relation=c("ccomp","dep","parataxis","dobj","nsubjpass","advcl"), label='quote')))
#' 
#' according = tquery(label='quote',
#'    children(relation='nmod:according_to', label='source',
#'         children(label='verb')))
#'
#' tokens = annotate_tqueries(tokens, 'quote', dir=direct, nos=nosrc, acc=according)
#' tokens
#'
#' ## now we add the span quotes. If a span quote is found, the algorithm will first
#' ## look for already annotated sources as source candidates. If there are none,
#' ## additional tqueries can be used to find candidates. Here we simply look for
#' ## the most recent PERSON entity
#' 
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text6',]
#' tokens = annotate_tqueries(tokens, 'quote', dir=direct, nos=nosrc, acc=according)
#' 
#' 
#' last_person = tquery(entity = 'PERSON*', label='source')
#' tokens = add_span_quotes(tokens, 'token', 
#'                          quote_col = 'quote', source_val = 'source', quote_val = 'quote', 
#'                          tqueries=last_person)
#' tokens
#' 
#' ## view as full text
#' syntax_reader(tokens, annotation = 'quote', value = 'source')
#' }
add_span_quotes <- function(tokens, text_col, quote_col='quotes', source_val='source', quote_val='quote', tqueries=NULL, par_col=NULL, space_col=NULL, lag_sentences=1, add_quote_symbols=NULL, quote_subset=NULL, copy=TRUE) {
  is_quoted = NULL
  if (rsyntax_threads() != data.table::getDTthreads()) {
    old_threads = data.table::getDTthreads()
    on.exit(data.table::setDTthreads(old_threads))
    data.table::setDTthreads(rsyntax_threads())
  }
  
  if (!quote_col %in% colnames(tokens)) stop('quote_col is not a column in tokens') 
  if (copy) tokens = data.table::copy(tokens)
  
  quote_subset = eval(substitute(quote_subset), tokens)
  if (!is.null(add_quote_symbols)) {
    add_quote_symbols = paste(add_quote_symbols, collapse='')
    quote_regex = sprintf('^[%s%s"]*$', smartquotes(), add_quote_symbols)
  } else {
    quote_regex = sprintf('^[%s"]*$', smartquotes())
  }
  is_quote = get_quote_positions(tokens, text_col, is_quote_regex=quote_regex, par_col = par_col, space_col = space_col, quote_subset=quote_subset)

  ## if a previously found source occurs in a span quote (all source nodes within the quote), remove it. nested quotes are a challenge for another day
  tokens = remove_nested_source(tokens, is_quote, quote_col, source_val)
  quotes = tokens[!is.na(is_quote),]
  quotes$.QUOTE = is_quote[!is.na(is_quote)]
  
  tokens = add_extended_source(tokens, is_quote, quotes, quote_col, quote_val) ## adds by reference
  if (!is.null(tqueries))
    tokens = add_new_source(tokens, is_quote, quotes, quote_col, source_val, quote_val, tqueries, lag_sentences) ## adds by reference

  verbatim_col = paste(quote_col, 'verbatim', sep='_')
  suppressWarnings({
    ## I don't like this solution, but data.table can give a useless warning
    tokens[, (verbatim_col) := !is.na(is_quote) & !is.na(tokens[[quote_col]]) & quote_val == 'quote']
  })
  tokens[]
}

remove_nested_source <- function(tokens, is_quote, quote_col, source_val) {
  quote_id_col = paste0(quote_col,'_id')
  source_in_quote = data.table::data.table(is_quote=!is.na(is_quote), id=tokens[[quote_id_col]])
  source_in_quote = source_in_quote[tokens[[quote_col]] == source_val]
  source_in_quote = tapply(source_in_quote$is_quote, source_in_quote$id, FUN=all)
  remove_id = names(source_in_quote)[source_in_quote]
  remove_i = tokens[[quote_id_col]] %in% remove_id 
  tokens[remove_i, (quote_col) := NA]
  tokens[remove_i, (quote_id_col) := NA]
  tokens
}


add_extended_source <- function(tokens, is_quote, quotes, quote_col, quote_val) {
  quote_id_col = paste0(quote_col,'_id')
  uquotes = unique(subset(quotes, select = c('doc_id',quote_id_col,quote_col,'.QUOTE')))
  
  no_source = uquotes[is.na(uquotes[[quote_id_col]])]
  has_source = uquotes[!is.na(uquotes[[quote_col]]) & uquotes$.QUOTE %in% no_source$.QUOTE]
  
  ## quote can only contain the specified quote val, to prevent nested quotes
  not_val = !grepl(paste0('\\b', quote_val, '\\b'), has_source[[quote_col]])
  nested_quote = has_source[not_val,]
  has_source = has_source[!has_source[[quote_id_col]] %in% nested_quote[[quote_id_col]]]
  
  matched_source = has_source[match(is_quote, has_source$.QUOTE)]
  .NOT_NA = !is.na(matched_source$doc_id) 
  tokens[.NOT_NA, (quote_col) := matched_source[[quote_col]][.NOT_NA]]
  tokens[.NOT_NA, (quote_id_col) := matched_source[[quote_id_col]][.NOT_NA]]
  tokens
}

add_new_source <- function(tokens, is_quote, quotes, quote_col, source_val, quote_val, tqueries, lag_sentences=1) {
  if (lag_sentences < 0) stop('lag_sentences must be 0 or higher')
  quote_id_col = paste0(quote_col,'_id')
  uquotes = unique(subset(quotes, select = c('doc_id','sentence','token_id',quote_id_col,quote_col,'.QUOTE')))
  
  already_coded = tapply(!is.na(uquotes[[quote_col]]), uquotes$.QUOTE, FUN=mean)
  has_source = as.numeric(names(already_coded)[already_coded == 1])
  #already_coded = uquotes[!is.na(uquotes[[quote_col]]),]
  #has_source = unique(already_coded$.QUOTE)
  no_source = uquotes[!uquotes$.QUOTE %in% has_source,]
  no_source = unique(no_source, by=c('doc_id','.QUOTE'))
  
  candidates = select_candidates(tokens, is_quote, quote_col, source_val, tqueries)
  
  if (nrow(candidates) == 0) return(tokens)
  
  no_source$start_sentence = no_source$sentence

  for (i in 0:lag_sentences) {
    no_source$sentence = no_source$start_sentence - i
    if (!any(no_source$sentence >= 0)) break
    
    sent = merge(no_source, candidates, by=c('doc_id','sentence'), allow.cartesian=TRUE)
    select_ids = unique(sent, by=c('.QUOTE'))$.ID
    
    sent = sent[list(unique(select_ids)), on='.ID']
    
    #sent = sent[!sent$.ROLE == quote_val,]
    if (nrow(sent) > 0) 
      tokens = add_selected_sources(tokens, sent, is_quote, quote_col, source_val, quote_val)
    
    no_source = no_source[!no_source$.QUOTE %in% sent$.QUOTE,]
  }

  
  tokens
}

select_candidates <- function(tokens, is_quote, quote_col, source_val, tqueries) {
  quote_id_col = paste0(quote_col,'_id')
  #is_source = tokens[!is.na(tokens[[quote_col]]),]  ## use any value for quote_col, but need to filter out quote value before add_selected_sources
  is_source = tokens[!is.na(tokens[[quote_col]]) & tokens[[quote_col]] == source_val,]
  if (nrow(is_source) > 0) {
    is_source = data.table::data.table(doc_id=is_source$doc_id, sentence=is_source$sentence, candidate_id=is_source$token_id,
                                       .ROLE=source_val, .ID=is_source[[quote_id_col]])
  }
  
  candidates = apply_queries(tokens, tqueries)
  if (nrow(candidates) > 0) {
    candidate_in_quote = subset(tokens, !is.na(is_quote), select=c('doc_id','sentence','token_id'))
    candidates = candidates[!candidate_in_quote,on=c('doc_id','sentence','token_id')]
    candidates = data.table::data.table(doc_id=candidates$doc_id, sentence=candidates$sentence, candidate_id=candidates$token_id, 
                                      .ROLE=candidates$.ROLE, .ID=candidates$.ID)
  }
  
  if (nrow(is_source) > 0 && nrow(candidates)) return(rbind(is_source, candidates))
  if (nrow(is_source) > 0) return(is_source)
  if (nrow(candidates) > 0) return(candidates)
  return(is_source)
}

add_selected_sources <- function(tokens, sources, is_quote, quote_col, source_val, quote_val) {
  quote_id_col = paste0(quote_col,'_id')
  
  sources$label = as.character(sources$.ROLE)
  sources$i = tokens[list(sources$doc_id, sources$sentence, sources$candidate_id), on=c('doc_id','sentence','token_id'),which=TRUE]
  
  ## prepare quotes
  matched_source = sources[match(is_quote, sources$.QUOTE)]
  matched_source$label = ifelse(is.na(matched_source$label), NA, as.character(quote_val))
  matched_source$i = 1:nrow(matched_source)
  
  ## merge
  sources = rbind(sources,matched_source[!is.na(matched_source$doc_id)])
  sources$new_id = as.character(sources$.ID)
  
  ## add
  #replace_col = as.character(tokens[sources$i,][[quote_col]])
  #replace_id_col = as.character(tokens[sources$i,][[quote_id_col]])
  replace_col = as.character(sources$label)
  replace_id_col = as.character(sources$new_id)
  
  ## In case that a new quote is found within an existing quote:  
  ## old solution was to nest it by concatenating quote_id and quote, but that doesn't fit into current rsyntax design anymore (nested ids are a bad id)
  ## Now we just use the most specific quote.
  #already_used = !is.na(replace_id_col) & !(replace_id_col == sources$new_id)
  #replace_col = ifelse(already_used, paste(replace_col, sources$label, sep=','), sources$label)
  #replace_id_col = ifelse(already_used, paste(replace_id_col, sources$new_id, sep=','), sources$new_id)
  
  levels(tokens[[quote_col]]) = union(levels(tokens[[quote_col]]), unique(replace_col))
  levels(tokens[[quote_id_col]]) = union(levels(tokens[[quote_id_col]]), unique(replace_id_col))
  tokens[sources$i, (quote_col) := replace_col]
  tokens[sources$i, (quote_id_col) := replace_id_col]
  
  tokens
}


smartquotes <- function() intToUtf8(c(8220,8221,8222))

get_quote_positions <- function(tokens, text_col, is_quote_regex, par_col=NULL, space_col=NULL, quote_subset=NULL) {
  par = get_paragraph(tokens, text_col, par_col, space_col)
  
  is_quote = grepl(is_quote_regex, tokens[[text_col]])
  if (!is.null(quote_subset)) is_quote = is_quote & quote_subset
  is_quote[c(FALSE,is_quote[-length(is_quote)])] = FALSE ## to ignore double quotes after reshaping
  
  par_quotes = split(is_quote, par)
  par_quotes = lapply(par_quotes, get_spans)
  
  add_count = cumsum(sapply(par_quotes, function(x) max(c(x,0), na.rm=TRUE)))
  add_count = data.table::shift(add_count, 1, fill = 0)
  par_quotes = lapply(1:length(par_quotes), function(i) par_quotes[[i]] + add_count[i])
  out = as.integer(unlist(par_quotes))
  out[is_quote] = NA
  out
}

get_spans <- function(quotes) {
  quotes = cumsum(quotes) + 1
  if (quotes[length(quotes)] < 2) {
    quotes = rep(NA, length(quotes))
  } else {
    quotes = match(quotes, seq(2, quotes[length(quotes)], by = 2))
    shifted = data.table::shift(quotes, 1)
    quotes = ifelse(is.na(quotes) & !is.na(shifted), shifted, quotes)
  }
  quotes
}

get_paragraph <- function(tokens, text_col, par_col, space_col) {
  if (is.null(par_col)) {
    is_break = grepl('\n', tokens[[text_col]], fixed=TRUE) 
    if (!is.null(space_col)) is_break = is_break | grepl('\n', tokens[[space_col]], fixed=TRUE)
    is_new = !is_break & c(FALSE, is_break[-length(is_break)])
    is_new[!duplicated(tokens$doc_id)] = TRUE
    par = cumsum(is_new)
  } else par = tokens[[par_col]]
  par
}
