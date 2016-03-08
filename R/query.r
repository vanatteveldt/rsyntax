

reindexSentenceId <- function(tokens, article_var='aid', sentence_var='sentence'){
  ## order by article_var and sentence_var, but keep ordering to restore original order before returning
  ord = order(tokens[,article_var], tokens[,sentence_var])
  tokens = tokens[ord,]
  
  ## calculate the number of tokens per article
  arttokens = c(which(!duplicated(tokens[,article_var])), nrow(tokens))
  arttokens = arttokens[2:length(arttokens)] - arttokens[1:length(arttokens)-1]
  arttokens[length(arttokens)] = arttokens[length(arttokens)] + 1
  
  ## calculate the first sentence number (minus 1) for each new article
  sentnr = tokens[,sentence_var][!duplicated(tokens[,article_var])] - 1
  
  ## subtract the first sentence number of article of the sentence number
  tokens[,sentence_var] = tokens[,sentence_var] - rep(sentnr, arttokens)
  tokens[match(1:nrow(tokens), ord),]
}

parseQueries <- function(query){
  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)
  
  ## also allow space as OR
  query = gsub('(?<=[*a-zA-Z0-9?)])[ ]+(?=[*a-zA-Z0-9?(])', ' | ', query, perl=T)
  
  ## make " * ", as a 'find all' solution, an immediate true
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)

  query_form = as.list(gsub('([*a-z0-9/~_-]+)', '%s', query)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr('([*a-z0-9/~_-]+)', query))
  
  query_form[query_form == ''] = NA
  t(mapply(function(x,y) list(form=x, terms=y), query_form, query_terms))
}

fillQuery <- function(query_values, query_form){
  do.call(sprintf, as.list(c(query_form, query_values)))
}

evalQuery <- function(query_values, query_form){
  eval(parse(text=fillQuery(query_values, query_form)))
}

evalQueryMatrix <- function(qm, terms, form){
  apply(qm[,terms, drop=F], MARGIN = 1, evalQuery, query_form=form)
}

getTermRegex <- function(terms, default.window=50){
  terms = parseQueries(terms)
  terms = unlist(terms[,2])
  terms = data.frame(term = terms, 
                     regex = gsub('~.*', '', terms),
                     window = as.numeric(ifelse(grepl('~', terms) == T, gsub('.*~', '', terms), default.window)))
  terms$regex = gsub('*', '.*', terms$regex, fixed=T)
  terms$regex = sprintf('\\b%s\\b', terms$regex)
  terms
}



#' Annotate a data frame of tokens with codes using Lucene-like search queries
#' 
#' @param tokens a data frame of tokens containing columns for article id, term location and term string (column names can be specified in aid_var, term_i_var and term_var parameters, respectively). 
#' @param queries a data frame containing the queries. 
#' @param default.window 
#' @param aid_var a character string giving the name of the article id column 
#' @param term_i_var a character string giving the name of the term location column
#' @param term_var a character string giving the name of the term string column 
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param presorted The data has to be sorted on order(aid_var, term_i_var). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#' @return the annotated tokens data frame
#' @export
codeTokens <- function(tokens, queries, default.window=25, columnname='code', filter=rep(T, nrow(tokens)), aid_var='aid', term_i_var='id', term_var='word', condition_once=FALSE, presorted=F){
  ## consider underscores as wordboundaries (important for some parsers that chunk words together, separated by underscores)
  if(!class(tokens[,term_var]) == 'factor') as.factor(tokens[,term_var])
  levels(tokens[,term_var]) = gsub('_', ' ', levels(tokens[,term_var])) 

  ## make query forms for the indicators and conditions
  ind = parseQueries(queries$indicator)
  con = parseQueries(queries$condition)
  
  #message('Building query matrices')
  ## get regular expressions for each term in the queries
  indr = getTermRegex(queries$indicator, 1) # for indicators the window should always be 1 (the exact location of the indicator)
  conr = getTermRegex(queries$condition, default.window)
  query_regex = unique(rbind(indr, conr))
  
  ## first lookup in which article at least one of the indicator terms occurs, to ignore irrelevant articles in getQueryMatrix (speeds up searches with with few indicator hits but many condition hits)
  #article_filter = unique(tokens[tokenGrepl(indr$regex, tokens[,term_var]), aid_var])
  article_filter = NULL
  
  ## create matrix where rows are tokens, columns are the query terms, and cells indicate whether the query terms occur (within the given word distance) at the place of each token.
  qm = getQueryMatrix(tokens, query_regex, aid_var, term_i_var, term_var, presorted, article_filter)
  
  #message('Evaluating queries')
  if(!columnname %in% colnames(tokens)) {
    tokens[,columnname] = '' 
  } else {
    tokens[,columnname] = as.character(tokens[,columnname])
    message(sprintf('Column "%s" already exists. Existing values are not removed (unless overwritten)', columnname))
  }
  
  nqueries = nrow(queries)
  for(i in 1:nqueries){
    if(i %% 10 == 0) message(sprintf('\t%s / %s', i, nqueries))
    indicator_columns = ind[i,]$terms
    ind_hit = Matrix::rowSums(qm[,indicator_columns,drop=F]) > 0 ## indicator query can only contain OR statements, so this is a fast alternative to evalQueryMatrix
    ind_hit = ind_hit & filter ## indicator tokens have to pass the filter
    if(sum(ind_hit) == 0) next

    ## evaluate condition queries 
    con_terms = con[i,]$terms
    con_form = con[i,]$form
    code = as.character(queries$code[i])
    if(is.na(con_form)){
      ## if no condition is given, indicator hits are all we need
      tokens[,columnname][ind_hit] = code
    } else {
      hit_and_condition = evalQueryMatrix(qm[ind_hit,,drop=F], con_terms, con_form)
      if(sum(hit_and_condition) == 0) next
      tokens[,columnname][ind_hit] = ifelse(hit_and_condition, code, tokens[,columnname][ind_hit])
    }
    
    ## if condition_once is TRUE, then all indicators hits are also coded if the indicator satisfies its condition at least once within the article
    if(condition_once){
      article_with_code = unique(tokens[tokens[,columnname] == code, aid_var]) # articles in which indicator satisfies condition at least once
      hit_and_code = tokens[ind_hit, aid_var] %in% article_with_code # for all indicator hits, check whether they occur in one of these articles.
      tokens[,columnname][ind_hit] = ifelse(hit_and_code, code, tokens[,columnname][ind_hit])
    }
  }
  tokens[,columnname] = as.factor(tokens[,columnname])
  tokens
}

#' Get keyword-in-context from a token list
#' 
#' @param tokens a data frame of tokens containing columns for article id, term location and term string (column names can be specified in aid_var, term_i_var and term_var parameters, respectively). 
#' @param token_i The tokens for which the context is given. Can be a logical vector or a numeric vector with indices.
#' @param nwords the number of words in front and after the keyword
#' @param aid_var a character string giving the name of the article id column 
#' @param term_i_var a character string giving the name of the term location column
#' @param term_var a character string giving the name of the term string column 
#' @return A data.frame with the keyword in context
#' @export
kwic <- function(tokens, hits, nwords=10, aid_var='aid', term_i_var='id', term_var='word', prettypaste=T){
  token_i = tokenLookup(tokens, hits[,aid_var], hits[,term_i_var], aid_var, term_i_var)
  
  kwicldply <- function(i, aids, terms, nwords){
    aid = aids[i]
    sent_i = (i-nwords):(i+nwords)
    sent = as.character(terms[sent_i])
    sent = gsub('\\[|\\]', '', sent)
    
    sent[nwords+1] = sprintf('[%s]', sent[nwords+1])
    sent = sent[aids[sent_i] == aid] # only show context words if they occur in the same article
    data.frame(aid=aid, kwic=paste(sent, collapse=' '))  
  }
  o = ldply(token_i, kwicldply, aids=tokens[,aid_var], terms=tokens[,term_var], nwords=nwords)
  if(prettypaste) {
    o$kwic = gsub('_', ' ', o$kwic)
    o$kwic = gsub('  ', ' ', o$kwic)
    o$kwic = gsub(" ([.,?!:;>)])", '\\1', o$kwic)
    o$kwic = gsub('([(<]) ', '\\1', o$kwic)
    o$kwic = sprintf('...%s...', o$kwic)
  }
  o
}

tokenLookup <- function(tokens, article_id, term_i, aid_var='aid', term_i_var='id'){
  tokens$i = 1:nrow(tokens)
  tokens = tokens[tokens[,aid_var] %in% unique(article_id), c('i', aid_var, term_i_var)]
  which.sub = match(paste(article_id, term_i, sep='___'),
                    paste(tokens[,aid_var], tokens[,term_i_var], sep='___'))
  tokens$i[which.sub]
}

#' Search for tokens in a tokenlist using indicators with conditions
#' 
#' Tokens need to be sorted on order(aid_var, term_i_var), or presorted needs to be set to FALSE. Note that if this function is used often, its faster to sort tokens first and use presorted = TRUE.
#'
#' @param tokens 
#' @param indicator 
#' @param condition 
#' @param sample_n 
#' @param meta 
#' @param aid_var 
#' @param term_i_var 
#' @param term_var 
#' @param keywordIC 
#' @param kwic_nwords 
#' @param filter 
#' @param condition_once logical. If TRUE, then if an indicator satisfies its conditions once in an article, all indicators within that article are coded.
#' @param presorted The data has to be sorted on order(aid_var, term_i_var). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#'
#' @return the tokens that match the query
#' @export
searchQuery <- function(tokens, indicator, condition='', default.window=25, meta=NULL, condition_once=FALSE, presorted=T, filter=rep(T, nrow(tokens)), aid_var='aid', term_i_var='id', term_var='word'){
  tokens$hit = NULL
  query = data.frame(code=1, indicator=indicator, condition=condition)
  tokens = codeTokens(tokens, query, columnname = 'hit', filter=filter, aid_var=aid_var, term_i_var=term_i_var, term_var=term_var, presorted=presorted, default.window = default.window)
  hits = tokens[tokens$hit == 1, c(aid_var, term_i_var, term_var)]
  
  ## report number of hits and articles
  nhits = nrow(hits)
  narts = length(unique(hits[,aid_var]))
  message(sprintf('%s hit%s in %s article%s (N = %s)', nhits, ifelse(nhits==1, '', 's'), 
                  narts, ifelse(narts==1, '', 's'), 
                  length(unique(tokens[,aid_var]))))
  if(!is.null(meta)) hits = merge(meta, hits, by=aid_var, all.x=F, all.y=T)
  hits
}


#' Search for tokens in a tokenlist using indicators with conditions
#' 
#' Tokens need to be sorted on order(aid_var, term_i_var), or presorted needs to be set to FALSE. Note that if this function is used often, its faster to sort tokens first and use presorted = TRUE.
#'
#' @param tokens 
#' @param indicator 
#' @param condition 
#' @param default.window 
#' @param tokenfreq 
#' @param keywordIC 
#' @param kwic_nwords 
#' @param keywordIC_sample 
#' @param random_sample 
#' @param meta 
#' @param condition_once 
#' @param presorted 
#' @param aid_var 
#' @param term_i_var 
#' @param term_var 
#'
#' @return prints stuff to evaluate the validity of queries
#' @export
testQuery <- function(tokens, indicator, condition='', default.window=25, tokenfreq=T, keywordIC=T, kwic_nwords=10, keywordIC_sample=10, random_sample=T, meta=NULL, condition_once=FALSE, presorted=T, aid_var='aid', term_i_var='id', term_var='word'){
  hits = searchQuery(tokens, indicator, condition, default.window, meta=meta, aid_var=aid_var, term_i_var=term_i_var, term_var=term_var, condition_once=condition_once, presorted=presorted)
  cat('\n')
  if(tokenfreq & nrow(hits) > 0) reportTokenFreq(hits, aid_var, term_var)

  if(keywordIC & nrow(hits) > 0) {
    if(!is.null(keywordIC_sample)) {
      if(random_sample) hits = hits[sample(1:nrow(hits), nrow(hits)),]
      hits = head(hits, keywordIC_sample) 
    }
    hits$kwic = kwic(tokens, hits, nwords = kwic_nwords)$kwic
    reportKWIC(hits, aid_var, term_i_var, term_var) 
  }
}

reportTokenFreq <- function(hits, aid_var, term_var){
  termfreq = aggregate(list(hits=hits[,aid_var]), by=list(token=as.character(hits[,term_var])), FUN='length')
  print(termfreq, row.names=F)
}

reportKWIC <- function(hits, aid_var='aid', term_i_var='id', term_var='word'){
  for(aid in unique(hits[,aid_var])){
    cat('###################')
    ahits = hits[hits[,aid_var] == aid,]
    metanames = colnames(ahits[!colnames(ahits) %in% c(term_i_var, term_var, 'kwic')])
    for(metaname in metanames){
      message(paste(metaname, paste(unique(ahits[,metaname]), collapse=' / '), sep=': '))
    }
    for(term_i in ahits[,term_i_var]){
      cat('\t',ahits[ahits$id == term_i,]$kwic)
      cat('\n')
    }
  }
}



compareHits <- function(tokens, hits.x, hits.y, sample_n=10, aid_var='aid', term_i_var='id'){
  id.x = paste(hits.x[,aid_var], hits.x[,term_i_var], sep='---')
  id.y = paste(hits.y[,aid_var], hits.y[,term_i_var], sep='---')
  x_not_y = hits.x[!id.x %in% id.y,] 
  y_not_x = hits.y[!id.y %in% id.x,] 
  x_and_y = hits.x[]
}

#hits.x = searchQuery(tokens, 'sap')
#hits.y = searchQuery(tokens, 'sap', 'jolande fractievoorzit* groenlinks')


#hits = list(test1=data.frame(aid=c(1,2,3), hits=c(1,1,1)),test2=data.frame(aid=c(1,2,3), hits=c(1,1,1)), test3=data.frame(aid=c(1,2,3), hits=c(1,1,1)))
#xy = combn(names(hits), 2)
#compareHitsList <- function(..., sample_n=10, aid_var='aid', term_i_var='id'){
#  hits = list(...)
#  xy = combn(names(hits), 2)
#  
#  o = NULL
#  for(comb in 1:ncol(xy)){
#   
#  }
#}



#################################
#################################
## Contains the functions to create a sparse matrix giving the windows within which queries occur.
## Please note that simplicity is sacrificed for performance without mercy

getQueryMatrix <- function(tokens, query_regex, aid_var='aid', term_i_var='id', term_var='word', presorted=F, article_filter=NULL){
  if(!presorted){
    ## unless explicitly noted that the token list is sorted, first sort tokens by article id and word id. (keeps order to restore original order)
    ord = order(tokens[,aid_var], tokens[,term_i_var])
    tokens = tokens[ord,]
  }
  if(is.null(article_filter)){
    tokens_filter = which(tokenGrepl(query_regex$regex, tokens[,term_var]))
  } else {
    ## article filter is for computational reasons only. It ignores the articles that are not selected. currently used to ignore articles in which none of the indicators occur 
    article_filter = which(tokens[,aid_var] %in% article_filter)
    tokens_within_article_filter = tokenGrepl(query_regex$regex, tokens[article_filter,term_var])
    tokens_filter = article_filter[tokens_within_article_filter]
  }
  
  m = getWindowMatrix(location=tokens[tokens_filter,term_i_var], 
                      context=tokens[tokens_filter,  aid_var], 
                      term = tokens[tokens_filter, term_var], 
                      window.size = max(query_regex$window))

  
  
  ## create the rows and columns for the query matrix by looking in which rows one of the terms that matches the regex is TRUE.
  ## by using the token_filter vector, the original positions in the token list are given to query_list  
  query_list = list()  
  for(j in 1:nrow(query_regex)){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m > 0 & query_m <= query_regex$window[j]) > 0 # matrix value has to be higher than 0, but not higher than the window size
    if(sum(hits) == 0) next
    query_list[['']] = data.frame(i = tokens_filter[hits], j = j) 
  }
  qm = rbind.fill(query_list)
  qm = spMatrix(nrow(tokens), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term
  #rownames(qm) = paste(tokens[,aid_var], tokens[,term_i_var], sep='.') # currently not used, and takes up space
  
  if(!presorted) qm = qm[match(1:nrow(qm), ord),] # return matrix in order of input tokens
  qm
}

tokenGrepl <- function(patterns, x, ignore.case=T, perl=F){
  ## a wrapper for grepl that takes multiple patterns and is efficient for a character vector (x) with many duplicates
  unique_x = unique(x)
  pattern = paste(patterns, collapse='|')
  relevant_terms = unique_x[grepl(pattern, unique_x, ignore.case=ignore.case, perl=perl)]
  x %in% relevant_terms
}

getWindowMatrix <- function(location, context, term, window.size){
  location = stretchLocation(location, context, window.size=window.size)
  length(location)
  
  shifts = -window.size:window.size
  
  terms = unique(term)
  term_index = match(term, terms)
  m = locationMatrix(i=location, j=term_index, shifts, distance.as.value=T)
  colnames(m) = terms
  m
}

stretchLocation <- function(location, context, window.size){
  ## makes the word location counter global, and adds dummy locations between contexts to prevent overlapping windows (so it can be used as an index). 
  ## this way, overlapping word windows can be calculated for multiple documents within a single matrix.
  ## location and context need to be sorted on order(context,location)!! (hence the presorted argument in getQueryMatrix)
  
  if(min(location) == 0) location = location + 1 ## location will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)
  
  newcontext = which(!duplicated(context)) # where does a new context start
  context.max = location[newcontext-1] + (window.size*2)
  multiplier_scores = cumsum(c(0,context.max)) # the amount that should be added to the location at the start of each context 
  repeat_multiplier = c(newcontext[2:length(newcontext)], length(location)+1) - newcontext # the number of times the multiplier scores need to be repeated to match the location vector
  multiplier_vector = rep(multiplier_scores, repeat_multiplier)
  return(location + multiplier_vector)
}

locationMatrix <- function(i, j, shifts=0, count.once=T, distance.as.value=F){
  mat = spMatrix(max(i), max(j))
  
  shifts = shifts[order(abs(shifts))] # order from 0 to higher (required if distance.as.value = T)
  for(shift in shifts){
    i_shift = i + shift
    select = i_shift > 0 & i_shift <= max(i)
    if(distance.as.value){
      mat = mat + spMatrix(nrow=max(i), ncol=max(j), i=i_shift[select], j=j[select], rep(abs(shift)+1, sum(select))) 
    } else{
      mat = mat + spMatrix(nrow=max(i), ncol=max(j), i=i_shift[select], j=j[select], rep(1, sum(select)))   
    }
  }
  
  if(distance.as.value){
    ## remove duplicates. since the stacked triples are ordered by shifts, this leaves the shortest distance to a term in case of duplicate cells
    count.once = F
    select = !duplicated(data.frame(mat@i, mat@j))
    mat = spMatrix(nrow(mat), ncol(mat), mat@i[select]+1, mat@j[select]+1, mat@x[select])
  }
  mat
  length(i)
  nrow(mat)
  mat = mat[i,]
  mat = as(mat, 'dgCMatrix')
  if(count.once) mat@x[mat@x>0] = 1
  mat
}
