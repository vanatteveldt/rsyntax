

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
  query = gsub('([a-zA-Z0-9*?)])[ ]+([a-zA-Z0-9*?(])', '\\1 | \\2', query)
  
  query_form = as.list(gsub('([*a-zA-Z0-9/~_-]+)', '%s', query))
  query_terms = regmatches(query, gregexpr('([*a-zA-Z0-9/~_-]+)', query))
  
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
  ## make query forms for the indicators and conditions
  ind = parseQueries(queries$indicator)
  con = parseQueries(queries$condition)
  
  #message('Building query matrices')
  ## get regular expressions for each term in the queries
  indr = getTermRegex(queries$indicator, 1) # for indicators the window should always be 1 (the exact location of the indicator)
  conr = getTermRegex(queries$condition, default.window)
  query_regex = unique(rbind(indr, conr))
  
  ## create matrix where rows are tokens, columns are the query terms, and cells indicate whether the query terms occur (within the given word distance) at the place of each token.
  qm = getQueryMatrix(tokens, query_regex, aid_var, term_i_var, term_var, presorted)
  
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
#' @return
#' @export
#'
#' @examples
searchQuery <- function(tokens, indicator, condition='', default.window=25, meta=NULL, condition_once=FALSE, presorted=T, filter=rep(T, nrow(tokens)), aid_var='aid', term_i_var='id', term_var='word'){
  tokens$hit = NULL
  query = data.frame(code=1, indicator=indicator, condition=condition)
  tokens = codeTokens(tokens, query, columnname = 'hit', filter=filter, aid_var='aid', term_i_var='id', term_var='word', presorted=presorted, default.window = default.window)
  o = tokens[tokens$hit == 1, c(aid_var, term_i_var, term_var)]
  
  nhits = nrow(o)
  narts = length(unique(o[,aid_var]))
  message(sprintf('%s hit%s in %s article%s (N = %s)', nhits, ifelse(nhits==1, '', 's'), 
                                                       narts, ifelse(narts==1, '', 's'), 
                                                       length(unique(tokens[,aid_var]))))

  if(!is.null(meta)) o = merge(meta, o, by=aid_var, all.x=F, all.y=T)
  o
}

testQuery <- function(tokens, indicator, condition='', default.window=25, tokenfreq=T, keywordIC=T, kwic_nwords=10, keywordIC_sample=10, random_sample=T, meta=NULL, condition_once=FALSE, presorted=T, aid_var='aid', term_i_var='id', term_var='word'){
  hits = searchQuery(tokens, indicator, condition, default.window, meta=meta, aid_var=aid_var, term_i_var=term_i_var, term_var=term_var, condition_once=condition_once, presorted=presorted)
  cat('\n')
  if(tokenfreq) reportTokenFreq(hits, aid_var, term_var)

  if(keywordIC) {
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