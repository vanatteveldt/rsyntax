#' Returns a list with the quote rules for ALPINO
#'
#' @return A list with rynstax rules, as created with \link{rule}
#' @export
alpino_quote_rules <- function() {
  # x zegt dat y
  zegtdat = rule(lemma = dutch$SIP,
                        children(save = 'source', relation=c('su')),
                        children(relation='vc', POS = c('C', 'comp'),
                                 children(save='quote', relation=c('body'))))
  
  # x stelt: y
  ystelt = rule(lemma = dutch$SIP, 
                       children(save = 'source', relation=c('su')),
                       children(save = 'quote', relation='nucl'),
                       children(lemma =  quote_punctuation))
  
  # y, stelt x
  xstelt = rule(save='quote', 
                       children(relation='tag', lemma = dutch$SIP,
                                children(save = 'source', relation=c('su'))))
  
  # y, volgens x
  volgens = rule(save='quote',
                        children(relation=c('mod','tag'), lemma = dutch$source_mod,
                                 children(save='source')))
  
  # y, zo noemt x het
  noemt = rule(relation='tag', 
                      children(save='source', relation=c('su')),
                      parents(save='quote',
                              children(relation = ' --', lemma = quote_punctuation)))
  
  # x is het er ook mee eens: y
  impliciet = rule(
                          children(lemma = quote_punctuation),
                          children(save='quote', relation=c('tag','nucl','sat')),
                          children(save='source', relation=c('su')))
  
  # x: y
  impliciet2 = rule(save='source',
                           children(lemma = quote_punctuation),
                           children(save='quote', relation=c('tag','nucl','sat')))
  
  ## order matters
  list(zegtdat=zegtdat, ystelt=ystelt, xstelt=xstelt, volgens=volgens, noemt=noemt, impliciet=impliciet, impliciet2=impliciet2)
}

#' Returns a list with the clause rules for ALPINO
#'
#' @return A list with rynstax rules, as created with \link{rule}
#' @export
alpino_clause_rules <- function(){
  ## [passive subject as object] [passive verb with modifier] [object as subject] 
  #passive_conj = rule(POS = 'verb', 
  #               parents(save='predicate', lemma = dutch$passive_vc),
  #               children(lemma = dutch$passive_mod, 
  #                        children(save='subject', relation='obj1')))
  
  passive = rule(POS = 'verb', 
                        parents(save='predicate', lemma = dutch$passive_vc),
                        children(lemma = dutch$passive_mod, 
                                 children(save='subject', relation='obj1')))
  
  ## [subject] [has/is/etc.] [verb] [object]
  perfect = rule(POS = 'verb',
                        parents(save='predicate', lemma = dutch$passive_vc),
                        children(save='subject', relation=c('su')))
  
  ## [subject] [verb] [object]
  active = rule(save='predicate', POS = 'verb',
                         children(save='subject', relation=c('su')),
                         children(NOT=T, POS = 'adj',                 ## cannot have children that are adj if the key (parent of child)
                                  parents(lemma = dutch$passive_vc))) ## is a passive verb. Excludes reality (e.g., "Joe was defeated") 
  
  ## [subject] [verb] 
  no_object = rule(save='predicate', POS = 'verb',
                children(save='subject', relation=c('su')))
                         
  list(passive=passive, perfect=perfect, active=active, no_object)
}


function(){
  
  tokens = as_tokenindex(tokens_dutchclauses)
  tokens = annotate_alpino(tokens)
  quotes = get_quotes_alpino(tokens)
  get_nodes(tokens, quotes)
  
  tokens = annotate(tokens, quotes, 'quote', use = c('quote','source'))
  tokens[,c('quote','quote_id','token')]
  
  tokens = as_tokenindex(tokens_dutchclauses)
  
  clauses = get_clauses_alpino(tokens)
  get_nodes(tokens, clauses)
  
  tokens = annotate(tokens, clauses, 'clause', use = c('predicate','subject'))
  tokens[,c('clause','clause_id','token')]
}
