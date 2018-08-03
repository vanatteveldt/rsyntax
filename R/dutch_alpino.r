#' Returns a list with the quote queries for ALPINO
#'
#' @return A list with rynstax queries, as created with \link{tquery}
#' @export
alpino_quote_queries <- function() {
  # x zegt dat y
  zegtdat = tquery(lemma = dutch$SIP, save='sayverb',
                        children(save = 'source', relation=c('su')),
                        children(relation='vc', POS = c('C', 'comp'),
                                 children(save='quote', relation=c('body'))))
  
  # x stelt: y
  ystelt = tquery(lemma = dutch$SIP, save='sayverb',
                       children(save = 'source', relation=c('su')),
                       children(save = 'quote', relation='nucl'),
                       children(lemma =  quote_punctuation))
  
  # y, stelt x
  xstelt = tquery(save='quote',
                       children(relation='tag', lemma = dutch$SIP, save='sayverb',
                                children(save = 'source', relation=c('su'))))
  
  # y, volgens x
  volgens = tquery(save='quote', 
                        children(relation=c('mod','tag'), lemma = dutch$source_mod,
                                 children(save='source')))
  
  # y, zo noemt x het
  noemt = tquery(relation='tag', 
                      children(save='source', relation=c('su')),
                      parents(save='quote',
                              children(relation = ' --', lemma = quote_punctuation)))
  
  # x is het er ook mee eens: y
  impliciet = tquery(
                          children(lemma = quote_punctuation),
                          children(save='quote', relation=c('tag','nucl','sat')),
                          children(save='source', relation=c('su')))
  
  # x: y
  impliciet2 = tquery(save='source',
                           children(lemma = quote_punctuation),
                           children(save='quote', relation=c('tag','nucl','sat')))
  
  ## order matters
  list(zegtdat=zegtdat, ystelt=ystelt, xstelt=xstelt, volgens=volgens, noemt=noemt, impliciet=impliciet, impliciet2=impliciet2)
}

#' Returns a list with the clause queries for ALPINO
#'
#' @return A list with rynstax queries, as created with \link{tquery}
#' @export
alpino_clause_queries <- function(){
  ## [passive subject as object] [passive verb with modifier] [object as subject] 
  #passive_conj = tquery(POS = 'verb', 
  #               parents(save='predicate', lemma = dutch$passive_vc),
  #               children(lemma = dutch$passive_mod, 
  #                        children(save='subject', relation='obj1')))
  
  passive = tquery(POS = 'verb', save='verb',
                        parents(save='predicate', lemma = dutch$passive_vc),
                        children(lemma = dutch$passive_mod, 
                                 children(save='subject', relation='obj1')))
  
  ## [subject] [has/is/etc.] [verb] [object]
  perfect = tquery(POS = 'verb', save='verb',
                        parents(save='predicate', lemma = dutch$passive_vc),
                        children(save='subject', relation=c('su')))
  
  ## [subject] [verb] [object]
  active = tquery(save='predicate', POS = 'verb',
                         children(save='subject', relation=c('su')),
                         children(NOT=T, POS = 'adj',                 ## cannot have children that are adj if the key (parent of child)
                                  parents(lemma = dutch$passive_vc))) ## is a passive verb. Excludes reality (e.g., "Joe was defeated") 
  
  ## [subject] [verb] 
  no_object = tquery(save='predicate', POS = 'verb',
                children(save='subject', relation=c('su')))
                         
  list(passive=passive, perfect=perfect, active=active, no_object)
}

