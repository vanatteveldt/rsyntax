context("Clauses Alpino")

get_quotes <- function(tokens, block=NULL) {
  queries = alpino_quote_queries()
  apply_queries(tokens, queries, as_chain=T, block = block, check = F)
}

get_clauses <- function(tokens, block=NULL){
  queries = alpino_clause_queries()
  apply_queries(tokens, queries, as_chain=T, block = block, check = F)
}

.check <- function(tokens, nodes, ...) {
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = get_nodes(tokens, nodes, token_cols = 'token')
    #at(name, ': ', as.character(actual$token[actual$.ROLE == name]), '\n')
    actual = as.character(actual$token[actual$.ROLE == name])
    expect_equal(expected, actual)
  }
}

test_that("extracting sources works", {
  tokens = as_tokenindex(tokens_dutchquotes)
  library(testthat)
  #plot_tree(tokens, sentence_i=7, token, allign_text = T)
  
  # 1 : Rutte stelt : " Een stem is verloren " . 
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  expect_equal(nrow(quotes), 5)
  .check(tokens, quotes, source="Rutte", quote=c('Een','stem','is','verloren'))
  
  # 2 : Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte : " Een stem op de PVV , is een verloren stem " . 
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  expect_equal(nrow(quotes), 10)
  .check(tokens, quotes, source="Rutte", quote=c('Een','stem','op','de','PVV','is','een','verloren','stem'))
  
  # 3 : " Verkiezingsblabla " , zegt PvdA-Kamerlid Kuiken . 
  plot_tree(tokens, sentence_i = 3, token)
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  #find_nodes2(tokens[tokens$sentence == 3,], alpino_quote_queries()[[3]])
  #find_nodes(tokens[tokens$sentence == 3,], alpino_quote_queries()[[3]])
  
  expect_equal(nrow(quotes), 8)
  .check(tokens, quotes, source=c("PvdA-Kamerlid","Kuiken"), verb='zegt', quote=c('"','Verkiezingsblabla','"',',','.'))
  
  # 4 : Minister Spies zei dat de PvdA een " Kapitale blunder " had begaan . 
  quotes = get_quotes(tokens[tokens$sentence == 4,])
  expect_equal(nrow(quotes), 14)
  .check(tokens, quotes, source=c("Minister","Spies"), verb=c('zei','dat','"','"','.'), quote=c('de','PvdA','een','Kapitale','blunder','had','begaan'))
  
  # 5 : Begrotingstekort is volgens CPB volgend jaar 3.7 procent . 
  quotes = get_quotes(tokens[tokens$sentence == 5,])
  expect_equal(nrow(quotes), 9)
  .check(tokens, quotes, source="CPB", quote=c('Begrotingstekort','is','volgend','jaar','3.7','procent','.'))
  
  # 6 : Hij veroordeelde het avontuur : " Alsof Nederland een politiek laboratorium is " . 
  quotes = get_quotes(tokens[tokens$sentence == 6,])
  expect_equal(nrow(quotes), 14)
  .check(tokens, quotes, source="Hij", verb=c('veroordeelde','het','avontuur',':','"','"','.'), quote=c('Alsof','Nederland','een','politiek','laboratorium','is'))
  
  # 7 : VVD : Doe recht aan alle werkenden , betaald of onbetaald . 
  quotes = get_quotes(tokens[tokens$sentence == 7,])
  expect_equal(nrow(quotes), 12)
  .check(tokens, quotes, source=c("VVD",':',',','.'), quote=c('Doe','recht','aan','alle','werkenden','betaald','of','onbetaald'))
})



test_that("extracting clauses works", {
  tokens = as_tokenindex(tokens_dutchclauses)

  tq = tquery(save='target', 
              children(relation = 'cnj', save='conj'))
  .tokens = tokens %>%
    flatten_conjunctions(tq=tq, target_is_cc = T) %>%
    plot_tree(token)
  
  find_nodes(tokens[tokens$sentence == 1,], 
             tquery(POS='verb', save='pred',
                   children(relation='su', save='subject',
                            children(relation='cnj', save='conj'))))
  
  clauses = get_clauses(tokens)
  
  #View(annotate_nodes(tokens,clauses,'clause'))
  
  # subject and subject -> verb -> object
  clauses = get_clauses(tokens[tokens$sentence == 1,])
  annotate_nodes(tokens[tokens$sentence == 1,], clauses, column='test')
  expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, subject=c('Jantje','en','Piet'), predicate = c('hebben','ruzie','.'))
  
  # subject -> verb -> object (with added nonsense)
  clauses = get_clauses(tokens[tokens$sentence == 2,])
  expect_equal(nrow(clauses), 10)
  .check(tokens, clauses, subject='Jantje', predicate = c('slaat','Piet','op','zijn','hoofd','met','een','hamer','.'))
  
  # passive: subject <- is/becomes verb <- object
  clauses = get_clauses(tokens[tokens$sentence == 3,])
  expect_equal(nrow(clauses), 4)
  .check(tokens, clauses, predicate = c('Piet','geslagen','door'), subject='Jantje')

  # passive: object <- is/becomes verb <- subject (complex subject; should this be filtered on nouns?)
  clauses = get_clauses(tokens[tokens$sentence == 4,])
  expect_equal(nrow(clauses), 7)
  .check(tokens, clauses, predicate = c('Piet','geschrokken','door'), subject=c('de',"aanval",'van','Jantje'))
  
  # quote and clause: object says: subject -> verb -> object
  clauses = get_clauses(tokens[tokens$sentence == 5,])
  expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, predicate = c('hem','heel','hard','geslagen','heeft'), subject="Jantje")
  
  # quote, clause and negation: subject says: subject -> dit not verb -> object
  quotes = get_quotes(tokens[tokens$sentence == 6,])
  clauses = get_clauses(tokens[tokens$sentence == 6,])
  expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, subject='ik', predicate = c('heb','Piet','niet','zomaar','geslagen'))
})

