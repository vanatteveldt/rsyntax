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
    actual = get_nodes(tokens, nodes, fill = F, token_cols = 'lemma')
    actual = as.character(actual$lemma[actual$.ROLE == name])
    expect_equal(expected, actual)
  }
}

test_that("extracting sources works", {
  tokens = as_tokenindex(tokens_dutchquotes)
  
  # 1 : Rutte stelt : " Een stem is verloren " . 
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="Rutte", quote="zijn")
  
  # 2 : Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte : " Een stem op de PVV , is een verloren stem " . 
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="Rutte", quote="zijn")
  
  # 3 : " Verkiezingsblabla " , zegt PvdA-Kamerlid Kuiken . 
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="PvdA_kamer_lid", quote="Verkiezingsblabla")
  
  # 4 : Minister Spies zei dat de PvdA een " Kapitale blunder " had begaan . 
  quotes = get_quotes(tokens[tokens$sentence == 4,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="minister", quote="hebben")
  
  # 5 : Begrotingstekort is volgens CPB volgend jaar 3.7 procent . 
  quotes = get_quotes(tokens[tokens$sentence == 5,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="CPB", quote="zijn")
  
  # 6 : Hij veroordeelde het avontuur : " Alsof Nederland een politiek laboratorium is " . 
  quotes = get_quotes(tokens[tokens$sentence == 6,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="hij", quote="alsof")
  
  # 7 : VVD : Doe recht aan alle werkenden , betaald of onbetaald . 
  quotes = get_quotes(tokens[tokens$sentence == 7,])
  expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="VVD", quote="doen")
})



test_that("extracting clauses works", {
  tokens = as_tokenindex(tokens_dutchclauses)
  
  find_nodes(tokens[tokens$sentence == 1,], POS='verb', save='pred',
             children(relation='su', save='subject',
                      children(relation='cnj', save='conj')))
  
  clauses = get_clauses(tokens)
  
  #View(annotate_nodes(tokens,clauses,'clause'))
  
  # subject and subject -> verb -> object
  clauses = get_clauses(tokens[tokens$sentence == 1,])
  annotate_nodes(tokens[tokens$sentence == 1,], clauses, column='test')
  expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate = 'hebben', subject='en')
  
  # distinguish multiple subjects if the subject is a conjunction
  #clauses = collapse_conjunctions(tokens, clauses, 'subject') 
  #expect_equal(nrow(clauses), 2)
  #.check(tokens, clauses, predicate = 'hebben', subject=c('Jantje','Piet'))
  
  # subject -> verb -> object (with added nonsense)
  clauses = get_clauses(tokens[tokens$sentence == 2,])
  expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate = 'slaan', subject="Jantje")
  
  # passive: subject <- is/becomes verb <- object
  clauses = get_clauses(tokens[tokens$sentence == 3,])
  expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate = c('slaan'), subject="Jantje")

  # passive: object <- is/becomes verb <- subject (complex subject; should this be filtered on nouns?)
  clauses = get_clauses(tokens[tokens$sentence == 4,])
  expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate = c('schrikken'), subject="aanval")
  
  # quote and clause: object says: subject -> verb -> object
  clauses = get_clauses(tokens[tokens$sentence == 5,])
  expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate = c('hebben'), subject="Jantje")
  
  # quote, clause and negation: subject says: subject -> dit not verb -> object
  quotes = get_quotes(tokens[tokens$sentence == 6,])
  clauses = get_clauses(tokens[tokens$sentence == 6,], block = quotes)
  expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate = 'slaan', subject="ik")
})

