
.check_clause <- function(tokens, nodes, ...) {
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = get_nodes(tokens, nodes, fill = F, token_cols = 'lemma')
    actual = as.character(actual$lemma[actual$.ROLE == name])
    expect_equal(expected, actual)
  }
}

get_quotes_alpino <- function(tokens, block=NULL) {
  rules = alpino_quote_rules()
  apply_rules(tokens, rules, as_chain=T, block = block, check = F)
}

get_clauses_alpino <- function(tokens, block=NULL){
  rules = alpino_clause_rules()
  apply_rules(tokens, rules, as_chain=T, block = block, check = F)
}

test_that("extracting clauses works", {
  tokens = as_tokenindex(tokens_dutchclauses)
  
  tokenindex_columns()
  
  # subject and subject -> verb -> object
  clauses = get_clauses_alpino(tokens[tokens$sentence == 1,])
  annotate_nodes(tokens[tokens$sentence == 1,], clauses, column='test')
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'hebben', subject='en')
  
  # distinguish multiple subjects if the subject is a conjunction
  #clauses = collapse_conjunctions(tokens, clauses, 'subject') 
  #expect_equal(nrow(clauses), 2)
  #.check_clause(tokens, clauses, predicate = 'hebben', subject=c('Jantje','Piet'))
  
  # subject -> verb -> object (with added nonsense)
  clauses = get_clauses_alpino(tokens[tokens$sentence == 2,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'slaan', subject="Jantje")
  
  # passive: subject <- is/becomes verb <- object
  clauses = get_clauses_alpino(tokens[tokens$sentence == 3,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'zijn', subject="Jantje")

  # passive: object <- is/becomes verb <- subject (complex subject; should this be filtered on nouns?)
  clauses = get_clauses_alpino(tokens[tokens$sentence == 4,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'zijn', subject="aanval")
  
  # quote and clause: object says: subject -> verb -> object
  quotes = get_quotes_alpino(tokens[tokens$sentence == 5,])
  clauses = get_clauses_alpino(tokens[tokens$sentence == 5,], block = quotes)
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'slaan', subject="Jantje")
  
  # quote, clause and negation: subject says: subject -> dit not verb -> object
  quotes = get_quotes_alpino(tokens[tokens$sentence == 6,])
  clauses = get_clauses_alpino(tokens[tokens$sentence == 6,], block = quotes)
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'slaan', subject="ik")
})

