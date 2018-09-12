context("Corenlp")


get_quotes <- function(tokens, block=NULL) {
  queries = corenlp_quote_queries()
  apply_queries(tokens, queries, as_chain=T, block = block, check = F)
}

get_clauses <- function(tokens, block=NULL){
  queries = corenlp_clause_queries(with_object = T)
  apply_queries(tokens, queries, as_chain=T, block = block, check = F)
}

.check <- function(tokens, nodes, ...) {
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = get_nodes(tokens, nodes, fill = F, token_cols = 'lemma')
    #print(actual)
    actual = as.character(actual$lemma[actual$.ROLE == name])
    #print(paste(name, '=', actual))
    expect_equal(expected, actual)
  }
}


test_that("extracting quotes works with coreNLP", {
  tokens = as_tokenindex(tokens_corenlp)
  library(testthat)
  #John says Mary is great.
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  testthat::expect_equal(nrow(quotes), 2)
  .check(tokens, quotes, source="John", verb="say")
  
  #  Pete promised to say he loves Mary
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  testthat::expect_equal(nrow(quotes), 2)
  .check(tokens, quotes, source="Pete", verb="promise")
  
  # According to Mark, he loves Mary.
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  testthat::expect_equal(nrow(quotes), 1)
  .check(tokens, quotes, source="Mark", verb='accord', quote="love")
  
  # John Loves Mary
  quotes = get_quotes(tokens[tokens$sentence >= 4,])
  testthat::expect_equal(nrow(quotes), 0)

  all_quotes = get_quotes(tokens)
  testthat::expect_equal(nrow(all_quotes), 5)
})

test_that("extracting clauses works with coreNLP", {
  tokens = as_tokenindex(tokens_corenlp)
  
  # John loves Mary
  clauses = get_clauses(tokens[tokens$sentence == 4,])
  testthat::expect_equal(nrow(clauses), 1)
  clauses
  .check(tokens, clauses, subject="John", predicate="love", object='Mary')
  
  # Mary is loved by John
  clauses = get_clauses(tokens[tokens$sentence == 5,])
  testthat::expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, subject="John", predicate="love", object='Mary')
  
  # Mary is loved (passive without subject)
  clauses = get_clauses(tokens[tokens$sentence == 6,])
  testthat::expect_equal(nrow(clauses), 1)
  .check(tokens, clauses, predicate="love", object='Mary')
  
})
