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
    actual = get_nodes(tokens, nodes, token_cols = 'token')
    #cat(name, ': ', as.character(actual$token[actual$.ROLE == name]), '\n')
    actual = as.character(actual$token[actual$.ROLE == name])
    expect_equal(expected, actual)
  }
}


test_that("extracting quotes works with coreNLP", {
  tokens = as_tokenindex(tokens_corenlp)
  library(testthat)
  #John says Mary is great.
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  testthat::expect_equal(nrow(quotes), 6)
  .check(tokens, quotes, source="John", verb="says", quote=c("Mary",'is','great','.'))
  
  #  Pete promised to say he loves Mary
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  testthat::expect_equal(nrow(quotes), 8)
  .check(tokens, quotes, source="Pete", verb="promised", quote=c('to','say','he','loves','Mary','.'))
  
  # According to Mark, he loves Mary.
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  testthat::expect_equal(nrow(quotes), 8)
  .check(tokens, quotes, source="Mark", verb=c('According','to'), quote=c(',','he','loves','Mary','.'))
  
  # John Loves Mary
  quotes = get_quotes(tokens[tokens$sentence >= 4,])
  testthat::expect_equal(nrow(quotes), 0)

  all_quotes = get_quotes(tokens)
  testthat::expect_equal(nrow(all_quotes), 22)
})

test_that("extracting clauses works with coreNLP", {
  tokens = as_tokenindex(tokens_corenlp)
  
  # John loves Mary
  clauses = get_clauses(tokens[tokens$sentence == 4,])
  testthat::expect_equal(nrow(clauses), 4)
  .check(tokens, clauses, subject="John", predicate=c("loves",'.'), object='Mary')
  
  # Mary is loved by John
  clauses = get_clauses(tokens[tokens$sentence == 5,])
  annotate(tokens[tokens$sentence == 5,], 'test', corenlp_clause_queries())
  testthat::expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, subject=c("by","John"), predicate=c("is","loved","."), object='Mary')
  
  # Mary is loved (passive without subject)
  clauses = get_clauses(tokens[tokens$sentence == 6,])
  testthat::expect_equal(nrow(clauses), 4)
  .check(tokens, clauses, predicate=c("is","loved","."), object='Mary')
  
})
