create_test_tokens <- function(libLoc) {
  library(coreNLP)
  coreNLP::initCoreNLP(libLoc = libLoc)
  a = annotateString("John says Mary is great. Pete promised to say he loves Mary. According to Mark, he loves Mary. John loves Mary. Mary is loved by John. Mary is loved.")
  tokens = tokens_from_coreNLP(a)
  save(tokens, file="data/example_tokens_corenlp.rda")
}

#create_test_tokens("/home/wva/stanford-corenlp-full-2015-12-09/")

.check_quote <- function(tokens, qrow, ...) {
  testthat::expect_equal(colnames(qrow), c("id", "source", "quote"))
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = as.character(tokens$lemma[tokens$id == qrow[[name]]])
    testthat::expect_equal(expected, actual)
  }
}
.check_clause <- function(tokens, qrow, ...) {
  testthat::expect_equal(colnames(qrow), c('clause_id', 'subject','predicate'))
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = as.character(tokens$lemma[tokens$id == qrow[[name]]])
    testthat::expect_equal(expected, actual)
  }
}

test_that("extracting quotes works with coreNLP", {
  
  data("example_tokens_corenlp")
  
  #John says Mary is great.
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  testthat::expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="John", quote="great")
  
  #  Pete promised to say he loves Mary
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  testthat::expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="Pete", quote="love")
  
  # According to Mark, he loves Mary.
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  testthat::expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="Mark", quote="love")
  
  # John Loves Mary
  quotes = get_quotes(tokens[tokens$sentence >= 4,])
  testthat::expect_equal(nrow(quotes), 0)

  all_quotes = get_quotes(tokens)
  testthat::expect_equal(nrow(all_quotes), 3)
})

test_that("extracting clauses works with coreNLP", {
  data("example_tokens_corenlp")
  
  all_quotes = get_quotes(tokens)
  
  # John loves Mary
  clauses = get_clauses(tokens[tokens$sentence == 4,], quotes = all_quotes)
  testthat::expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, subject="John", predicate="love")
  
  # Mary is loved by John
  clauses = get_clauses(tokens[tokens$sentence == 5,], quotes = all_quotes)
  testthat::expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, subject="John", predicate="love")
  
  # Mary is loved (passive without subject)
  clauses = get_clauses(tokens[tokens$sentence == 6,], quotes = all_quotes)
  testthat::expect_equal(nrow(clauses), 1)
  testthat::expect_true(is.na(clauses$subject))
  
    .check_clause(tokens, clauses, predicate="love")
  
})