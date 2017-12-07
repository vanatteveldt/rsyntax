test_that("Aliases", {
  tokens = as_tokenindex(tokens_dutchquotes)
  
  
  quotes = get_quotes_alpino(tokens)
  clauses = get_clauses_alpino(tokens)
  quotes_nodes = get_nodes(tokens, quotes)
  clauses_nodes = get_nodes(tokens, clauses)
  tokens_quotes = annotate(tokens, quotes, 'quote')
  tokens_clauses = annotate(tokens, clauses, 'clause')
  
  ## not with aliases
  tokens = as_tokenindex(tokens_dutchquotes)
  tokenindex_columns(doc_id = 'test', token_id = 'alternative', parent = 'column', relation = 'names')
  setnames(tokens, c('doc_id','token_id','parent','relation'), c('test','alternative','column','names'))
  
  quotes2 = get_quotes_alpino(tokens)
  clauses2 = get_clauses_alpino(tokens)
  quotes_nodes2 = get_nodes(tokens, quotes2)
  clauses_nodes2 = get_nodes(tokens, clauses2)
  tokens_quotes2 = annotate(tokens, quotes2, 'quote')
  tokens_clauses2 = annotate(tokens, clauses2, 'clause')
  
  expect_identical(dim(quotes),dim(quotes2))
  expect_identical(dim(clauses),dim(clauses2))
  expect_identical(dim(quotes_nodes),dim(quotes_nodes2))
  expect_identical(dim(clauses_nodes),dim(clauses_nodes2))
  expect_true('quote' %in% colnames(tokens_quotes))
  expect_true('quote' %in% colnames(tokens_quotes2))
  expect_true('clause' %in% colnames(tokens_clauses))
  expect_true('clause' %in% colnames(tokens_clauses2))
  
  tokenindex_columns()  ## reset
})


