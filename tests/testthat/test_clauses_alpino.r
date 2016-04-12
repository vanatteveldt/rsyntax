.check_clause <- function(tokens, qrow, ...) {
  expect_equal(colnames(qrow), c('clause_id', 'subject','predicate'))
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = as.character(tokens$lemma[tokens$id %in% qrow[[name]]])
    expect_equal(expected, actual)
  }
}

test_that("extracting clauses works", {
  data(example_tokens_dutchclauses)

  # subject and subject -> verb -> object
  clauses = get_clauses_nl(tokens[tokens$sentence == 1,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'hebben', subject='en')
  
  # distinguish multiple subjects if the subject is a conjunction
  clauses = collapse_conjunctions(tokens, clauses, 'subject') 
  expect_equal(nrow(clauses), 2)
  .check_clause(tokens, clauses, predicate = 'hebben', subject=c('Jantje','Piet'))
  
  # subject -> verb -> object (with added nonsense)
  clauses = get_clauses_nl(tokens[tokens$sentence == 2,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'slaan', subject="Jantje")
  
  # passive: subject <- is/becomes verb <- object
  clauses = get_clauses_nl(tokens[tokens$sentence == 3,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'zijn', subject="Jantje")

  # passive: object <- is/becomes verb <- subject (complex subject; should this be filtered on nouns?)
  clauses = get_clauses_nl(tokens[tokens$sentence == 4,])
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'zijn', subject="aanval")
  
  # quote and clause: object says: subject -> verb -> object
  quotes = get_quotes_nl(tokens[tokens$sentence == 5,])
  clauses = get_clauses_nl(tokens[tokens$sentence == 5,], quotes)
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'slaan', subject="Jantje")
  
  # quote, clause and negation: subject says: subject -> dit not verb -> object
  quotes = get_quotes_nl(tokens[tokens$sentence == 6,])
  clauses = get_clauses_nl(tokens[tokens$sentence == 6,], quotes)
  expect_equal(nrow(clauses), 1)
  .check_clause(tokens, clauses, predicate = 'slaan', subject="ik")
})



function() {
  ## clauses opnieuw draaien want parsing is natuurlijk stom (doordat zinnen niet goed gesplitst zijn)
  data(example_tokens_dutchclauses)
  quotes = get_quotes_nl(tokens)
  clauses = get_clauses_nl(tokens, quotes)
  
  tokens = tokenClauseAnnotation(tokens, clauses)
  tokens[,c('word','sentence','pos','relation','clause_role')]
  
  
  
  fill(clauses$subject, tokens)
  
  tokens$clause_id = interaction(tokens$aid, tokens$sentence)
  
  cat(as.character(tokens$word[match(fill(clauses$predicate[1], tokens)$filled, tokens$id)]))
  

  par(mar=c(0,0,0,0))  
  plot(graph_from_sentence(tokens[tokens$sentence == 1,]))
  cat(as.character(tokens$word[tokens$sentence == 1]))
  plot(graph_from_sentence(tokens[tokens$sentence == 2,]))
  cat(as.character(tokens$word[tokens$sentence == 2]))
  plot(graph_from_sentence(tokens[tokens$sentence == 3,]))
  cat(as.character(tokens$word[tokens$sentence == 3]))
  plot(graph_from_sentence(tokens[tokens$sentence == 4,]))
  cat(as.character(tokens$word[tokens$sentence == 4]))
  plot(graph_from_sentence(tokens[tokens$sentence == 5,]))
  cat(as.character(tokens$word[tokens$sentence == 5]))
  plot(graph_from_sentence(tokens[tokens$sentence == 6,]))
  cat(as.character(tokens$word[tokens$sentence == 6]))
  plot(graph_from_sentence(tokens[tokens$sentence == 7,]))
  cat(as.character(tokens$word[tokens$sentence == 7]))
  plot(graph_from_sentence(tokens[tokens$sentence == 8,]))
  cat(as.character(tokens$word[tokens$sentence == 8]))
  plot(graph_from_sentence(tokens[tokens$sentence == 9,]))
  cat(as.character(tokens$word[tokens$sentence == 9]))
  
  #data(example_tokens_dutchquotes)
  #quotes = get_quotes_nl(tokens)
}

# Jantje kreeg een klap van Piet
# Jantje is boos op Piet (om te kijken of passive1 klopt)
# Piet is gelagen in opdracht van Jantje.
# Piet is gelagen vanwege zijn slechte karakter.
# De aanval van Jantje heeft Piet veel pijn gedaan.

