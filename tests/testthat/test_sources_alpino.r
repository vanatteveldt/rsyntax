.check_quote <- function(tokens, nodes, ...) {
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


test_that("extracting sources works", {
  tokens = as_tokenindex(tokens_dutchquotes)

  # 1 : Rutte stelt : " Een stem is verloren " . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 1,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="Rutte", quote="zijn")

  # 2 : Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte : " Een stem op de PVV , is een verloren stem " . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 2,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="Rutte", quote="zijn")

  # 3 : " Verkiezingsblabla " , zegt PvdA-Kamerlid Kuiken . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 3,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="PvdA_kamer_lid", quote="Verkiezingsblabla")
  
  # 4 : Minister Spies zei dat de PvdA een " Kapitale blunder " had begaan . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 4,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="minister", quote="hebben")
  
  # 5 : Begrotingstekort is volgens CPB volgend jaar 3.7 procent . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 5,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="CPB", quote="zijn")
  
  # 6 : Hij veroordeelde het avontuur : " Alsof Nederland een politiek laboratorium is " . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 6,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="hij", quote="alsof")
  
  # 7 : VVD : Doe recht aan alle werkenden , betaald of onbetaald . 
  quotes = get_quotes_alpino(tokens[tokens$sentence == 7,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="VVD", quote="doen")
})

