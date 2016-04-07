context("String length")

.check_quote <- function(tokens, qrow, ...) {
  expect_equal(colnames(qrow), c("id", "source", "quote"))
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = as.character(tokens$lemma[tokens$id == qrow[[name]]])
    expect_equal(expected, actual)
  }
}

test_that("extracting sources works", {
  data(example_tokens_dutchquotes)

  # 1 : Rutte stelt : " Een stem is verloren " . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 1,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="Rutte", quote="zijn")

  # 2 : Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte : " Een stem op de PVV , is een verloren stem " . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 2,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="Rutte", quote="zijn")

  # 3 : " Verkiezingsblabla " , zegt PvdA-Kamerlid Kuiken . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 3,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="PvdA_kamer_lid", quote="Verkiezingsblabla")
  
  # 4 : Minister Spies zei dat de PvdA een " Kapitale blunder " had begaan . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 4,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="minister", quote="hebben")
  
  # 5 : Begrotingstekort is volgens CPB volgend jaar 3.7 procent . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 5,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="CPB", quote="zijn")
  
  # 6 : Hij veroordeelde het avontuur : " Alsof Nederland een politiek laboratorium is " . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 6,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="hij", quote="alsof")
  
  # 7 : VVD : Doe recht aan alle werkenden , betaald of onbetaald . 
  quotes = get_quotes_nl(tokens[tokens$sentence == 7,])
  expect_equal(nrow(quotes), 1)
  .check_quote(tokens, quotes, source="VVD", quote="doen")
})

function() {
  data(example_tokens_dutchquotes)
  tokens$offset = as.numeric(as.character(tokens$offset))
  tokens = tokens[tokens$sentence == 6,]
  for(i in 1:7)   cat("#", i, ":", rsyntax::get_text(tokens[tokens$sentence == i, ]), "\n")
  rsyntax::get_text(tokens[tokens$sentence == 2, ])
  rsyntax::get_text(tokens[tokens$sentence == 3, ])
  rsyntax::get_text(tokens[tokens$sentence == 4, ])
  rsyntax::get_text(tokens[tokens$sentence == 5, ])
  rsyntax::get_text(tokens[tokens$sentence == 6, ])
  rsyntax::get_text(tokens[tokens$sentence == 7, ])
  rsyntax::get_text(tokens[tokens$sentence == 1, ])
  plot(graph_from_sentence(tokens))
}