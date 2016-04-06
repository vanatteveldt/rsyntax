library(amcatr)
conn = amcat.connect('http://preview.amcat.nl')
tokens = amcat.gettokens(conn, project=1, articleset=25226, module = 'morphosyntactic')

tokens$id = as.character(tokens$term_id)
tokens$parent = as.character(tokens$parent)
g = graph_from_sentence(tokens, 1)
plot(g)

head(tokens)
save(tokens, file='data/example_tokens_dutchquotes.rda')

