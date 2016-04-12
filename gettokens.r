library(amcatr)
conn = amcat.connect('http://preview.amcat.nl')
tokens = amcat.gettokens(conn, project=1, articleset=25268, module = 'morphosyntactic', 
                         only_cached = T, page_size = 10, max_page=1)
head(tokens)


us = interaction(tokens$aid, tokens$sentence)
for(s in unique(us)[1:100]){
  cat('\n\n')
  cat(s, as.character(tokens$word)[us == s])
}
tokens

demotok = NULL
#load('data/example_tokens_dutchclauses.rda')
demotok = tokens
demotok  = rbind(demotok, tokens[us == 159367246.98,])
demotok  = rbind(demotok, tokens[us == 156936879.69,])
demotok  = rbind(demotok, tokens[us == 159367246.73,])
demotok  = rbind(demotok, tokens[us == 156936879.54,])

tokens = demotok
#save(tokens, file='data/example_tokens_dutchclauses.rda')

data(example_tokens_dutchclauses)
tokens

tokens$sentence = match(tokens$sentence, unique(tokens$sentence))

tokens = unique_ids(tokens)

head(tokens)
?amcat.gettokens
tokens$id = as.character(tokens$term_id)
tokens$parent = as.character(tokens$parent)
g = graph_from_sentence(tokens, 1)
plot(g)

head(tokens)
save(tokens, file='data/example_tokens_dutchclauses.rda')
