d = readRDS('~/Downloads/de_sample_ams50.RDS')

library(nlpiper)
library(rsyntax)

## inlog info removed

d$id = 1:nrow(d)
d$fulltext = paste(d$title, d$content2, sep='\n\n')
#de_ids = nlpiper::process_async('parzu', ids = d$id, texts = d$fulltext)

tl = list()
for (i in 1:nrow(d)) {
  if (i %% 10 == 0) print(sprintf('%s / %s', i, nrow(d)))
  id = d$id[i]
  tok = suppressMessages(nlpiper::result('parzu', ids = id, format = 'csv'))
  tok$doc_id = id
  tok$sentence = cumsum(tok$id == 1)
  tl[[id]] = tok
}

tokens = data.table::rbindlist(tl)

tokens$parent[tokens$parent == 0] = NA
tokens$parent = as.integer(as.character(tokens$parent))
tokens = as_tokenindex(tokens, token_id = 'id')

plot_tree(tokens, word, lemma, pos, doc_id = 2, sentence=23)
#browseURL('germans.pdf')
View(tokens)


stokens = tokens[list(doc_id = 2, sentence=23)]

sayverbs = c('warnen')
verbquote = tquery(lemma = sayverbs,
                   children(relation = 'subj', label = 'source'),
                   children(label = 'quote'))
direct = tquery(pos = 'V', NOT(lemma = sayverbs),
                children(relation = 'subj', label = 'subject'),
                children(label = 'predicate'))

stokens %>%
  annotate('quotes', verbquote) %>%
  annotate('clauses', direct) %>%
  plot_tree(word, lemma, pos, annotation='quotes')



stokens = tokens[list(doc_id = 2, sentence=23)]

direct = tquery(pos = 'V',
                children(lemma__I = 'islam*', relation = 'subj', label = 'subject'),
                children(label = 'predicate'))

tokens = tokens %>%
  annotate('clauses', direct, overwrite = T)

table(as.character(tokens$lemma[tokens$clauses == 'subject']))


plot_tree(tokens, word, lemma, pos, doc_id = 2, sentence=23)


