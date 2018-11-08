d = readRDS('~/Downloads/de_sample_ams50.RDS')

library(nlpiper)
library(rsyntax)

d$id = 1:nrow(d)
d$fulltext = paste(d$title, d$content2, sep='\n\n')
#de_ids = nlpiper::process_async('parzu', ids = d$id, texts = d$fulltext)


library(corpustools)
tc = create_tcorpus('Ich bin ein Berliner', udpipe_model='german', use_parser=T)
tc$tokens

tl = list()
for (id in d$id) {
  tok = suppressMessages(nlpiper::result('parzu', ids = id, format = 'csv'))
  tok$doc_id = id
  tok$sentence = cumsum(tok$id == 1)
  tl[[id]] = tok
}

tokens = data.table::rbindlist(tl)
tokens$parent[tokens$parent == 0] = NA
tokens$parent = as.integer(as.character(tokens$parent))
tokens = as_tokenindex(tokens, token_id = 'id')

plot_tree(tokens, sentence_i = 4)
browseURL('germans.pdf')
View(tokens)
