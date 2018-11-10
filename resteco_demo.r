## prepare data: split sentences, select sentences that contain terror*, parse with spacy
library(quanteda)
d = readRDS('~/Documents/en_resteco.rds')
d$fulltext = paste(d$headline, d$text, sep='. ')

corp = corpus(d, docid_field = 'id', text_field = 'fulltext')
corp = corpus_reshape(corp, to='sentence')
corp$documents$texts[[2]]

ds = data.frame(id = corp$documents$`_docid`,
                date = corp$documents$date,
                medium = corp$documents$medium,
                text = corp$documents$texts)
ds = ds[grepl('terror', ds$text, ignore.case = T) & grepl('islam|muslim', ds$text, ignore.case = T),]

library(spacyr)
tokens = spacy_parse(as.character(ds$text), dependency=T)

saveRDS(tokens, file='tokens_resteco_demo.rds')

###################################################################
###################################################################

tokens = readRDS('tokens_resteco_demo.rds')
tokens = as_tokenindex(tokens)

tokens = tokens[list(doc_id = unique(tokens$doc_id)[1:100]),]


######################### simplify tree
################## simple anafora
tq = tquery(relation = 'relcl', label='relcl',
            children(lemma = c("who", "that"), label='reference'),
            parents(label = "parent"))


tokens = select_nodes(tokens, tq) %>%
  copy_nodes('parent', new = 'parent_copy', copy_fill = T) %>%
  mutate_nodes('parent_copy', parent = reference$parent, relation = reference$relation) %>%
  remove_nodes('reference', with_fill = T) %>%
  mutate_nodes('relcl', parent = NA, relation = 'ROOT')

tq = tquery(
  children(label='sub', relation = 'nsubj', pos = 'NOUN', depth=Inf),
  children(label='pron', relation = 'nsubj', pos = 'PRON', depth=Inf)
)

## create subset_nodes, to remove nodes where sub$token_id > pron$token_id
tokens = select_nodes(tokens, tquery = tq) %>%
  subset_nodes(sub$token_id < pron$token_id) %>%
  copy_nodes('sub', 'sub_copy', copy_fill = T) %>%
  mutate_nodes('sub_copy', parent = pron$parent) %>%
  remove_nodes('pron')

###################### conjunctions

tokens = tokens %>%
  inherit('conj') %>%
  chop(relation = 'cc')


plot_tree(tokens, sentence_i = 1, pdf_file='test.pdf')
browseURL('test.pdf')

#######################################################################

