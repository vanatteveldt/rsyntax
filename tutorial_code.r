library(spacyr)
spacy_initialize()
tokens = spacy_parse('Mary had a little lamb.', dependency=T)

spacy_parse('Media Research Center vice president Dan Gainor told Fox News that the Times’ front page is a microcosm of bias in the mainstream media.', dependency=T) %>%
  plot_tree()

spacy_parse('Thatcher attacks Argentina', dependency=T) %>%
  plot_tree()


spacy_parse('John gave Bob a book', dependency=T) %>%
  plot_tree()

spacy_parse('John was given a book by Bob', dependency=T) %>%
  plot_tree()


spacy_parse('Bob and John ate bread and drank wine.', dependency=T) %>%
  plot_tree()

spacy_parse('Bob and John ate bread and drank wine.', dependency=T) %>%
  inherit('conj') %>%
  chop(relation = 'cc') %>%
  plot_tree()


tokens = as_tokenindex(tokens)
annotate(tokens, 'annotation', tq)

plot_tree(tokens, token, lemma, pos, entity)



tq = tquery(pos = "VERB", label = "predicate",
       children(relation = "nsubj", label = "subject"))

spacy_parse('Mary had a little lamb.', dependency=T) %>%
  annotate('clauses', active=tq) %>%
  plot_tree(annotation = 'clauses')


library(rsyntax)

quote_queries = spacy_english_quote_queries()
clause_queries = spacy_english_clause_queries()



t = spacy_parse("It's complicated.", dependency=T)
plot_tree(t)


spacy_parse('When the media talks about “moderate Muslims”, they are perpetuating a dangerous narrative of Islam.', dependency=T) %>%
  annotate('quotes', quote_queries) %>%
  annotate('clauses', clause_queries) %>%
  plot_tree(annotation = 'clauses')


tokens = spacy_parse('When the media talks about “moderate Muslims”, they are perpetuating a dangerous narrative of Islam.', dependency=T)
tq = tquery(
  children(label='sub', relation = 'nsubj', pos = 'NOUN', depth=Inf),
  children(label='pron', relation = 'nsubj', pos = 'PRON', depth=Inf)
)
  
## create subset_nodes, to remove nodes where sub$token_id > pron$token_id
select_nodes(tokens, tquery = tq) %>%
  subset_nodes(sub$token_id < pron$token_id) %>%
  copy_nodes('sub', 'sub_copy', copy_fill = T) %>%
  mutate_nodes('sub_copy', parent = pron$parent) %>%
  remove_nodes('pron') %>%
  plot_tree()

tokens = spacy_parse('The outer boxes show the annotations, and the inner boxes show the labels', dependency=T)

tokens %>%
  annotate('quotes', quote_queries) %>%
  annotate('clauses', clause_queries) %>%
  plot_tree(annotation = 'annotation')


tokens %>%
  annotate('annotation', query_name = tq) %>%
  plot_tree(annotation = 'annotation', pdf_file = 'tutorial_plots/workflow.pdf')


tokens
find_nodes(tokens, tquery(NOT(pos = 'VE*'), label='test'), fill=F)


tq = tquery(pos = 'VERB', label='predicate', 
            children(label='subject', relation='nsubj'))
find_nodes(tokens, tquery(pos = 'VER*', label='test'))

plot_tree(tokens, token, lemma, pos, entity, pdf_file='tutorial_plots/plot_tree_example2.pdf')

tq = tquery(pos = 'VERB', label='predicate', 
            children(label='subject', relation='nsubj'))


tokens = annotate(tokens, 'annotation', tq)
  
  
  
  subset(select = c('token_id','token','parent','relation','annotation','annotation_id','annotation_FILL')) %>%
  xtable()

tq

tokens = spacy_parse('Bob bought a house and hired a kitten.', dependency=T)
plot_tree(tokens)

passive = tquery(pos = 'VERB*', label='predicate',
                 children(relation = c('agent'), label = "subject"))

active = tquery(pos = 'VERB*', label='predicate',
                children(relation = c('nsubj', 'nsubjpass'), label = "subject"))

tokens = spacy_parse('Mary loves John, and Mary was loved by John.', dependency=T)
plot_tree(tokens, pdf_file = 'tutorial_plots/passive_active.pdf')


cast_tokens_text(tokens, by = list(subject='subject'), id = 'token', text = 'token', collapse_id = T)

tokens = spacy_parse('Mary loves John, and Mary was loved by John.', dependency=T)
#plot_tree(tokens)

annotate(tokens, 'subject', passive=passive, active=active)

tokens = tokens %>%
  annotate('subject', passive=passive) %>%
  annotate('subject', active=active) 
  plot_tree(annotation = 'subject')




tokens = spacy_parse('John told Mary to go', dependency=T) %>%
  

spacy_parse('John told Mary to go', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file = 'tutorial_plots/xcomp.pdf')

spacy_parse('Hamas fired rockets at Israel, killing 20 civilians', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file = 'tutorial_plots/conj_example.pdf')

spacy_parse('Hamas fired rockets at Israel, killing 20 civilians', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file = 'tutorial_plots/conj_example.pdf')


spacy_parse('Hamas attacked the state of Israel, killing 20 civilians', dependency=T) %>%
  #inherit('advcl') %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file='tutorial_plots/example_reshape.pdf')


spacy_parse('Bob and John ate bread and drank wine.', dependency=T) %>%
  inherit('conj') %>%
  chop(relation = 'cc') %>%
  plot_tree()


spacy_parse('Hamas attacked the state of Israel, killing 20 civilians', dependency=T) %>%
  #inherit('advcl') %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('Hamas attacked Israel to avenge its citizens.', dependency=T) %>%
  inherit('advcl') %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('John attacked Bob to protect his friends.', dependency=T) %>%
  inherit('advcl') %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')

spacy_parse('John attacked Bob to protect his friends and family.', dependency=T) %>%
  inherit(c('advcl','conj')) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('John attacked Bob to protect his friends and to look cool.', dependency=T) %>%
  flatten_conjunctions(c('advcl','conj')) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('Mary loves John, who loves her.', dependency=T) %>%
  flatten_conjunctions('conj', 'cc') %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')

tokens = spacy_parse('Bob hit Steve, which is never very nice.', dependency=T) 
plot_tree(tokens)




tokens = spacy_parse('Hamas attacked the state of Israel, who responded by bombarding Gaza', dependency=T) 
tokens %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file = 'tutorial_plots/relcl_example.pdf')

plot_tree(tokens)

tq = tquery(relation = 'relcl', label='relcl',
            children(lemma = "who", label='reference'),
            parents(label = "parent"))

tokens = select_nodes(tokens, tq) %>%
  copy_nodes('parent', new = 'parent_copy', copy_fill = T) %>%
  mutate_nodes('parent_copy', parent = reference$parent, relation = reference$relation) %>%
  remove_nodes('reference', with_fill = T) %>%
  mutate_nodes('relcl', parent = NA, relation = 'ROOT') %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file = 'tutorial_plots/reshape.pdf')


tq = tquery(label = 'verb',
            children(relation = 'nsubj', label = 'subject'))

tokens = select_nodes(tokens, tq) %>%
  mutate_nodes('subject', parent = verb$parent, relation = verb$relation) %>%
  copy_fill('parent', 'subject') %>%
  remove_nodes('parent')


tokens %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')

tokens = spacy_parse('Hamas attacked the state of Israel, who responded by bombarding Gaza.', dependency=T) 
tokens %>%
  isolate_relcl() %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('Mary loves John, and Mary was loved by John', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file='tutorial_plots/passive_active.pdf')


spacy_parse('Hamas fired rockets at Israel, who responded by bombarding Gaza', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')


agg_action = c('violence','attacks')  ## abbreviated

nominal = tquery(lemma = agg_action, label = "predicate",
                 children(relation = "poss", label = "subject"))

spacy_parse("Israel's excessive violence: UN condemns Israel's attacks", dependency=T) %>%
  annotate('clauses', d = direct, x = xcomp) %>%
  annotate('clauses', n = nominal, block_fill = T) %>%
  plot_tree(annotation = 'clauses')

spacy_parse("Israel's attacks ", dependency=T) %>%
  annotate('clauses', d = direct) %>%
  annotate('clauses', n = nominal, block_fill = T) %>%
  plot_tree(annotation = 'clauses')



spacy_parse("John's love was unparalleled", dependency=T) %>%
  annotate('clauses', n = nominal, d = direct) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('John told Mary to go', dependency=T) %>%
  annotate('clauses', xcomp) %>%
  plot_tree(annotation = 'clauses')


spacy_parse('John told Mary to go', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')

spacy_parse('Mary had a little lamb.', dependency=T) %>%
  annotate('clauses', p=passive, a=active, x=xcomp) %>%
  plot_tree(annotation = 'clauses')



say_verbs = ENGLISH_SAY_VERBS  ## lemma of source indicating verbs (e.g., say, tell, admit)

speechverb = tquery(lemma = say_verbs, label='verb',
                    children(relation=c('nsubj', 'agent'), label='source'),
                    children(label='quote'))
according = tquery(lemma = 'accord', label = 'verb',                
                   children(relation='prep',        
                            children(relation = 'pobj', label='source')),
                   parents(label = 'quote'))

spacy_parse("John said he loves Mary, but according to Bob that's a lie.", dependency=T) %>%
  annotate('quotes', a=according, s=speechverb) %>%
  plot_tree(annotation = 'quotes', pdf_file= 'tutorial_plots/quotes.pdf')

spacy_parse('According to Bob, Steve is sad.', dependency=T) %>%
  annotate('quotes', s=speechverb, a=according) %>%
  plot_tree(annotation = 'quotes')


spacy_parse("John said he loves Mary, but according to Bob that's a lie.", dependency=T) %>%
  annotate('quotes', a=according, s=speechverb) %>%
  plot_tree(annotation = 'quotes', pdf_file= 'tutorial_plots/quotes.pdf')


spacy_parse('John told Mary to go.', dependency=T) %>%
  plot_tree()

direct = tquery(pos = 'VERB*', NOT(lemma = say_verbs), label='predicate',
                children(relation = c('nsubj', 'agent'), label = "subject"))
xcomp = tquery(pos = 'VERB*',
               children(relation = 'xcomp', NOT(lemma=say_verbs), label = "predicate"),
               children(relation = 'dobj', label = "subject"))

spacy_parse("Mary fell in love with Bob, so John told Mary to go.", dependency=T) %>%
  annotate('clauses', d=direct, x=xcomp) %>%
  plot_tree(annotation = 'clauses', pdf_file='tutorial_plots/clauses_ab.pdf')


spacy_parse("The Hamas attacks.", dependency=T) %>%
  plot_tree()


nominal = tquery(lemma = 'accord', label = 'verb',                
                 children(relation='prep',        
                          children(relation = 'pobj', label='source')),
                 parents(label = 'quote'))



tokens = spacy_parse("British Muslim leaders have condemned the terrorist attack in Westminster, with many imams expressing shock and horror", dependency=T)
tokens = as_tokenindex(tokens)
tokens
plot_tree(tokens)

direct = tquery(pos = 'VERB*', NOT(lemma = say_verbs), label='verb',
                fill(relation='aux'),
                children(relation = c('nsubj', 'agent'), label = "subject"),
                children(relation = 'dobj', label='object'))

tokens %>%
  annotate('clauses', d=direct) %>%
  plot_tree(annotation = 'clauses', pdf_file='journalism_abstract.pdf')
