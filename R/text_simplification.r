subject_does_by <- function(tokens) {
  # [subject] does something by [doing] [something else]
  #   - [subject] does something
  #   - [subject] [doing] [something else]
  
  tq = tquery(label='verb',
              children(relation='nsubj', label='subject'),
              children(relation='prep', lemma = 'by', label='prep',
                       children(relation='pcomp', label='pcomp')))
  
  select_nodes(tokens, tq) %>%
    copy_nodes('subject', new = 'subject_copy', copy_fill=T) %>%
    mutate_nodes('subject_copy', parent = pcomp$token_id) %>%
    mutate_nodes('pcomp', parent = NA, relation='ROOT') %>%
    remove_nodes('prep')
}

object_for_doing_what <- function(tokens) {
  # something happens to [object] for [doing] [something else] 
  #   - something happens to [object]
  #   - [object] [doing] [something else]
  
  tq = tquery(relation='pobj', label='object',
              children(relation='prep', lemma = c('for','on'), label='prep',
                       children(relation='pcomp', label='what')))
  
  
  select_nodes(tokens, tq) %>%
    copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
    mutate_nodes('object_copy', parent = what$token_id) %>%
    mutate_nodes('what', parent = NA, relation='ROOT') %>%
    remove_nodes('prep')
}


object_for_what <- function(tokens) {
  tq = tquery(label='verb',
              children(relation=c('dobj','nsubjpass'), label='object'),
              children(relation='prep', lemma = c('for','on'), label='prep',
                       children(relation='pobj', label='what')))
  
  print(apply_queries(tokens, tq))
  
   select_nodes(tokens, tq) %>%
      copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
      mutate_nodes('object_copy', parent = prep$token_id) %>%
      mutate_nodes('prep', parent = NA, relation='ROOT') 
}

object_to <- function(tokens) {
  tq = tquery(label='verb',
              children(relation=c('dobj','nsubjpass'), label='object'),
              children(relation='prep', lemma = c('to','with','out'), label='prep'))
  
  select_nodes(tokens, tq) %>%
    copy_nodes('verb', new = 'verb_copy', copy_fill=F) %>%
    copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
    mutate_nodes('object_copy', parent = verb_copy$token_id) %>%
    mutate_nodes('prep', parent=verb_copy$token_id) %>%
    mutate_nodes('verb_copy', parent = NA, relation='ROOT') 
}

conj_compound <- function(tokens, conj_rel='conj', cc_rel='cc') {
  ## Change certain relations between two nodes with the same entity label, 
  ## to compound, to prevent splitting them
  tq = tquery(label='parent', NOT(entity = ''),
              children(label= cc_rel, relation='cc'),
              children(label= conj_rel, relation='conj'))
  
  select_nodes(tokens, tq) %>%
    subset_nodes(subset = conj$entity == parent$entity & conj$entity == cc$entity) %>%
    mutate_nodes(node = 'conj', relation = 'compound') %>%
    mutate_nodes(node = 'cc', relation = 'compound')
}

split_conjunctions <- function(tokens) {
  ## Use different fill settings for long and short distance conjunctions (as a rough heuristic for argument drop)
  no_fill_long_dist = c('acl:relcl','acl','appos','relcl', 'cop', 
                        'advmod','advcl','xcomp','obl','ccomp','aux','det')
  no_fill_short_dist = c('acl:relcl','relcl', 'conj', 'cop')
  
  tokens %>%
    split_UD_conj(min_dist = 3, no_fill=no_fill_long_dist) %>%
    split_UD_conj(no_fill= no_fill_short_dist)
}

function(){

  
library(spacyr)
library(rsyntax)

txt = "Bob and Steve ate bread and drank wine"  
spacy_initialize()
spacy_install()
spacy_download_langmodel()
tokens = spacy_parse(txt, dependency=T)

plot_tree(tokens)

tokens2 = split_conjunctions(tokens)
  

library(magrittr)

txt = "President Donald Trump's fresh efforts to undermine medical and scientific experts and his failure to ask the country to make the sacrifices that could quell the coronavirus are consigning America to a constantly worsening pandemic with no clear route back to health"
txt = "Trump recommitted to that strategy -- which defies the example of nations that have at least temporarily beaten back the virus -- by blasting the US Centers for Disease Control and Prevention guidelines on reopening schools on Wednesday as \"very tough and expensive.\""  
txt = 'Bob Smith, Steve Smith and Dan were best friends'
txt = 'Trump promised to send troops to Afghanistan and Iraq'


tokens = spacyr::spacy_parse(txt, dependency=T)
tokens = as_tokenindex(tokens)

tokens %>% plot_tree()


tq = tquery(pos = 'VERB', label='verb',
            children(relation = 'nsubj', label='subject'),
            children(relation = 'pobj', label='object', req=F),
            children(NOT(relation = 'relcl'), depth=Inf, connected=T,
                     children(relation = 'pobj', req=F, label='object')))

tokens %>%
  split_conjunctions() %>%
  annotate_tqueries('clause', tq=tq) %>%
  plot_tree(annotation='clause')


tokens %>%
  conj_compound() %>%
  object_for_doing_what() %>%
  object_for_what() %>%
  object_to() %>%
  subject_does_by() %>%
  isolate_branch(relation = c('appos','acl','acl:relcl','relcl','advcl')) %>%
  split_conjunctions() %>%
  chop(relation = 'punct') %>%
  plot_tree()

}
