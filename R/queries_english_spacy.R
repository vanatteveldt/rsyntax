
spacy_quotes <- function(tokens, verbs=ENGLISH_SAY_VERBS, exclude_verbs=NULL) {
  direct = tquery(lemma = verbs, NOT(lemma = exclude_verbs), label='verb',
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                  children(label='quote'))
  
  nosrc = tquery(pos='VERB*', 
                 children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                 children(lemma = verbs, NOT(lemma = exclude_verbs), relation='xcomp', label='verb',
                          children(relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), label='quote')))
  
  according = tquery(label='quote',
                     children(relation='nmod:according_to', label='source',
                              children(label='verb')))
  
  tokens = annotate(tokens, 'quote', dir=direct, nos=nosrc, acc=according)
  
  span1 = tquery(pos = 'VERB*', lemma = verbs, 
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  span2 = tquery(pos = 'VERB*', 
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  tokens = add_span_quotes(tokens, 'token', quote_col = 'quote', source_val = 'source', quote_val = 'quote', tqueries=list(span1,span2))
  
  tokens
}

spacy_clauses <- function(tokens, verbs=NULL, exclude_verbs=ENGLISH_SAY_VERBS, with_subject=T, with_object=F, sub_req=T, ob_req=F) {
  subject_name = if (with_subject) 'subject' else NA
  object_name = if (with_object) 'object' else NA
  
  passive = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                   children(relation = c('agent'), label = subject_name, req=sub_req),
                   children(relation = c('nsubjpass','pobj','nsubj'), label=object_name, req=ob_req)) 
  
  direct = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                  not_children(relation = 'auxpass'),
                  children(relation = c('nsubj', 'nsubjpass'), label=subject_name, req=sub_req),
                  children(relation = c('dobj'), label=object_name, req=ob_req)) 
  
  
  copula_direct = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), 
                         parents(label='predicate', NOT(lemma = exclude_verbs),
                                 children(relation = c('su', 'nsubj', 'agent'), label=subject_name, req=sub_req),
                                 children(relation = c('dobj'), label=object_name, req=ob_req))) 
  
  copula_passive = tquery(pos = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs),
                          parents(label='predicate', NOT(lemma = exclude_verbs),
                                  children(relation = c('su', 'nsubj', 'agent'), label=subject_name, req=sub_req),
                                  children(relation = c('dobj'), label=object_name, req=ob_req))) 
  
  
  annotate(tokens, 'clause', pas=passive, dir=direct, cd=copula_direct, cp=copula_passive)
}

spacy_conjunctions <- function(tokens) {
  no_fill = c('compound*','case', 'relcl')
  tq = tquery(label='target', NOT(relation = 'conj'),
              fill(NOT(relation = no_fill), window = c(Inf,0)),
              children(relation = 'conj', label='origin',
                       fill(NOT(relation = no_fill), window=c(0,Inf))))
  tokens = climb_tree(tokens, tq)  
  chop(tokens, relation = 'cc')
}


spacy_relcl <- function(tokens, pron=NULL) {
  if (!is.null(pron)) {
    tq = tquery(relation = c('acl:relcl', 'relcl'), label='relcl',
                children(lemma =  pron, label='reference'),
                parents(label = "parent", pos=c('PROPN','NOUN')))
    
    tokens = select_nodes(tokens, tq)
    tokens = copy_nodes(tokens, 'parent', new = 'parent_copy', copy_fill = T)
    tokens = mutate_nodes(tokens, 'parent_copy', parent = reference$parent, relation = reference$relation)
    tokens = remove_nodes(tokens, 'reference', with_fill = T)
  }
  tq = tquery(relation = c('acl:relcl', 'relcl'), label='relcl',
              parents(label = "parent"))
  tokens = select_nodes(tokens, tq)
  tokens = mutate_nodes(tokens, 'relcl', parent = NA, relation = 'ROOT')
  #tokens = mutate_nodes(tokens, 'relcl', parent = parent$parent, relation = 'iso_rel')
  tokens
}

spacy_simplify <- function(tokens) {
  tokens = spacy_relcl(tokens, pron=c('who','that','which','he','she','it','they'))
  tokens = spacy_conjunctions(tokens)
  tokens
}

spacy_tokenindex <- function(txt) {
  require(spacyr)
  as_tokenindex(spacy_parse(txt, dependency=T))
}

function(){
  sayverbs = c("tell", "show", "acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "think","warn","write", "add")
  
  txt = 'Bob does not consume wine and delicious cheese alone'
  spacy_tokenindex(txt) %>%
    plot_tree(token,pos)
  
  spacy_tokenindex(txt) %>%
    spacy_simplify() %>%
    plot_tree(token,pos)
    

  tokens = spacy_tokenindex(txt) %>%
    spacy_simplify() %>%
    spacy_quotes(verbs = sayverbs) %>%
    spacy_clauses(exclude_verbs = sayverbs) %>%
    chop(relation = 'punct') %>%
    plot_tree(token, pos)
  tokens
    
  cast_tokens_text(tokens, 'token', by=list(quote='source', clause='subject'), id='token', collapse_id = T)
  
  inspect_family(tokens, )
  
  df = readRDS('testdata.rds')
  head(df)
  rawtok = spacy_tokenindex(df$texts)
  saveRDS(rawtok, 'testtokens.rds')
  
  rawtok = readRDS('testtokens.rds')
  tokens = rawtok %>%
    spacy_quotes(verbs = sayverbs)
  
  library(tokenbrowser)
  
  url = highlighted_reader(tokens, tokens$quote=='source', token_col = 'token', filename = '~/Desktop/bronherkenning_demo.html')
  view_reader(url)
  browseURL(url)
  
  
  tokens = rawtok %>%
    spacy_quotes(verbs = sayverbs) %>%
    spacy_simplify() %>%
    spacy_clauses(exclude_verbs = sayverbs) %>%
    chop(relation = 'punct')
  
  testtok = rawtok[rawtok$doc_id==10 & rawtok$sentence==1,]
  plot_tree(testtok, token, pos)
  
  spacy_simplify(testtok) %>%
    plot_tree(token)
  
  spacy_tokenindex('I said hi to Bob and Mary') %>%
    spacy_simplify() %>%
    plot_tree(token)
  
  plot_tree(rawtok, token, sentence_i = 4)
  plot_tree(tokens, token, sentence_i = 4)
  
  plot_tree(tokens, token, sentence_i = 2, pdf_file = '~/Desktop/test.pdf')
  browseURL('~/Desktop/test.pdf')
  
  x = cast_tokens_text(tokens, 'token', by=list(quote='source', clause='subject'), id='token', collapse_id = T)
  head(x,20)
  
  head(tokens)
  
  d = read.csv2('~/Downloads/berichten-17-01-2019_12_37.csv')
  nrow(d)
  head(d)
  
  spacy_simplify() %>%
    spacy_quotes(verbs=sayverbs) %>%
    plot_tree(token, pos, sentence_i = 68)
  
  head(tokens,60)
  
  tokens
  cast_tokens_text(tokens, 'token', by=list(quote='source', clause='subject'), id='token', collapse_id = T)
  cast_tokens_text(tokens, 'token', by=list(clause='subject'), id='token', collapse_id = T)
  
  
  cast_tokens(tokens, by=list(quote='source', clause='subject'), id='token')
  
  dtm = cast_tokens_dfm(tokens, 'lemma', length, by=list(quote='source', clause='subject'), id='token')
  docvars(dtm)
  View(tokens)
  
  
  
  tokens[]
  column = 'quote'
  column_id = 'quote_id'
  group = 'quote'
  group_values='token'
  
  subset(tokens, tokens[[column]] == group, select = c(group_values, column_id))
  
  unique(tokens[,c('quote_id','quote')])
  
  
  ## find quotes
  tokens = tokens %>%
    annotate('quotes', spacy_quote_queries()) %>%
    add_span_quotes()
  
  
  
  
  spacy_tokenindex('The man that loved dogs and birds did not love cats.') %>%
    spacy_simplify() %>%
    plot_tree(token, lemma, pos)
  
  spacy_tokenindex('Dogs and birds were loved by the man that did not like cats.') %>%
    spacy_simplify() %>%
    plot_tree(token, lemma, pos)
  
  spacy_tokenindex('Dogs and birds were loved by the man that did not like cats.') %>%
    spacy_simplify() %>%
    spacy_quotes(verbs=sayverbs) %>% 
    spacy_clauses(exclude_verbs = sayverbs) %>%
    print() %>%
    cast_tokens_text('token', by=list(quote='source', clause='subject'), id='token', collapse_id = T)
  
  
  
  
  
  spacy_tokenindex('Bob did go to the meeting, but did he leave?') %>%
    plot_tree(token, lemma, pos)
  
  
  quote_queries = spacy_english_quote_queries()  
  clause_queries = spacy_english_clause_queries()
  tokens = annotate(tokens, quote_queries, column='quotes', fill=T)
  tokens = annotate(tokens, clause_queries, column='clauses', fill=T)
  tokens
}

