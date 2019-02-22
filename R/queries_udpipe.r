udpipe_quotes <- function(tokens, verbs=NULL, exclude_verbs=NULL) {
  direct = tquery(lemma = verbs, NOT(lemma = exclude_verbs), label='verb',
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                  children(label='quote'))
  
  nosrc = tquery(POS='VERB*', 
                 children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                 children(lemma = verbs, NOT(lemma = exclude_verbs), relation='xcomp', label='verb',
                          children(relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), label='quote')))
  
  according = tquery(lemma = 'accord', label = 'verb',                
                     parents(label = 'source',
                             parents(label = 'quote')))
  
  tokens = annotate(tokens, 'quote', dir=direct, nos=nosrc, acc=according)
  
  ## add span quotes (Bob did not agree. "blablabla".)
  span1 = tquery(POS = 'VERB*', lemma = verbs, 
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  span2 = tquery(POS = 'VERB*', 
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'))
  tokens = add_span_quotes(tokens, 'token', quote_col = 'quote', source_val = 'source', quote_val = 'quote', tqueries=list(span1,span2))
  
  tokens
}

udpipe_clauses <- function(tokens, verbs=NULL, exclude_verbs=NULL) {
  passive = tquery(POS = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                   children(relation = 'aux:pass'),
                   children(relation = c('dobj','obl','obj'), label = 'subject', req=T),
                   children(relation = c('nsubj:pass','nsubj'), label='object', req=F)) 
  
  direct = tquery(POS = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                  not_children(relation = 'aux:pass'),
                  children(relation = c('nsubj','nsubj:pass'), label='subject', req=T),
                  children(relation = c('dobj','obl','obj'), label='object', req=F)) 
  
  nosub_passive = tquery(POS = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                   not_children(relation = c('agent')),
                   children(relation = c('nsubjpass','pobj','nsubj','obl'), label='object')) 
  
  nosub_direct = tquery(POS = 'VERB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                  not_children(relation = 'auxpass'),
                  not_children(relation = c('nsubj', 'nsubjpass')),
                  children(relation = c('dobj','obl'), label='object')) 
  
  annotate(tokens, 'clause', pas=passive, dir=direct, nosub_pas=nosub_passive, nosub_dir=nosub_direct)
}

udpipe_conjunctions <- function(tokens) {
  no_fill = c('compound*','case', 'advmod', 'relcl')
  tokens = inherit(tokens, 'conj', 
                   take_fill = fill(NOT(relation = no_fill), connected=T), 
                   give_fill = fill(NOT(relation = no_fill), connected=T))
  chop(tokens, relation = 'cc')
}


udpipe_relcl <- function(tokens, pron=NULL) {
  if (!is.null(pron)) {
    tq = tquery(relation = c('acl:relcl', 'relcl'), label='relcl',
                children(lemma =  pron, label='reference'),
                parents(label = "parent", POS=c('PROPN','NOUN')))

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

udpipe_simplify <- function(tokens) {
  tokens = udpipe_relcl(tokens, pron=c('who','that','which','he','she','it','they'))
  tokens = udpipe_conjunctions(tokens)
  tokens
}

function(){
  sayverbs = c("tell", "show", "acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "think","warn","write", "add")

  tokens = udpipe_tokenindex('The Times’ front page is a microcosm of bias in the mainstream media.') %>%
    udpipe_simplify() %>%
    udpipe_quotes(verbs = sayverbs) %>%
    udpipe_clauses(exclude_verbs = sayverbs) %>%
    chop(relation = 'punct')
  
  cast_tokens_text(tokens, 'token', by=list(quote='source', clause='subject'), id='token', collapse_id = T)
  
  inspect_family(tokens, )
  
  df = readRDS('testdata.rds')
  head(df)
  rawtok = udpipe_tokenindex(df$texts)
  saveRDS(rawtok, 'testtokens.rds')
  
  rawtok = readRDS('testtokens.rds')
  tokens = rawtok %>%
    udpipe_quotes(verbs = sayverbs)
  
  library(tokenbrowser)
  
  url = highlighted_reader(tokens, tokens$quote=='source', token_col = 'token', filename = '~/Desktop/bronherkenning_demo.html')
  view_reader(url)
  browseURL(url)
  
  
  tokens = rawtok %>%
    udpipe_quotes(verbs = sayverbs) %>%
    udpipe_simplify() %>%
    udpipe_clauses(exclude_verbs = sayverbs) %>%
    chop(relation = 'punct')
  
  testtok = rawtok[rawtok$doc_id==10 & rawtok$sentence==1,]
  plot_tree(testtok, token, POS)
  
  udpipe_simplify(testtok) %>%
    plot_tree(token)
  
  udpipe_tokenindex('I said hi to Bob and Mary') %>%
    udpipe_simplify() %>%
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
  
  udpipe_simplify() %>%
  udpipe_quotes(verbs=sayverbs) %>%
  plot_tree(token, POS, sentence_i = 68)
  
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
    annotate('quotes', udpipe_quote_queries()) %>%
    add_span_quotes()
    
    
  
  
  udpipe_tokenindex('The man that loved dogs and birds did not love cats.') %>%
    udpipe_simplify() %>%
    plot_tree(token, lemma, POS)

  udpipe_tokenindex('Dogs and birds were loved by the man that did not like cats.') %>%
    udpipe_simplify() %>%
    plot_tree(token, lemma, POS)
  
  udpipe_tokenindex('Dogs and birds were loved by the man that did not like cats.') %>%
    udpipe_simplify() %>%
    udpipe_quotes(verbs=sayverbs) %>% 
    udpipe_clauses(exclude_verbs = sayverbs) %>%
    print() %>%
    cast_tokens_text('token', by=list(quote='source', clause='subject'), id='token', collapse_id = T)
  
    

    
  
  udpipe_tokenindex('Bob did go to the meeting, but did he leave?') %>%
    plot_tree(token, lemma, POS)
  
  
  quote_queries = spacy_english_quote_queries()  
  clause_queries = spacy_english_clause_queries()
  tokens = annotate(tokens, quote_queries, column='quotes', fill=T)
  tokens = annotate(tokens, clause_queries, column='clauses', fill=T)
  tokens
}