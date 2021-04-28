context("Corenlp")

ENGLISH_SAY_VERBS = c("tell", "show", " acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "proclaim", "promise", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "write", "add")

corenlp_quote_queries <- function(verbs=ENGLISH_SAY_VERBS, exclude_verbs=NULL) {
  direct = tquery(lemma = verbs, NOT(lemma = exclude_verbs), label='verb',
                  children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'), 
                  children(label='quote'))
  
  nosrc = tquery(POS='VB*', 
                 children(relation= c('su', 'nsubj', 'agent', 'nmod:agent'), label='source'),
                 children(lemma = verbs, NOT(lemma = exclude_verbs), relation='xcomp', label='verb',
                          children(relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), label='quote')))
  
  according = tquery(label='quote',
                     children(relation='nmod:according_to', label='source',
                              children(label='verb')))
  
  
  list(direct=direct, nosrc=nosrc, according=according)
}

corenlp_clause_queries <- function(verbs=NULL, exclude_verbs=ENGLISH_SAY_VERBS, with_subject=TRUE, with_object=FALSE, sub_req=TRUE, ob_req=FALSE) {
  subject_name = if (with_subject) 'subject' else NA
  object_name = if (with_object) 'object' else NA
  
  #tokens = as_tokenindex(tokens_corenlp)
  
  direct = tquery(POS = 'VB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                  children(relation = c('su', 'nsubj', 'agent'), label=subject_name, req=sub_req),
                  children(relation = c('dobj'), label=object_name, req=ob_req)) 
  
  
  passive = tquery(POS = 'VB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                   children(relation = 'auxpass'),
                   children(relation = 'nmod:agent', label=subject_name, req=FALSE),
                   children(relation = 'nsubjpass', label=object_name, req=ob_req)) 
  
  copula_direct = tquery(POS = 'VB*', lemma = verbs, NOT(lemma = exclude_verbs),
                         parents(label='predicate',
                                 children(relation = c('su', 'nsubj', 'agent'), label=subject_name, req=sub_req),
                                 children(relation = c('dobj'), label=object_name, req=ob_req))) 
  
  copula_passive = tquery(POS = 'VB*', lemma = verbs, NOT(lemma = exclude_verbs),
                          parents(label='predicate',
                                  children(relation = c('su', 'nsubj', 'agent'), label=subject_name, req=sub_req),
                                  children(relation = c('dobj'), label=object_name, req=ob_req))) 
  
  
  list(direct=direct, passive=passive, copula_direct=copula_direct, copula_passive=copula_passive)
}


get_quotes <- function(tokens, block=NULL) {
  queries = corenlp_quote_queries()
  apply_queries(tokens, queries, as_chain=TRUE, block = block, check = FALSE)
}

get_clauses <- function(tokens, block=NULL){
  queries = corenlp_clause_queries(with_object = TRUE)
  apply_queries(tokens, queries, as_chain=TRUE, block = block, check = FALSE)
}

.check <- function(tokens, nodes, ...) {
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = get_nodes(tokens, nodes, token_cols = 'token')
    #cat(name, ': ', as.character(actual$token[actual$.ROLE == name]), '\n')
    actual = as.character(actual$token[actual$.ROLE == name])
    expect_equal(expected, actual)
  }
}


test_that("extracting quotes works with coreNLP", {
  tokens = as_tokenindex(tokens_corenlp)
  library(testthat)
  #John says Mary is great.
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  testthat::expect_equal(nrow(quotes), 6)
  
  .check(tokens, quotes, source="John", verb="says", quote=c("Mary",'is','great','.'))
  
  #  Pete promised to say he loves Mary
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  testthat::expect_equal(nrow(quotes), 8)
  .check(tokens, quotes, source="Pete", verb="promised", quote=c('to','say','he','loves','Mary','.'))
  
  # According to Mark, he loves Mary.
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  testthat::expect_equal(nrow(quotes), 8)
  .check(tokens, quotes, source="Mark", verb=c('According','to'), quote=c(',','he','loves','Mary','.'))
  
  # John Loves Mary
  quotes = get_quotes(tokens[tokens$sentence >= 4,])
  testthat::expect_equal(nrow(quotes), 0)

  all_quotes = get_quotes(tokens)
  testthat::expect_equal(nrow(all_quotes), 22)
})

test_that("extracting clauses works with coreNLP", {
  tokens = as_tokenindex(tokens_corenlp)
  
  # John loves Mary
  clauses = get_clauses(tokens[tokens$sentence == 4,])
  testthat::expect_equal(nrow(clauses), 4)
  .check(tokens, clauses, subject="John", predicate=c("loves",'.'), object='Mary')
  
  # Mary is loved by John
  clauses = get_clauses(tokens[tokens$sentence == 5,])
  annotate_tqueries(tokens[tokens$sentence == 5,], 'test', corenlp_clause_queries())
  testthat::expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, subject=c("by","John"), predicate=c("is","loved","."), object='Mary')
  
  # Mary is loved (passive without subject)
  clauses = get_clauses(tokens[tokens$sentence == 6,])
  testthat::expect_equal(nrow(clauses), 4)
  .check(tokens, clauses, predicate=c("is","loved","."), object='Mary')
  
})
