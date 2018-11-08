ENGLISH_SAY_VERBS = c("tell", "show", "acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "think","warn","write", "add")

#' Returns a list with the quote queries for spacy_english_
#'
#' @param verbs         A character vector with verbs used to indicate quotes ("say", "report", "admit", etc.). A default list of verbs is provided
#'                      in ENGLISH_SAY_VERBS. If NULL, all verbs are used (except those listed in exclude verbs)
#' @param exclude_verbs A character vector with verbs that are exluded. If NULL, no verbs are excluded.
#'
#' @return A list with rynstax queries, as created with \link{tquery}
#' @export
spacy_english_quote_queries <- function(verbs=ENGLISH_SAY_VERBS, exclude_verbs=NULL) {
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
  
  
  list(direct=direct, nosrc=nosrc, according=according)
}



#' Returns a list with the clause queries for spacy_english_
#'
#' @param verbs         A character vector with verbs used to indicate clauses. If NULL (default), all verbs are used (except those listed in exclude verbs)
#' @param exclude_verbs A character vector with verbs that are not used in clauses. By default, this is the list of ENGLISH_SAY_VERBS, 
#'                      which are the verbs used in the spacy_english__quote_queries(). If set to NULL, no verbs are excluded.
#' @param with_subject  If True, subject nodes will be "labeld" as "subject" 
#' @param with_object   Same for object nodes
#' @param sub_req       If True, subject nodes are required (must be matched). By default this is true.
#' @param ob_req        Same for object nodes. By default this is false.
#' 
#' @return a data.table with nodes (as .G_ID) for id, subject and predicate
#' @export
spacy_english_clause_queries <- function(verbs=NULL, exclude_verbs=ENGLISH_SAY_VERBS, with_subject=T, with_object=F, sub_req=T, ob_req=F) {
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
  
  
  list(p=passive, d=direct, cd=copula_direct, cp=copula_passive)
}

spacy_english_reshape <- function() {
  subject_rels = c('nsubj','su', 'nsubjpass','pobj','nsubj')
  
  isolate = treshape_isolate(relation=c('appos','relcl'), copy_parent = T)
  
  link = treshape_link(relation = 'conj', link_children=subject_rels)
  
  bypass = treshape_bypass(relation='conj')
  
  rm = treshape_remove(relation=c('punct','cc'), not_children())
  
  list(isolate,link,bypass,rm)
}



function(){
  #tokens = as_tokenindex(tokens_spacy_english_)
  
  
  quote_queries = spacy_english_quote_queries()  
  clause_queries = spacy_english_clause_queries()
  tokens = annotate(tokens, quote_queries, column='quotes', fill=T)
  tokens = annotate(tokens, clause_queries, column='clauses', fill=T)
  tokens
}