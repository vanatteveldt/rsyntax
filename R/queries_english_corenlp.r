ENGLISH_SAY_VERBS = c("tell", "show", " acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "proclaim", "promise", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "write", "add")

CORENLP_QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl")
CORENLP_SUBJECT_RELS = c('su', 'nsubj', 'agent', 'nmod:agent') 

CORENLP_VERB_POS = c('MD','VB','VBD','VBG','VBN','VBP','VBZ')
CORENLP_NOUN_POS = c('NN','NNS','FW')
CORENLP_ADJ_POS = c('JJ','JJR','JJS','WRB')
CORENLP_PREP_POS = 'IN'
CORENLP_CONJ_POS = 'LS'
CORENLP_PNOUN_POS = c('NNS','NNPS')
CORENLP_DETER_POS = c('PDT','DT','WDT')
CORENLP_ADVERB_POS = c('RB','RBR','RBS')

#' Returns a list with the quote queries for CoreNLP
#'
#' @param verbs         A character vector with verbs used to indicate quotes ("say", "report", "admit", etc.). A default list of verbs is provided
#'                      in ENGLISH_SAY_VERBS. If NULL, all verbs are used (except those listed in exclude verbs)
#' @param exclude_verbs A character vector with verbs that are exluded. If NULL, no verbs are excluded.
#'
#' @return A list with rynstax queries, as created with \link{tquery}
#' @export
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
  


#' Returns a list with the clause queries for CoreNLP
#'
#' @param verbs         A character vector with verbs used to indicate clauses. If NULL (default), all verbs are used (except those listed in exclude verbs)
#' @param exclude_verbs A character vector with verbs that are not used in clauses. By default, this is the list of ENGLISH_SAY_VERBS, 
#'                      which are the verbs used in the corenlp_quote_queries(). If set to NULL, no verbs are excluded.
#' @param tokens     a token list data frame
#'
#' @return a data.table with nodes (as .G_ID) for id, subject and predicate
#' @export
corenlp_clause_queries <- function(verbs=NULL, exclude_verbs=ENGLISH_SAY_VERBS, with_subject=T, with_object=F, sub_req=T, ob_req=F) {
  subject_name = if (with_subject) 'subject' else NA
  object_name = if (with_object) 'object' else NA

  #tokens = as_tokenindex(tokens_corenlp)
  
  direct = tquery(POS = 'VB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                  children(relation = c('su', 'nsubj', 'agent'), label=subject_name, req=sub_req),
                  children(relation = c('dobj'), label=object_name, req=ob_req)) 
 
  
  passive = tquery(POS = 'VB*', lemma = verbs, NOT(lemma = exclude_verbs), label='predicate',
                   children(relation = 'auxpass'),
                   children(relation = 'nmod:agent', label=subject_name, req=F),
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

