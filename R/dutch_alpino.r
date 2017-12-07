.ALPINO_SAY_VERBS = c("zeggen", "stellen", "roepen", "schrijven", "denken", "vaststellen")
.ALPINO.VIND_VERBS = union(.ALPINO_SAY_VERBS, c("accepteer", "antwoord", "beaam", "bedenk", "bedoel", "begrijp", "beken", "beklemtoon", "bekrachtig", "belijd", "beluister", "benadruk", "bereken", "bericht", "beschouw", "beschrijf", "besef", "betuig", "bevestig", "bevroed", "beweer", "bewijs", "bezweer", "biecht", "breng", "brul", "concludeer", "confirmeer", "constateer", "debiteer", "declareer", "demonstreer", "denk", "draag_uit", "email", "erken", "expliceer", "expliciteer", "fantaseer", "formuleer", "geef_aan", "geloof", "hoor", "hamer", "herinner", "houd_vol", "kondig_aan", "kwetter", "licht_toe", "maak_bekend", "maak_hard", "meld", "merk", "merk_op", "motiveer", "noem", "nuanceer", "observeer", "onderschrijf", "onderstreep", "onthul", "ontsluier", "ontval", "ontvouw", "oordeel", "parafraseer", "postuleer", "preciseer", "presumeer", "pretendeer", "publiceer", "rapporteer", "realiseer", "redeneer", "refereer", "reken", "roep", "roer_aan", "ruik", "schat", "schets", "schilder", "schreeuw", "schrijf", "signaleer", "snap", "snater", "specificeer", "spreek_uit", "staaf", "stellen", "stip_aan", "suggereer", "tater", "teken_aan", "toon_aan", "twitter", "verbaas", "verhaal", "verklaar", "verklap", "verkondig", "vermoed", "veronderstel", "verraad", "vertel", "vertel_na", "verwacht", "verwittig", "verwonder", "verzeker", "vind", "voel", "voel_aan", "waarschuw", "wed", "weet", "wijs_aan", "wind", "zeg", "zet_uiteen", "zie", "twitter"))

.VOLGENS = c("volgens", "aldus")
.QUOTES = c('"', "'", "''", "`", "``")
.QPUNC = union(.QUOTES, c(":"))

.ALPINO.SUBJECT_REL = c('su')
.ALPINO.SUBJECT_BODY = c('body')

.ALPINO.PASSIVE_VC = c('zijn', 'worden', 'hebben')
.ALPINO.PASSIVE_MOD = c('door', 'vanwege', 'omwille')


#' Get quotes from tokens parsed by Alpino
#'
#' @param tokens     A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param block      Another set of nodes (as created with \link{find_nodes}), of which the .KEY values will be blocked for annotations
#'
#' @return a data.table with nodes
#' @export
get_quotes_alpino <- function(tokens, block=NULL) {
  tokens = as_tokenindex(tokens)
  rules = alpino_quote_rules()
  apply_rules(tokens, rules, as_chain=T, block = block, check = F)
}

#' Get clauses from tokens parsed by Alpino
#' 
#' @param tokens     A tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param block      Optionally, another set of nodes (as created with \link{find_nodes}), of which the .KEY values will be blocked for annotations
#'
#' @return a data.table with nodes
#' @export
get_clauses_alpino <- function(tokens, block=NULL){
  tokens = as_tokenindex(tokens)
  rules = alpino_clause_rules()
  apply_rules(tokens, rules, as_chain=T, block = block, check = F)
}

#' Quote and clause extraction for Dutch texts, parsed with ALPINO
#' 
#' This is a convenience function that uses all the (currently implemented) rules for quote and clause extraction.
#'
#' @param tokens     Either a tokenIndex data.table, created with \link{as_tokenindex}, or any data.frame with the required columns (see \link{tokenindex_columns}).
#' @param no_overlap If TRUE (default), then quote patterns (e.g., SOURCE says QUOTE) are excluded for clauses. Clauses can then still occur within quotes, such as in: SOURCE says: "SUBJECT is happy". 
#'                   If a quote occurs within a clause, the clause will stop, as in: (SUBJECT is mad at) (SOURCE, who said QUOTE)
#'
#' @return the tokens data as a tokenIndex, with additional columns: quote, quote_id, clause and clause_id
#' @export
annotate_alpino <- function(tokens) {
  tokens = as_tokenindex(tokens)
  quotes = get_quotes_alpino(tokens)
  clauses = get_clauses_alpino(tokens)
  tokens = annotate(tokens, quotes, column = 'quote', check=F)
  tokens = annotate(tokens, clauses, column = 'clause', block=quotes, check=F)
  tokens
}

#' Returns a list with the quote rules for ALPINO
#'
#' @return A list with rynstax rules, as created with \link{create_rule}
#' @export
alpino_quote_rules <- function() {
  # x zegt dat y
  zegtdat = create_rule(select = lemma %in% .ALPINO.VIND_VERBS,
                        children(save = 'source', rel=.ALPINO.SUBJECT_REL),
                        children(rel='vc', select = POS %in% c('C', 'comp'),
                                 children(save='quote', rel=.ALPINO.SUBJECT_BODY)))
  
  # x stelt: y
  ystelt = create_rule(select = lemma %in% .ALPINO.VIND_VERBS, 
                       children(save = 'source', rel=.ALPINO.SUBJECT_REL),
                       children(save = 'quote', rel='nucl'),
                       children(select = lemma %in% .QPUNC))
  
  # y, stelt x
  xstelt = create_rule(save='quote', 
                       children(rel='tag', select = lemma %in% .ALPINO.VIND_VERBS,
                                children(save = 'source', rel=.ALPINO.SUBJECT_REL)))
  
  # y, volgens x
  volgens = create_rule(save='quote',
                        children(rel=c('mod','tag'), select = lemma %in% .VOLGENS,
                                 children(save='source')))
  
  # y, zo noemt x het
  noemt = create_rule(rel='tag', 
                      children(save='source', rel=.ALPINO.SUBJECT_REL),
                      parents(save='quote',
                              children(rel = ' --', select = lemma %in% .QPUNC)))
  
  # x is het er ook mee eens: y
  impliciet = create_rule(
                          children(select = lemma %in% .QPUNC),
                          children(save='quote', rel=c('tag','nucl','sat')),
                          children(save='source', rel=.ALPINO.SUBJECT_REL))
  
  # x: y
  impliciet2 = create_rule(save='source',
                           children(select = lemma %in% .QPUNC),
                           children(save='quote', rel=c('tag','nucl','sat')))
  
  ## order matters
  list(zegtdat=zegtdat, ystelt=ystelt, xstelt=xstelt, volgens=volgens, noemt=noemt, impliciet=impliciet, impliciet2=impliciet2)
}

#' Returns a list with the clause rules for ALPINO
#'
#' @return A list with rynstax rules, as created with \link{create_rule}
#' @export
alpino_clause_rules <- function(){
  ## [passive subject as object] [passive verb with modifier] [object as subject] 
  passive = create_rule(select = POS == 'verb', 
                        parents(save='predicate', select = lemma %in% .ALPINO.PASSIVE_VC),
                        children(select = lemma %in% .ALPINO.PASSIVE_MOD, 
                                 children(save='subject', rel='obj1')))
  
  ## [subject] [has/is/etc.] [verb] [object]
  perfect = create_rule(select = POS == 'verb',
                        parents(save='predicate', select = lemma %in% .ALPINO.PASSIVE_VC),
                        children(save='subject', rel=.ALPINO.SUBJECT_REL))
  
  ## [subject] [verb] [object]
  active = create_rule(save='predicate', select = POS == 'verb',
                         children(save='subject', rel=.ALPINO.SUBJECT_REL))
    
  
  ## order matters
  list(passive=passive, perfect=perfect, active=active)
}


function(){
  
  tokens = as_tokenindex(tokens_dutchclauses)
  tokens = annotate_alpino(tokens)
  
  quotes = get_quotes_alpino(tokens)
  get_nodes(tokens, quotes)
  
  tokens = annotate(tokens, quotes, 'quote', use = c('quote','source'))
  tokens[,c('quote','quote_id','token')]
  
  tokens = as_tokenindex(tokens_dutchclauses)
  
  clauses = get_clauses_alpino(tokens)
  get_nodes(tokens, clauses)
  
  tokens = annotate(tokens, clauses, 'clause', use = c('predicate','subject'))
  tokens[,c('clause','clause_id','token')]
}
