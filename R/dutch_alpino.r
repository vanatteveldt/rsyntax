

.ALPINO_SAY_VERBS = c("zeggen", "stellen", "roepen", "schrijven", "denken", "vaststellen")
.ALPINO.VIND_VERBS = union(.ALPINO_SAY_VERBS, c("accepteer", "antwoord", "beaam", "bedenk", "bedoel", "begrijp", "beken", "beklemtoon", "bekrachtig", "belijd", "beluister", "benadruk", "bereken", "bericht", "beschouw", "beschrijf", "besef", "betuig", "bevestig", "bevroed", "beweer", "bewijs", "bezweer", "biecht", "breng", "brul", "concludeer", "confirmeer", "constateer", "debiteer", "declareer", "demonstreer", "denk", "draag_uit", "email", "erken", "expliceer", "expliciteer", "fantaseer", "formuleer", "geef_aan", "geloof", "hoor", "hamer", "herinner", "houd_vol", "kondig_aan", "kwetter", "licht_toe", "maak_bekend", "maak_hard", "meld", "merk", "merk_op", "motiveer", "noem", "nuanceer", "observeer", "onderschrijf", "onderstreep", "onthul", "ontsluier", "ontval", "ontvouw", "oordeel", "parafraseer", "postuleer", "preciseer", "presumeer", "pretendeer", "publiceer", "rapporteer", "realiseer", "redeneer", "refereer", "reken", "roep", "roer_aan", "ruik", "schat", "schets", "schilder", "schreeuw", "schrijf", "signaleer", "snap", "snater", "specificeer", "spreek_uit", "staaf", "stellen", "stip_aan", "suggereer", "tater", "teken_aan", "toon_aan", "twitter", "verbaas", "verhaal", "verklaar", "verklap", "verkondig", "vermoed", "veronderstel", "verraad", "vertel", "vertel_na", "verwacht", "verwittig", "verwonder", "verzeker", "vind", "voel", "voel_aan", "waarschuw", "wed", "weet", "wijs_aan", "wind", "zeg", "zet_uiteen", "zie", "twitter"))

.VOLGENS = c("volgens", "aldus")
.QUOTES = c('"', "'", "''", "`", "``")
.QPUNC = union(.QUOTES, c(":"))

.ALPINO.SUBJECT_REL = c('su')
.ALPINO.SUBJECT_BODY = c('body')

.ALPINO.PASSIVE_VC = c('zijn', 'worden', 'hebben')
.ALPINO.PASSIVE_MOD = c('door', 'vanwege', 'omwille')


#' Returns a list with the quote rules for ALPINO
#'
#' @return A list with rynstax rules, as created with \link{rule}
#' @export
alpino_quote_rules <- function() {
  # x zegt dat y
  zegtdat = rule(lemma = .ALPINO.VIND_VERBS,
                        children(save = 'source', rel=.ALPINO.SUBJECT_REL),
                        children(rel='vc', POS = c('C', 'comp'),
                                 children(save='quote', rel=.ALPINO.SUBJECT_BODY)))
  
  # x stelt: y
  ystelt = rule(lemma = .ALPINO.VIND_VERBS, 
                       children(save = 'source', rel=.ALPINO.SUBJECT_REL),
                       children(save = 'quote', rel='nucl'),
                       children(lemma =  .QPUNC))
  
  # y, stelt x
  xstelt = rule(save='quote', 
                       children(rel='tag', lemma = .ALPINO.VIND_VERBS,
                                children(save = 'source', rel=.ALPINO.SUBJECT_REL)))
  
  # y, volgens x
  volgens = rule(save='quote',
                        children(rel=c('mod','tag'), lemma = .VOLGENS,
                                 children(save='source')))
  
  # y, zo noemt x het
  noemt = rule(rel='tag', 
                      children(save='source', rel=.ALPINO.SUBJECT_REL),
                      parents(save='quote',
                              children(rel = ' --', lemma = .QPUNC)))
  
  # x is het er ook mee eens: y
  impliciet = rule(
                          children(lemma = .QPUNC),
                          children(save='quote', rel=c('tag','nucl','sat')),
                          children(save='source', rel=.ALPINO.SUBJECT_REL))
  
  # x: y
  impliciet2 = rule(save='source',
                           children(lemma = .QPUNC),
                           children(save='quote', rel=c('tag','nucl','sat')))
  
  ## order matters
  list(zegtdat=zegtdat, ystelt=ystelt, xstelt=xstelt, volgens=volgens, noemt=noemt, impliciet=impliciet, impliciet2=impliciet2)
}

#' Returns a list with the clause rules for ALPINO
#'
#' @return A list with rynstax rules, as created with \link{rule}
#' @export
alpino_clause_rules <- function(){
  ## [passive subject as object] [passive verb with modifier] [object as subject] 
  passive = rule(POS = 'verb', 
                        parents(save='predicate', lemma = .ALPINO.PASSIVE_VC),
                        children(lemma = .ALPINO.PASSIVE_MOD, 
                                 children(save='subject', rel='obj1')))
  
  ## [subject] [has/is/etc.] [verb] [object]
  perfect = rule(POS = 'verb',
                        parents(save='predicate', lemma = .ALPINO.PASSIVE_VC),
                        children(save='subject', rel=.ALPINO.SUBJECT_REL))
  
  ## [subject] [verb] [object]
  active = rule(save='predicate', POS = 'verb',
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
