#DUTCH_SAY_VERBS = c("aan_bieden","zeggen", "stellen", "roepen", "schrijven", "denken", "vaststellen", "accepteer", "antwoord", "beaam", "bedenk", "bedoel", "begrijp", "beken", "beklemtoon", "bekrachtig", "belijd", "beluister", "benadruk", "bereken", "bericht", "beschouw", "beschrijf", "besef", "betuig", "bevestig", "bevroed", "beweer", "bewijs", "bezweer", "biecht", "breng", "brul", "concludeer", "confirmeer", "constateer", "debiteer", "declareer", "demonstreer", "denk", "draag_uit", "email", "erken", "expliceer", "expliciteer", "fantaseer", "formuleer", "geef_aan", "geloof", "hoor", "hamer", "herinner", "houd_vol", "kondig_aan", "kwetter", "licht_toe", "maak_bekend", "maak_hard", "meld", "merk", "merk_op", "motiveer", "noem", "nuanceer", "observeer", "onderschrijf", "onderstreep", "onthul", "ontsluier", "ontval", "ontvouw", "oordeel", "parafraseer", "postuleer", "preciseer", "presumeer", "pretendeer", "publiceer", "rapporteer", "realiseer", "redeneer", "refereer", "reken", "roep", "roer_aan", "ruik", "schat", "schets", "schilder", "schreeuw", "schrijf", "signaleer", "snap", "snater", "specificeer", "spreek_uit", "staaf", "stip_aan", "suggereer", "tater", "teken_aan", "toon_aan", "twitter", "verbaas", "verhaal", "verklaar", "verklap", "verkondig", "vermoed", "veronderstel", "verraad", "vertel", "vertel_na", "verwacht", "verwittig", "verwonder", "verzeker", "vind", "voel", "voel_aan", "waarschuw", "wed", "weet", "wijs_aan", "wind", "zeg", "zet_uiteen", "zie")
DUTCH_SAY_VERBS = c("accepteren", "antwoorden", "beamen", "bedenken", "bedoelen", "begrijpen", "bekenen", 
                    "beklemtonen", "bekrachtigen", "belijden", "beluisteren", "benadruken", "berekenen", "berichten", "beschouwen", "beschrijven", "beseffen", "betuigen", "bevestigen", "bevroeden", 
                    "beweren", "bewijzen", "bezweren", "biechten", "aan_bieden", "brengen", "brullen", "concluderen", "confirmeren", "constateren", "debiteren", "declareren", "demonstreren", "denken", "uit_dragen", 
                    "emailen", "erkenen", "expliceren", "expliciteren", "fantaseren", "formuleren", "aan_geven", "geloven", "horen", "hameren", "herinneren", "vol_houden", "aan_kondigen", "kwetteren", 
                    "toe_lichten", "bekend_maken", "hard_maken", "melden", "merken", "op_merken", "motiveren", "noemen", "nuanceren", "observeren", "onderschrijven", "onderstrepen", "onthullen", "ontsluieren", 
                    "ontvallen", "ontvouwen", "oordelen", "parafraseren", "postuleren", "preciseren", "presumeren", "pretenderen", "publiceren", "rapporteren", "realiseren", "redeneren", "refereren", 
                    "rekenen", "roepen", "aan_roeren", "ruiken", "schaten", "schetsen", "schilderen", "schreeuwen", "schrijven", "signaleeren", "snappen", "snateren", "specificeren", "uit_spreken", "staven", "stellen",
                    "vast_stellen","aan_stippen", "suggereren", "tateren", "aan_tekenen", "aan_tonen", "twitteren", "verbazen", "verhalen", "verklaren", "verklappen", "verkondigen", "vermoeden", "veronderstellen", "verraden", "vertellen", "na_vertellen", 
                    "verwachten", "verwittigen", "verwonderen", "verzekeren", "vinden", "voelen", "aan_voelen", "waarschuwen", "wedden", "weten", "aan_wijzen", "winden", "zeggen", "uiteen_zetten", "zien")


#' Returns a list with the quote queries for Alpino
#'
#' @param verbs         A character vector with verbs used to indicate quotes. A default list of verbs is provided
#'                      in DUTCH_SAY_VERBS. If NULL, all verbs are used (except those listed in exclude verbs)
#' @param exclude_verbs A character vector with verbs that are exluded. If NULL, no verbs are excluded.
#'
#' @return A list with rynstax queries, as created with \link{tquery}
#' @export
alpino_quote_queries <- function(verbs=DUTCH_SAY_VERBS, exclude_verbs=NULL) {
  # x zegt dat y
  zegtdat = tquery(save='verb', lemma = verbs,  
                   children(save = 'source', relation=c('su')),
                   children(relation='vc', POS = c('C', 'comp'),
                            children(save= 'quote', relation=c('body'))),
                   not_parents(lemma=c('kun','moet','zal')))   ## exclude "kun/moet/zal je zeggen dat ..."   

  # x stelt: y
  ystelt = tquery(lemma = verbs, 
                       children(save = 'source', relation=c('su')),
                       children(save = 'quote', relation='nucl'),
                       children(lemma =  quote_punctuation))
  
  # y, stelt x
  xstelt = tquery(save='quote', fill = fill(relation__N='tag'),
                       children(save='verb', relation='tag', lemma = verbs,
                                children(save = 'source', relation=c('su'))))
  
  # y, volgens x
  volgens = tquery(save='quote',
                        children(save='verb', relation=c('mod','tag'), lemma = c('volgens','aldus'),
                                 children(save='source')))
  
  # y, zo noemt x het
  noemt = tquery(save='verb', relation='tag',
                      children(save='source', relation=c('su')),
                      parents(save='quote',
                              children(relation = ' --', lemma = quote_punctuation)))
  
  # x is het er ook mee eens: y
  impliciet = tquery(save='verb',
    children(lemma = c('"', "'")),
    children(save='quote', relation=c('tag','nucl','sat')),
    children(save='source', relation=c('su')))
  
  # x: y
  impliciet2 = tquery(save='source',
                      children(lemma = ':'),
                      children(save='quote', relation=c('tag','nucl','sat')),
                      not_children(relation='su'))
  
  # moet/kan/zal zeggen mag wel als eerste persoon is
  moetzeggen = tquery(save='verb', lemma=c('kunnen','moeten','zullen'), 
                      children(lemma=verbs,
                               children(save = 'source', lemma=c('ik','wij'), relation=c('su')), 
                               children(relation='vc', POS = c('C', 'comp'),
                                        children(save= 'quote', relation=c('body')))))

  
  ## order matters
  list(zegtdat=zegtdat, ystelt=ystelt, xstelt=xstelt, volgens=volgens, noemt=noemt, 
       impliciet=impliciet, impliciet2=impliciet2, moetzeggen=moetzeggen)
}

#' Returns a list with the clause queries for CoreNLP
#'
#' @param verbs         A character vector with verbs used to indicate clauses. If NULL (default), all verbs are used (except those listed in exclude verbs)
#' @param exclude_verbs A character vector with verbs that are not used in clauses. By default, this is the list of DUTCH_SAY_VERBS, 
#'                      which are the verbs used in the corenlp_quote_queries(). If set to NULL, no verbs are excluded.
#' @param tokens     a token list data frame
#'
#' @return a data.table with nodes (as .G_ID) for id, subject and predicate
alpino_clause_queries <- function(verbs=NULL, exclude_verbs=DUTCH_SAY_VERBS, with_subject=T, with_object=F) {
  
  passive = tquery(POS = 'verb', lemma__N = exclude_verbs, save='predicate',
                        parents(lemma = c('zijn','worden','hebben')),
                        children(lemma = c('door','vanwege','omwille'), 
                                 children(save='subject', relation='obj1')))
  
  ## [subject] [has/is/etc.] [verb] [object]
  perfect = tquery(POS = 'verb', lemma__N = exclude_verbs, 
                   parents(save='predicate', lemma = c('zijn','worden','hebben')),
                        children(save='subject', relation=c('su')))
  
  ## [subject] [verb] [object]
  active = tquery(save='predicate', POS = 'verb', relation__N = 'vc', lemma__N = exclude_verbs,
                         children(save='subject', relation=c('su')))

  ## [subject] [verb] 
  catch_rest = tquery(save='predicate', POS = 'verb', lemma__N = exclude_verbs,
                children(save='subject', relation=c('su')))
                         
  list(passive=passive, perfect=perfect, active=active, catch_rest=catch_rest)
}

