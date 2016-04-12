.SAY_VERBS = c("zeggen", "stellen", "roepen", "schrijven", "denken", "vaststellen")

.VIND_VERBS = union(.SAY_VERBS, c("accepteren", "antwoorden", "beamen", "bedenken", "bedoelen", "begrijpen", "bekenen", "beklemtonen", "bekrachtigen", "belijden", "beluisteren", "benadrukken", "berekenen", "berichten", "beschouwen", "beschrijven", "beseffen", "betuigen", "bevestigen", "bevroeden", "beweren", "bewijzen", "bezweren", "biechten", "brengen", "brullen", "concluderen", "confirmeren", "constateren", "debiteren", "declareren", "demonstreren", "denken", "uitdragen", "emailen", "erkennen", "expliceren", "expliciteren", "fantaseren", "formuleren", "aangeven", "geloven", "horen", "hameren", "herinneren", "volhouden", "aankondigen", "kwetteren", "toelichten", "bekendmaken", "hardmaken", "melden", "merken", "opmerken", "motiveren", "noemen", "nuanceren", "observeren", "onderschrijven", "onderstrepen", "onthullen", "ontsluieren", "ontvallen", "ontvouwen", "oordelen", "parafraseren", "postuleren", "preciseren", "presumeren", "pretenderen", "publiceren", "rapporteren", "realiseren", "redeneren", "refereren", "rekenen", "roepen", "aanroeren", "ruiken", "schatten", "schetsen", "schilderen", "schreeuwen", "schrijven", "signaleren", "snappen", "snateren", "specificeren", "uitspreken", "staven", "stellen", "aanstippen", "suggereren", "tateren", "aantekenen", "aantonen", "twitteren", "verbazen", "verhalen", "verklaren", "verklappen", "verkondigen", "vermoeden", "veronderstellen", "verraaden", "vertellen", "navertellen", "verwachten", "verwittigen", "verwonderen", "verzekeren", "vinden", "voelen", "aanvoelen", "waarschuwen", "wedden", "weten", "aanwijzen", "winden", "zeggen", "uiteenzetten", "zien", "twitteren"))

.VOLGENS = c("volgens", "aldus")
.QUOTES = c('"', "'", "''", "`", "``")
.QPUNC = union(.QUOTES, c(":"))

.SUBJECT_RELS = c('su')
.CONJUNCTIONS = c('cnj')

.PASSIVE_VC = c('zijn', 'worden')
.PASSIVE_MOD = c('door', 'vanwege', 'omwille')

#' Get quotes from tokens parsed by Alpino
#'
#' @param tokens a token list data frame
#'
#' @return a data frame with columns id, source, and quote
#' @export
get_quotes_nl <- function(tokens) {
  
  .removetokens <- function(tokens, quotes) tokens[!(tokens$sentence %in% tokens$sentence[tokens$id %in% quotes$source]), ]
  ## Is alleen de id node verwijderen niet voldoende? 
  
  # x zegt dat y
  zegtdat = find_nodes(tokens, lemma__in = .VIND_VERBS, children=list(source="su", body=list("vc", pos="comp", children=list(quote="body"))))
  zegtdat = zegtdat[c("id", "source", "quote")]
  tokens = .removetokens(tokens, zegtdat)
  
  # x stelt: y
  stelt = find_nodes(tokens, lemma__in=.VIND_VERBS, children=list(source="su", quote="nucl", punc=list(lemma__in = .QPUNC)))
  stelt = stelt[c("id", "source", "quote")]
  stelt = unique(stelt)
  tokens = .removetokens(tokens, stelt)
  
  # y, stelt x
  yzegt = find_nodes(tokens, children=list(key=list("tag", lemma__in=.VIND_VERBS, children=list(source="su")))) 
  yzegt = with(yzegt, data.frame(id=key, source=source, quote=id))
  tokens = .removetokens(tokens, yzegt)
  
  # y, volgens x
  volgens = find_nodes(tokens, children=list(key=list(lemma__in=.VOLGENS, relation__in=c("mod", "tag"), children=list(source="obj1"))))
  volgens = with(volgens, data.frame(id=key, source=source, quote=id))
  tokens = .removetokens(tokens, volgens)
  
  # x is het er ook mee eens: y
  impliciet = find_nodes(tokens, children=list(punc=list(lemma__in = .QPUNC), quote=list(relation__in=c("tag", "nucl", "sat")), source="su"))
  impliciet = unique(with(impliciet, data.frame(id=rep(NA, nrow(impliciet)), source=source, quote=quote)))
  tokens = .removetokens(tokens, impliciet)
  
  # x: y
  impliciet2 = find_nodes(tokens, children=list(punc=list(lemma__in = .QPUNC), quote=list(relation__in=c("tag", "nucl", "sat"))))
  impliciet2 = unique(with(impliciet2, data.frame(id=rep(NA, nrow(impliciet2)), source=id, quote=quote)))

  rbind(zegtdat, stelt, yzegt, volgens, impliciet, impliciet2)
}

#' Collapse conjunctions
#'
#' @param tokens a token list data frame
#' @param nodes_df the output of get_clauses
#' @param id_col the name of the column in which conjunctions are looked up
#' @param remove_conj_node if TRUE, the row with the conjunction word will be removed from the clauses
#'
#' @return 
#' @export
collapse_conjunctions <- function(tokens, nodes_df, id_col, remove_conj_node=T){
  conj = find_nodes(tokens, id__in=nodes_df[,id_col], children=list(list(relation__in=.CONJUNCTIONS, rename='conj_id')))
  nodes_df_conj = nodes_df[match(conj$id, nodes_df[,id_col]),,drop=F]
  
  nodes_df_conj[,id_col] = conj$conj_id
  nodes_df = rbind(nodes_df, nodes_df_conj)
  if(remove_conj_node) nodes_df = nodes_df[!nodes_df[,id_col] %in% conj$id,]
  nodes_df  
}

function(){
  data(example_tokens_dutchclauses)
  tokens = tokens[tokens$sentence == 3,]
}

#data(example_tokens_dutchclauses)

#' Get clauses from tokens parsed by Alpino
#'
#' @param tokens a token list data frame
#' @param quotes the return value of get_quotes_nl
#'
#' @return a data frame with columns clause_id, source, predicate
#' @export
get_clauses_nl <- function(tokens, quotes = NULL){
  block = if (is.null(quotes)) NULL else unique(quotes$id)
  
  ## [subject] [verb] [object]
  verbbased = find_nodes(tokens, pos='verb', id__not_in=block, 
                  children=list(subject = list(relation__in=.SUBJECT_RELS)))
  colnames(verbbased) = c('predicate', 'subject')
  
  ##### The following rules check whether a subject is passive -> object zijn/worden verb (door subject)
  ## first remove subject from passive phrases.
  passive = find_nodes(tokens, lemma__in=.PASSIVE_VC,
                        children = list(verb = list(id__in=verbbased$predicate)))
  verbbased$subject[match(passive$verb, verbbased$predicate)] = NA
  ## then, if an obj1 is found through a passive modifier (an 'ld' or 'mod', generally the lemma 'door') assume this is the subject.
  passive_subject = find_nodes(tokens, id__in=passive$verb, 
                        children = list(passmod = list(lemma__in=.PASSIVE_MOD,
                                        children = list(subject = list(relation='obj1')))))
  verbbased$subject[match(passive_subject$id, verbbased$predicate)] = passive_subject$subject
  
  ## if a verb clause is passive, use the .PASSIVE_VC (which is the verb's parent) to indicate the predicate.
  verbbased$predicate[match(passive$verb, verbbased$predicate)] = passive$id
  
  #### prepare clauses
  clauses = rbind(verbbased)
  clauses$clause_id = 1:nrow(clauses)
    
  ## collapse conjuctions
  #clauses = collapse_conjunctions(tokens, clauses, 'subject')
  #clauses = collapse_conjunctions(tokens, clauses, 'predicate') # also collapse conjunctions in predicate? (this seems to have been a step in the stanford get_clauses)
  
  clauses[,c('clause_id','subject','predicate')]
}


### ANAPHOR
.ANAPHOR_self = c('zich','zichzelf')
.ANAPHOR_male = c('hij', 'zijn', 'hem')
.ANAPHOR_plur = c('zij', 'ze', 'hen', 'hun')
.ANAPHOR_fema = c('zij', 'ze', 'haar')
.ANAPHOR = unique(c(.ANAPHOR_male, .ANAPHOR_fema, .ANAPHOR_plur))

anafora_resolution <- function(tokens, clauses){
  #tokens
  ## exclude self ref for subjects in the same sentence
  allsubjects = fill(clauses$subject, tokens)$filled
  sent_subj = unique(tokens[tokens$id %in% allsubjects, c('id','sentence')])

  ## hoe aan te pakken? misschien een sparse matrix waar nrow is nrow(tokens), met alle mogelijke entiteiten waar een anafora naar kan verwijzena als kolommen. In eerste instantie alle entiteiten in hetzelfde artikel. Dan filterregels toevoegen (geslacht/meervoud/zelfref). Dan de eerstvorige entiteit die overblijft.
  
  #is_ana = tokens$lemma %in% .ANAPHOR
}

## sommige anafora kunnen niet verwijzen naar het subject in dezelfde clause: hem, haar (toekennen aan eerstvorige entiteit dat niet het subject van dezelfde zin is)
## sommige anafore zijn zelf referentieel: zich, 

## eerst binnen clauses kijken
## dan de nog niet toegekende anafora behandelen


