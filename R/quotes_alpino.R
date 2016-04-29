.SAY_VERBS = c("zeggen", "stellen", "roepen", "schrijven", "denken", "vaststellen")
#.VIND_VERBS = c("vinden", "meen", "beken", "concludeer", "erken", "waarschuw", "weet")

.VIND_VERBS = union(.SAY_VERBS, c("accepteer", "antwoord", "beaam", "bedenk", "bedoel", "begrijp", "beken", "beklemtoon", "bekrachtig", "belijd", "beluister", "benadruk", "bereken", "bericht", "beschouw", "beschrijf", "besef", "betuig", "bevestig", "bevroed", "beweer", "bewijs", "bezweer", "biecht", "breng", "brul", "concludeer", "confirmeer", "constateer", "debiteer", "declareer", "demonstreer", "denk", "draag_uit", "email", "erken", "expliceer", "expliciteer", "fantaseer", "formuleer", "geef_aan", "geloof", "hoor", "hamer", "herinner", "houd_vol", "kondig_aan", "kwetter", "licht_toe", "maak_bekend", "maak_hard", "meld", "merk", "merk_op", "motiveer", "noem", "nuanceer", "observeer", "onderschrijf", "onderstreep", "onthul", "ontsluier", "ontval", "ontvouw", "oordeel", "parafraseer", "postuleer", "preciseer", "presumeer", "pretendeer", "publiceer", "rapporteer", "realiseer", "redeneer", "refereer", "reken", "roep", "roer_aan", "ruik", "schat", "schets", "schilder", "schreeuw", "schrijf", "signaleer", "snap", "snater", "specificeer", "spreek_uit", "staaf", "stellen", "stip_aan", "suggereer", "tater", "teken_aan", "toon_aan", "twitter", "verbaas", "verhaal", "verklaar", "verklap", "verkondig", "vermoed", "veronderstel", "verraad", "vertel", "vertel_na", "verwacht", "verwittig", "verwonder", "verzeker", "vind", "voel", "voel_aan", "waarschuw", "wed", "weet", "wijs_aan", "wind", "zeg", "zet_uiteen", "zie", "twitter"))

.VOLGENS = c("volgens", "aldus")
.QUOTES = c('"', "'", "''", "`", "``")
.QPUNC = union(.QUOTES, c(":"))

.SUBJECT_RELS = c('su')


#' Get quotes from tokens parsed by Alpino
#'
#' @param tokens a token list data frame
#'
#' @return a data frame with columns id, source, and quote
#' @export
get_quotes_nl <- function(tokens) {
  
  .removetokens <- function(tokens, quotes) tokens[!(tokens$sentence %in% tokens$sentence[tokens$id %in% quotes$source]), ]
  
  # x zegt dat y
  zegtdat = find_nodes(tokens, lemma__in=.VIND_VERBS, children=list(source="su", body=list("vc", pos="comp", children=list(quote="body"))))
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
  
  # y, zo noemt x het
  puncparents = unique(tokens$parent[tokens$lemma %in% .QPUNC & tokens$relation == " --"])
  noem = find_nodes(tokens, children=list(source="su"), relation="tag", parent=list(rename="quote"))
  noem = noem[noem$quote %in% puncparents, ]
  tokens = .removetokens(tokens, noem)
  
  # x is het er ook mee eens: y
  impliciet = find_nodes(tokens, children=list(punc=list(lemma__in = .QPUNC), quote=list(relation__in=c("tag", "nucl", "sat")), source="su"))
  impliciet = unique(with(impliciet, data.frame(id=rep(NA, nrow(impliciet)), source=source, quote=quote)))
  tokens = .removetokens(tokens, impliciet)
  
  # x: y
  impliciet2 = find_nodes(tokens, children=list(punc=list(lemma__in = .QPUNC), quote=list(relation__in=c("tag", "nucl", "sat"))))
  impliciet2 = unique(with(impliciet2, data.frame(id=rep(NA, nrow(impliciet2)), source=id, quote=quote)))

  rbind(zegtdat, stelt, yzegt, volgens, noem, impliciet, impliciet2)
}

