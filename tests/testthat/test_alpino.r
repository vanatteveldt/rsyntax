context("Clauses Alpino")

DUTCH_SAY_VERBS = c("accepteren", "antwoorden", "beamen", "bedenken", "bedoelen", "begrijpen", "bekenen", 
                    "beklemtonen", "bekrachtigen", "belijden", "beluisteren", "benadruken", "berekenen", "berichten", "beschouwen", "beschrijven", "beseffen", "betuigen", "bevestigen", "bevroeden", 
                    "beweren", "bewijzen", "bezweren", "biechten", "aan_bieden", "brengen", "brullen", "concluderen", "confirmeren", "constateren", "debiteren", "declareren", "demonstreren", "denken", "uit_dragen", 
                    "emailen", "erkenen", "expliceren", "expliciteren", "fantaseren", "formuleren", "aan_geven", "geloven", "horen", "hameren", "herinneren", "vol_houden", "aan_kondigen", "kwetteren", 
                    "toe_lichten", "bekend_maken", "hard_maken", "melden", "merken", "op_merken", "motiveren", "noemen", "nuanceren", "observeren", "onderschrijven", "onderstrepen", "onthullen", "ontsluieren", 
                    "ontvallen", "ontvouwen", "oordelen", "parafraseren", "postuleren", "preciseren", "presumeren", "pretenderen", "publiceren", "rapporteren", "realiseren", "redeneren", "refereren", 
                    "rekenen", "roepen", "aan_roeren", "ruiken", "schaten", "schetsen", "schilderen", "schreeuwen", "schrijven", "signaleeren", "snappen", "snateren", "specificeren", "uit_spreken", "staven", "stellen",
                    "vast_stellen","aan_stippen", "suggereren", "tateren", "aan_tekenen", "aan_tonen", "twitteren", "verbazen", "verhalen", "verklaren", "verklappen", "verkondigen", "vermoeden", "veronderstellen", "verraden", "vertellen", "na_vertellen", 
                    "verwachten", "verwittigen", "verwonderen", "verzekeren", "vinden", "voelen", "aan_voelen", "waarschuwen", "wedden", "weten", "aan_wijzen", "winden", "zeggen", "uiteen_zetten", "zien")

alpino_quote_queries <- function(verbs=DUTCH_SAY_VERBS, exclude_verbs=NULL) {
  # x zegt dat y
  zegtdat = tquery(label='verb', lemma = verbs,  
                   children(label = 'source', relation=c('su')),
                   children(relation='vc', POS = c('C', 'comp'),
                            children(label= 'quote', relation=c('body'))),
                   not_parents(lemma=c('kun','moet','zal')))   ## exclude "kun/moet/zal je zeggen dat ..."   
  
  # x stelt: y
  ystelt = tquery(lemma = verbs, 
                  children(label = 'source', relation=c('su')),
                  children(label = 'quote', relation='nucl'),
                  children(lemma =  quote_punctuation))
  
  # y, stelt x
  xstelt = tquery(label='quote', 
                  custom_fill(NOT(relation='tag')),
                  children(label='verb', relation='tag', lemma = verbs,
                           children(label = 'source', relation=c('su'))))
  
  # y, volgens x
  volgens = tquery(label='quote',
                   children(label='verb', relation=c('mod','tag'), lemma = c('volgens','aldus'),
                            children(label='source')))
  
  # y, zo noemt x het
  noemt = tquery(label='verb', relation='tag',
                 children(label='source', relation=c('su')),
                 parents(label='quote',
                         children(relation = ' --', lemma = quote_punctuation)))
  
  # x is het er ook mee eens: y
  impliciet = tquery(label='verb',
                     children(lemma = c('"', "'")),
                     children(label='quote', relation=c('tag','nucl','sat')),
                     children(label='source', relation=c('su')))
  
  # x: y
  impliciet2 = tquery(label='source',
                      children(lemma = ':'),
                      children(label='quote', relation=c('tag','nucl','sat')),
                      not_children(relation='su'))
  
  # moet/kan/zal zeggen mag wel als eerste persoon is
  moetzeggen = tquery(label='verb', lemma=c('kunnen','moeten','zullen'), 
                      children(lemma=verbs,
                               children(label = 'source', lemma=c('ik','wij'), relation=c('su')), 
                               children(relation='vc', POS = c('C', 'comp'),
                                        children(label= 'quote', relation=c('body')))))
  
  
  ## order matters
  list(zegtdat=zegtdat, ystelt=ystelt, xstelt=xstelt, volgens=volgens, noemt=noemt, 
       impliciet=impliciet, impliciet2=impliciet2, moetzeggen=moetzeggen)
}

alpino_clause_queries <- function(verbs=NULL, exclude_verbs=DUTCH_SAY_VERBS) {
  
  passive = tquery(POS = 'verb', NOT(lemma = exclude_verbs), label='predicate',
                   parents(lemma = c('zijn','worden','hebben')),
                   children(lemma = c('door','vanwege','omwille'), 
                            children(label='subject', relation='obj1')))
  
  ## [subject] [has/is/etc.] [verb] [object]
  perfect = tquery(POS = 'verb', NOT(lemma = exclude_verbs), 
                   parents(label='predicate', lemma = c('zijn','worden','hebben')),
                   children(label='subject', relation=c('su')))
  
  ## [subject] [verb] [object]
  active = tquery(label='predicate', POS = 'verb', NOT(relation = 'vc', lemma = exclude_verbs),
                  children(label='subject', relation=c('su')))
  
  ## [subject] [verb] 
  catch_rest = tquery(label='predicate', POS = 'verb', NOT(lemma = exclude_verbs),
                      children(label='subject', relation=c('su')))
  
  list(passive=passive, perfect=perfect, active=active, catch_rest=catch_rest)
}


get_quotes <- function(tokens, block=NULL) {
  queries = alpino_quote_queries()
  apply_queries(tokens, queries, as_chain=TRUE, block = block, check = FALSE)
}

get_clauses <- function(tokens, block=NULL){
  queries = alpino_clause_queries()
  apply_queries(tokens, queries, as_chain=TRUE, block = block, check = FALSE)
}

.check <- function(tokens, nodes, ...) {
  check = list(...)
  for(name in names(check)) {
    expected = as.character(check[[name]])
    actual = get_nodes(tokens, nodes, token_cols = 'token')
    #at(name, ': ', as.character(actual$token[actual$.ROLE == name]), '\n')
    actual = as.character(actual$token[actual$.ROLE == name])
    expect_equal(expected, actual)
  }
}

test_that("extracting sources works", {
  tokens = as_tokenindex(tokens_dutchquotes)
  library(testthat)
  #plot_tree(tokens, sentence_i=7, token, allign_text = TRUE)
  
  # 1 : Rutte stelt : " Een stem is verloren " . 
  quotes = get_quotes(tokens[tokens$sentence == 1,])
  expect_equal(nrow(quotes), 5)
  .check(tokens, quotes, source="Rutte", quote=c('Een','stem','is','verloren'))
  
  # 2 : Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte : " Een stem op de PVV , is een verloren stem " . 
  quotes = get_quotes(tokens[tokens$sentence == 2,])
  expect_equal(nrow(quotes), 10)
  .check(tokens, quotes, source="Rutte", quote=c('Een','stem','op','de','PVV','is','een','verloren','stem'))
  
  # 3 : " Verkiezingsblabla " , zegt PvdA-Kamerlid Kuiken . 
  quotes = get_quotes(tokens[tokens$sentence == 3,])
  #find_nodes2(tokens[tokens$sentence == 3,], alpino_quote_queries()[[3]])
  #find_nodes(tokens[tokens$sentence == 3,], alpino_quote_queries()[[3]])
  
  expect_equal(nrow(quotes), 8)
  .check(tokens, quotes, source=c("PvdA-Kamerlid","Kuiken"), verb='zegt', quote=c('"','Verkiezingsblabla','"',',','.'))
  
  # 4 : Minister Spies zei dat de PvdA een " Kapitale blunder " had begaan . 
  quotes = get_quotes(tokens[tokens$sentence == 4,])
  expect_equal(nrow(quotes), 14)
  .check(tokens, quotes, source=c("Minister","Spies"), verb=c('zei','dat','"','"','.'), quote=c('de','PvdA','een','Kapitale','blunder','had','begaan'))
  
  # 5 : Begrotingstekort is volgens CPB volgend jaar 3.7 procent . 
  quotes = get_quotes(tokens[tokens$sentence == 5,])
  expect_equal(nrow(quotes), 9)
  .check(tokens, quotes, source="CPB", quote=c('Begrotingstekort','is','volgend','jaar','3.7','procent','.'))
  
  # 6 : Hij veroordeelde het avontuur : " Alsof Nederland een politiek laboratorium is " . 
  quotes = get_quotes(tokens[tokens$sentence == 6,])
  expect_equal(nrow(quotes), 14)
  .check(tokens, quotes, source="Hij", verb=c('veroordeelde','het','avontuur',':','"','"','.'), quote=c('Alsof','Nederland','een','politiek','laboratorium','is'))
  
  # 7 : VVD : Doe recht aan alle werkenden , betaald of onbetaald . 
  quotes = get_quotes(tokens[tokens$sentence == 7,])
  expect_equal(nrow(quotes), 12)
  .check(tokens, quotes, source=c("VVD",':',',','.'), quote=c('Doe','recht','aan','alle','werkenden','betaald','of','onbetaald'))
})



test_that("extracting clauses works", {
  tokens = as_tokenindex(tokens_dutchclauses)

  tq = tquery(label='target', 
              children(relation = 'cnj', label='conj'))

  find_nodes(tokens[tokens$sentence == 1,], 
             tquery(POS='verb', label='pred',
                   children(relation='su', label='subject',
                            children(relation='cnj', label='conj'))))
  
  clauses = get_clauses(tokens)
  
  # subject and subject -> verb -> object
  clauses = get_clauses(tokens[tokens$sentence == 1,])
  annotate_nodes(tokens[tokens$sentence == 1,], clauses, column='test')
  expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, subject=c('Jantje','en','Piet'), predicate = c('hebben','ruzie','.'))
  
  # subject -> verb -> object (with added nonsense)
  clauses = get_clauses(tokens[tokens$sentence == 2,])
  expect_equal(nrow(clauses), 10)
  .check(tokens, clauses, subject='Jantje', predicate = c('slaat','Piet','op','zijn','hoofd','met','een','hamer','.'))
  
  # passive: subject <- is/becomes verb <- object
  clauses = get_clauses(tokens[tokens$sentence == 3,])
  expect_equal(nrow(clauses), 4)
  .check(tokens, clauses, predicate = c('Piet','geslagen','door'), subject='Jantje')

  # passive: object <- is/becomes verb <- subject (complex subject; should this be filtered on nouns?)
  clauses = get_clauses(tokens[tokens$sentence == 4,])
  expect_equal(nrow(clauses), 7)
  .check(tokens, clauses, predicate = c('Piet','geschrokken','door'), subject=c('de',"aanval",'van','Jantje'))
  
  # quote and clause: object says: subject -> verb -> object
  clauses = get_clauses(tokens[tokens$sentence == 5,])
  expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, predicate = c('hem','heel','hard','geslagen','heeft'), subject="Jantje")
  
  # quote, clause and negation: subject says: subject -> dit not verb -> object
  quotes = get_quotes(tokens[tokens$sentence == 6,])
  clauses = get_clauses(tokens[tokens$sentence == 6,])
  expect_equal(nrow(clauses), 6)
  .check(tokens, clauses, subject='ik', predicate = c('heb','Piet','niet','zomaar','geslagen'))
  
  #tokens_dutchclauses %>%
  #  as_tokenindex() %>%
  #  annotate_tqueries('clauses', alpino_clause_queries()) %>%
  #  syntax_reader(annotation='clauses', value='subject')
  
})
