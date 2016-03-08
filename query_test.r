library(amcatr)
library(rsyntax)

source('R/query.r')

conn = amcat.connect('http://preview.amcat.nl')
tokens = amcat.gettokens(conn, project = 978, articleset = 24249, module = 'alpino', only_cached = T, page_size = 1000)

queries = data.frame(code = c('pim fortuyn', 'pim fortuyn', 'leefbaar nederland', 'nederland', 'beyonce'),
               indicator = c('pim', 'fortuyn', 'leefbaar', 'nederland*', 'beyonce'),
               condition = c('fortuyn OR (leefbaar AND nederland)', 'pim OR (leefbaar AND nederland)', 'nederland*~2', '', 'destiny OR child'))

tokens = read.csv('~/Dropbox/tokens/tokens/set18585_1to5000.csv')
head(tokens)

tokens[tokens$aid == 96469707,]

test = searchQuery(tokens, 'wilders', 'geert OR pvv')
test

testQuery(tokens, 'wilders', 'pvv OR geert', tokenfreq = F, keywordIC = F)
testQuery(tokens, 'wilders', 'pvv OR geert', tokenfreq = T, keywordIC = F)
testQuery(tokens, 'wilders', 'pvv OR geert', tokenfreq = F, keywordIC = T)
testQuery(tokens, 'wilders', 'pvv OR geert')


testQuery(tokens, 'energie*', 'duurza* OR groen* OR milieuvriendelijk*')


reportQueryResults(hits)

tokens = codeTokens(tokens, queries)
table(tokens$code)
rsyntax:::getQueryMatrix
grepl('test/dit', 'test/ dit')

table(tokens$pos1[tokens$code == 'nederland'])

tokens = codeTokens(tokens, queries, filter=tokens$pos1 == 'M')
table(tokens$code)


politicians = read.csv('~/Dropbox/oekraine/politicians.csv')
queries = politicians

amcat.getarticlemeta(conn, article_ids = 96468994)

head(tokens)
table(tokens$code)

head(tokens)
tokens[!tokens$concept == '',]
