function(){
  
tokens = tokens_dutchclauses

quote = alpino_quote_queries()
clause = alpino_clause_queries()

nodes = apply_queries(tokens, quote)
tokens = annotate_nodes(tokens, nodes, column='quote', fill=T)

nodes = apply_queries(tokens, clause)
tokens = annotate_nodes(tokens, nodes, column='clause', fill=T)

tokens[1:100,]

'quote: source > quote'
quote = 'source:lemma > quote:lemma' 
annotate_nodes(tokens, apply_queries(tokens, clause$no_object), column='test')


'quote:source(lemma) > clause:subject(lemma) ~ clause:predicate'




columns = c('quote','clause')
umatches = unique(subset(tokens, select=paste(columns, 'id', sep='_')))
umatches


unique(paste(tokens$quote_id,tokens$clause_id))

tokens

}