library(nlpiper)
options(nlpiper.token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ2ZXJzaW9uIjoxLCJpYXQiOjE1MDE4NzgzMDF9.Zqjoo_NcUTWQu6bZce4q3FhQRcKEbwfkzi2w2-yqK2g")
options(nlpiper.server="https://nlpipe.amcat.nl")

# zorg dat rsyntax up-to-date is:
# devtools::install_github('vanatteveldt/rsyntax')

## dezelfde zin, met en zonder komma (bepalend voor waar de "omdat" op terugslaag)
tokens = nlpiper::process("alpinocoref", format = 'csv',
                          text = "Ik haat Piet, die Jan sloeg, omdat hij boos was. Ik haat Piet, die Jan sloeg omdat hij boos was.")

## eerst de standaard quotes en clauses eraan hangen
tokens = annotate_qc(tokens, 
                     quote_rules = alpino_quote_rules(),
                     clause_rules = alpino_clause_rules())

## maak regel voor 'omdat' relatie
omdat = rule(save='connection', lemma = c('omdat','vanwege'),
             parents(save = 'consequence'),
             children(save = 'cause'))
tokens = annotate(tokens, omdat, 'causal')

plot_tree(tokens, sentence_i = 1)
plot_tree(tokens, sentence_i = 2)

tokens = nlpiper::process("alpinocoref", format = 'csv',
                          text = "Ik haat Piet, die Jan sloeg, omdat hij boos was. Ik haat Piet, die Jan sloeg omdat hij boos was.")


## voor de duidelijkheid: je mag meerdere rules gewoon in een list stoppen. Ze worden dan in de opgegeven volgorde uitgevoerd.
## (als voorbeeld hier alleen de enkele causal regel)
causal_rules = list(omdat = omdat)
causal_rules
tokens = annotate(tokens, causal_rules, 'causal')

