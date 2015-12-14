.SAY_VERBS = c("tell", "show", " acknowledge", "admit", "affirm", "allege", "announce", "assert", "attest", "avow", "claim", "comment", "concede", "confirm", "declare", "deny", "exclaim", "insist", "mention", "note", "proclaim", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "write", "add")
.QUOTE_RELS=  c("ccomp", "dep", "parataxis", "dobj", "nsubjpass")
.SUBJECT_RELS = c('su', 'nsubj', 'agent') 

#' Get all quotes in the tokens
#' 
#' Runs the syntax rules to get quotes/paraphrases from the sentences.
#' Note: The token ids should be globally unique, see unique_ids!
#' 
#' @param tokens a data frame of tokens containing id, parent, relation, pos1 and lemma columns. 
#' @return the quotes in long format, i.e. a data frame with quote id, quote role, and token id
#' @export
get_quotes <- function(tokens) {
  
  src_expr=list(relation__in=.SUBJECT_RELS, rename="source")
  quote_expr=list(relation__in=.QUOTE_RELS, rename="quote")
  
  quotes = find_nodes(tokens, lemma__in=.SAY_VERBS, children=list(src_expr, quote_expr))
  
  # check for xcomp - subjects
  quotes_nosrc = subset(find_nodes(tokens, lemma__in=.SAY_VERBS, children=list(quote_expr)), !(id %in% quotes$id))
  xcomp_src = find_nodes(tokens, pos1="V", children=list(src_expr, list(relation="xcomp", id__in=quotes_nosrc$id)))
  xcomp_src$id = NULL
  quotes = rbind(quotes, merge(quotes_nosrc, xcomp_src, by.x="id", by.y="xcomp"))
  
  
  
  quotes$quote_id = seq_along(quotes$id)
  quotes = melt(quotes, id.vars=c("quote_id", "id"))
  colnames(quotes) = c("quote_id", "key", "quote_role", "id")
  
  filled= fill(id=quotes$id, tokens)
  quotes = merge(quotes, filled)
  quotes$id <- NULL
  colnames(quotes)[4] = "id"
  quotes
}
  

#' Get the clauses for the given sentences
#' 
#' Runs the grammar rules on the tokens to get clauses (subject - predicate pairs).
#' If you give the quotes  (as returned by get.quotes), they will be excluded from being clauses as well.
#' Note: the token ids should be globally unique, see unique_ids. 
#' If the tokens contain a boolean 'attack' column, nouns with attack=T are considered for nominal actions
#' 
#' @param tokens a data frame of tokens containing id, parent, relation, pos1 and lemma columns. 
#' @param quotes optionally, the quotes in long format, i.e. a data frame with quote id, quote role, and token id
#' @return the clauses in long format, i.e. a data frame with clause id, clause role, and token id
#' @export
get_clauses <- function(tokens, quotes=NULL) {
  block = if (is.null(quotes)) NULL else unique(c(quotes$id[quotes$quote_role == "source"], quotes$key))
                                                
  clauses = data.frame(subject=tokens$id[tokens$relation %in% .SUBJECT_RELS & !(tokens$id %in% block)])
  clauses$predicate = tokens$parent[match(clauses$subject, tokens$id)]
  
  # remove non-verbal predicates
  clauses = clauses[tokens$pos1[match(clauses$predicate, tokens$id)] == 'V', ]
  
  # add passives without agent
  passives = tokens$parent[tokens$relation == "nsubjpass"]
  passives = setdiff(passives, clauses$predicate)
  clauses = rbind(clauses, data.frame(subject=rep(NA, length(passives)), predicate=passives))
  
  # add verbal xcomps
  xcomps = find_nodes(tokens, children=c("xcomp", "dobj"))
  xcomps = xcomps[!(xcomps$xcomp %in% c(block, clauses$subject)), ]
  
  clauses = rbind(clauses, data.frame(subject=xcomps$dobj, predicate=xcomps$xcomp))
  
  # add nominal actions. Currently restrict to 'attack' nouns, should get a list of nominal actions somewhere
  if (!is.null(tokens$attack)) {
    tokens$action = tokens$attack & !(tokens$lemma %in% c("soldier", "troops"))
    nominal = find_nodes(tokens, pos1="N", action=T, children="poss")
    clauses = rbind(clauses, data.frame(subject=nominal$poss, predicate=nominal$id))
    
    # battle between X and Y
    battles = find_nodes(tokens, pos1="N", attack=T, children=list("prep_between", rename="actor"))
    clauses = rbind(clauses, data.frame(subject=battles$actor, predicate=battles$id))
  } else battles = NULL
  
  
  
  
  # deal with conjunctions
  pred_tokens = merge(clauses, tokens[c("id", "relation", "parent")], by.x="predicate", by.y="id")
  conj = with(pred_tokens[pred_tokens$relation == "conj_and",], data.frame(subject=subject, predicate=parent))
  clauses = rbind(clauses, conj)
  
  clauses$clause_id = 1:nrow(clauses)
  
  # Deal with subordinate 'who' clauses
  parents = match(tokens$parent, tokens$id)
  grandparents = tokens$id[parents[parents]]
  subord_who = tokens$id[!is.na(grandparents) & tokens$lemma == "who" & tokens$relation[parents] == "rcmod"]
  clause_gps = grandparents[match(clauses$subject, tokens$id)]
  clauses$subject[clauses$subject %in% subord_who] = clause_gps[clauses$subject %in% subord_who]
  
  # melt to long format, fill out children, and return
  clauses = melt(clauses, id.vars="clause_id", na.rm = T)
  if (!is.null(battles)) {
    # add battle objects to predicate
    battles = subset(merge(battles, battles, by="id"), actor.x != actor.y)
    extra = merge(subset(clauses, variable=="subject"), battles, by.x="value", by.y="actor.x")
    if (nrow(extra)>0) clauses = rbind(clauses, with(extra, data.frame(clause_id=clause_id, variable="predicate", value=actor.y)))
  }
  
  
  
  filled= fill(id=clauses$value, tokens, block=c(clauses$value, block))
  clauses = merge(clauses, filled, by.x="value", by.y="id")
  
  
  with(clauses, data.frame(clause_id=clause_id, clause_role=variable, id=filled))
}
