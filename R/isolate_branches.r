#' Isolate a branch in a dependency tree
#'
#' cuts of a branch at the nodes that match the lookup arguents (...).
#' A "branch_parent" column is added to the tokenindex, that indicates for the new roots
#' which node the parent was.  
#'
#' @param tokens 
#' @param ...      lookup arguments to find the node to split. For example, isolate_branch(tokens, relation='relcl') 
#'                 isolates branches of which the top node (the new root) has the relation "relcl". 
#' @param copy_parent If TRUE (default) copy the parent of the branch and include it in the isolated branch
#'
#' @return
#' @export
#'
#' @examples
isolate_branch <- function(tokens, ..., copy_parent=T) {
  tokens = data.table::copy(tokens)
  if (!copy_parent) {
    ## this is simply, because there can be no issues with nesting, so we can split everything in one go
    tq = tquery(label='parent',
                children(..., label='branch'))
    tokens = select_nodes(tokens, tq)
    tokens = mutate_nodes(tokens, 'branch', parent = NA, relation = 'ROOT', branch_parent=parent$token_id)
    return(tokens)  
  }
  
  ## if we do copy the parent, we need to do it recursively from root to bottom 
  tq = tquery(label='cut_here',                ## the cut_here is necessary for the recursive loop
              children(label='parent',
                       children(..., label='branch')))
  
  rec_isolate <- function(tokens, tq) {
    tokens = select_nodes(tokens, tq, fill_only_first=T, .one_per_sentence = T)
    if (nrow(selected_nodes(tokens)$nodes) == 0) return(tokens)
    tokens = copy_nodes(tokens, 'parent', 'parent_copy', copy_fill=T)
    tokens = mutate_nodes(tokens, 'branch', parent = parent_copy$token_id)
    tokens = mutate_nodes(tokens, 'parent_copy', parent = NA, relation = 'ROOT', branch_parent=cut_here$token_id)
    rec_isolate(tokens, tq)
  }
  rec_isolate(tokens, tq)
}

#' Add the branch id as a column to the tokenindex
#'
#' After splitting trees into branches 
#'
#' @param tokens  A tokenindex
#'
#' @return the tokenindex
#' @export
#'
#' @examples
get_branch_id <- function(tokens) {
  tokens[, branch_id := ifelse(is.na(tokens$parent), tokens$token_id, NA)]
  tokens = fix_missing_parents(tokens)
  
  i = which(is.na(tokens$parent))
  safe_count = 1
  while(TRUE) {
    parents = tokens[i,c('doc_id','sentence','token_id','branch_id')]
    data.table::setnames(parents, 'token_id','parent')
    parents = merge(parents, tokens[,c('doc_id','sentence','token_id','parent')], by=c('doc_id','sentence','parent'))
    if (nrow(parents) == 0) break
    i = tokens[parents, on=c('doc_id','sentence','token_id'), which=T]
    tokens[i, branch_id := parents$branch_id]
    
    if (safe_count == 200) {
      warning("stopped recursive loop at iteration 200. This is supposedly the depth of the tree, but
              since language is not THAT complex (unless you're working with German philosophers) it is
              most likely that something else went wrong. Please check your data or submit a bug report if its my fault")
    }
    safe_count = safe_count + 1
  }
  tokens
}

split_conjunctions <- function(tokens) {
  ## ignore most fill nodes for distant conjunctions (>= 3 words)
  tokens = split_tree(tokens, rel='conj', no_fill=c('relcl', 'conj', 'cop', 'acl', 'dobj', 'advmod','advcl','xcomp','obl','ccomp','aux','det'), min_dist = 3)
  ## copy most fill nodes for close conjunctions
  tokens = split_tree(tokens, rel='conj', no_fill=c('relcl', 'conj', 'cop'), max_dist=2)
  chop(tokens, relation='cc')
}

split_tree <- function(tokens, rel='conj', no_fill=NULL, min_dist=0, max_dist=Inf) {
  tq = tquery(label='target', NOT(relation = rel),
              children(relation = c('compound*', 'flat', 'amod'), label='ignore', req=F),
              children(NOT(relation=rel), max_window=c(0,Inf), label='ignore2', req=F, connected=T),
              fill(NOT(relation = no_fill), max_window = c(Inf,Inf), connected=T),
              children(relation = rel, label='origin', min_window=min_dist, max_window = max_dist,
                       fill(NOT(relation = no_fill), max_window=c(0,Inf), connected=T)))
  ## ok, this requires some explanation
  ## essentially the tquery looks for an 'origin' node with a specific relation, and 
  ## its parent 'target'. We want the origin to copy the position of the target node,
  ## and to adopt certain fill nodes from the target, but not all.
  ## The arguments to limit fill are not sufficient, so we add two children queries (that have priority over fill) for more control
  ## The first one, with label "ignore", prevents compounds of the target
  ## The second one, "ignore2", prevents all nodes between the origin and target by taking all nodes untill the relation.
  
  tokens = climb_tree(tokens, tq)
}

print_sentences <- function(tokens, sentence_i=1, token_col='token') {
  sentences = unique(tokens[,c('doc_id','sentence')])
  if (sentence_i > nrow(sentences)) stop('sentence_i is higher than number of sentences in tokens')
  sents = get_branch_id(tokens[sentences[1,], on=c('doc_id','sentence')])
  
  bp = sents[!is.na(sents$branch_parent),c('doc_id','sentence','branch_parent','token_id')]
  bp = merge(bp, sents[,c('doc_id','sentence','token_id','branch_id')], by.x=c('doc_id','sentence','branch_parent'), by.y=c('doc_id','sentence','token_id'), all.x=T)
  sents[bp, branch_parent_id := bp$branch_id, on=c('doc_id','sentence','token_id')]
  
  get_bp <- function(x) if (any(!is.na(x))) first(na.omit(x)) else numeric()
  sents = sents[,list(doc_id=unique(doc_id), sentence=unique(sentence), branch_parent=get_bp(branch_parent_id), text=paste(get(token_col), collapse=' ')), by='branch_id']
  
  for (i in which(is.na(sents$branch_parent))) {
    rec_print_sentences(sents, i)
    cat('\n')
  }
  tokens
}

rec_print_sentences <- function(sents, ivec, level=1) {
  if (length(ivec) == 0) return(NULL)
  for (i in ivec) {
    cat(rep('  ', level), gsub('\n', '', sents$text[i]), '\n')
    rec_print_sentences(sents, which(floor(sents$branch_parent) == floor(sents$branch_id[i])), level=level+1)
  }
}




function() {
  library(magrittr)
  library(spacyr)
  library(data.table)
  spacy_initialize('en_coref_lg')
  
  "The muslims, who loved cheese and toast, burned the churches and houses because they could" %>%
    spacy_parse(dependency=T, coref=T) %>%
    as_tokenindex() %>%
    isolate_branch(relation = c('acl:relcl','relcl','advcl','dep','appos','acl'), copy_parent = T) %>%
    split_conjunctions() %>%
    plot_tree(token, lemma, pos)
  
  tokens = readRDS('~/Dropbox/restecode/Suspect_communities/backup.rds')
  tokens = as_tokenindex(tokens)
  
  tokens['australia-news/2014/dec/15/man-haron-monis-sydney-siege-suspect', on='doc_id'] %>%
    spacy_quotes() %>%
    syntax_reader('quote','source')
  
  
  
  islam = unique(tokens[grep('islam|muslim', tokens$token, ignore.case = T),c('doc_id','sentence')])
  terror = unique(tokens[grep('terror', tokens$token, ignore.case = T),c('doc_id','sentence')])
  both = islam[terror, on=c('doc_id','sentence')]
  
  i=2
  message(paste(tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')]$token, collapse=' '))
  tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    isolate_branch(relation = c('acl:relcl','relcl','advcl','dep','appos','acl'), copy_parent = F) %>%
    split_conjunctions() %>%
    print_sentences() %>%
    plot_tree(token, lemma, pos, coref_text)
  
  ## add coref to token
  has_coref = !is.na(tokens$coref_text)
  copy_coref = has_coref & is.na(c(tokens$coref_text[-1], NA)) ## for multi token names
  tokens$token = ifelse(!copy_coref, tokens$token, sprintf('%s [%s]', tokens$token, gsub(' ','_', tokens$coref_text)))
  
  #tokens = tokens[both, on=c('doc_id','sentence') ,]
  #View(tokens['politics/2003/nov/22/religion.september11', on=c('doc_id')])
  
  
  tokens %>%
    spacy_quotes() %>%
    syntax_reader('quote', 'source', random_seed = 2)
  
  
  t = tokens[c('commentisfree/2016/aug/31/jokowis-islamist-challenge-curbing-terrorism-and-religious-intolerance',
            'world/2016/may/21/egyptair-plane-crash-ms804',
            'uk/2007/jul/03/terrorism.religion'), on='doc_id']

  t$doc_id = as.character(match(t$doc_id, unique(t$doc_id)))
  
  t %>%
    spacy_quotes() %>%
    syntax_reader('quote', 'source', random_seed = 2)
  
  
  
  t[c('commentisfree/2016/aug/31/jokowis-islamist-challenge-curbing-terrorism-and-religious-intolerance',
      'world/2016/may/21/egyptair-plane-crash-ms804',
      'uk/2007/jul/03/terrorism.religion'), on='doc_id']%>%
    syntax_reader('quote', 'source', random_seed = 2)
  
  
  
  tokens[list(doc_id='world/live/2014/oct/07/islamic-state-fighters-battle-kurdish-militia-for-kobani-live-updates'),] %>%
    spacy_quotes() %>%
    plot_tree(token, lemma, pos, coref_text, sentence_i=1, annotation='quote')
  
  
  tokens = tokens[both, on=c('doc_id','sentence')] %>%
    spacy_quotes() %>%
    spacy_actions() %>%
    spacy_links()
  
  syntax_reader(tokens, 'quote', 'source')
  syntax_reader(tokens, 'action', 'subject')
  
  
  spacy_parse('Every terrorist who thought he could do this did it.', dependency=T, coref=T) %>%
    as_tokenindex() %>%
    spacy_quotes() %>%
    plot_tree(annotation='quote')
  
  
  
  
  
  
  #write.csv(tokens[,c('doc_id','sentence','token','coref_text')], '~/Desktop/test.csv')

  sents = tokens[both[1,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    isolate_branch(relation = c('appos','acl','acl:relcl','relcl','advcl')) %>%
    #split_conjunctions() %>%
    plot_tree(token, lemma, pos)
  
  tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    #isolate_verb_modifier() %>%
    #isolate_noun_modifier() %>%
    #split_conjunctions() %>%
    spacy_actions() %>%
    plot_tree(token, lemma, pos, coref_text, annotation='action')
  
  

  
  i=4
  message(paste(tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')]$token, collapse=' '))
  tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    isolate_branch(relation = c('acl:relcl','relcl','advcl','dep','appos','acl'), copy_parent = F) %>%
    split_conjunctions() %>%
    print_sentences() %>%
    plot_tree(token, lemma, pos, coref_text)
  
  message(paste(tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')]$token, collapse=' '))
  tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    isolate_noun_modifier() %>%
    isolate_verb_modifier() %>%
    split_conjunctions() %>%
    print_sentences() %>%
    plot_tree(token, lemma, pos, coref_text)
  
  
  
  "The police, who are good people, and whoe li" %>%
    spacy_parse(dependency=T, coref=T) %>%
    as_tokenindex() %>%
    spacy_actions() %>%
    plot_tree(token, lemma, pos, annotation='action')
  
  
  
  
  library(magrittr)
  
  tokens = readRDS('~/Dropbox/restecode/Suspect_communities/backup.rds')
  tokens = as_tokenindex(tokens)
  
  #tokens$token = ifelse(is.na(tokens$coref_text), tokens$token, gsub(' ','_', tokens$coref_text))
  
  islam = unique(tokens[grep('islam|muslim', tokens$token, ignore.case = T),c('doc_id','sentence')])
  terror = unique(tokens[grep('terror', tokens$token, ignore.case = T),c('doc_id','sentence')])
  both = islam[terror, on=c('doc_id','sentence')]

  tokens %>%
    spacy_quotes() %>%
    syntax_reader('quote', 'source', random_seed = 2)
    
  
  tokens = tokens[both, on=c('doc_id','sentence')] %>%
    spacy_quotes() %>%
    spacy_actions() %>%
    spacy_links()
  
  syntax_reader(tokens, 'quote', 'source')
  syntax_reader(tokens, 'action', 'subject')
  
  
  spacy_parse('Every terrorist who thought he could do this did it.', dependency=T, coref=T) %>%
    as_tokenindex() %>%
    spacy_quotes() %>%
    plot_tree(annotation='quote')
    
    
  
  
  
  
  
  extract_annotations <- function(tokens, ann, regex=NULL, ..., regex_ann=NULL, regex_col='token') {
    s = !is.na(tokens[[paste0(ann,'_id')]])
    for (r in regex) s = s & grepl(r, tokens[[regex_col]], ...)
    if (!is.null(regex_ann)) s = s & tokens[[ann]] == regex_ann 
    ann_id = unique(tokens[s, get(paste0(ann,'_id'))])
    tokens[list(ann_id), on=paste0(ann,'_id')]
  }
  
  test = extract_annotations(tokens, 'link', regex = 'islam|muslim', regex_ann='subject', ignore.case=T)
  x = test[,list(text=paste(token, collapse=' ')), by=c('link_id')]
  View(x)
  


  test = extract_annotations(tokens, 'quote', regex = 'islam|muslim', regex_ann='source', ignore.case=T)
  x = test[,list(text=paste(token, collapse=' ')), by=c('quote_id')]
  View(x)
  
    
  
  tokens2 = tokens %>%
    isolate_branch(relation = c('acl:relcl','relcl','advcl','dep')) %>%
    isolate_branch(relation = c('appos','acl'), min_dist=4) %>%
    split_conjunctions() %>%
    
  tokens2 = tokens2 %>%  
    spacy_links()
  
  
  test = extract_annotations(tokens2, 'link', regex = 'islam|muslim', ignore.case=T)
  x = test[,list(text=paste(token, collapse=' ')), by=c('link_id')]
  View(x)
  
  test = extract_annotations(tokens2, 'link', regex = c('islam|muslim','terror'), ignore.case=T)
  View(test[,list(text=paste(token, collapse=' ')), by=c('link_id')])
  
  
  i=1
  tokens[both[i,c('doc_id','sentence')], on=c('doc_id','sentence')] %>%
    isolate_noun_modifier() %>%
    isolate_verb_modifier() %>%
    split_conjunctions() %>%
    print_sentences() %>%
    plot_tree(token, lemma, pos, coref_text)
  
  
}