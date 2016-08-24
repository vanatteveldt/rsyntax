### ANAPHORA

.ANAPHORA = list('hij' = c(gender='male', number='single'),
                 'zijn' = c(gender='male', number='single'),
                 'hem' = c(gender='male', number='single'),
                 'zij' = c(gender='female', number='single'),
                 'ze' = c(gender='female', number='single'),
                 'zich' = c(),
                 'zichzelf' = c(),
                 'zij' = c(number='plural'),
                 'ze' = c(number='plural'),
                 'hen' = c(number='plural'),
                 'hunzelf' = c(number='plural'),
                 'henzelf' = c(number='plural'),
                 'wij' = c(),
                 'ons' = c(), 
                 'onze' = c(),
                 'zich' = c(),
                 'zichzelf' = c())


.ANAPHORA = list(male_single = c('hij','zijn','hem','zich','zichzelf'),
                 male_plural = c(),
                 female_single = c('zij','ze','haar'),
                 female_plural = c(),
                 plural = c('zij','ze','hen','hun','hunzelf','henzelf','wij','ons','onze','zich','zichzelf'),
                 single = c('ik','me','mij','mijn', 'zich', 'zichzelf'))

.ANAPHORA_selfref = c('ik','me','mij','mijn','wij','ons','onze')
.ANAPHORA_reflexive = c('zich','zichzelf','haar','haarzelf')

.ANTEDECENT_rel = c('su','obj1')

getGenderNumberTable <- function(gnterms=.ANAPHORA){
  gender_vals = c('male','female', NA)
  number_vals = c('single','plural', NA)
  
  gn_combis = data.frame(gender = rep(gender_vals, each=length(number_vals)),
                         number = rep(number_vals, times=length(gender_vals)))
  
  qnterms_vec = sapply(names(gnterms), strsplit, split='_')
  for(i in 1:nrow(qn_combis)){
    gendermatch = grepl(gn_combis$gender[i], names(gnterms))
    
    gn_combis[i,]
    
    q = names(gnterms)[i]
    
    gender = rep(F, nrow(tokens))
    if(grepl('male', q) & (grepl('male', ))) 
      
      gender = 'male'
    number = NA
    
    
    gender = grep('male|female', names(.ANAPHORA[i]), value = T)
  }
}

codeAnaphora <- function(anaphoralist, x) {
  a = rep(NA, length(x))
  for(i in 1:length(anaphoralist)){
    pattern = paste(sprintf('\\b%s\\b', anaphoralist[[i]]), collapse='|')
    matched = grepl(pattern, x, ignore.case = T)
    condition = names(anaphoralist)[i]
    a[matched] = ifelse(is.na(a[matched]), names(anaphoralist)[i], paste(a[matched], names(anaphoralist)[i], sep='|'))
  }
  a
}

possibleAnaphora <- function(tokens, anophoraDict){
  
}


anaphora_resolution <- function(tokens, antecedent_memory=10){
  ## niet in zelfde clause zoeken (ccommand)
  
  ## define antecedents (to which anaphora can refer) and anaphora
  tokens$antecedent = ifelse(tokens$relation %in% .ANTEDECENT_rel, T, F)
  tokens$anaphora = codeAnaphora(.ANAPHORA, tokens$word)
  
  ## create data.frame where each anaphora matches the preceding n antecedents (n = antecedent_membory)
  antecedents = tokens[tokens$antecedent | !is.na(tokens$anaphora),]
  antecedents$i = 1:nrow(antecedents)
  antecedents$gendnumb = paste(if(!is.null(antecedents$gender)) antecedents$gender else rep('male,fema', nrow(antecedents)), 
                                    if(!is.null(antecedents$number)) antecedents$number else rep('sing,plur', nrow(antecedents)), sep=',') 
  antecedents = antecedents[,c('i', 'aid','id','sentence','pos','relation','gendnumb')]
  

  anaphora = antecedents[!is.na(antecedents$anaphora), c('i', 'aid','sentence', 'pos','relation', 'anaphora')]
  candidate_i = sapply(anaphora$i, simplify = F, function(x) (x-antecedent_memory):(x-1))
  anaphora = anaphora[rep(1:nrow(anaphora), each=antecedent_memory),] # copy each anaphora row to match with antecedent candidates
  anaphora$candidate_i = unlist(candidate_i) # add antecedent candidate index to the duplicated rows
  anaphora = anaphora[anaphora$candidate_i > 0,] # ignore candidates 

  colnames(antecedents) = paste('a', colnames(antecedents), sep='.')
  anaphora = cbind(anaphora, antecedents[anaphora$candidate_i,])
  
  ## if number and gender information for tokens is available, filter candidates accordingly
}

antecedentLagMatrix <- function(antecedents){
  newaid = which(!duplicated(antecedents$aid))
  nextaid = c(newaid[-1], nrow(antecedents)+1)
  repeat_multiplier = nextaid - newaid
  
  antecedents$start = 1:nrow(antecedents)
  antecedents$end = rep(nextaid-1, repeat_multiplier)
  
  antlag = antecedents[, c('id', 'start', 'end')]
  antlag_i = mapply(function(start,end) start:end, antlag$start, antlag$end)
  antlag_i_len = sapply(antlag_i, length)
  
  antlag_i = unlist(antlag_i)
  antlag_j = rep(antlag$id, antlag_i_len)
  antlag_v = unlist(mapply(function(start, len) rep(start, len), antlag$start, antlag_i_len))
  
  spMatrix(nrow(antecedents), max(antlag_j), antlag_i, antlag_j, antlag_v)
}

lagMatrix <- function(tokens){
  newaid = which(!duplicated(antecedents$aid))
  nextaid = c(newaid[-1], nrow(en)+1)
  repeat_multiplier = nextaid - newaid
  
  entities$start = 1:nrow(entities)
  entities$end = rep(nextaid-1, repeat_multiplier)
  
  entlag = entities[!is.na(entities$entity), c('id', 'start', 'end')]
  entlag_i = mapply(function(start,end) start:end, entlag$start, entlag$end)
  entlag_i_len = sapply(entlag_i, length)
  
  entlag_i = unlist(entlag_i)
  entlag_j = rep(entlag$id, entlag_i_len)
  entlag_v = unlist(mapply(function(start, len) rep(start, len), entlag$start, entlag_i_len))
  
  spMatrix(nrow(entities), max(entlag_j), entlag_i, entlag_j, entlag_v)
}

 
 