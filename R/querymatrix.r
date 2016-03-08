## Contains the functions to create a sparse matrix giving the windows within which queries occur.
## Please note that simplicity is sacrificed for performance without mercy

#' @export
getQueryMatrix <- function(tokens, query_regex, aid_var='aid', term_i_var='id', term_var='word', presorted=F){
  levels(tokens[,term_var]) = gsub('_', ' ', levels(tokens[,term_var])) # consider underscores as wordboundaries
  
  if(!presorted){
    ## unless explicitly noted that the token list is sorted, first sort tokens by article id and word id. (keeps order to restore original order)
    ord = order(tokens[,aid_var], tokens[,term_i_var])
    tokens = tokens[ord,]
  }
  
  tokens_filter = which(tokenGrepl(query_regex$regex, tokens[,term_var]))
  m = getWindowMatrix(tokens[tokens_filter,term_i_var], 
                      tokens[tokens_filter,  aid_var], 
                      tokens[tokens_filter, term_var], 
                      window.size = max(query_regex$window))
  
  query_list = list()  
  for(j in 1:nrow(query_regex)){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m > 0 & query_m <= query_regex$window[j]) > 0 # matrix value has to be higher than 0, but not higher than the window size
    if(sum(hits) == 0) next
    query_list[['']] = data.frame(i = tokens_filter[hits], j = j) 
  }
  
  qm = rbind.fill(query_list)
  qm = spMatrix(nrow(tokens), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term
  #rownames(qm) = paste(tokens[,aid_var], tokens[,term_i_var], sep='.') # currently not used, and takes up space
  
  if(!presorted) qm = qm[match(1:nrow(qm), ord),] # return matrix in order of input tokens
  
  qm
}

tokenGrepl <- function(patterns, x, ignore.case=T, perl=F){
  ## a wrapper for grepl that takes multiple patterns and is efficient for a character vector (x) with many duplicates
  unique_x = unique(x)
  pattern = paste(patterns, collapse='|')
  relevant_terms = unique_x[grepl(pattern, unique_x, ignore.case=ignore.case, perl=perl)]
  x %in% relevant_terms
}

getWindowMatrix <- function(location, context, term, window.size){
  location = stretchLocation(location, context, window.size=window.size)
  shifts = -window.size:window.size
  
  terms = unique(term)
  term_index = match(term, terms)
  m = locationMatrix(location, term_index, shifts, distance.as.value=T)
  colnames(m) = terms
  m
}

stretchLocation <- function(location, context, window.size){
  ## makes the word location counter global, and adds dummy locations between contexts to prevent overlapping windows.
  ## this way, overlapping word windows can be calculated for multiple documents within a single matrix.
  ## location and context need to be sorted on order(context,location)!! (hence the presorted argument in getQueryMatrix)
  newcontext = which(!duplicated(context)) # where does a new context start
  context.max = location[newcontext-1] + (window.size*2)
  multiplier_scores = cumsum(c(0,context.max)) # the amount that should be added to the location at the start of each context 
  repeat_multiplier = c(newcontext[2:length(newcontext)], length(location)+1) - newcontext # the number of times the multiplier scores need to be repeated to match the location vector
  multiplier_vector = rep(multiplier_scores, repeat_multiplier)
  return(location + multiplier_vector)
}

locationMatrix <- function(i, j, shifts=0, count.once=T, distance.as.value=F){
  mat = spMatrix(max(i), max(j))
  
  shifts = shifts[order(abs(shifts))] # order from 0 to higher (required if distance.as.value = T)
  for(shift in shifts){
    i_shift = i + shift
    select = i_shift > 0 & i_shift <= max(i)
    if(distance.as.value){
      mat = mat + spMatrix(nrow=max(i), ncol=max(j), i=i_shift[select], j=j[select], rep(abs(shift)+1, sum(select))) 
    } else{
      mat = mat + spMatrix(nrow=max(i), ncol=max(j), i=i_shift[select], j=j[select], rep(1, sum(select)))   
    }
  }
  
  if(distance.as.value){
    ## remove duplicates. since the stacked triples are ordered by shifts, this leaves the shortest distance to a term in case of duplicate cells
    count.once = F
    select = !duplicated(data.frame(mat@i, mat@j))
    mat = spMatrix(nrow(mat), ncol(mat), mat@i[select]+1, mat@j[select]+1, mat@x[select])
  }
  
  mat = mat[i,]
  mat = as(mat, 'dgCMatrix')
  if(count.once) mat@x[mat@x>0] = 1
  mat
}
