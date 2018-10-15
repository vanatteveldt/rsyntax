annotation_pipe <- function(...) {
  l = list(...)
  invalid = !sapply(l, class) %in% c('tQuery','tReshape')
  if (any(invalid)) stop(sprintf('Items need to be tQuery or tReshape objects'))
  class(l) = c('annotationPipe', class(l))
  l
}

print.annotationPipe <- function(x, ...) {
  print(x$reshape)
  cat('\n')
  for (i in seq_along(x$queries)) {
    cat('\n')
    if (is.null(names(x$queries)[i])) {
      cat('tQuery', i, '\n')
    } else print(names(x$queries)[i])
    print(x$queries[[i]])
  }
}

