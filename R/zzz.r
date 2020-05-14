.onAttach <- function(...) {
  max_threads = 2   ## has to be <= 2 for daily CRAN checks
  if (data.table::getDTthreads() > max_threads) {
    set_rsyntax_threads(max_threads)
    packageStartupMessage(sprintf('rsyntax uses the data.table package, but limits the number of threads used:\n\t- data.table currently uses %s threads\n\t- rsyntax uses %s threads\n\nYou can use set_rsyntax_threads() to use all data.table threads, or set a specific number', data.table::getDTthreads(), max_threads))
  }
}
