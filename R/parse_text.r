
#' Parse text using udpipe
#'
#' @param text       A character vector with text(s) to parse
#' @param udpipe_model   The name of a udpipe language model (e.g., "english", "dutch", "german"), to use the udpipe package to perform natural language processing. On first use, the model will be downloaded to the location specified in the udpipe_model_path argument. 
#' @param udpipe_model_path If udpipe_model is used, this path wil be used to look for the model, and if the model doesn't yet exist it will be downloaded to this location. If no path is given, the directory in which the corpustools package is installed will be used. 
#'
#' @return A tokenIndex
#' @export
udpipe_tokenindex <- function(text, udpipe_model='english', udpipe_model_path=getOption("corpustools_resources", NULL)) {
  tc = corpustools::create_tcorpus(text, udpipe_model=udpipe_model, use_parser=T)
  as_tokenindex(tc$tokens)
}
