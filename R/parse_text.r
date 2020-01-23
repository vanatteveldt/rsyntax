
#' Parse text using udpipe
#'
#' @param text       A character vector with text(s) to parse
#' @param udpipe_model Optionally, the name of a Universal Dependencies language model (e.g., "english-ewt", "dutch-alpino"), to use the udpipe package
#'                     (\code{\link[udpipe]{udpipe_annotate}}) for natural language processing. You can use \code{\link{show_udpipe_models}} to get
#'                     an overview of the available models. For more information about udpipe and performance benchmarks of the UD models, see the
#'                     GitHub page of the \href{https://github.com/bnosac/udpipe}{udpipe package}.
#' @param udpipe_model_path This path wil be used to look for the model, and if the model doesn't yet exist it will be downloaded to this location. Defaults to working directory
#'
#' @return A tokenIndex
#' @export
udpipe_tokenindex <- function(text, udpipe_model='english-ewt', udpipe_model_path=getwd()) {
  tc = corpustools::create_tcorpus(text, udpipe_model=udpipe_model, use_parser=T)
  as_tokenindex(tc$tokens)
}
