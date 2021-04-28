#' Create an igraph tree from a sentence
#' 
#' @description
#' Create an igraph tree from a token_index (\link{as_tokenindex}) or a data.frame that can be coerced to a tokenindex.
#' 
#' By default, all columns in the data are included as labels. This can be changes by using the ... argument.
#' 
#' @param tokens      A tokenIndex data.table, or any data.frame coercible with \link{as_tokenindex}. Can also be a corpustools tCorpus.
#' @param ...         Optionally, select which columns to include as labels and how to present them. Can be quoted or unquoted names and expressions, using columns in the tokenIndex. For example, plot_tree(tokens, token, pos) will use the $token and $pos columns in tokens. You can also use expressions for easy controll of visulizations. For example: plot_tree(tokens, tolower(token), abbreviate(pos,1)). (note that abbreviate() is really usefull here)  
#' @param sentence_i  By default, plot_tree uses the first sentence (sentence_i = 1) in the data. sentence_i can be changed to select other sentences by position (the i-th unique sentence in the data). Note that sentence_i does not refer to the values in the sentence column (for this use the sentence argument together with doc_id)
#' @param doc_id      Optionally, the document id can be specified. If so, sentence_i refers to the i-th sentence within the given document. 
#' @param sentence    Optionally, the sentence id can be specified (note that sentence_i refers to the position). If sentence is given, doc_id has to be given as well. 
#' @param annotation  Optionally, a column with an rsyntax annotation, to add boxes around the annotated nodes.
#' @param only_annotation If annotation is given, only_annotation = TRUE will print only the nodes with annotations.
#' @param pdf_file    Directly save the plot as a pdf file
#' @param allign_text If TRUE (default) allign text (the columns specified in ...) in a single horizontal line at the bottom, instead of following the different levels in the tree
#' @param ignore_rel  Optionally, a character vector with relation names that will not be shown in the tree
#' @param all_lower   If TRUE, make all text lowercase
#' @param all_abbrev  If an integer, abbreviate all text, with the number being the target number of characters. 
#' @param textsize    A number to manually change the textsize. The function tries to set a suitable textsize for the plotting device, but if this goes wrong and now everything is broken and sad, you can multiply the textsize with the given number. 
#' @param spacing     A number for scaling the distance between words (between 0 and infinity) 
#' @param use_color   If true, use colors
#' @param max_curve   A number for controlling the allowed amount of curve in the edges. 
#' @param palette     A function for creating a vector of n contiguous colors. See ?terrain.colors for standard functions and documentation
#' @param pdf_viewer  If TRUE, view the plot as a pdf. If no pdf_file is specified, the pdf will be saved to the temp folder
#' @param viewer_mode By default, the plot is saved as a PNG embedded in a HTML and opened in the viewer. This hack makes it independent of the 
#'                    size of the plotting device and enables scrolling. By setting viewer_mode to False, the current plotting device is used.
#' @param viewer_size A vector of length 2, that multiplies the width (first value) and height (second value) of the viewer_mode PNG 
#'   
#' @return  plots a dependency tree. 
#' @export
#' @examples 
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' 
#' \donttest{
#' if (interactive()) plot_tree(tokens, token, pos)
#' 
#' ## plot with annotations
#' direct = tquery(label = 'verb', pos = 'VERB', fill=FALSE,
#'                 children(label = 'subject', relation = 'nsubj'),
#'                 children(label = 'object', relation = 'dobj'))
#' passive = tquery(label = 'verb', pos = 'VERB', fill=FALSE,
#'                  children(label = 'subject', relation = 'agent'),
#'                  children(label = 'object', relation = 'nsubjpass'))
#'  
#' if (interactive()) {                
#' tokens %>%
#'    annotate_tqueries('clause', pas=passive, dir=direct) %>%
#'    plot_tree(token, pos, annotation='clause')
#' }
#' }
plot_tree <-function(tokens, ..., sentence_i=1, doc_id=NULL, sentence=NULL, annotation=NULL, only_annotation=FALSE, pdf_file=NULL, allign_text=TRUE, ignore_rel=NULL, all_lower=FALSE, all_abbrev=NULL, textsize=1, spacing=1, use_color=TRUE, max_curve=0.3, palette=grDevices::terrain.colors, pdf_viewer=FALSE, viewer_mode=TRUE, viewer_size=c(100,100)) {  
  if (methods::is(tokens, 'tCorpus')) tokens = tokens$tokens
  if (pdf_viewer && is.null(pdf_file)) pdf_file = tempfile('plot_tree', fileext = '.pdf')
  if (!is.null(pdf_file)) if (!grepl('\\.pdf$', pdf_file)) stop('pdf_file needs to have extension ".pdf"')
  if (!is.null(pdf_file)) viewer_mode = FALSE
  
  if (is.null(pdf_file) && is.null(viewer_mode)) graphics::plot.new()
  require_plot_new()
  
  tokens = as_tokenindex(tokens) 
  
  if (!is.null(annotation) && only_annotation) {
    tokens = tokens[!is.na(tokens[[annotation]]),]
  }
  
  nodes =  get_sentence(tokens, doc_id, sentence, sentence_i)
  nodes$rel_label = if (!is.null(all_abbrev)) abbreviate(nodes[['relation']], all_abbrev) else nodes[['relation']]
  nodes$label = nodes$token_id

  nodes = split_adjacent(nodes)
  
  sentmes = sprintf('Document: %s\nSentence: %s', unique(nodes$doc_id), unique(nodes$sentence))
  annotations = gsub('\\_.*', '', grep('\\_fill', colnames(nodes), value=TRUE))
  text_cols = get_text_cols(tokens, nodes, tidyselect::quos(...), annotations)
  edges = nodes[!is.na(nodes[['parent']]), c('parent', 'token_id', 'relation'), with=FALSE]
  edges = edges[edges$parent %in% nodes$token_id,]
  
  text = NULL
  for (tc in text_cols) {
    textval = if (!is.null(all_abbrev)) abbreviate(tc, minlength = all_abbrev) else tc
    textval = ifelse(is.na(textval), '', gsub('\n|\t', '', as.character(textval)))
    text = if (is.null(text)) textval else paste(text, textval, sep='\n')
  }
  if (!is.null(ignore_rel)) nodes$label[nodes$label %in% ignore_rel] = ''
  if (all_lower) {
    text = tolower(text)
    nodes$label = tolower(nodes$label)
  }
  
  g = igraph::graph.data.frame(edges, vertices=nodes, directed = TRUE)
  igraph::V(g)$id = as.numeric(igraph::V(g)$name)
  
  ## order nodes, split by roots
  comps = igraph::decompose(g)
  if (length(comps) > 1) {
    reorder_list = sapply(comps, function(x) sort(igraph::V(x)$id), simplify = FALSE)
    reorder = unlist(reorder_list)
    g = igraph::permute(g, match(as.numeric(igraph::V(g)$id), as.numeric(reorder)))
    reorder_i = match(as.numeric(reorder), as.numeric(nodes$token_id))
    text = text[reorder_i]
    nodes = nodes[reorder_i,]
    tree_boundaries = sapply(reorder_list, length)
  } else tree_boundaries = NULL
  

  root = find_roots(g)
  g$layout = igraph::layout_as_tree(g, root = root)
  
  if (!is.null(ignore_rel)) g = igraph::delete.edges(g, which(igraph::get.edge.attribute(g, 'relation') %in% ignore_rel))
  
  co = g$layout
  e = igraph::get.edges(g, igraph::E(g))
  co[,1] = arrange_horizontal(g, text, tree_boundaries)
  g = format_edges(g, max_curve, e)
  nlevels = max(co[,2]) - min(co[,2])  ## remember for png mode
  co[,2] = arrange_vertical(co, text_cols)

  
  ## make empty plot to get positions in current plot device
  if (!is.null(pdf_file)) {
    height = 7
    width = height * (grDevices::dev.size()[1] / grDevices::dev.size()[2])
    grDevices::pdf(pdf_file, height = height, width=width)
  }
  if (viewer_mode) {
    png_file = tempfile(fileext = '.png')
    height = max(c(400, (nlevels/2) * viewer_size[1]))
    width = max(400,sum(graphics::strwidth(text, units='inches')*1.25) * viewer_size[2])
    grDevices::png(png_file, height = height, width=width)
  }
  
  oldpar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))  
  graphics::par(mar=c(0,0,0,0))
  graphics::plot(0, type="n", ann=FALSE, axes=FALSE, xlim=grDevices::extendrange(co[,1]),ylim=grDevices::extendrange(c(-1,1)))
  
  text = stringi::stri_trans_general(text,"any-latin")
  text = stringi::stri_trans_general(text,"latin-ascii")
  cex = calc_cex(g, co, text, tree_boundaries, spacing, textsize)
  g = set_graph_attr(g, e, cex, ignore_rel, palette, use_color) 
 
  graphics::plot(g, layout=co, rescale=FALSE, add=TRUE)
  graphics::text(co[,1], co[,2]+(0.02*cex), labels=igraph::V(g)$rel_label, 
       col = 'black', cex=cex*0.9, pos=3, font = 3)

  ## add text and lines
  
  ## non-integers are added. highlight these in red for clarity
  added = as.numeric(igraph::V(g)$name)
  added = (round(added) - added) != 0
  if (any(added)) {
    col = ifelse(added, ifelse(use_color, 'red', 'darkgrey'),'black')
  } else col = 'black'
  
  if (allign_text) {
    texty = min(co[,2])
  } else {
    texty = co[,2]
  }
  
  
  graphics::text(co[,1], texty-(0.1*cex), labels=text, col = col, cex=cex, adj=c(0.5,1))
  add_annotation(co, annotation, nodes, cex)
  message(sentmes)
  drop = if (is.null(ignore_rel)) rep(FALSE, igraph::vcount(g)) else igraph::V(g)$relation %in% ignore_rel
  if (allign_text && length(text_cols) > 0) graphics::segments(co[,1], min(co[,2]), co[,1], co[,2]-0.05, lwd = ifelse(drop, NA, 0.5), lty=2, col='grey')
  if (!is.null(pdf_file) || !is.null(viewer_mode)) grDevices::dev.off()
  
  if (pdf_viewer) {
    viewer = getOption('viewer')
    if (!is.null(viewer))
      viewer(pdf_file)
    else
      utils::browseURL(pdf_file)
  }
  if (viewer_mode) {
    png_in_viewer(png_file)
  }
  invisible(tokens)
}


get_text_cols <- function(tokens, nodes, l, annotations) {
  text_cols = list()
  if (length(l) > 0) {
    for (i in seq_along(l)) {
      e = rlang::quo_get_expr(l[[i]])
      if (is.character(i)) i = parse(text=i)
      text_cols[[names(l)[[i]]]] = eval(e, nodes)
    }
  } else {
    cols = setdiff(colnames(tokens), c('doc_id','sentence','token_id','parent','relation'))
    ann_cols = unlist(sapply(annotations, paste0, c('','_id','_fill'), simplify=FALSE))
    cols = setdiff(cols, ann_cols)
    for (col in cols) {
      text_cols[[col]] = nodes[[col]]   
    }
  }
  text_cols
}



get_sentence <- function(tokens, .DOC_ID=NULL, .SENTENCE=NULL, sentence_i=1) {
  if (!length(sentence_i) == 1) stop('Can only select one sentence_i') 
  if (!is.null(.DOC_ID)) {
    if (!length(.DOC_ID) == 1) stop('Can only select one doc_id') 
    sent = tokens[list(.DOC_ID), on='doc_id', nomatch=0]
    if (nrow(sent) == 0) return(sent)
    if (is.null(.SENTENCE)) {
      sentences = unique(sent[['sentence']])
      if (length(sentences) < sentence_i) stop(sprintf('Cannot select sentence_i = %s, only %s sentences available', sentence_i, length(sentences)))
      .SENTENCE = sentences[sentence_i]
    }
    if (!length(.SENTENCE) == 1) stop('Can only select one sentence') 
    sent = sent[list(.SENTENCE), on='sentence', nomatch=0]
  } else {
    if (!is.null(.SENTENCE)) stop('Cannot specificy "sentence" without specifying "doc_id"')
    .DOC_SENT = unique(subset(tokens, select = c('doc_id','sentence')))
    if (nrow(.DOC_SENT) < sentence_i) stop(sprintf('Cannot select sentence_i = %s, only %s sentences available', sentence_i, nrow(.DOC_SENT)))
    .DOC_SENT = .DOC_SENT[sentence_i,]
    sent = tokens[.DOC_SENT, on=c('doc_id','sentence'), nomatch=0]
  }
  data.table::setcolorder(sent, union('token_id', colnames(sent))) ## set token_id first for matching with edges
  
  sent
}


width_boundaries <- function(width, tree_boundaries) {
   if (!is.null(tree_boundaries)) {
    ## add space between isolated trees
    tree_boundaries = tree_boundaries[-length(tree_boundaries)]  ## don't add space after last tree
    tree_boundaries = cumsum(tree_boundaries)
    width[tree_boundaries] = width[tree_boundaries] * 1.5
   }
  width
}

centered_width <- function(width) (width / 2) + data.table::shift(width / 2, type = 'lead', fill=0)

format_edges <- function(g, max_curve, e) {
  ## format edges
  vdist = (e[,2] - e[,1])
  maxcurve = 1 / (1 + exp(-max(abs(vdist))*0.05))
  maxcurve = min(maxcurve, max_curve) # max_curve, with underscore, is a parameter
  curve = rescale_var(abs(vdist)^2, 0, maxcurve) * sign(vdist)
  #curve = maxcurve * sign(vdist)
  igraph::E(g)$curved = curve
  igraph::E(g)$width = 2
  igraph::E(g)$color = 'darkgrey'
  g  
}

calc_cex <- function(g, co, text, tree_boundaries, spacing, textsize) {
  width_label = graphics::strwidth(igraph::V(g)$label, units='inches')
  width_label = centered_width(width_label)
  width_label2 = graphics::strwidth(igraph::V(g)$rel_label, units='inches')
  width_label2 = centered_width(width_label2)
  width_label = ifelse(width_label > width_label2, width_label, width_label2)
  
  width_text = graphics::strwidth(text, units='inches')
  need_width = ifelse(width_label > width_text, width_label, width_text)
  need_width = width_boundaries(need_width, tree_boundaries)
  need_width = sum(need_width)
  need_width = need_width + (graphics::strwidth('  ', units='inches') * (spacing+0.1) * igraph::vcount(g))
  
  max_width = grDevices::dev.size(units = 'in')[1]
  max_width = max_width * (1 - min(co[,1])) / 2
  cex = if (max_width < need_width) max_width / need_width else 1
  textsize * cex
}


set_graph_attr <- function(g, e, cex, ignore_rel, palette, use_color) {
  width = (graphics::strwidth(igraph::V(g)$label, cex=cex) + graphics::strwidth(' ', cex=cex*0.5)) * 100
  width2 = (graphics::strwidth(igraph::V(g)$rel_label, cex=cex) + graphics::strwidth(' ', cex=cex*0.5)) * 100
  width = ifelse(width > width2, width, width2)
  
  height = (max(graphics::strheight(igraph::V(g)$label, cex=cex), graphics::strheight('I', cex=cex)) + graphics::strheight('I',cex=cex)*0.25) * 100
  igraph::V(g)$label.cex = cex
  igraph::V(g)$label.color = 'black'
  igraph::V(g)$shape = 'rectangle'
  igraph::V(g)$size = width
  igraph::V(g)$size2 = height
  igraph::V(g)$color = 'white'
  igraph::V(g)$border.color = 'white'
  igraph::V(g)$frame.color = 'white'
  igraph::V(g)$label.font=2
  
  drop = if (is.null(ignore_rel)) rep(FALSE, igraph::vcount(g)) else igraph::V(g)$relation %in% ignore_rel
  igraph::V(g)$size[drop] = 0
  igraph::V(g)$size2[drop] = 0
  igraph::V(g)$label[drop] = ''
  
  if ('.REL_LEVEL' %in% igraph::vertex_attr_names(g)) {
    hl = !is.na(igraph::V(g)$.REL_LEVEL)
  } else hl = rep(FALSE, igraph::vcount(g))
  if ('.IS_SPLIT' %in% igraph::vertex_attr_names(g)) {
    is_split = igraph::V(g)$.IS_SPLIT
  } else is_split = rep(FALSE, igraph::vcount(g))
  
  if (use_color) {
    igraph::V(g)$color = festival(igraph::V(g)$label, palette)
    igraph::E(g)$color = igraph::V(g)$color[e[,2]]
    igraph::E(g)$color[is_split[e[,2]]] = 'red'
    igraph::V(g)$frame.color[hl] = 'red'
    igraph::E(g)$lty = ifelse(hl[e[,2]] | is_split[e[,2]], 2, 1)
  } else {
    igraph::V(g)$color =  'lightgrey'
    igraph::V(g)$frame.color =  'darkgrey'
    igraph::V(g)$frame.color[hl] =  'black'
    igraph::E(g)$lty = ifelse(hl[e[,2]] | is_split[e[,2]], 2, 1)
  }
  
  igraph::E(g)$arrow.mode=2
  igraph::E(g)$arrow.size=0.4
  
  g
}


  
festival <- function(labels, palette=palette){
  pal = palette(4097) #(16^3 + 1 for indexing)
  color = NA
  for (label in labels) {
    if (is.na(label)) next
    if (label == '') next
    hash = digest::digest(label, 'xxhash32')
    hash = digest::digest(hash, 'xxhash32')  ## somehow first hash doesn't seem random enough (I know)
    labelint = strtoi(substr(hash, 2,4), 16) + 1
    color[labels == label] = pal[labelint]
  }
  color
}

find_roots <- function(g) {
  comps = igraph::decompose(g)
  
  roots = c()
  for (i in 1:length(comps)) {
    comp = comps[[i]]
    root = names(which.min(igraph::degree(comp, mode = 'in')))
    if (length(root) > 1) {
      out = igraph::degree(comp, mode='out')
      out = out[match(root, names(out))]
      root = names(out)[which.max(out)]
    } 
    if (length(root) > 1) {
      root = root[1]
    }
    roots = union(roots, root)
  }
  roots
}

split_adjacent <- function(nodes) {
  nodes$.IS_SPLIT = FALSE
  is_dup = duplicated(data.table(token_id=round(nodes$token_id), parent=nodes$parent))
  is_dup = is_dup &! is.na(nodes$parent)
  if (any(is_dup)) {
    tolast_ids = nodes$token_id[which(is_dup)]
    for (tolast_id in rev(tolast_ids)) {
      tolast_i = which(nodes$token_id == tolast_id)
      nodes$.IS_SPLIT[tolast_i] = TRUE
      tolast_i = get_children_i(nodes, tolast_i)
      tolast_i = sort(tolast_i)
      nodes = rbind(nodes[-tolast_i,], nodes[tolast_i,])  
    } 
  }
  nodes
}

arrange_horizontal <- function(g, text, tree_boundaries) {
  width = get_width(g, text, tree_boundaries)
  right_allign = cumsum(width)
  left_allign = c(0,right_allign[-length(right_allign)])
  rescale_var(left_allign, new_min = -1, new_max = 1, x_min = 0, x_max=max(right_allign))
}

arrange_vertical <- function(co, text_cols) {
  levels = max(co[,2]) - min(co[,2])
  bottom_offset = length(text_cols)
  if (bottom_offset < 5) bottom_offset = 5  
  if (bottom_offset > 10) bottom_offset = 10
  bottom_offset = -(1 - bottom_offset / 10)
  maxheight = if (levels > 10) 1 else bottom_offset + levels*0.15
  rescale_var(co[,2], new_min = bottom_offset, new_max = maxheight)
}

require_plot_new <- function() {
  ## this is a ridiculous function to trigger plot.new if necessary
  trigger = tryCatch(graphics::strwidth('banana'), error=function(e) 0)
  if (trigger == 0) graphics::plot.new()
}

get_width <- function(g, text, tree_boundaries){
  textwidth = graphics::strwidth(text)
  textwidth = centered_width(textwidth)
  relwidth = graphics::strwidth(igraph::V(g)$label)
  relwidth2 = graphics::strwidth(igraph::V(g)$rel_label)
  relwidth = ifelse(relwidth > relwidth2, relwidth, relwidth2)
  relwidth = centered_width(relwidth) ## relwidth is annoying, because nodes are centered. Therefore, use halved length of current node and next
  
  width = ifelse(textwidth > relwidth, textwidth, relwidth)
  width_boundaries(width, tree_boundaries)
}




rescale_var <- function(x, new_min=0, new_max=1, x_min=min(x), x_max=max(x)){
  if (x_min == x_max) return(x)
  x = (x - x_min) / (x_max - x_min) # normalize
  x = x * (new_max-new_min)
  return(x + new_min)
}

add_annotation <- function(co, annotation, nodes, cex) {
  ## draw boxes for the annotations
  if (!is.null(annotation)) {
    vdist = co[,2]
    vdist = max(vdist) - max(vdist[vdist < max(vdist)])
    id_start = 1
    role_start = 1
    ann_id = paste0(annotation, '_id')
    for (i in 1:nrow(nodes)) {
      role_done = !nodes[[annotation]][role_start] == nodes[[annotation]][i]
      id_done = !nodes[[ann_id]][id_start] == nodes[[ann_id]][i]
      if (is.na(role_done)) {
        if (role_start == i) {
          role_done = FALSE
          role_start = i
        } else role_done = TRUE
      }
      if (is.na(id_done)) {
        if (id_start == i) {
          id_done = FALSE
          id_start = i
        } else id_done = TRUE
      }
      if (role_done || id_done) {
        value = nodes[[annotation]][role_start]
        if (!is.na(value)) draw_box(co, role_start, i-1, label=value, is_outer = FALSE, vdist, cex=cex)
        role_start = i
      }
      if (id_done) {      
        value = nodes[[ann_id]][id_start]
        if (!is.na(value)) draw_box(co, id_start, i-1, label=value, is_outer=TRUE, vdist, cex=cex)
        id_start = i
      }
    }
    if (role_start <= i) {
      value = nodes[[annotation]][role_start]
      if (!is.na(value)) draw_box(co, role_start, nrow(nodes), label=value, is_outer=FALSE, vdist, cex=cex)
    }
    if (id_start <= i) {
      value = nodes[[ann_id]][id_start]
      if (!is.na(value)) draw_box(co, id_start, nrow(nodes), label=value, is_outer=TRUE, vdist, cex=cex)
    }
  }
}

draw_box <- function(co, start, end, vdist, label, is_outer=FALSE, hexp=1, vexp=1, cex=1,  ...) {
  vexp = if (is_outer) 1.2 else 1
  hexp = if (is_outer) 1 else 0.95
  ldist = if (start == 1) abs(co[2,1] - co[1,1]) else co[start,1] - co[start-1,1] 
  rdist = if (end == nrow(co)) co[nrow(co),1] - co[end-1,1] else abs(co[end,1] - co[end+1,1]) 
  xl = co[start,1] - ((ldist/2.1)*hexp)
  xr = co[end,1] + ((rdist/2.1)*hexp)
  yt = max(co[start:end,2]) + (vdist*vexp)
  if (is_outer) yt = yt + vdist
  yb = min(co[start:end,2]) - (vdist/2)*vexp
  if (yb < -0.5) yb = -0.5 - (0.05*cex)
  graphics::rect(xl,yb,xr,yt, lty=if(is_outer) 1 else 2, ...)
  labelx = mean(c(xl,xr))
  labely = yt + (vdist/2)
  if (is_outer) {
    graphics::text(labelx, labely, label, cex=cex*0.8, font= 2)
  } else {
    graphics::text(labelx, labely, label, cex=cex*0.8, font= 4)
  }
}

crop_png <- function(png_file, new_file=tempfile(), margins=c(20,20)) {
  m = png::readPNG(png_file)
  
  margin_x = margins[1]
  white_x = colMeans(m[,,1]) == 1
  white_x = c(which(!white_x)[1], length(white_x) - which(!rev(white_x))[1]) 
  white_x = c(max(white_x[1] - margin_x, 0), min(white_x[2] + margin_x, ncol(m))) 
  
  margin_y = margins[2]
  white_y = rowMeans(m[,,1]) == 1
  white_y = c(which(!white_y)[1], length(white_y) - which(!rev(white_y))[1]) 
  white_y = c(max(white_y[1] - margin_y, 0), min(white_y[2] + margin_y, nrow(m)))
  
  png::writePNG(m[white_y[1]:white_y[2],
                  white_x[1]:white_x[2],], 
                  new_file)
  new_file
}

png_in_viewer <- function(png_file) {
  png_file = crop_png(png_file)
  png = paste0('data:image/png;base64,', base64enc::base64encode(png_file))
  tf = tempfile(fileext = '.html')
  
  html = sprintf('<!DOCTYPE html><html>
                  <body><div>
                      <img src="%s" alt="Could not display (viewer_mode not working)">
                  </div></body></html>', png)
  writeLines(html , tf)
  v = getOption('viewer')  
  if (!is.null(v))
    v(tf)
  else
    utils::browseURL(tf)
}
