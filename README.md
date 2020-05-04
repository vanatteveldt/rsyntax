R functions for working with syntactic structure coded as token lists
(e.g. CONLL format)

Installation
============

You can install from CRAN:

    install.packages('rsyntax')

Or install the development version from github: directly from github:

    library(devtools)
    install_github("vanatteveldt/rsyntax")

Tutorial
========

For a detailed explanation please see [this working
paper](https://github.com/vanatteveldt/rsyntax/blob/master/Querying_dependency_trees.pdf).

As a quick demonstration, you can run the following code. First, we’ll
need to parse some data. In the working paper we use the spacyr package
(for the spaCy parser), but this requires running Python. Another option
that does run in native R is the udpipe package (for the UDPipe parser).
The following code automatically downloads the English model and applies
it to parse the given text.

    library(udpipe)
    tokens = udpipe(c('Mary Jane loves John Smith, and Mary is loved by John'), 'english')

rsyntax requires the tokens to be in a certain format. The
as\_tokenindex() function converts a data.frame to this format. For
popular parsers in R (spacyr and udpipe) the correct column name
specifications are known, so the following is sufficient.

    library(rsyntax)
    tokens = as_tokenindex(tokens)

To query a dependency tree, it is important to have a good understanding
of what these trees look like, and how this tree data is represented in
a data.frame format. To facilitate this understanding, the plot\_tree
function visualizes the dependency tree, together with a given selection
of columns from the data (see working paper for why this is possible for
most types of dependency trees). We don’t show the visualization in this
readme, because rsyntax creates a PDF that’s shown in the viewer panel,
and at this moment I’m too lazy to figure out how to make this work in
the readme markdown.

    plot_tree(tokens, token, lemma, upos)

Note that this function only prints one sentence a time, so if the
sentence is not specified it uses the first sentence in the data.

The main functionality of rsyntax is that you can query the dependency
tree. While there are several query languages for networks, these are
quite complicated and not specialized for querying dependency trees. We
therefore developed a new query format that is (supposed to be) easy to
understand if you undestand R data.frames.

Simply put, you can provide lookup values for selecting rows from the
data.frame. For example, the following query would find all rows where
the upos value is either “VERB” or “PROPN”:

    tquery(upos = "VERB")

To query the edges of a dependency tree, you can perform another row
lookup for the parents or children of the results of this query, by
nesting the parents() and children() functions. The following query
says: for all tokens (i.e. rows) where upos is “VERB”, find the ones
that have a child for which the relation column says “nsubj”.

    tquery(upos = 'VERB', 
           children(relation = 'nsubj'))

You can look up multiple parents and children, and also nest parents and
children within each other to query larger parts of the tree.

The above query only finds a match. To see which tokens are matched you
need to provide labels for the parts of the query that you want to find.
The following query looks for a simple direct clause with a verb,
subject and object.

    direct = tquery(label = 'verb', upos = 'VERB', 
                    children(label = 'subject', relation = 'nsubj'),
                    children(label = 'object', relation = 'obj'))

Specifically this says: find all tokens where upos is “VERB”, and that
have a child with the relation “nsubj” AND a child with the relation
“obj”. If this condition is met, give these tokens the labels “verb”,
“subject” and “object”.

With the annotate function, we can use this tquery to add these labels
to the token data. Here we say that we use the column name “clause” for
these labels.

    tokens = annotate(tokens, 'clause', direct)

    tokens[,c('doc_id','sentence','token','clause','clause_fill')]
    #>     doc_id sentence token  clause clause_fill
    #>  1:   doc1        1  Mary subject           0
    #>  2:   doc1        1  Jane subject           1
    #>  3:   doc1        1 loves    verb           0
    #>  4:   doc1        1  John  object           0
    #>  5:   doc1        1 Smith  object           1
    #>  6:   doc1        1     ,  object           2
    #>  7:   doc1        1   and  object           2
    #>  8:   doc1        1  Mary  object           2
    #>  9:   doc1        1    is  object           2
    #> 10:   doc1        1 loved  object           1
    #> 11:   doc1        1    by  object           3
    #> 12:   doc1        1  John  object           2

In the output we see that “Mary Jane” is labeled as subject, “loves” is
labeled as verb, but also that ALL the rest is labeled as object. The
reason for this is that by default, rsyntax will label all children of a
matched token with the same label. We call this behavior “fill”, and
while it is weird in this case, the default is to use fill because in
many cases this is convenient (e.g. for labeling both Mary and Jane).
You can use the `fill = TRUE` argument to disable fill (as shown in a
following example), or provide more specific criteria for which nodes to
fill. In the clause\_fill column you also see at what level a token was
matched. The value 0 means the match itself, 1 means a direct child,
etc.

But rather than turning off fill in our example sentence, we would argue
that the bigger problem is that our current query only captures one type
of way in which people express a subject - verb - object relation in
english. To find this type of expression more accurately, we therefore
need to use multiple queries. One of the ways in which the rsyntax query
format is tailored for dependency trees is that it allows tqueries to be
piped together.

For example, let’s add the following query for a passive sentence.

    passive = tquery(label = 'verb', upos = 'VERB', fill=F,
                     children(label = 'subject', relation = 'obl'),
                     children(label = 'object', relation = 'nsubj:pass'))

Now we can add both tqueries to the annotate function. For convenience,
we can also specify labels for the queries by passing them as named
arguments. Here we label the direct query “dir” and the passive query
“pas”. Also, and very importantly, note that we add the `overwrite = T`
argument, which means that we’ll overwrite the previous “clause” column.
(By default, annotate would not overwrite previous results, which
enables another way of piping queries that we won’t discuss here.)

    tokens = annotate(tokens, 'clause', 
                      dir = direct, 
                      pas = passive, 
                      overwrite = T)

    tokens[,c('doc_id','sentence','token','clause', 'clause_id')]
    #>     doc_id sentence token  clause     clause_id
    #>  1:   doc1        1  Mary subject  dir#doc1.1.3
    #>  2:   doc1        1  Jane subject  dir#doc1.1.3
    #>  3:   doc1        1 loves    verb  dir#doc1.1.3
    #>  4:   doc1        1  John  object  dir#doc1.1.3
    #>  5:   doc1        1 Smith  object  dir#doc1.1.3
    #>  6:   doc1        1     ,    <NA>          <NA>
    #>  7:   doc1        1   and    <NA>          <NA>
    #>  8:   doc1        1  Mary  object pas#doc1.1.10
    #>  9:   doc1        1    is    <NA>          <NA>
    #> 10:   doc1        1 loved    verb pas#doc1.1.10
    #> 11:   doc1        1    by subject pas#doc1.1.10
    #> 12:   doc1        1  John subject pas#doc1.1.10

This time, the sentence has two annotations. In the clause\_id column
you can also see that the first one was found with the direct (dir)
tquery, and the second one with the passive (pas) tquery. Importantly,
the second annotation blocked the “fill” of the first annotation. More
generally, a query will only “fill” children nodes that are not yet
assigned to other queries. This way, you can easily pipe multiple
queries together.

Finally, you can also visualize annotations with plot\_tree.

    plot_tree(tokens, token, lemma, upos, annotation='clause')
    #> Document: doc1
    #> Sentence: 1

The rsyntax package also supports more advanced features for writing and
piping queries. Furthermore, since language can get quite complicated
(gotta love concatenations, relative clauses and recursion), rsyntax
also provides functions for transforming and cutting up dependency
trees. How to best use this is still something we’re experimenting with,
and more details are provided in the working paper.

Aside from the rsyntax package we will (soon?) create a github
repository for an rsyntax cookbook, to share the queries and
transformation that we use in our research. If you are interested in
using rsyntax and have any questions, concerns or ideas, please do
contact us. It turns out language is hard, and this is an ongoing
project to help us deal with that fact.
