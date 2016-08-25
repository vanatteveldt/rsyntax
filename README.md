R functions for working with syntactic structure coded as token lists
(e.g. CONLL format)

Installation
============

You can install directly from github:

    library(devtools)
    install_github("vanatteveldt/rsyntax")

Usage
=====

The functions in this module assume that you have a list of tokens in a
data frame. A simple example is provided with the module:

    library(rsyntax)
    data(example_tokens)
    head(tokens)

Get the text of a sentence, optionally specifying which column(s) to
use:

    get_text(tokens)

    ## [1] "John says that Mary hit him"

    get_text(tokens, word.column = c("lemma", "pos"))

    ## [1] "John/NNP say/VBZ that/IN Mary/NNP hit/VBD he/PRP"

Plot the syntactic structure of a sentence: (Note: if you have multiple
sentences in one token list, you should filter it or provide a sentence=
argument)

    g = graph_from_sentence(tokens)
    plot(g)

![Syntactic Structure of example sentence](.readme_example_plot-1.png)

Clauses and Sources
===================

You can use the `get_quotes` function to extract quotes and paraphrases
from the sentences. Note that for this, the token ids need to be
globally unique. If that is not the case, you can use the `unique.ids`
function to make them unique:

    tokens = unique_ids(tokens)

You can get the quotes from the tokens with `get_quotes`:

    quotes = get_quotes(tokens)
    head(quotes)

<table>
<thead>
<tr class="header">
<th align="right">id</th>
<th align="right">source</th>
<th align="right">quote</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">2</td>
<td align="right">1</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

A single quote was found, with node 2 ("say") as the key, node 1
("John") as the sources, and nodes 3 through 6 ("that Mary hit him") as
quote.

To find the clauses, you can use the get\_clauses function, which takes
the quotes as an optional argument to make sure that speech actions are
not listed as clauses:

    clauses = get_clauses(tokens, quotes=quotes)
    head(clauses)

<table>
<thead>
<tr class="header">
<th align="right">clause_id</th>
<th align="right">subject</th>
<th align="right">predicate</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="right">4</td>
<td align="right">5</td>
</tr>
</tbody>
</table>

You can annotate the original tokens file with the quotes and tokens to
facilitate processing (e.g. to create a word cloud or topic model of all
utterances per source):

    tokens = annotate_tokens(tokens, quotes, clauses)
    head(tokens)

<table>
<thead>
<tr class="header">
<th align="right">id</th>
<th align="left">word</th>
<th align="right">parent</th>
<th align="right">sentence</th>
<th align="right">coref</th>
<th align="left">pos</th>
<th align="left">entity</th>
<th align="left">lemma</th>
<th align="left">relation</th>
<th align="right">offset</th>
<th align="right">aid</th>
<th align="left">pos1</th>
<th align="left">attack</th>
<th align="right">quote_id</th>
<th align="left">quote_role</th>
<th align="right">clause_id</th>
<th align="left">clause_role</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">John</td>
<td align="right">2</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="left">NNP</td>
<td align="left">PERSON</td>
<td align="left">John</td>
<td align="left">nsubj</td>
<td align="right">0</td>
<td align="right">156884180</td>
<td align="left">M</td>
<td align="left">FALSE</td>
<td align="right">1</td>
<td align="left">source</td>
<td align="right">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">says</td>
<td align="right">NA</td>
<td align="right">1</td>
<td align="right">NA</td>
<td align="left">VBZ</td>
<td align="left"></td>
<td align="left">say</td>
<td align="left"></td>
<td align="right">5</td>
<td align="right">156884180</td>
<td align="left">V</td>
<td align="left">FALSE</td>
<td align="right">NA</td>
<td align="left">NA</td>
<td align="right">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">that</td>
<td align="right">5</td>
<td align="right">1</td>
<td align="right">NA</td>
<td align="left">IN</td>
<td align="left"></td>
<td align="left">that</td>
<td align="left">mark</td>
<td align="right">10</td>
<td align="right">156884180</td>
<td align="left">P</td>
<td align="left">FALSE</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">predicate</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">Mary</td>
<td align="right">5</td>
<td align="right">1</td>
<td align="right">NA</td>
<td align="left">NNP</td>
<td align="left">PERSON</td>
<td align="left">Mary</td>
<td align="left">nsubj</td>
<td align="right">15</td>
<td align="right">156884180</td>
<td align="left">M</td>
<td align="left">FALSE</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">subject</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">hit</td>
<td align="right">2</td>
<td align="right">1</td>
<td align="right">NA</td>
<td align="left">VBD</td>
<td align="left"></td>
<td align="left">hit</td>
<td align="left">ccomp</td>
<td align="right">20</td>
<td align="right">156884180</td>
<td align="left">V</td>
<td align="left">FALSE</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">predicate</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">him</td>
<td align="right">5</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="left">PRP</td>
<td align="left"></td>
<td align="left">he</td>
<td align="left">dobj</td>
<td align="right">24</td>
<td align="right">156884180</td>
<td align="left">O</td>
<td align="left">FALSE</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">predicate</td>
</tr>
</tbody>
</table>

Finally, you can also provide the quotes and clauses to the
`graph_from_sentence` function. This will fill the clauses in a
desaturated rainbow, with the subject as a circle and the predicate as
rectangle. Quotes are represented with a bright node for the source, and
the border in the same colour for the quote.

    g = graph_from_sentence(tokens)
    plot(g)

![Syntactic Structure of example sentence with clauses and quotes
marked](.readme_example_plot_clauses-1.png)

Use with coreNLP
================

You can use the coreNLP package to directly parse (English) sentences
and create a token list.

First, initialize coreNLP and parse the sentence:

    coreNLP::initCoreNLP()

    a = coreNLP::annotateString("John told Mary he loves her")

Now, you can create a token list from the coreNLP annotation and use
that to compute sources and clauses as normal:

    tokens = tokens_from_coreNLP(a)
    quotes = get_quotes(tokens)
    clauses = get_clauses(tokens, quotes)
    tokens = annotate_tokens(tokens, quotes, clauses)
    head(tokens)

<table>
<thead>
<tr class="header">
<th align="right">id</th>
<th align="right">sentence</th>
<th align="left">token</th>
<th align="left">lemma</th>
<th align="right">CharacterOffsetBegin</th>
<th align="right">CharacterOffsetEnd</th>
<th align="left">POS</th>
<th align="left">NER</th>
<th align="left">Speaker</th>
<th align="right">parent</th>
<th align="left">relation</th>
<th align="left">pos1</th>
<th align="right">quote_id</th>
<th align="left">quote_role</th>
<th align="right">clause_id</th>
<th align="left">clause_role</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="right">1</td>
<td align="left">John</td>
<td align="left">John</td>
<td align="right">0</td>
<td align="right">4</td>
<td align="left">NNP</td>
<td align="left">PERSON</td>
<td align="left">PER0</td>
<td align="right">2</td>
<td align="left">nsubj</td>
<td align="left">N</td>
<td align="right">1</td>
<td align="left">source</td>
<td align="right">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="right">1</td>
<td align="left">told</td>
<td align="left">tell</td>
<td align="right">5</td>
<td align="right">9</td>
<td align="left">VBD</td>
<td align="left">O</td>
<td align="left">PER0</td>
<td align="right">NA</td>
<td align="left">root</td>
<td align="left">V</td>
<td align="right">NA</td>
<td align="left">NA</td>
<td align="right">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="right">1</td>
<td align="left">Mary</td>
<td align="left">Mary</td>
<td align="right">10</td>
<td align="right">14</td>
<td align="left">NNP</td>
<td align="left">PERSON</td>
<td align="left">PER0</td>
<td align="right">2</td>
<td align="left">dobj</td>
<td align="left">N</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="right">1</td>
<td align="left">he</td>
<td align="left">he</td>
<td align="right">15</td>
<td align="right">17</td>
<td align="left">PRP</td>
<td align="left">O</td>
<td align="left">PER0</td>
<td align="right">5</td>
<td align="left">nsubj</td>
<td align="left">P</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">subject</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="right">1</td>
<td align="left">loves</td>
<td align="left">love</td>
<td align="right">18</td>
<td align="right">23</td>
<td align="left">VBZ</td>
<td align="left">O</td>
<td align="left">PER0</td>
<td align="right">3</td>
<td align="left">acl:relcl</td>
<td align="left">V</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">predicate</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="right">1</td>
<td align="left">her</td>
<td align="left">she</td>
<td align="right">24</td>
<td align="right">27</td>
<td align="left">PRP$</td>
<td align="left">O</td>
<td align="left">PER0</td>
<td align="right">5</td>
<td align="left">dobj</td>
<td align="left">P</td>
<td align="right">1</td>
<td align="left">quote</td>
<td align="right">1</td>
<td align="left">predicate</td>
</tr>
</tbody>
</table>

And plot the sentence:

    plot(graph_from_sentence(tokens))

![Syntactic Structure of example sentence from
coreNLP](.readme_example_plot_corenlp-1.png)
