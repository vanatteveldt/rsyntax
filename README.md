R functions for working with syntactic structure coded as token lists
(e.g. CONLL format)

Installation
============

You can install directly from github:

    library(devtools)
    install_github("kasperwelbers/corpus-tools")

Usage
=====

The functions in this module assume that you have a list of tokens in a
data frame. A simple example is provided with the module:

    library(rsyntax)
    data(example_tokens)
    head(tokens)

<table>
<thead>
<tr class="header">
<th align="left">word</th>
<th align="right">parent</th>
<th align="right">sentence</th>
<th align="left">pos</th>
<th align="left">lemma</th>
<th align="left">relation</th>
<th align="right">offset</th>
<th align="left">aid</th>
<th align="right">id</th>
<th align="left">pos1</th>
<th align="left">entity</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">This</td>
<td align="right">2</td>
<td align="right">1</td>
<td align="left">DT</td>
<td align="left">this</td>
<td align="left">det</td>
<td align="right">0</td>
<td align="left">NA</td>
<td align="right">1</td>
<td align="left">D</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">sentence</td>
<td align="right">4</td>
<td align="right">1</td>
<td align="left">NN</td>
<td align="left">sentence</td>
<td align="left">nsubjpass</td>
<td align="right">5</td>
<td align="left">NA</td>
<td align="right">2</td>
<td align="left">N</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">is</td>
<td align="right">4</td>
<td align="right">1</td>
<td align="left">VBZ</td>
<td align="left">be</td>
<td align="left">auxpass</td>
<td align="right">14</td>
<td align="left">NA</td>
<td align="right">3</td>
<td align="left">V</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">parsed</td>
<td align="right">NA</td>
<td align="right">1</td>
<td align="left">VBN</td>
<td align="left">parse</td>
<td align="left"></td>
<td align="right">17</td>
<td align="left">NA</td>
<td align="right">4</td>
<td align="left">V</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">by</td>
<td align="right">NA</td>
<td align="right">1</td>
<td align="left">IN</td>
<td align="left">by</td>
<td align="left"></td>
<td align="right">24</td>
<td align="left">NA</td>
<td align="right">5</td>
<td align="left">P</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Stanford</td>
<td align="right">7</td>
<td align="right">1</td>
<td align="left">NNP</td>
<td align="left">Stanford</td>
<td align="left">nn</td>
<td align="right">27</td>
<td align="left">NA</td>
<td align="right">6</td>
<td align="left">M</td>
<td align="left">ORGANIZATION</td>
</tr>
</tbody>
</table>

Get the text of a sentence, optionally specifying which column(s) to
use:

    get_text(tokens)

    ## [1] "This sentence is parsed by Stanford CoreNLP"

    get_text(tokens, word.column = c("lemma", "pos"))

    ## [1] "this/DT sentence/NN be/VBZ parse/VBN by/IN Stanford/NNP CoreNLP/NNP"

Plot the syntactic structure of a sentence: (Note: if you have multiple
sentences in one token list, you should filter it or provide a sentence=
argument)

    g = graph_from_sentence(tokens)
    plot(g)

![Syntactic Structure of example sentence](.readme_example_plot-1.png)
