context("find nodes")

test_that("find_nodes works", {
  library(testthat)
  tokens = as_tokenindex(tokens_dutchquotes)
  
  #dat = find_nodes(tokens, label='id', lemma = 'dat')
  dat = find_nodes(tokens, 
                    tquery(label='id',lemma='dat', fill=F))
  
  
  
  
  expect_equal(nrow(dat), 1)
  expect_equal(dat$token_id, 45)
  
  vcs = find_nodes(tokens, 
                    tquery(label='parent', relation='vc', fill=F))
  expect_equal(nrow(vcs), 3)
  expect_equal(sort(vcs$token_id), c(7,45,54))

  body = find_nodes(tokens, 
                     tquery(label='id', relation="vc", fill=F,
                            children(label='id', relation='body', fill=F)))
  expect_equal(nrow(body), 2)

  # can we get_children with children?
  children = find_nodes(tokens, 
                         tquery(children(label='child', relation="body", fill=F,
                                         children(label='grandchild', relation='vc', fill=F))))
  expect_equal(nrow(children), 2)
  
  nodes = find_nodes(tokens, 
                      tquery(label='test', relation='su', fill=F,
                             children(label='child', fill=F)))

  # get parents
  parents = find_nodes(tokens, 
                        tquery(relation="vc", parents(label='parent', POS = 'verb', fill=F)))

  
  expect_equal(nrow(parents), 3)
  expect_equal(parents$token_id, c(6,44,53))
  
  # get parents, grandparents, children and grandchildren
  family = find_nodes(tokens, 
                       tquery(relation='vc', fill=F,
                              parents(label='parent', fill=F,
                                      parents(label='grandparent', fill=F)),
                              children(label='child', relation='obj1', fill=F,
                                       children(label='grandchild', relation='mod', fill=F))))
  
  expect_equal(nrow(family), 4)
  expect_equal(family$token_id, c(53,45,51,50))
  
  # test using req for optional arguments
  test_req = tokens_corenlp
  nodes1 = find_nodes(test_req, 
                       tquery(POS = 'VB*', label='verb',
                              children(relation = 'nsubj', label='subject'),
                              children(relation = 'dobj', label='object', req=F)))
  nodes = find_nodes(test_req, 
                       tquery(POS = 'VB*', label='verb',
                              children(relation = 'nsubj', label='subject'),
                              children(relation = 'dobj', label='object', req=T)))
  nodes3 = find_nodes(test_req, 
                       tquery(POS = 'VB*', label='verb',
                              children(relation = 'nsubj', label='subject')))
  expect_equal(unique(nodes1$.ID), unique(nodes3$.ID))
  expect_true(length(unique(nodes$.ID))< length(unique(nodes1$.ID)))
  
  find_nodes(test_req, 
              tquery(POS = 'VB*', label='verb',
                     children(relation = 'nsubj', label='subject'),
                     children(relation = 'dobj', label='object', req=F)))
})
