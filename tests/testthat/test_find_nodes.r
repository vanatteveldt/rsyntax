context("find nodes")

test_that("find_nodes works", {
  library(testthat)
  tokens = as_tokenindex(tokens_dutchquotes)
  
  #dat = find_nodes(tokens, label='id', lemma = 'dat')
  dat = find_nodes(tokens, 
                    tquery(label='id',lemma='dat', fill=FALSE))
  
  
  
  
  expect_equal(nrow(dat), 1)
  expect_equal(dat$token_id, 45)
  
  vcs = find_nodes(tokens, 
                    tquery(label='parent', relation='vc', fill=FALSE))
  expect_equal(nrow(vcs), 3)
  expect_equal(sort(vcs$token_id), c(7,45,54))

  body = find_nodes(tokens, 
                     tquery(label='id', relation="vc", fill=FALSE,
                            children(label='id', relation='body', fill=FALSE)))
  expect_equal(nrow(body), 2)

  # can we get_children with children?
  children = find_nodes(tokens, 
                         tquery(children(label='child', relation="body", fill=FALSE,
                                         children(label='grandchild', relation='vc', fill=FALSE))))
  expect_equal(nrow(children), 2)
  
  nodes = find_nodes(tokens, 
                      tquery(label='test', relation='su', fill=FALSE,
                             children(label='child', fill=FALSE)))

  # get parents
  parents = find_nodes(tokens, 
                        tquery(relation="vc", parents(label='parent', POS = 'verb', fill=FALSE)))

  
  expect_equal(nrow(parents), 3)
  expect_equal(parents$token_id, c(6,44,53))
  
  # get parents, grandparents, children and grandchildren
  family = find_nodes(tokens, 
                       tquery(relation='vc', fill=FALSE,
                              parents(label='parent', fill=FALSE,
                                      parents(label='grandparent', fill=FALSE)),
                              children(label='child', relation='obj1', fill=FALSE,
                                       children(label='grandchild', relation='mod', fill=FALSE))))
  expect_equal(nrow(family), 4)
  expect_equal(family$token_id, c(53,45,51,50))
  
  # test using req for optional arguments
  test_req = tokens_corenlp
  nodes1 = find_nodes(test_req, 
                       tquery(POS = 'VB*', label='verb',
                              children(relation = 'nsubj', label='subject'),
                              children(relation = 'dobj', label='object', req=FALSE)))
  nodes = find_nodes(test_req, 
                       tquery(POS = 'VB*', label='verb',
                              children(relation = 'nsubj', label='subject'),
                              children(relation = 'dobj', label='object', req=TRUE)))
  nodes3 = find_nodes(test_req, 
                       tquery(POS = 'VB*', label='verb',
                              children(relation = 'nsubj', label='subject')))
  expect_equal(unique(nodes1$.ID), unique(nodes3$.ID))
  expect_true(length(unique(nodes$.ID))< length(unique(nodes1$.ID)))
  
  find_nodes(test_req, 
              tquery(POS = 'VB*', label='verb',
                     children(relation = 'nsubj', label='subject'),
                     children(relation = 'dobj', label='object', req=FALSE)))
})
