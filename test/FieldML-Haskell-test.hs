{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} FieldML.Utility01Test
import {-@ HTF_TESTS @-} FieldML.Library02Test
import {-@ HTF_TESTS @-} FieldML.Library01Test
import {-@ HTF_TESTS @-} FieldML.CoreTest
import {-@ HTF_TESTS @-} FieldML_test_mesh01Test

main :: IO()
main = htfMain htf_importedTests
