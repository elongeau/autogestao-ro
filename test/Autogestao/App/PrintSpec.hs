{-# OPTIONS_GHC -Wno-deprecations #-}

module Autogestao.App.PrintSpec where

import Autogestao.App.Print
import Autogestao.Core.Task (
  Task (..),
  TaskId (..),
 )
import Test.Hspec

spec :: Spec
spec = describe "Printing" do
  describe "report generation" do
    it "return an informative message when there is no tasks" do
      printReport [] `shouldBe` "no tasks found"
    it "generate a report from tasks" do
      let report = printReport [Todo (TaskId 1) "do", Todo (TaskId 2) "make"]
      report `shouldBe` unlines ["1 - do", "2 - make"]
