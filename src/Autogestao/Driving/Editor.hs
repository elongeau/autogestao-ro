module Autogestao.Driving.Editor (runDefaultEditor) where

import Data.ByteString qualified as B
import GHC.IO.Handle (hClose, hSetBinaryMode)
import System.IO.Temp (withSystemTempFile)
import System.Process (spawnProcess, waitForProcess)

{-
Took from https://hackage.haskell.org/package/editor-open-0.6.0.0
I copy it instead of adding the dependency because I've had a problem with zlib that I can't fix it (and don't want to spent time to fix it)
-}

runDefaultEditor ::
  -- |Initial contents of the file.
  Text ->
  -- |Resulting ByteString.
  IO ByteString
runDefaultEditor initialContents = do
  defaultEditor <- userEditor
  runSpecificEditor defaultEditor "tmp" (encodeUtf8 initialContents)

userEditor :: IO String
userEditor = do
  editors <- mapM lookupEnv _editors
  return $ fromMaybe "vi" (asum editors)

_editors :: [String]
_editors = ["EDITOR", "VISUAL"]

runSpecificEditor ::
  -- |Name of the editor.
  String ->
  -- |Template for the file name.
  String ->
  -- |Initial contents of the file.
  ByteString ->
  -- |Resulting ByteString.
  IO ByteString
runSpecificEditor editorName templ initialContents =
  withSystemTempFile
    templ
    ( \filePath hdl -> do
        -- Write to the handle
        hSetBinaryMode hdl True
        hSetBuffering hdl NoBuffering
        B.hPut hdl initialContents
        -- Close the handle
        hClose hdl
        -- Open the editor
        prc <- spawnProcess editorName [filePath]
        -- Wait for the editor
        _ <- waitForProcess prc
        -- Send back the file contents
        readFileBS filePath
    )
