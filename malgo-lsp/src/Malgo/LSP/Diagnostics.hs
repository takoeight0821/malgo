{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Conversion from Malgo compiler errors to LSP diagnostics.
module Malgo.LSP.Diagnostics
  ( compileToDiagnostics,
    rangeToLsp,
    srcPosToLsp,
  )
where

import Control.Exception (displayException)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Typeable (cast)
import Data.Void ()
import Malgo.LSP.Protocol (Diagnostic, LspPosition (..), LspRange (..), mkDiagnostic)
import Malgo.Pass (CompileError (..))
import Malgo.Prelude
import Malgo.Rename.RnEnv (RenameError (..))
import Text.Megaparsec
  ( ParseError,
    ParseErrorBundle (..),
    attachSourcePos,
    errorOffset,
    parseErrorTextPretty,
  )
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)

-- | Convert a megaparsec 'SourcePos' (1-indexed) to an LSP 'LspPosition' (0-indexed).
srcPosToLsp :: SourcePos -> LspPosition
srcPosToLsp pos =
  LspPosition
    { _line = unPos (sourceLine pos) - 1,
      _character = unPos (sourceColumn pos) - 1
    }

-- | Convert a Malgo 'Range' to an LSP 'LspRange'.
rangeToLsp :: Range -> LspRange
rangeToLsp r =
  LspRange
    { _start = srcPosToLsp r._start,
      _end = srcPosToLsp r._end
    }

-- | Zero-width range at the start of the file (used as fallback position).
zeroRange :: LspRange
zeroRange = LspRange (LspPosition 0 0) (LspPosition 0 0)

-- | Convert a 'CompileError' to a list of LSP diagnostics.
compileToDiagnostics :: CompileError -> [Diagnostic]
compileToDiagnostics (CompileError {compileError}) =
  case cast compileError of
    Just (bundle :: ParseErrorBundle TL.Text Void) ->
      parseBundleToDiagnostics bundle
    Nothing ->
      case cast compileError of
        Just (renameErr :: RenameError) ->
          [renameErrorToDiagnostic renameErr]
        Nothing ->
          [mkDiagnostic zeroRange (T.pack (displayException compileError))]

parseBundleToDiagnostics :: ParseErrorBundle TL.Text Void -> [Diagnostic]
parseBundleToDiagnostics bundle =
  let errors = bundleErrors bundle
      initialState = bundlePosState bundle
      (errorsWithPos, _) = attachSourcePos errorOffset errors initialState
   in map (uncurry parseErrorToDiagnostic) (toList errorsWithPos)

parseErrorToDiagnostic :: ParseError TL.Text Void -> SourcePos -> Diagnostic
parseErrorToDiagnostic err pos =
  let lspPos = srcPosToLsp pos
      r = LspRange lspPos lspPos
      msg = T.pack (parseErrorTextPretty err)
   in mkDiagnostic r msg

renameErrorToDiagnostic :: RenameError -> Diagnostic
renameErrorToDiagnostic renameErr =
  let r = case renameErr of
        NoSuchNameInScope rng _ _ -> rangeToLsp rng
        NotInScope rng _ -> rangeToLsp rng
        NoSuchNameInModule rng _ _ _ -> rangeToLsp rng
        NotInModule rng _ _ -> rangeToLsp rng
        DuplicateName rng _ -> rangeToLsp rng
        DuplicateNames rng _ -> rangeToLsp rng
      msg = T.pack (displayException renameErr)
   in mkDiagnostic r msg
