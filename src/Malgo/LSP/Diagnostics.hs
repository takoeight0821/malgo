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
import Data.Void (Void)
import Language.LSP.Protocol.Types qualified as LSP
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

-- | Convert a megaparsec 'SourcePos' (1-indexed) to an LSP 'Position' (0-indexed).
srcPosToLsp :: SourcePos -> LSP.Position
srcPosToLsp pos =
  LSP.Position
    { _line = fromIntegral (unPos (sourceLine pos)) - 1,
      _character = fromIntegral (unPos (sourceColumn pos)) - 1
    }

-- | Convert a Malgo 'Range' to an LSP 'Range'.
rangeToLsp :: Range -> LSP.Range
rangeToLsp r =
  LSP.Range
    { _start = srcPosToLsp r._start,
      _end = srcPosToLsp r._end
    }

-- | Build an error-severity LSP 'Diagnostic' from a range and message.
mkDiagnostic :: LSP.Range -> T.Text -> LSP.Diagnostic
mkDiagnostic r msg =
  LSP.Diagnostic
    { _range = r,
      _severity = Just LSP.DiagnosticSeverity_Error,
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "malgo",
      _message = msg,
      _tags = Nothing,
      _relatedInformation = Just [],
      _data_ = Nothing
    }

-- | Zero-width range at the start of the file (used as fallback position).
zeroRange :: LSP.Range
zeroRange = LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)

-- | Convert a 'CompileError' to a list of LSP diagnostics.
compileToDiagnostics :: CompileError -> [LSP.Diagnostic]
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

parseBundleToDiagnostics :: ParseErrorBundle TL.Text Void -> [LSP.Diagnostic]
parseBundleToDiagnostics bundle =
  let errors = bundleErrors bundle
      initialState = bundlePosState bundle
      (errorsWithPos, _) = attachSourcePos errorOffset errors initialState
   in map (uncurry parseErrorToDiagnostic) (toList errorsWithPos)

parseErrorToDiagnostic :: ParseError TL.Text Void -> SourcePos -> LSP.Diagnostic
parseErrorToDiagnostic err pos =
  let lspPos = srcPosToLsp pos
      r = LSP.Range lspPos lspPos
      msg = T.pack (parseErrorTextPretty err)
   in mkDiagnostic r msg

renameErrorToDiagnostic :: RenameError -> LSP.Diagnostic
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
