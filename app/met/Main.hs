module Main (main) where

import Malgo.Debug.Pipeline (runTrace)
import Malgo.Prelude
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Options.Applicative
import Server (app)

data Opt = Opt
  { srcPath :: FilePath,
    port :: Int,
    useInfer :: Bool,
    malgo2025 :: Bool
  }

optParser :: Parser Opt
optParser =
  Opt
    <$> strArgument (metavar "SOURCE" <> help "Source .mlg file to trace" <> action "file")
    <*> option auto (long "port" <> value 8080 <> help "Port to listen on (default: 8080)")
    <*> switch (long "infer" <> help "Run type inference pass (mirrors `malgo eval --infer`)")
    <*> switch (long "malgo2025" <> help "Run ElaboratePass (mirrors the malgo2025 feature flag)")

main :: IO ()
main = do
  opt <-
    execParser
      $ info (optParser <**> helper)
      $ fullDesc
      <> progDesc "MET (M-exp-Tracer): trace a .mlg file's compilation pipeline in a browser"
      <> header "met"
  stages <- runTrace opt.srcPath opt.useInfer opt.malgo2025
  hPutStrLn stderr
    $ "MET: traced "
    <> opt.srcPath
    <> " ("
    <> show (length stages)
    <> " stages). Listening on http://localhost:"
    <> show opt.port
  runSettings (setHost "127.0.0.1" $ setPort opt.port defaultSettings) (app stages)
