{-# LANGUAGE TemplateHaskell #-}

-- | Embeds the checked-in Zig runtime (@runtime\/zig\/runtime.zig@) into the
-- @malgo@ binary at compile time, so the emitted program is a single,
-- hermetic @.zig@ file (no relative-path @\@import@ needed at build time).
module Malgo.Backend.Zig.Runtime (zigRuntime) where

import Data.FileEmbed (embedStringFile)
import Malgo.Prelude

zigRuntime :: Text
zigRuntime = $(embedStringFile "runtime/zig/runtime.zig")
