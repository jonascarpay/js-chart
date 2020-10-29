{-# LANGUAGE DeriveDataTypeable #-}

-- | Module for accessing minified flot code (<http://www.flotcharts.org/>).
--   As an example:
--
-- > import qualified Language.Javascript.Flot as Flot
-- >
-- > main = do
-- >     putStrLn $ "Flot version " ++ show Flot.version ++ " source:"
-- >     putStrLn =<< readFile =<< Flot.file Flot.Flot
--
--   This package installs data files containing the Flot sources, which must be available at runtime.
--   If you want to produce an executable with no dependency on associated data files, you can use the
--   @file-embed@ library (<https://hackage.haskell.org/package/file-embed>):
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.FileEmbed
-- > import qualified Data.ByteString as BS
-- > import qualified Language.Javascript.Flot as Flot
-- > import Language.Haskell.TH.Syntax
-- >
-- > main = print flotContents
-- >
-- > flotContents :: BS.ByteString
-- > flotContents = $(embedFile =<< runIO (Flot.file Flot.Flot))
module Language.Javascript.Flot
  ( Flot (..),
    version,
    file,
  )
where

import Data.Char
import Data.Data
import Data.Version
import qualified Paths_js_flot as Paths

-- | The Flot code to obtain. Use 'Flot' for the base system and the other values
--   for the various addins shipped with Flot.
data Flot
  = Flot
  | FlotAxislabels
  | FlotBrowser
  | FlotCategories
  | FlotComposeImages
  | FlotCrosshair
  | FlotDrawSeries
  | FlotErrorbars
  | FlotFillbetween
  | FlotFlatdata
  | FlotHover
  | FlotImage
  | FlotLegend
  | FlotLogaxis
  | FlotNavigate
  | FlotPie
  | FlotResize
  | FlotSaturated
  | FlotSelection
  | FlotStack
  | FlotSymbol
  | FlotThreshold
  | FlotTime
  | FlotTouch
  | FlotTouchNavigate
  | FlotUiConstants
  | LibMousewheel
  | LibCanvaswrapper
  | LibColorhelpers
  | LibEventDrag
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

-- | A local file containing the minified Flot code for 'version'.
file :: Flot -> IO FilePath
file = Paths.getDataFileName . name

name :: Flot -> String
name Flot = "jquery.flot.min.js"
name LibMousewheel = "jquery.mousewheel.min.js"
name LibCanvaswrapper = "jquery.canvaswrapper.min.js"
name LibColorhelpers = "jquery.colorhelpers.min.js"
name LibEventDrag = "jquery.event.drag.min.js"
name x = "jquery.flot." ++ lowerHead (drop 4 $ show x) ++ ".min.js"
  where
    lowerHead (x : xs) = toLower x : xs
    lowerHead [] = []

-- | The version of Flot provided by this package. Not necessarily the version of this package,
--   but the versions will match in the first three digits.
version :: Version
version = Version (take 3 $ versionBranch Paths.version) []
