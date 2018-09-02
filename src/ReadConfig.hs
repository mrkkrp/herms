{-# LANGUAGE LambdaCase #-}

module ReadConfig where

import Prelude hiding (readFile)
import UnitConversions
import qualified Text.Read as TR
import Control.Exception
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import Data.Typeable
import Data.Char (toLower)
import Data.List.Split
import Lang.Pirate
import Lang.French
import Lang.Portuguese
import Lang.Spanish
import System.FilePath ((</>))
import System.Directory
import Paths_herms hiding (getDataDir) -- This module is generated by cabal

------------------------------
------- Config Types ---------
------------------------------

-- TODO Allow record synonyms with that fancy stuff

data ConfigInfo = ConfigInfo
  { defaultUnit           :: Conversion
  , defaultServingSize    :: Int
  , recipesFile           :: String
  , language              :: String
  } deriving (Read, Show)

data Config = Config
  { defaultUnit'        :: Conversion
  , defaultServingSize' :: Int
  , dataDir             :: String
  , configDir           :: String
  , recipesFile'        :: String
  , translator          :: String -> String
  }

data Language = English
              | Pirate
              | Portuguese
              | Spanish
              | French

type Translator = String -> String

------------------------------
---- Exception Handling ------
------------------------------

data ConfigParseError = ConfigParseError
  deriving Typeable

instance Show ConfigParseError where
  show ConfigParseError = "Error parsing config.hs"

instance Exception ConfigParseError

------------------------------
------ Language Synonyms -----
------------------------------

-- Contributor's note: Make sure that all of these synonyms are lower-case as
-- we handle case-sensitivity by first converting the user's settings input
-- to all lower-case.

englishSyns = [ "english"
              , "en"
              , "en-us"
              , "\'murican"
              ]

pirateSyns = [ "pirate"
             , "pr"
             ]

portugueseSyns = [ "portuguese"
                 , "português"
                 , "portugues"
                 , "pt"
                 ]

spanishSyns = [ "spanish"
              , "español"
              , "es"
              ]

frenchSyns = [ "french"
              , "fr"
              , "fr-fr"
              , "français"
              , "francais"
              ]


------------------------------
--------- Functions ----------
------------------------------

getLang :: ConfigInfo -> Language
getLang c
  | isIn pirateSyns     = Pirate
  | isIn frenchSyns     = French
  | isIn portugueseSyns = Portuguese
  | isIn spanishSyns    = Spanish
  | otherwise           = English
  where isIn = elem (map toLower $ language c)

getTranslator :: Language -> Translator
getTranslator lang = case lang of
                       Pirate     -> pirate
                       French     -> french
                       Portuguese -> portuguese
                       Spanish    -> spanish
                       _          -> id :: String -> String

dropComments :: String -> String
dropComments = unlines . map (head . splitOn "--") . lines

processConfig :: FilePath -> FilePath -> ConfigInfo -> Config
processConfig dataDir configDir raw = Config
  { defaultUnit'        = defaultUnit raw
  , defaultServingSize' = defaultServingSize raw
  , recipesFile'        = dataDir </> recipesFile raw
  , translator          = getTranslator $ getLang raw
  , dataDir             = dataDir
  , configDir           = configDir
  }

-- | The directory of the config.hs file. Its location is dictated by
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG
--   Base Directory Specification>.
getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "herms"

-- | The directory of the recipes.herms file.
getDataDir :: IO FilePath
getDataDir = getXdgDirectory XdgData "herms"

-- | Create a directory if it doesn't exist, ensure it is readable,
-- writable, and executable.
directoryWithPermissions :: FilePath -> IO ()
directoryWithPermissions dir = do
  createDirectoryIfMissing True dir
  setPermissions dir $
    setOwnerReadable True $
    setOwnerWritable True $
    setOwnerExecutable True
    emptyPermissions

writeDefaultFile :: String -> FilePath -> IO ()
writeDefaultFile name path = do
  putStrLn $ "Couldn't find " ++ path
  templatePath <- getDataFileName name
  putStrLn $ "Copying default from " ++ templatePath
  copyFile templatePath path

-- | @readFileOrDefault attempts to read a file from a path. If that file
-- doesn't exist, it copies a default version that was installed by cabal
-- with @writeDefaultFile.
readFileOrDefault :: String -> FilePath -> IO BS.ByteString
readFileOrDefault name path =
  (try (BS.readFile path) :: IO (Either IOError BS.ByteString)) >>=
    \case
      Left _ -> writeDefaultFile name path >> BS.readFile path
      Right content -> return content

getConfig :: IO Config
getConfig = do

  -- Create configuration and data directories
  configDir <- getConfigDir
  dataDir   <- getDataDir
  mapM_ directoryWithPermissions [configDir, dataDir]

  -- Try to read configuration file
  contents  <- unpack <$>
    readFileOrDefault "config.hs" (configDir </> "config.hs")

  -- Parse the configuration
  let result = TR.readEither (dropComments contents) :: Either String ConfigInfo
  case result of
    Left  _ -> throw ConfigParseError
    Right r -> return (processConfig dataDir configDir r)
