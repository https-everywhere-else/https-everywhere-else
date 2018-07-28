module Paths_https_everywhere_rules where
import System.FilePath.Posix (combine)

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (combine "vendor/https-everywhere/rules")
