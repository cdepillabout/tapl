module Options (getFile) where

import Data.Monoid ((<>))
import Options.Applicative (
    Parser, ParserInfo, execParser, fullDesc, header, help, helper, info,
    fullDesc, metavar, progDesc, strArgument
    )

parser :: Parser FilePath
parser = strArgument $ metavar "FILE"
                  <> help "File to read in to process."

getFile :: IO FilePath
getFile = execParser opts
  where
    opts :: ParserInfo FilePath
    opts = info (helper <*> parser) $
            fullDesc
         <> progDesc "Interpret FILE"
         <> header "A small interpreter."
