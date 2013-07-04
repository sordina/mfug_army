import Options.Applicative
import Data.Monoid (mempty)
import Options.Applicative.Types (ParserPrefs)

data Options = Opts {quiet :: Bool} deriving Show

optParser :: Parser Options
optParser = Opts <$> quietOption
  where quietFlag   = short 'q'
        quietHelp   = help "silence the output"
        quietOption = switch ( quietFlag <> quietHelp )

pureParser :: (Parser a) -> [String] -> Maybe a
pureParser parser = eitherToMaybe . execParserPure preferences information
  where information   = (info parser mempty)
        preferences   = prefs mempty
        eitherToMaybe = either (const Nothing) Just

main :: IO ()
main = do print $ pureParser optParser (words "-q")
          print $ pureParser optParser (words "I do what I want!")
