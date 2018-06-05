module LocalCooking.Global.Links.External where


import Prelude
import Data.URI.Location (Location (..), class ToLocation)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Path.Pathy ((</>), file, rootDir)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (char, string, eof)



data ThirdPartyLoginReturnLinks
  = FacebookLoginReturn -- (Maybe {code :: String, state :: Maybe Unit}) -- FIXME hardcode a facebook login state

instance toLocationThirdPartyLoginReturnLinks :: ToLocation ThirdPartyLoginReturnLinks where
  toLocation x = case x of
    FacebookLoginReturn -> Location (Right $ rootDir </> file "facebookLoginReturn") Nothing Nothing


thirdPartyLoginReturnLinksParser :: Parser ThirdPartyLoginReturnLinks
thirdPartyLoginReturnLinksParser = do
  let facebook = do
        void divider
        FacebookLoginReturn <$ (string "facebookLoginReturn" *> eof)
  facebook
  where
    divider = char '/'

