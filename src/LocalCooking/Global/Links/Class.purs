module LocalCooking.Global.Links.Class where

import Prelude
import Data.URI.Location
  ( class ToLocation, class FromLocation
  , printLocation, parseLocation, toLocation, fromLocation)
import Data.URI.Location as Location
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Foreign (toForeign, unsafeFromForeign, isNull)
import Data.Argonaut (encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.String (string, char, eof)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Uncurried (mkEffFn1, runEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM (DOM)
import DOM.HTML.History (DocumentTitle (..), pushState, replaceState, URL (..))
import DOM.HTML.Window.Extra (onPopStateImpl)
import DOM.HTML.Types (History, HISTORY, Window)
import Unsafe.Coerce (unsafeCoerce)



-- | Datums supported by all subsidiary sites' user details page
class Eq userDetailsLinks <= LocalCookingUserDetailsLinks userDetailsLinks where
  userDetailsGeneralLink :: userDetailsLinks
  userDetailsSecurityLink :: userDetailsLinks
  toUserDetailsDocumentTitle :: userDetailsLinks -> String -- ^ The prefix, i.e. `Security - `

-- | Datums supported by all subsidary sites' top-level site links
class ( Eq siteLinks
      , LocalCookingUserDetailsLinks userDetailsLinks
      ) <= LocalCookingSiteLinks siteLinks userDetailsLinks
           | siteLinks -> userDetailsLinks
           , userDetailsLinks -> siteLinks where
  rootLink :: siteLinks
  registerLink :: siteLinks
  userDetailsLink :: Maybe userDetailsLinks -> siteLinks
  getUserDetailsLink :: siteLinks -> Maybe (Maybe userDetailsLinks)
  toDocumentTitle :: siteLinks -> String -- ^ The prefix, i.e. `Register - `
  subsidiaryTitle :: Proxy siteLinks -> String -- ^ The suffix, i.e. ` Chefs`


-- | Parse casual site-links, while allowing for others to be parsed out of this scope
defaultSiteLinksPathParser :: forall siteLinks userDetailsLinks
                            . LocalCookingSiteLinks siteLinks userDetailsLinks
                           => Parser userDetailsLinks -> Parser siteLinks
defaultSiteLinksPathParser userDetailsLinksParser = do
  let root = rootLink <$ eof
      register = do
        void (string "register")
        pure registerLink
      userDetails = do
        void (string "userDetails")
        mUserDetails <- optionMaybe userDetailsLinksParser
        pure (userDetailsLink mUserDetails)
  try register
    <|> try userDetails
    <|> root
  where
    divider = char '/'


-- | Given a site link, generate a nice browser Document Title
defaultSiteLinksToDocumentTitle :: forall siteLinks userDetailsLinks
                                 . LocalCookingSiteLinks siteLinks userDetailsLinks
                                => Eq siteLinks
                                => siteLinks
                                -> DocumentTitle
defaultSiteLinksToDocumentTitle link =
  DocumentTitle $ case getUserDetailsLink link of
    Just mDetails ->
      let x = case mDetails of
                Nothing -> ""
                Just d -> toUserDetailsDocumentTitle d
      in  x <> "User Details - " <> docT
    _ | link == rootLink -> docT
      | link == registerLink -> "Register - " <> docT
      | otherwise -> toDocumentTitle link <> docT
  where
    docT = "Local Cooking" <> subsidiaryTitle (Proxy :: Proxy siteLinks)



pushState' :: forall eff siteLinks userDetailsLinks
            . ToLocation siteLinks
           => Eq siteLinks
           => LocalCookingSiteLinks siteLinks userDetailsLinks
           => siteLinks -> History -> Eff (history :: HISTORY | eff) Unit
pushState' x h = do
  pushState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (defaultSiteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


replaceState' :: forall eff siteLinks userDetailsLinks
               . ToLocation siteLinks
              => Eq siteLinks
              => Show siteLinks
              => LocalCookingSiteLinks siteLinks userDetailsLinks
              => siteLinks -> History -> Eff (history :: HISTORY | eff) Unit
replaceState' x h = do
  unsafeCoerceEff $ log $ "Replacing state... " <> show x
  replaceState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (defaultSiteLinksToDocumentTitle x)
    (URL $ Location.printLocation $ toLocation x)
    h


onPopState :: forall eff siteLinks userDetailsLinks
            . FromLocation siteLinks
           => LocalCookingSiteLinks siteLinks userDetailsLinks
           => (siteLinks -> Eff (dom :: DOM, exception :: EXCEPTION, console :: CONSOLE | eff) Unit)
           -> Window
           -> Eff (dom :: DOM, exception :: EXCEPTION, console :: CONSOLE | eff) Unit
onPopState go w =
  onPopState' \fgn -> if isNull fgn
                         then go rootLink
                         else case decodeJson (unsafeFromForeign fgn) of
    Left e -> do
      log (unsafeCoerce fgn)
      throw $ "onPopState decoding error: " <> e
    Right str -> case runParser parseLocation str of
      Left e -> throw $ "onPopState location parsing error: " <> show e <> ", original: " <> str
      Right loc -> case fromLocation loc of
        Left e -> throw $ "onPopState fromLocation error: " <> e <> ", original: " <> printLocation loc
        Right (x :: siteLinks) -> go x
  where
    onPopState' f = runEffFn2 onPopStateImpl (mkEffFn1 f) w