module LocalCooking.Global.Links.Class where

import LocalCooking.Global.Error (GlobalError (GlobalErrorRedirect), RedirectError (..))
import Auth.AccessToken.Session (SessionToken)

import Prelude
import Data.URI.Location
  ( class ToLocation, class FromLocation
  , printLocation, parseLocation, toLocation, fromLocation
  , Location (..), fromURI)
import Data.URI.URI as URI
import Data.URI.Location as Location
import Data.URI.Query (Query (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.StrMap as StrMap
import Data.Foreign (toForeign, unsafeFromForeign, isNull)
import Data.Argonaut (encodeJson, decodeJson)
import Data.UUID (GENUUID)
import Type.Proxy (Proxy (..))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.String (string, char, eof)
import Control.Alternative ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Uncurried (mkEffFn1, runEffFn2)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle (..), pushState, replaceState, URL (..))
import DOM.HTML.Window (location)
import DOM.HTML.Window.Extra (onPopStateImpl)
import DOM.HTML.Location (href)
import DOM.HTML.Types (History, HISTORY, Window)
import Unsafe.Coerce (unsafeCoerce)

import Queue.Types (WRITE)
import Queue.One as One




-- | Datums supported by all subsidiary sites' user details page
class Eq userDetailsLinks <= LocalCookingUserDetailsLinks userDetailsLinks where
  userDetailsGeneralLink :: userDetailsLinks
  userDetailsSecurityLink :: userDetailsLinks
  toUserDetailsDocumentTitle :: userDetailsLinks -> String -- ^ The prefix, i.e. `Security - `

-- | Datums supported by all subsidary sites' top-level site links
class ( Eq siteLinks
      , LocalCookingUserDetailsLinks userDetailsLinks
      ) <= LocalCookingSiteLinks
           siteLinks
           userDetailsLinks
           | siteLinks -> userDetailsLinks
           , userDetailsLinks -> siteLinks where
  rootLink :: siteLinks
  registerLink :: siteLinks
  userDetailsLink :: Maybe userDetailsLinks -> siteLinks
  getUserDetailsLink :: siteLinks -> Maybe (Maybe userDetailsLinks)
  emailConfirmLink :: siteLinks
  -- NOTE toDocumentTitle now addressed as a parameter - effective function
  subsidiaryTitle :: Proxy siteLinks -> String -- ^ The suffix, i.e. ` Chefs`


-- | Parse casual site-links, while allowing for others to be parsed out of this scope
defaultSiteLinksPathParser :: forall siteLinks userDetailsLinks
                            . LocalCookingSiteLinks siteLinks userDetailsLinks
                           => Parser userDetailsLinks
                           -> Maybe (Parser siteLinks)
                           -> Parser siteLinks
defaultSiteLinksPathParser userDetailsLinksParser mMoreGeneral = do
  let root = rootLink <$ eof
      register = registerLink <$ string "register"
      emailConfirm = emailConfirmLink <$ string "emailConfirm"
      userDetails = do
        void (string "userDetails")
        let none = Nothing <$ eof
            some = Just <$> userDetailsLinksParser
        mUserDetails <- some <|> none
        pure (userDetailsLink mUserDetails)
  try register
    <|> try emailConfirm
    <|> try userDetails
    <|> ( case mMoreGeneral of
            Nothing -> root
            Just moreGeneral -> moreGeneral <|> root
        )
  where
    divider = char '/'



prefixDocumentTitle :: forall siteLinks userDetailsLinks
                     . LocalCookingSiteLinks siteLinks userDetailsLinks
                    => Eq siteLinks
                    => String -- ^ prefix, after effects for instance
                    -> siteLinks -- ^ link
                    -> DocumentTitle -> DocumentTitle
prefixDocumentTitle pfx link xx@(DocumentTitle x) =
  if shouldPrefixDocumentTitle
    then DocumentTitle (pfx <> x)
    else xx
  where
    shouldPrefixDocumentTitle = case getUserDetailsLink link of
      Just _ -> false
      _ | link == rootLink -> false
        | link == registerLink -> false
        | otherwise -> true


-- | Given a site link, generate a nice browser Document Title - prefix to this
--   after obtaining an effectful prefix, for instance
defaultSiteLinksToDocumentTitle :: forall siteLinks userDetailsLinks
                                 . LocalCookingSiteLinks siteLinks userDetailsLinks
                                => Eq siteLinks
                                => String -> siteLinks -> DocumentTitle
defaultSiteLinksToDocumentTitle pfx link =
  let sfx = DocumentTitle $ case getUserDetailsLink link of
              Just mDetails ->
                let x = case mDetails of
                          Nothing -> ""
                          Just d -> toUserDetailsDocumentTitle d
                in  x <> "User Details - " <> docT
              _ | link == rootLink -> docT
                | link == registerLink -> "Register - " <> docT
                | otherwise -> docT -- NOTE this is where the prefix will occur
  in  prefixDocumentTitle pfx link sfx
  where
    docT = "Local Cooking" <> subsidiaryTitle (Proxy :: Proxy siteLinks)



type WREffects eff =
  ( ref :: REF
  , timer :: TIMER
  , console :: CONSOLE
  , uuid :: GENUUID
  | eff)


withRedirectPolicy :: forall eff siteLinks userDetails userDetailsLinks siteError
                    . LocalCookingSiteLinks siteLinks userDetailsLinks
                   => Eq siteLinks
                   => Show siteLinks
                   => Show userDetails
                   => { onError          :: Eff (WREffects eff) Unit
                      , extraRedirect    :: siteLinks -> Maybe userDetails -> Maybe {siteLink :: siteLinks, siteError :: siteError}
                      , sessionToken        :: Maybe SessionToken
                      , userDetails      :: Maybe userDetails
                      , globalErrorQueue :: One.Queue (write :: WRITE) (WREffects eff) GlobalError
                      , siteErrorQueue   :: One.Queue (write :: WRITE) (WREffects eff) siteError
                      }
                   -> siteLinks
                   -> Eff (WREffects eff) siteLinks
withRedirectPolicy
  { onError
  , extraRedirect
  , sessionToken: mAuth
  , userDetails: mUserDetails
  , globalErrorQueue
  , siteErrorQueue
  }
  siteLink
  = case getUserDetailsLink siteLink of
  Just _ -> case mAuth of
    Just _ -> pure siteLink
    Nothing -> do
      warn $ "Redirecting: no Auth while in /userDetails/* - " <> show siteLink
      void $ setTimeout 1000 $ -- FIXME timeouts suck
        One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectUserDetailsNoAuth)
      onError
      pure rootLink
  _ | siteLink == registerLink -> case mAuth of
      Nothing -> pure siteLink
      Just _ -> do
        warn $ "Redirecting: auth while in /register - " <> show siteLink
        void $ setTimeout 1000 $ -- FIXME timeouts suck
          One.putQueue globalErrorQueue (GlobalErrorRedirect RedirectRegisterAuth)
        onError
        pure rootLink
    | otherwise -> do
      -- Gets the user details on /next/ submission - hopefully to avoid initSiteLinks
      -- race condition
      case extraRedirect siteLink mUserDetails of
        Nothing -> pure siteLink
        Just {siteLink:y,siteError} -> do
          warn $ "Redirecting: extra redirect produced new link - old: " <> show siteLink <> ", new: " <> show y <> ", user details: " <> show mUserDetails
          void $ setTimeout 1000 $ -- FIXME timeouts suck
            One.putQueue siteErrorQueue siteError
          onError
          pure y



pushState' :: forall eff siteLinks userDetailsLinks
            . ToLocation siteLinks
           => Eq siteLinks
           => LocalCookingSiteLinks siteLinks userDetailsLinks
           => String -- ^ Prefix, post effects
           -> siteLinks
           -> History
           -> Eff (history :: HISTORY, console :: CONSOLE | eff) Unit
pushState' pfx x h = do
  pushState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (defaultSiteLinksToDocumentTitle pfx x)
    (URL $ Location.printLocation $ toLocation x)
    h


replaceState' :: forall eff siteLinks userDetailsLinks
               . ToLocation siteLinks
              => Eq siteLinks
              => LocalCookingSiteLinks siteLinks userDetailsLinks
              => String -- ^ Prefix, post effects
              -> siteLinks
              -> History
              -> Eff (history :: HISTORY, console :: CONSOLE | eff) Unit
replaceState' pfx x h = do
  replaceState
    (toForeign $ encodeJson $ printLocation $ toLocation x)
    (defaultSiteLinksToDocumentTitle pfx x)
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



initSiteLinks :: forall eff siteLinks userDetailsLinks
               . LocalCookingSiteLinks siteLinks userDetailsLinks
              => ToLocation siteLinks
              => FromLocation siteLinks
              => Eq siteLinks
              => Show siteLinks
              => Eff ( console :: CONSOLE
                     , dom     :: DOM
                     | eff) siteLinks
initSiteLinks = do
  w <- window
  l <- location w
  p <- href l
  let rootLink' :: siteLinks
      rootLink' = rootLink
  case URI.parse p of
    Left e -> do
      warn $ "Href parsing error: " <> show e
      pure rootLink'
    Right uri -> case fromURI uri of
      Nothing -> do
        warn $ "URI can't be a location: " <> show uri
        pure rootLink'
      Just {location: location@(Location _ mQuery _)} -> case fromLocation location of
        Left e -> do
          warn $ "Location can't be a SiteLinks: " <> e <> ", " <> printLocation location
          pure rootLink'
        Right (x :: siteLinks) -> do
          -- FIXME only adjust for sessionToken when it's parsable? Why?
          case mQuery of
            Nothing -> do
              log $ "Sucessfully parsed initSiteLinks from location: " <> printLocation location <> ", " <> show x
              pure x
            Just (Query qs) -> do
              case
                    StrMap.lookup "sessionToken" (StrMap.fromFoldable qs)
                <|> StrMap.lookup "formData" (StrMap.fromFoldable qs)
                <|> StrMap.lookup "emailToken" (StrMap.fromFoldable qs)
                of
                Nothing -> do
                  log $ "Sucessfully parsed initSiteLinks from location: " <> printLocation location <> ", " <> show x
                  pure x
                Just _
                  | x == emailConfirmLink -> do
                    warn "Redirecting to root due to email confirm token"
                    pure rootLink'
                  | otherwise -> do
                    warn $ "Redirecting to parsed value " <> show x <> ", due to presence of query parameters"
                    pure x
