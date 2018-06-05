module Facebook.Call where

import Facebook.Types (FacebookClientId (..))
import Facebook.State (FacebookLoginState)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.URI (URI (..), HierarchicalPart (..), Scheme (..), Host (..), Authority (..), Query (..))
import Data.URI.URI as URI
import Data.List (List (..))
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.Argonaut (encodeJson)


newtype FacebookLoginLink
  = FacebookLoginLink
    { redirectURL :: URI
    , state :: FacebookLoginState
    }

facebookLoginLinkToURI :: FacebookClientId -> FacebookLoginLink -> URI
facebookLoginLinkToURI (FacebookClientId clientId) (FacebookLoginLink {redirectURL,state}) =
  URI
    (Just $ Scheme "https")
    ( HierarchicalPart
      (Just $ Authority Nothing [Tuple (NameAddress "www.facebook.com") Nothing])
      (Just $ Right $ rootDir </> dir "v2.12" </> dir "dialog" </> file "oauth")
    )
    ( Just $ Query
      $ Cons
        (Tuple "client_id" $ Just clientId)
      $ Cons
        (Tuple "redirect_uri" $ Just $ URI.print redirectURL)
      $ Cons
        (Tuple "state" $ Just $ show $ encodeJson state)
        Nil
    )
    Nothing
