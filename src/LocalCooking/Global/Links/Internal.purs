module LocalCooking.Global.Links.Internal where

import Prelude
import Data.URI.Location (Location (..), class ToLocation)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Path.Pathy ((</>), dir, file, rootDir)




data ImageLinks
  = LogoPng
  | Logo40Png
  | LogoWhitePng
  | LogoWhite40Png
  | IconPng
  | IconSvg


instance toLocationImageLinks :: ToLocation ImageLinks where
  toLocation x = case x of
    LogoPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo.png") Nothing Nothing
    Logo40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-40.png") Nothing Nothing
    LogoWhitePng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white.png") Nothing Nothing
    LogoWhite40Png -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "logo-white-40.png") Nothing Nothing
    IconPng -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.png") Nothing Nothing
    IconSvg -> Location (Right $ rootDir </> dir "static" </> dir "images" </> file "icon.svg") Nothing Nothing


data PolicyLinks
  = PrivacyPolicyLink

instance toLocationPolicyLinks :: ToLocation PolicyLinks where
  toLocation x = case x of
    PrivacyPolicyLink -> Location (Right $ rootDir </> file "privacypolicy.html") Nothing Nothing
