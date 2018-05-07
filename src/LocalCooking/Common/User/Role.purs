module LocalCooking.Common.User.Role where

import Prelude
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)


data UserRole
  = Customer
  | Chef
  | Farmer
  | Editor
  | Manager
  | Admin

derive instance genericUserRole :: Generic UserRole

instance eqUserRole :: Eq UserRole where
  eq = gEq

instance showUserRole :: Show UserRole where
  show x = case x of
    Customer -> "Customer"
    Chef     -> "Chef"
    Farmer   -> "Farmer"
    Editor   -> "Editor"
    Manager  -> "Manager"
    Admin    -> "Admin"

instance encodeJsonUserRole :: EncodeJson UserRole where
  encodeJson x = encodeJson $ case x of
    Customer -> "customer"
    Chef     -> "chef"
    Farmer   -> "farmer"
    Editor   -> "editor"
    Manager  -> "manager"
    Admin    -> "admin"

instance decodeJsonUserRole :: DecodeJson UserRole where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "customer" -> pure Customer
        | s == "chef" -> pure Chef
        | s == "farmer" -> pure Farmer
        | s == "editor" -> pure Editor
        | s == "manager" -> pure Manager
        | s == "admin" -> pure Admin
        | otherwise -> fail "Not a UserRole"
