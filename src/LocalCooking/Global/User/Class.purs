module LocalCooking.Global.User.Class where

import LocalCooking.Semantics.Common (User)


-- | Fields assumed to be supported by the subsidiary site's user details -
-- | additional fields might include customer details, chef details, etc.
class UserDetails userDetails where
  getUser :: userDetails -> User
  setUser :: User -> userDetails -> userDetails
