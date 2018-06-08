module LocalCooking.Global.User.Class where

-- import Text.Email.Validate (EmailAddress)
-- import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Semantics.Common (User)


-- | Fields assumed to be supported by the subsidiary site's user details -
-- | additional fields might include customer details, chef details, etc.
class UserDetails userDetails where
  getUser :: userDetails -> User
