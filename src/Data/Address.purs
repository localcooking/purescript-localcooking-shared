module Data.Address where

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gCompare, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)



data USAState
  = AL
  | AK
  | AZ
  | AR
  | CA
  | CO
  | CT
  | DE
  | FL
  | GA
  | HI
  | ID
  | IL
  | IN
  | IA
  | KS
  | KY
  | LA
  | ME
  | MD
  | MA
  | MI
  | MN
  | MS
  | MO
  | MT
  | NE
  | NV
  | NH
  | NJ
  | NM
  | NY
  | NC
  | ND
  | OH
  | OK
  | OR
  | PA
  | RI
  | SC
  | SD
  | TN
  | TX
  | UT
  | VT
  | VA
  | WA
  | WV
  | WI
  | WY

derive instance genericUSAState :: Generic USAState

instance arbitraryUSAState :: Arbitrary USAState where
  arbitrary = oneOf $ NonEmpty
    ( pure AL
    )
    [ pure AK
    , pure AZ
    , pure AR
    , pure CA
    , pure CO
    , pure CT
    , pure DE
    , pure FL
    , pure GA
    , pure HI
    , pure ID
    , pure IL
    , pure IN
    , pure IA
    , pure KS
    , pure KY
    , pure LA
    , pure ME
    , pure MD
    , pure MA
    , pure MI
    , pure MN
    , pure MS
    , pure MO
    , pure MT
    , pure NE
    , pure NV
    , pure NH
    , pure NJ
    , pure NM
    , pure NY
    , pure NC
    , pure ND
    , pure OH
    , pure OK
    , pure OR
    , pure PA
    , pure RI
    , pure SC
    , pure SD
    , pure TN
    , pure TX
    , pure UT
    , pure VT
    , pure VA
    , pure WA
    , pure WV
    , pure WI
    , pure WY
    ]

usaStateParser :: Parser USAState
usaStateParser = do
  let al = AL <$ string "AL"   
      ak = AK <$ string "AK"   
      az = AZ <$ string "AZ"   
      ar = AR <$ string "AR"   
      ca = CA <$ string "CA"   
      co = CO <$ string "CO"   
      ct = CT <$ string "CT"   
      de = DE <$ string "DE"   
      fl = FL <$ string "FL"   
      ga = GA <$ string "GA"   
      hi = HI <$ string "HI"   
      id = ID <$ string "ID"   
      il = IL <$ string "IL"   
      in' = IN <$ string "IN"   
      ia = IA <$ string "IA"   
      ks = KS <$ string "KS"   
      ky = KY <$ string "KY"   
      la = LA <$ string "LA"   
      me = ME <$ string "ME"   
      md = MD <$ string "MD"   
      ma = MA <$ string "MA"   
      mi = MI <$ string "MI"   
      mn = MN <$ string "MN"   
      ms = MS <$ string "MS"   
      mo = MO <$ string "MO"   
      mt = MT <$ string "MT"   
      ne = NE <$ string "NE"   
      nv = NV <$ string "NV"   
      nh = NH <$ string "NH"   
      nj = NJ <$ string "NJ"   
      nm = NM <$ string "NM"   
      ny = NY <$ string "NY"   
      nc = NC <$ string "NC"   
      nd = ND <$ string "ND"   
      oh = OH <$ string "OH"   
      ok = OK <$ string "OK"   
      or = OR <$ string "OR"   
      pa = PA <$ string "PA"   
      ri = RI <$ string "RI"   
      sc = SC <$ string "SC"   
      sd = SD <$ string "SD"   
      tn = TN <$ string "TN"   
      tx = TX <$ string "TX"   
      ut = UT <$ string "UT"   
      vt = VT <$ string "VT"   
      va = VA <$ string "VA"   
      wa = WA <$ string "WA"   
      wv = WV <$ string "WV"   
      wi = WI <$ string "WI"   
      wy = WY <$ string "WY"   
  al
    <|> ak
    <|> az
    <|> ar
    <|> ca
    <|> co
    <|> ct
    <|> de
    <|> fl
    <|> ga
    <|> hi
    <|> id
    <|> il
    <|> in'
    <|> ia
    <|> ks
    <|> ky
    <|> la
    <|> me
    <|> md
    <|> ma
    <|> mi
    <|> mn
    <|> ms
    <|> mo
    <|> mt
    <|> ne
    <|> nv
    <|> nh
    <|> nj
    <|> nm
    <|> ny
    <|> nc
    <|> nd
    <|> oh
    <|> ok
    <|> or
    <|> pa
    <|> ri
    <|> sc
    <|> sd
    <|> tn
    <|> tx
    <|> ut
    <|> vt
    <|> va
    <|> wa
    <|> wv
    <|> wi
    <|> wy
  

allUSAStates :: Array USAState
allUSAStates =
  [ AL
  , AK
  , AZ
  , AR
  , CA
  , CO
  , CT
  , DE
  , FL
  , GA
  , HI
  , ID
  , IL
  , IN
  , IA
  , KS
  , KY
  , LA
  , ME
  , MD
  , MA
  , MI
  , MN
  , MS
  , MO
  , MT
  , NE
  , NV
  , NH
  , NJ
  , NM
  , NY
  , NC
  , ND
  , OH
  , OK
  , OR
  , PA
  , RI
  , SC
  , SD
  , TN
  , TX
  , UT
  , VT
  , VA
  , WA
  , WV
  , WI
  , WY
  ]

instance eqUSAState :: Eq USAState where
  eq = gEq

instance ordUSAState :: Ord USAState where
  compare = gCompare

instance showUSAState :: Show USAState where
  show x = case x of
    AL -> "AL"
    AK -> "AK"
    AZ -> "AZ"
    AR -> "AR"
    CA -> "CA"
    CO -> "CO"
    CT -> "CT"
    DE -> "DE"
    FL -> "FL"
    GA -> "GA"
    HI -> "HI"
    ID -> "ID"
    IL -> "IL"
    IN -> "IN"
    IA -> "IA"
    KS -> "KS"
    KY -> "KY"
    LA -> "LA"
    ME -> "ME"
    MD -> "MD"
    MA -> "MA"
    MI -> "MI"
    MN -> "MN"
    MS -> "MS"
    MO -> "MO"
    MT -> "MT"
    NE -> "NE"
    NV -> "NV"
    NH -> "NH"
    NJ -> "NJ"
    NM -> "NM"
    NY -> "NY"
    NC -> "NC"
    ND -> "ND"
    OH -> "OH"
    OK -> "OK"
    OR -> "OR"
    PA -> "PA"
    RI -> "RI"
    SC -> "SC"
    SD -> "SD"
    TN -> "TN"
    TX -> "TX"
    UT -> "UT"
    VT -> "VT"
    VA -> "VA"
    WA -> "WA"
    WV -> "WV"
    WI -> "WI"
    WY -> "WY"


instance encodeJsonUSAState :: EncodeJson USAState where
  encodeJson = encodeJson <<< show

instance decodeJsonUSAState :: DecodeJson USAState where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "AL" -> pure AL
        | s == "AK" -> pure AK
        | s == "AZ" -> pure AZ
        | s == "AR" -> pure AR
        | s == "CA" -> pure CA
        | s == "CO" -> pure CO
        | s == "CT" -> pure CT
        | s == "DE" -> pure DE
        | s == "FL" -> pure FL
        | s == "GA" -> pure GA
        | s == "HI" -> pure HI
        | s == "ID" -> pure ID
        | s == "IL" -> pure IL
        | s == "IN" -> pure IN
        | s == "IA" -> pure IA
        | s == "KS" -> pure KS
        | s == "KY" -> pure KY
        | s == "LA" -> pure LA
        | s == "ME" -> pure ME
        | s == "MD" -> pure MD
        | s == "MA" -> pure MA
        | s == "MI" -> pure MI
        | s == "MN" -> pure MN
        | s == "MS" -> pure MS
        | s == "MO" -> pure MO
        | s == "MT" -> pure MT
        | s == "NE" -> pure NE
        | s == "NV" -> pure NV
        | s == "NH" -> pure NH
        | s == "NJ" -> pure NJ
        | s == "NM" -> pure NM
        | s == "NY" -> pure NY
        | s == "NC" -> pure NC
        | s == "ND" -> pure ND
        | s == "OH" -> pure OH
        | s == "OK" -> pure OK
        | s == "OR" -> pure OR
        | s == "PA" -> pure PA
        | s == "RI" -> pure RI
        | s == "SC" -> pure SC
        | s == "SD" -> pure SD
        | s == "TN" -> pure TN
        | s == "TX" -> pure TX
        | s == "UT" -> pure UT
        | s == "VT" -> pure VT
        | s == "VA" -> pure VA
        | s == "WA" -> pure WA
        | s == "WV" -> pure WV
        | s == "WI" -> pure WI
        | s == "WY" -> pure WY
        | otherwise -> fail "Not a USAState"



newtype USAAddress = USAAddress
  { street :: String
  , city   :: String
  , state  :: USAState
  , zip    :: Int
  }

derive instance genericUSAAddress :: Generic USAAddress

instance arbitraryUSAAddress :: Arbitrary USAAddress where
  arbitrary = do
    street <- arbitrary
    city <- arbitrary
    state <- arbitrary
    zip <- arbitrary
    pure (USAAddress {street,city,state,zip})

instance eqUSAAddress :: Eq USAAddress where
  eq = gEq

instance showUSAAddress :: Show USAAddress where
  show = gShow

instance encodeJsonUSAAddress :: EncodeJson USAAddress where
  encodeJson (USAAddress {street,city,state,zip})
    =  "street" := street
    ~> "city" := city
    ~> "state" := state
    ~> "zip" := zip
    ~> jsonEmptyObject

instance decodeJsonUSAAddress :: DecodeJson USAAddress where
  decodeJson json = do
    o <- decodeJson json
    street <- o .? "street"
    city <- o .? "city"
    state <- o .? "state"
    zip <- o .? "zip"
    pure (USAAddress {street,city,state,zip})
