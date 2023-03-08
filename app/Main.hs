{-# LANGUAGE
      OverloadedStrings 
    , DeriveGeneric 
    , DuplicateRecordFields
    , ViewPatterns 
    , NamedFieldPuns 
    , DuplicateRecordFields
    #-}

module Main where 

import Data.Lightning 
import Data.Lightning.Generic 
import Control.Plugin 
import Control.Client 
import Control.Conduit 
import Data.Aeson
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class 
import GHC.Generics 
import Data.Text (Text)
import qualified Data.Text.Lazy as LT 
import qualified Data.Text as T 
import Fmt 
import Data.Text.Format.Numbers 
import Data.List 
import Data.Maybe
import Data.Ratio
import Data.Foldable

prin m = liftIO $ appendFile "/home/o/.ao/storm" $ show m 

main = plugin manifest start app 

manifest = object [
      "dynamic" .= True
    , "options" .= ([] :: [Option])
    , "rpcmethods" .= [
         RpcMethod "wallet" "" "print wallet totals" Nothing False 
       , RpcMethod "balances" "" "print channel balances" Nothing False 
       , RpcMethod "fees" "" "print channel fees" Nothing False
       ]
    ]

start :: PluginInit ()
start _ = pure () 

app :: PluginApp () 
app (Just i, "wallet", _) = do 
    Just (Res (fromJSON -> Success x@(Funds{}) ) _) <- lightningCli (Command "listfunds" fundFilter fundParams) 
    respond (summarizeFunds x) i  
    where 
        summarizeFunds :: Funds -> Value
        summarizeFunds myf = object [
              "withdraw" .= (prettyI (Just ',') . (`div` 1000) . outSum . outputs $ myf)
            , "pay" .= (prettyI (Just ',') (payable $ fchannels myf))
            , "invoice" .= (prettyI (Just ',') $ recable (fchannels myf))
            ]
            where outSum :: [Outs] -> Msat 
                  outSum = sum . map __amount_msat . filter ((=="confirmed") . __status) 
                  payable = sum . map our_amount_msat . filter ((=="CHANNELD_NORMAL") . _state)
                  recable = sum . map recable' . filter ((=="CHANNELD_NORMAL")._state) 
                  recable' a = _amount_msat a - our_amount_msat a
        
app (Just i, "balances", _) = do
    Just (Res (fromJSON -> Success (Funds{channels})) _) <- lightningCli (Command "listfunds" fundFilter fundParams) 
    respond (balances channels) i 
    where 
        balances c = toJSON  (map showChan $ sort $ c)
        showChan :: Chans -> Text
        showChan (Chans a o s n i) = 
            let ours = round $ 13 * o % a 
                theirs = 13 - ours
                redash = LT.repeat '-'
            in "" +| (build $ T.justifyLeft 13 ' ' <$> i ) 
                  +| " |" 
                  +| (build $ LT.take ours redash)
                  +| "#"
                  +| (build $ LT.take theirs redash) 
                  +| "|" 
                  +| if s /= "CHANNELD_NORMAL" then build s else ""  
                

app (Just i, "fees", _) = do 
    Just (Res (fromJSON -> Success (Funds{channels}) ) _) <- lightningCli (Command "listfunds" fundFilter fundParams)
    fees <- mapM (getFees) $ sort $ filter (isJust._short_channel_id) channels
    respond (toJSON fees) i 
    where 
        getFees :: Chans -> PluginMonad s Text
        getFees Chans{_short_channel_id, peer_id} = 
            let 
              sci = fromJust _short_channel_id
              listchans = Command "listchannels" feeFields params
              params = object [ "short_channel_id" .= sci ]
              orderer (Fee{_destination}) _ = if _destination == peer_id then LT else GT
              buildFee f = build $ " " <> show (_base_fee_millisatoshi f, _fee_per_millionth f, _active f) 
            in do
                prin sci
                Just (Res (fromJSON -> Success (Fees fx)) _) <- lightningCli listchans
                pure $ "" +| (build $ T.justifyLeft 13 ' ' sci ) 
                          +| fold (map buildFee $ sortBy orderer fx)

fundFilter = object [
      "outputs" .= [outputFields]
    , "channels" .= [chanFields]
    ]
outputFields = object [
      "amount_msat" .= True
    , "status" .= True     
    ]
chanFields = object [
      "amount_msat" .= True
    , "our_amount_msat" .= True
    , "state" .= True
    , "peer_id" .= True
    , "short_channel_id" .= True
    ]

feeFields = object [
    "channels" .= [ object [
          "fee_per_millionth" .= True
        , "base_fee_millisatoshi" .= True
        , "active" .= True
        , "destination" .= True 
    ]]]

data Fees = Fees {
    channels ::  [Fee] 
    } deriving (Show, Generic)
instance ToJSON Fees 
instance FromJSON Fees 

data Fee = Fee {
      _fee_per_millionth :: Int
    , _base_fee_millisatoshi :: Msat
    , _active :: Bool
    , _destination :: Text 
    } deriving (Show, Generic) 
instance FromJSON Fee where 
    parseJSON = defaultParse
instance ToJSON Fee

data Funds = Funds {
      outputs :: [Outs]
    , channels :: [Chans] 
    } deriving Generic 
instance FromJSON Funds
fchannels :: Funds->[Chans]
fchannels = channels 

data Outs = Outs {
      __amount_msat :: Msat 
    , __status :: Text 
    } deriving Generic 
instance FromJSON Outs where 
    parseJSON = defaultParse

data Chans = Chans {
      _amount_msat :: Msat
    , our_amount_msat :: Msat 
    , _state :: Text
    , peer_id :: Text 
    , _short_channel_id :: Maybe Text 
    } deriving (Eq, Generic, Show) 
instance FromJSON Chans where 
    parseJSON = defaultParse
instance Ord Chans where 
    compare x y = compare (_short_channel_id x) (_short_channel_id y)

fundParams = object [ ] 
