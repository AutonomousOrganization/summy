{-# LANGUAGE
      OverloadedStrings 
    , DeriveGeneric 
    , DuplicateRecordFields
    , ViewPatterns 
    , NamedFieldPuns 
    , DuplicateRecordFields
    , LambdaCase
    , RecordWildCards
    #-}

module Main where 

import Data.Lightning 
import Data.Lightning.Generic 
import Control.Plugin 
import Control.Client 
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Key
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class 
import GHC.Generics 
import System.Directory
import Data.Text (Text)
import qualified Data.Text.Lazy as LT 
import qualified Data.Text as T 
import qualified Data.Text.IO as T 
import Fmt 
import Data.Text.Format.Numbers 
import Data.List 
import Data.Maybe
import Data.Ratio
import Data.Foldable
import Data.Time.LocalTime
import Data.Time.Format
import System.Process (callCommand) 

prin m = liftIO $ appendFile "/home/o/.ao/storm" $ show m 

main = plugin manifest start app 

manifest = object [
      "dynamic" .= True
    , "options" .= ([
         Option "logfile" "string" "" "file path for logs" False
      ])
    , "rpcmethods" .= [
         RpcMethod "wallet" "" "print wallet totals" Nothing False 
       , RpcMethod "balances" "" "print channel balances" Nothing False 
       , RpcMethod "fees" "" "print channel fees" Nothing False
       ]
    , "subscriptions" .= (["forward_event"] :: [Text] ) 
    ]

start :: InitMonad (Msat, Maybe FilePath) 
start = do 
    f <- getValidOption
    pure (0, f) 

app :: PluginApp (Msat, Maybe FilePath) 

app (Nothing, "forward_event", 
    fromJSON -> Success (ForwardEvent{status = "settled", ..})) = do
        (x, Just filepath) <- get 
        time <- liftIO $ do 
            zone <- getZonedTime 
            pure $ formatTime defaultTimeLocale "%H:%M" zone
        liftIO $ movelog filepath time (x + fee_msat) fee_msat 
        put (x + fee_msat, Just filepath) 
        liftIO . T.appendFile filepath $ "" +| build in_channel  
                    +| " > "
                    +| build out_channel 
                    +| "\n"

app (Just i, "wallet", _) = do 
    Just (Res (fromJSON -> Success funds@(Funds{}) ) _) <- lightningCli $ 
        Command "listfunds" fundFilter fundParams
    Just (Res (fromJSON -> Success pends@(Pays{})) _) <- lightningCli $ 
        Command "listpays" payFilter (object ["status".= ("pending"::Text) ]) 
    respond (summarizeFunds funds pends) i  
    where 
        summarizeFunds myf myp = object $ [
              "withdraw" .= (prettyI (Just ',') . (`div` 1000) . outSum . outputs $ myf)
            , "pay" .= (prettyI (Just ',') (payable $ fchannels myf))
            , "invoice" .= (prettyI (Just ',') $ recable (fchannels myf))
            ] <> 
                case limbo (fchannels myf) of 
                    [] -> []
                    x -> ["_channel_limbo" .= object x]
              <> 
                case pays myp of
                    [] -> []
                    px -> ["_pending_pays" .= px] 
            where outSum :: [Outs] -> Msat 
                  outSum = sum . map __amount_msat . filter ((=="confirmed") . __status) 
                  payable = sum . map our_amount_msat . normal 
                  recable = sum . map recable' . normal 
                  recable' a = _amount_msat a - our_amount_msat a
                  normal = filter ((=="CHANNELD_NORMAL")._state)
                  abnormal = filter ((/="CHANNELD_NORMAL")._state)
                  limbo = map keyOb . abnormal 
                  keyOb (Chans{peer_id, _short_channel_id, our_amount_msat}) = fromText sho .= our_amount_msat
                      where sho = case _short_channel_id of 
                                             Just x -> x 
                                             _      -> peer_id
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

app (Just i, _, _) = release i 
app _ = pure () 


movelog :: FilePath -> String -> Msat -> Msat -> IO () 
movelog path time  tots now = do 
    liftIO . (T.appendFile path) . fmtLn $ mlog
    where 
        mlog = (build.(prettyI (Just ',')).(`div` 1000) $ tots) 
            +| " total " 
            +| build time
            +| " +"
            +| build now

getValidOption :: InitMonad (Maybe FilePath)
getValidOption = do 
    Init opts _ <- asks (conf)
    Just f <- pure $ parseMaybe (.: "logfile") $ opts
    liftIO $ getFile f
    where getFile y = doesPathExist y >>= \case 
              True -> pure (Just y)
              False -> pure Nothing    
          getFile _ = pure Nothing 

data Pays = Pays {
      pays :: [Pay]
    } deriving (Show, Generic)
instance ToJSON Pays
instance FromJSON Pays

data Pay = Pay {
      ___destination :: Text
    , ___amount_msat :: Msat
    } deriving (Show, Generic)
instance ToJSON Pay where 
    toJSON (Pay d a) = toJSON $ d <> " : " <> (T.pack.show $ a) 
instance FromJSON Pay where 
    parseJSON = defaultParse

payFilter = Just $ object [ "pays" .= [object [
      "destination" .= True
    , "amount_msat" .= True
    ]]]

fundFilter = Just $ object [
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

feeFields = Just $ object [
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
