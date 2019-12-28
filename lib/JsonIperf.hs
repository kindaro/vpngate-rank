
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module JsonIperf where

import Prelude

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero)
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data ConnectedElt = ConnectedElt { 
    connectedEltRemotePort :: Double,
    connectedEltSocket :: Double,
    connectedEltRemoteHost :: Text,
    connectedEltLocalHost :: Text,
    connectedEltLocalPort :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ConnectedElt where
  parseJSON (Object v) = ConnectedElt <$> v .:  "remote_port" <*> v .:  "socket" <*> v .:  "remote_host" <*> v .:  "local_host" <*> v .:  "local_port"
  parseJSON _          = mzero


instance ToJSON ConnectedElt where
  toJSON     (ConnectedElt {..}) = object ["remote_port" .= connectedEltRemotePort, "socket" .= connectedEltSocket, "remote_host" .= connectedEltRemoteHost, "local_host" .= connectedEltLocalHost, "local_port" .= connectedEltLocalPort]
  toEncoding (ConnectedElt {..}) = pairs  ("remote_port" .= connectedEltRemotePort<>"socket" .= connectedEltSocket<>"remote_host" .= connectedEltRemoteHost<>"local_host" .= connectedEltLocalHost<>"local_port" .= connectedEltLocalPort)


data ConnectingTo = ConnectingTo { 
    connectingToHost :: Text,
    connectingToPort :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ConnectingTo where
  parseJSON (Object v) = ConnectingTo <$> v .:  "host" <*> v .:  "port"
  parseJSON _          = mzero


instance ToJSON ConnectingTo where
  toJSON     (ConnectingTo {..}) = object ["host" .= connectingToHost, "port" .= connectingToPort]
  toEncoding (ConnectingTo {..}) = pairs  ("host" .= connectingToHost<>"port" .= connectingToPort)


data Timestamp = Timestamp { 
    timestampTime :: Text,
    timestampTimesecs :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Timestamp where
  parseJSON (Object v) = Timestamp <$> v .:  "time" <*> v .:  "timesecs"
  parseJSON _          = mzero


instance ToJSON Timestamp where
  toJSON     (Timestamp {..}) = object ["time" .= timestampTime, "timesecs" .= timestampTimesecs]
  toEncoding (Timestamp {..}) = pairs  ("time" .= timestampTime<>"timesecs" .= timestampTimesecs)


data TestStart = TestStart { 
    testStartBlocks :: Double,
    testStartTos :: Double,
    testStartProtocol :: Text,
    testStartOmit :: Double,
    testStartBlksize :: Double,
    testStartReverse :: Double,
    testStartDuration :: Double,
    testStartNumStreams :: Double,
    testStartBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TestStart where
  parseJSON (Object v) = TestStart <$> v .:  "blocks" <*> v .:  "tos" <*> v .:  "protocol" <*> v .:  "omit" <*> v .:  "blksize" <*> v .:  "reverse" <*> v .:  "duration" <*> v .:  "num_streams" <*> v .:  "bytes"
  parseJSON _          = mzero


instance ToJSON TestStart where
  toJSON     (TestStart {..}) = object ["blocks" .= testStartBlocks, "tos" .= testStartTos, "protocol" .= testStartProtocol, "omit" .= testStartOmit, "blksize" .= testStartBlksize, "reverse" .= testStartReverse, "duration" .= testStartDuration, "num_streams" .= testStartNumStreams, "bytes" .= testStartBytes]
  toEncoding (TestStart {..}) = pairs  ("blocks" .= testStartBlocks<>"tos" .= testStartTos<>"protocol" .= testStartProtocol<>"omit" .= testStartOmit<>"blksize" .= testStartBlksize<>"reverse" .= testStartReverse<>"duration" .= testStartDuration<>"num_streams" .= testStartNumStreams<>"bytes" .= testStartBytes)


data Start = Start { 
    startCookie :: Text,
    startSystemInfo :: Text,
    startConnected :: [ConnectedElt],
    startSockBufsize :: Double,
    startConnectingTo :: ConnectingTo,
    startTcpMssDefault :: Double,
    startRcvbufActual :: Double,
    startVersion :: Text,
    startTimestamp :: Timestamp,
    startTestStart :: TestStart,
    startSndbufActual :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Start where
  parseJSON (Object v) = Start <$> v .:  "cookie" <*> v .:  "system_info" <*> v .:  "connected" <*> v .:  "sock_bufsize" <*> v .:  "connecting_to" <*> v .:  "tcp_mss_default" <*> v .:  "rcvbuf_actual" <*> v .:  "version" <*> v .:  "timestamp" <*> v .:  "test_start" <*> v .:  "sndbuf_actual"
  parseJSON _          = mzero


instance ToJSON Start where
  toJSON     (Start {..}) = object ["cookie" .= startCookie, "system_info" .= startSystemInfo, "connected" .= startConnected, "sock_bufsize" .= startSockBufsize, "connecting_to" .= startConnectingTo, "tcp_mss_default" .= startTcpMssDefault, "rcvbuf_actual" .= startRcvbufActual, "version" .= startVersion, "timestamp" .= startTimestamp, "test_start" .= startTestStart, "sndbuf_actual" .= startSndbufActual]
  toEncoding (Start {..}) = pairs  ("cookie" .= startCookie<>"system_info" .= startSystemInfo<>"connected" .= startConnected<>"sock_bufsize" .= startSockBufsize<>"connecting_to" .= startConnectingTo<>"tcp_mss_default" .= startTcpMssDefault<>"rcvbuf_actual" .= startRcvbufActual<>"version" .= startVersion<>"timestamp" .= startTimestamp<>"test_start" .= startTestStart<>"sndbuf_actual" .= startSndbufActual)


data Sender = Sender { 
    senderMinRtt :: Double,
    senderSocket :: Double,
    senderStart :: Double,
    senderSender :: Bool,
    senderBitsPerSecond :: Double,
    senderMaxSndCwnd :: Double,
    senderRetransmits :: Double,
    senderEnd :: Double,
    senderMeanRtt :: Double,
    senderSeconds :: Double,
    senderMaxRtt :: Double,
    senderBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Sender where
  parseJSON (Object v) = Sender <$> v .:  "min_rtt" <*> v .:  "socket" <*> v .:  "start" <*> v .:  "sender" <*> v .:  "bits_per_second" <*> v .:  "max_snd_cwnd" <*> v .:  "retransmits" <*> v .:  "end" <*> v .:  "mean_rtt" <*> v .:  "seconds" <*> v .:  "max_rtt" <*> v .:  "bytes"
  parseJSON _          = mzero


instance ToJSON Sender where
  toJSON     (Sender {..}) = object ["min_rtt" .= senderMinRtt, "socket" .= senderSocket, "start" .= senderStart, "sender" .= senderSender, "bits_per_second" .= senderBitsPerSecond, "max_snd_cwnd" .= senderMaxSndCwnd, "retransmits" .= senderRetransmits, "end" .= senderEnd, "mean_rtt" .= senderMeanRtt, "seconds" .= senderSeconds, "max_rtt" .= senderMaxRtt, "bytes" .= senderBytes]
  toEncoding (Sender {..}) = pairs  ("min_rtt" .= senderMinRtt<>"socket" .= senderSocket<>"start" .= senderStart<>"sender" .= senderSender<>"bits_per_second" .= senderBitsPerSecond<>"max_snd_cwnd" .= senderMaxSndCwnd<>"retransmits" .= senderRetransmits<>"end" .= senderEnd<>"mean_rtt" .= senderMeanRtt<>"seconds" .= senderSeconds<>"max_rtt" .= senderMaxRtt<>"bytes" .= senderBytes)


data Receiver = Receiver { 
    receiverSocket :: Double,
    receiverStart :: Double,
    receiverSender :: Bool,
    receiverBitsPerSecond :: Double,
    receiverEnd :: Double,
    receiverSeconds :: Double,
    receiverBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Receiver where
  parseJSON (Object v) = Receiver <$> v .:  "socket" <*> v .:  "start" <*> v .:  "sender" <*> v .:  "bits_per_second" <*> v .:  "end" <*> v .:  "seconds" <*> v .:  "bytes"
  parseJSON _          = mzero


instance ToJSON Receiver where
  toJSON     (Receiver {..}) = object ["socket" .= receiverSocket, "start" .= receiverStart, "sender" .= receiverSender, "bits_per_second" .= receiverBitsPerSecond, "end" .= receiverEnd, "seconds" .= receiverSeconds, "bytes" .= receiverBytes]
  toEncoding (Receiver {..}) = pairs  ("socket" .= receiverSocket<>"start" .= receiverStart<>"sender" .= receiverSender<>"bits_per_second" .= receiverBitsPerSecond<>"end" .= receiverEnd<>"seconds" .= receiverSeconds<>"bytes" .= receiverBytes)


data StreamsElt = StreamsElt { 
    streamsEltPmtu :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltOmitted :: (Maybe (Bool:|:[(Maybe Value)])),
    streamsEltSocket :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltStart :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltSender :: Bool:|:Sender:|:[(Maybe Value)],
    streamsEltBitsPerSecond :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltRetransmits :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltReceiver :: (Maybe (Receiver:|:[(Maybe Value)])),
    streamsEltRttvar :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltEnd :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltRtt :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltSndCwnd :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltSeconds :: (Maybe (Double:|:[(Maybe Value)])),
    streamsEltBytes :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON StreamsElt where
  parseJSON (Object v) = StreamsElt <$> v .:? "pmtu" <*> v .:? "omitted" <*> v .:? "socket" <*> v .:? "start" <*> v .:  "sender" <*> v .:? "bits_per_second" <*> v .:? "retransmits" <*> v .:? "receiver" <*> v .:? "rttvar" <*> v .:? "end" <*> v .:? "rtt" <*> v .:? "snd_cwnd" <*> v .:? "seconds" <*> v .:? "bytes"
  parseJSON _          = mzero


instance ToJSON StreamsElt where
  toJSON     (StreamsElt {..}) = object ["pmtu" .= streamsEltPmtu, "omitted" .= streamsEltOmitted, "socket" .= streamsEltSocket, "start" .= streamsEltStart, "sender" .= streamsEltSender, "bits_per_second" .= streamsEltBitsPerSecond, "retransmits" .= streamsEltRetransmits, "receiver" .= streamsEltReceiver, "rttvar" .= streamsEltRttvar, "end" .= streamsEltEnd, "rtt" .= streamsEltRtt, "snd_cwnd" .= streamsEltSndCwnd, "seconds" .= streamsEltSeconds, "bytes" .= streamsEltBytes]
  toEncoding (StreamsElt {..}) = pairs  ("pmtu" .= streamsEltPmtu<>"omitted" .= streamsEltOmitted<>"socket" .= streamsEltSocket<>"start" .= streamsEltStart<>"sender" .= streamsEltSender<>"bits_per_second" .= streamsEltBitsPerSecond<>"retransmits" .= streamsEltRetransmits<>"receiver" .= streamsEltReceiver<>"rttvar" .= streamsEltRttvar<>"end" .= streamsEltEnd<>"rtt" .= streamsEltRtt<>"snd_cwnd" .= streamsEltSndCwnd<>"seconds" .= streamsEltSeconds<>"bytes" .= streamsEltBytes)


data Sum = Sum { 
    sumOmitted :: Bool,
    sumStart :: Double,
    sumSender :: Bool,
    sumBitsPerSecond :: Double,
    sumRetransmits :: Double,
    sumEnd :: Double,
    sumSeconds :: Double,
    sumBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Sum where
  parseJSON (Object v) = Sum <$> v .:  "omitted" <*> v .:  "start" <*> v .:  "sender" <*> v .:  "bits_per_second" <*> v .:  "retransmits" <*> v .:  "end" <*> v .:  "seconds" <*> v .:  "bytes"
  parseJSON _          = mzero


instance ToJSON Sum where
  toJSON     (Sum {..}) = object ["omitted" .= sumOmitted, "start" .= sumStart, "sender" .= sumSender, "bits_per_second" .= sumBitsPerSecond, "retransmits" .= sumRetransmits, "end" .= sumEnd, "seconds" .= sumSeconds, "bytes" .= sumBytes]
  toEncoding (Sum {..}) = pairs  ("omitted" .= sumOmitted<>"start" .= sumStart<>"sender" .= sumSender<>"bits_per_second" .= sumBitsPerSecond<>"retransmits" .= sumRetransmits<>"end" .= sumEnd<>"seconds" .= sumSeconds<>"bytes" .= sumBytes)


data DoubleervalsElt = DoubleervalsElt { 
    intervalsEltStreams :: [StreamsElt],
    intervalsEltSum :: Sum
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON DoubleervalsElt where
  parseJSON (Object v) = DoubleervalsElt <$> v .:  "streams" <*> v .:  "sum"
  parseJSON _          = mzero


instance ToJSON DoubleervalsElt where
  toJSON     (DoubleervalsElt {..}) = object ["streams" .= intervalsEltStreams, "sum" .= intervalsEltSum]
  toEncoding (DoubleervalsElt {..}) = pairs  ("streams" .= intervalsEltStreams<>"sum" .= intervalsEltSum)


data SumSent = SumSent { 
    sumSentStart :: Double,
    sumSentSender :: Bool,
    sumSentBitsPerSecond :: Double,
    sumSentRetransmits :: Double,
    sumSentEnd :: Double,
    sumSentSeconds :: Double,
    sumSentBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SumSent where
  parseJSON (Object v) = SumSent <$> v .:  "start" <*> v .:  "sender" <*> v .:  "bits_per_second" <*> v .:  "retransmits" <*> v .:  "end" <*> v .:  "seconds" <*> v .:  "bytes"
  parseJSON _          = mzero


instance ToJSON SumSent where
  toJSON     (SumSent {..}) = object ["start" .= sumSentStart, "sender" .= sumSentSender, "bits_per_second" .= sumSentBitsPerSecond, "retransmits" .= sumSentRetransmits, "end" .= sumSentEnd, "seconds" .= sumSentSeconds, "bytes" .= sumSentBytes]
  toEncoding (SumSent {..}) = pairs  ("start" .= sumSentStart<>"sender" .= sumSentSender<>"bits_per_second" .= sumSentBitsPerSecond<>"retransmits" .= sumSentRetransmits<>"end" .= sumSentEnd<>"seconds" .= sumSentSeconds<>"bytes" .= sumSentBytes)


data CpuUtilizationPercent = CpuUtilizationPercent { 
    cpuUtilizationPercentHostSystem :: Double,
    cpuUtilizationPercentHostTotal :: Double,
    cpuUtilizationPercentRemoteSystem :: Double,
    cpuUtilizationPercentRemoteUser :: Double,
    cpuUtilizationPercentHostUser :: Double,
    cpuUtilizationPercentRemoteTotal :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON CpuUtilizationPercent where
  parseJSON (Object v) = CpuUtilizationPercent <$> v .:  "host_system" <*> v .:  "host_total" <*> v .:  "remote_system" <*> v .:  "remote_user" <*> v .:  "host_user" <*> v .:  "remote_total"
  parseJSON _          = mzero


instance ToJSON CpuUtilizationPercent where
  toJSON     (CpuUtilizationPercent {..}) = object ["host_system" .= cpuUtilizationPercentHostSystem, "host_total" .= cpuUtilizationPercentHostTotal, "remote_system" .= cpuUtilizationPercentRemoteSystem, "remote_user" .= cpuUtilizationPercentRemoteUser, "host_user" .= cpuUtilizationPercentHostUser, "remote_total" .= cpuUtilizationPercentRemoteTotal]
  toEncoding (CpuUtilizationPercent {..}) = pairs  ("host_system" .= cpuUtilizationPercentHostSystem<>"host_total" .= cpuUtilizationPercentHostTotal<>"remote_system" .= cpuUtilizationPercentRemoteSystem<>"remote_user" .= cpuUtilizationPercentRemoteUser<>"host_user" .= cpuUtilizationPercentHostUser<>"remote_total" .= cpuUtilizationPercentRemoteTotal)


data SumReceived = SumReceived { 
    sumReceivedStart :: Double,
    sumReceivedSender :: Bool,
    sumReceivedBitsPerSecond :: Double,
    sumReceivedEnd :: Double,
    sumReceivedSeconds :: Double,
    sumReceivedBytes :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SumReceived where
  parseJSON (Object v) = SumReceived <$> v .:  "start" <*> v .:  "sender" <*> v .:  "bits_per_second" <*> v .:  "end" <*> v .:  "seconds" <*> v .:  "bytes"
  parseJSON _          = mzero


instance ToJSON SumReceived where
  toJSON     (SumReceived {..}) = object ["start" .= sumReceivedStart, "sender" .= sumReceivedSender, "bits_per_second" .= sumReceivedBitsPerSecond, "end" .= sumReceivedEnd, "seconds" .= sumReceivedSeconds, "bytes" .= sumReceivedBytes]
  toEncoding (SumReceived {..}) = pairs  ("start" .= sumReceivedStart<>"sender" .= sumReceivedSender<>"bits_per_second" .= sumReceivedBitsPerSecond<>"end" .= sumReceivedEnd<>"seconds" .= sumReceivedSeconds<>"bytes" .= sumReceivedBytes)


data End = End { 
    endSumSent :: SumSent,
    endCpuUtilizationPercent :: CpuUtilizationPercent,
    endStreams :: [StreamsElt],
    endSenderTcpCongestion :: Text,
    endSumReceived :: SumReceived
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON End where
  parseJSON (Object v) = End <$> v .:  "sum_sent" <*> v .:  "cpu_utilization_percent" <*> v .:  "streams" <*> v .:  "sender_tcp_congestion" <*> v .:  "sum_received"
  parseJSON _          = mzero


instance ToJSON End where
  toJSON     (End {..}) = object ["sum_sent" .= endSumSent, "cpu_utilization_percent" .= endCpuUtilizationPercent, "streams" .= endStreams, "sender_tcp_congestion" .= endSenderTcpCongestion, "sum_received" .= endSumReceived]
  toEncoding (End {..}) = pairs  ("sum_sent" .= endSumSent<>"cpu_utilization_percent" .= endCpuUtilizationPercent<>"streams" .= endStreams<>"sender_tcp_congestion" .= endSenderTcpCongestion<>"sum_received" .= endSumReceived)


data TopLevel = TopLevel { 
    topLevelStart :: Start,
    topLevelDoubleervals :: [DoubleervalsElt],
    topLevelEnd :: End
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "start" <*> v .:  "intervals" <*> v .:  "end"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["start" .= topLevelStart, "intervals" .= topLevelDoubleervals, "end" .= topLevelEnd]
  toEncoding (TopLevel {..}) = pairs  ("start" .= topLevelStart<>"intervals" .= topLevelDoubleervals<>"end" .= topLevelEnd)




parse :: FilePath -> IO TopLevel
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left  err -> fatal $ case (eitherDecode input :: Either String Value) of
                           Left  err' -> "Invalid JSON file: " ++ filename ++ "\n" ++ err'
                           Right _   -> "Mismatched JSON value from file: " ++ filename
                                     ++ "\n" ++ err
      Right r   -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
