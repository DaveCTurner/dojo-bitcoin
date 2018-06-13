{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import           Data.Bits
import           Data.ByteArray
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base16    as B16
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time
import           Data.Word
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status 
import           Test.Hspec

data Block = Block
  { blockIndex        :: !Int
  , blockMinedBy      :: !B.ByteString
  , blockData         :: !B.ByteString
  , blockPreviousHash :: !B.ByteString
  , blockNonce        :: !Int
  } deriving (Show, Eq)

hashFromBlock :: Block -> B.ByteString
hashFromBlock Block{..} = B.map upper $ B16.encode $ convert h
  where
  h :: Digest SHA256
  h = hash $ B.intercalate " "
    [ T.encodeUtf8 $ T.pack $ show blockIndex
    , blockMinedBy
    , blockData
    , blockPreviousHash
    , T.encodeUtf8 $ T.pack $ show  blockNonce
    ]

  upper w = if w > 64 then w .&. 0x5f else w

mineBlock :: Block -> Block
mineBlock b
  | "0000" `B.isPrefixOf` hashFromBlock b = b 
  | otherwise                             = mineBlock b { blockNonce = blockNonce b + 1 }

readBlock :: B.ByteString -> Block
readBlock s = Block{..}
  where
    (blockIndexBytes:blockMinedBy:blockDataEtc) = B.split 0x20 s
    (_:blockNonceBytes:blockPreviousHash:blockDataWordsRev) = reverse blockDataEtc
    blockData = B.intercalate (B.singleton 0x20) $ reverse blockDataWordsRev

    blockIndex = read $ T.unpack $ T.decodeUtf8 blockIndexBytes
    blockNonce = read $ T.unpack $ T.decodeUtf8 blockNonceBytes

instance ToJSON Block where
  toJSON b@Block{..} = object
    [ "index"        .= show blockIndex
    , "minedBy"      .= T.decodeUtf8 blockMinedBy
    , "data"         .= T.decodeUtf8 blockData
    , "previousHash" .= T.decodeUtf8 blockPreviousHash
    , "nonce"        .= show blockNonce
    , "hash"         .= T.decodeUtf8 (hashFromBlock b)
    ]

main :: IO ()
main = do
  manager  <- newManager tlsManagerSettings
  request  <- parseRequest "https://yorkshire-brassbitcoin.azurewebsites.net/api/blocks?latest=true"
  response <- httpLbs request {requestHeaders = [(hAccept, "text/plain")]} manager
  let lastBlock = readBlock $ BL.toStrict $ responseBody response
  putStrLn $ "Found last block: " ++ show lastBlock

  loop manager lastBlock

  where
  loop manager lastBlock = do
    transactionData <- B.readFile "transaction.txt"
    miningStart <- getCurrentTime
    let nextBlock = mineBlock $ Block
          { blockIndex = blockIndex lastBlock + 1
          , blockMinedBy = "DavidT"
          , blockData = transactionData
          , blockPreviousHash = hashFromBlock lastBlock
          , blockNonce = 1
          }
    putStrLn $ "Found next block: " ++ show nextBlock
    miningEnd <- getCurrentTime
    putStrLn $ "took: " ++ show (diffUTCTime miningEnd miningStart)

    uploadRequestBase <- parseRequest "http://yorkshire-brassbitcoin.azurewebsites.net/Blocks/Upload"
    let uploadRequest = uploadRequestBase
          { method = "POST"
          , requestHeaders =
            [ (hContentType, "application/json")
            ]
          , requestBody = RequestBodyLBS $ encode nextBlock
          }
    uploadResponse <- httpLbs uploadRequest manager
    when (statusCode (responseStatus uploadResponse) == 201) $ loop manager nextBlock

test :: IO ()
test = hspec $ do
    describe "genesis" $ do
      it "validates the genesis block"
        $ hashFromBlock Block
            { blockIndex = 0
            , blockMinedBy = "Genesis"
            , blockData = "Genesis"
            , blockPreviousHash = "0"
            , blockNonce = 52458
            }
            `shouldBe` "000021C1766F55BD5D413F0AC128A5D3D6B50E4F0D608B653209C4D468232C11"
      it "mines another block"
        $ mineBlock Block
            { blockIndex = 3
            , blockMinedBy = "David"
            , blockData = "Data-here"
            , blockPreviousHash = "000021C1766F55BD5D413F0AC128A5D3D6B50E4F0D608B653209C4D468232C11"
            , blockNonce = 0
            }
            `shouldBe` Block
            { blockIndex = 3
            , blockMinedBy = "David"
            , blockData = "Data-here"
            , blockPreviousHash = "000021C1766F55BD5D413F0AC128A5D3D6B50E4F0D608B653209C4D468232C11"
            , blockNonce = 16968
            }
      it "reads a block"
        $ readBlock "3 Adam Hello 00009127D6E3F7757E68A8C7D470CA3D7CA669C422C578BF9037D42F5C81B769 98300 0000D1EF7C103EA81F8191A677F65C2509A743EFBCF64B5894513FBBA4B87C8B"
            `shouldBe` Block
              { blockIndex = 3
              , blockMinedBy = "Adam"
              , blockData = "Hello"
              , blockPreviousHash = "00009127D6E3F7757E68A8C7D470CA3D7CA669C422C578BF9037D42F5C81B769"
              , blockNonce = 98300
              }

      it "reads a block with spaces"
        $ readBlock "6 Kev Hello York Code Dojo 000098EC76D1CA1A06598959A9338B6888106E7D6FF30539B4A1E35540A967FD 68893 00009D181C69486B9AA2332C306C792A3430026F7750007EC056A09AF4B3B8F4"
            `shouldBe` Block
              { blockIndex = 6
              , blockMinedBy = "Kev"
              , blockData = "Hello York Code Dojo"
              , blockPreviousHash = "000098EC76D1CA1A06598959A9338B6888106E7D6FF30539B4A1E35540A967FD"
              , blockNonce = 68893
              }


