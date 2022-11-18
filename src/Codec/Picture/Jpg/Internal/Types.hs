{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Codec.Picture.Jpg.Internal.Types( MutableMacroBlock
                              , createEmptyMutableMacroBlock
                              , printMacroBlock
                              , printPureMacroBlock
                              , DcCoefficient
                              , JpgImage( .. )
                              , JpgComponent( .. )
                              , JpgFrameHeader( .. )
                              , JpgFrame( .. )
                              , JpgFrameKind( .. )
                              , JpgScanHeader( .. )
                              , JpgQuantTableSpec( .. )
                              , JpgHuffmanTableSpec( .. )
                              , JpgImageKind( .. )
                              , JpgScanSpecification( .. )
                              , JpgColorSpace( .. )
                              , AdobeTransform( .. )
                              , JpgAdobeApp14( .. )
                              , JpgJFIFApp0( .. )
                              , JFifUnit( .. )
                              , TableList( .. )
                              , RestartInterval( .. )
                              , calculateSize
                              , dctBlockSize
                              , skipUntilFrames
                              , parseNextFrameLazy
                              , parseFrames
                              , parseToFirstFrameHeader
                              ) where


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure, (<*>), (<$>) )
#endif

import Control.DeepSeq( NFData(..) )
import Control.Monad( when, replicateM, forM, forM_, unless )
import Control.Monad.ST( ST )
import Data.Bits( (.|.), (.&.), unsafeShiftL, unsafeShiftR )
import Data.List( partition )
import GHC.Generics( Generic )

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid( (<>) )
#endif

import Foreign.Storable ( Storable )
import Data.Vector.Unboxed( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Int( Int16 )
import Data.Word(Word8, Word16 )
import Data.Binary( Binary(..) )

import Data.Binary.Get( Get
                      , getWord8
                      , getWord16be
                      , getByteString
                      , skip
                      , bytesRead
                      , lookAhead
                      )

import Data.Binary.Put( Put
                      , putWord8
                      , putWord16be
                      , putLazyByteString
                      , putByteString
                      , runPut
                      )

import Codec.Picture.InternalHelper
import Codec.Picture.Jpg.Internal.DefaultTable
import Codec.Picture.Tiff.Internal.Types
import Codec.Picture.Tiff.Internal.Metadata( exifOffsetIfd )
import Codec.Picture.Metadata.Exif

import Debug.Trace
import Text.Printf

-- | Type only used to make clear what kind of integer we are carrying
-- Might be transformed into newtype in the future
type DcCoefficient = Int16

-- | Macroblock that can be transformed.
type MutableMacroBlock s a = M.STVector s a

data JpgFrameKind =
      JpgBaselineDCTHuffman
    | JpgExtendedSequentialDCTHuffman
    | JpgProgressiveDCTHuffman
    | JpgLosslessHuffman
    | JpgDifferentialSequentialDCTHuffman
    | JpgDifferentialProgressiveDCTHuffman
    | JpgDifferentialLosslessHuffman
    | JpgExtendedSequentialArithmetic
    | JpgProgressiveDCTArithmetic
    | JpgLosslessArithmetic
    | JpgDifferentialSequentialDCTArithmetic
    | JpgDifferentialProgressiveDCTArithmetic
    | JpgDifferentialLosslessArithmetic
    | JpgQuantizationTable
    | JpgHuffmanTableMarker
    | JpgStartOfScan
    | JpgEndOfImage
    | JpgAppSegment Word8
    | JpgExtensionSegment Word8

    | JpgRestartInterval
    | JpgRestartIntervalEnd Word8
    deriving (Eq, Show, Generic)
instance NFData JpgFrameKind

data JpgFrame =
      JpgAppFrame        !Word8 B.ByteString
    | JpgAdobeAPP14      !JpgAdobeApp14
    | JpgJFIF            !JpgJFIFApp0
    | JpgExif            ![ImageFileDirectory]
    | JpgExtension       !Word8 B.ByteString
    | JpgQuantTable      ![JpgQuantTableSpec]
    | JpgHuffmanTable    ![(JpgHuffmanTableSpec, HuffmanPackedTree)]
    | JpgScanBlob        !JpgScanHeader !L.ByteString
    | JpgScans           !JpgFrameKind !JpgFrameHeader
    | JpgIntervalRestart !Word16
    deriving (Eq, Show, Generic)
instance NFData JpgFrame

data JpgColorSpace
  = JpgColorSpaceYCbCr
  | JpgColorSpaceYCC
  | JpgColorSpaceY
  | JpgColorSpaceYA
  | JpgColorSpaceYCCA
  | JpgColorSpaceYCCK
  | JpgColorSpaceCMYK
  | JpgColorSpaceRGB
  | JpgColorSpaceRGBA
  deriving (Eq, Show, Generic)
instance NFData JpgColorSpace

data AdobeTransform
  = AdobeUnknown    -- ^ Value 0
  | AdobeYCbCr      -- ^ value 1
  | AdobeYCck       -- ^ value 2
  deriving (Eq, Show, Generic)
instance NFData AdobeTransform

data JpgAdobeApp14 = JpgAdobeApp14
  { _adobeDctVersion :: !Word16
  , _adobeFlag0      :: !Word16
  , _adobeFlag1      :: !Word16
  , _adobeTransform  :: !AdobeTransform
  }
  deriving (Eq, Show, Generic)
instance NFData JpgAdobeApp14

-- | Size: 1
data JFifUnit
  = JFifUnitUnknown   -- ^ 0
  | JFifPixelsPerInch -- ^ 1
  | JFifPixelsPerCentimeter -- ^ 2
  deriving (Eq, Show, Generic)
instance NFData JFifUnit

instance Binary JFifUnit where
  put v = putWord8 $ case v of
    JFifUnitUnknown -> 0
    JFifPixelsPerInch -> 1
    JFifPixelsPerCentimeter -> 2
  get = do
    v <- getWord8
    pure $ case v of
      0 -> JFifUnitUnknown
      1 -> JFifPixelsPerInch
      2 -> JFifPixelsPerCentimeter
      _ -> JFifUnitUnknown

data JpgJFIFApp0 = JpgJFIFApp0
  { _jfifUnit      :: !JFifUnit
  , _jfifDpiX      :: !Word16
  , _jfifDpiY      :: !Word16
  , _jfifThumbnail :: !(Maybe {- (Image PixelRGB8) -} Int)
  }
  deriving (Eq, Show, Generic)
instance NFData JpgJFIFApp0

instance Binary JpgJFIFApp0 where
  get = do
    sig <- getByteString 5
    when (sig /= BC.pack "JFIF\0") $
        fail "Invalid JFIF signature"
    major <- getWord8
    minor <- getWord8
    when (major /= 1 && minor > 2) $
        fail "Unrecognize JFIF version"
    unit <- get
    dpiX <- getWord16be
    dpiY <- getWord16be
    w <- getWord8
    h <- getWord8
    let pxCount = 3 * w * h
    img <- case pxCount of
      0 -> return Nothing
      _ -> return Nothing
    return $ JpgJFIFApp0
        { _jfifUnit      = unit
        , _jfifDpiX      = dpiX
        , _jfifDpiY      = dpiY
        , _jfifThumbnail = img
        }


  put jfif = do
    putByteString $ BC.pack "JFIF\0" -- 5
    putWord8 1                       -- 1 6
    putWord8 2                       -- 1 7
    put $ _jfifUnit jfif             -- 1 8
    putWord16be $ _jfifDpiX jfif     -- 2 10
    putWord16be $ _jfifDpiY jfif     -- 2 12
    putWord8 0                       -- 1 13
    putWord8 0                       -- 1 14

{-Thumbnail width (tw) 	1 	Horizontal size of embedded JFIF thumbnail in pixels-}
{-Thumbnail height (th) 	1 	Vertical size of embedded JFIF thumbnail in pixels-}
{-Thumbnail data 	3 × tw × th 	Uncompressed 24 bit RGB raster thumbnail-}

instance Binary AdobeTransform where
  put v = case v of
    AdobeUnknown -> putWord8 0
    AdobeYCbCr -> putWord8 1
    AdobeYCck -> putWord8 2

  get = do
    v <- getWord8
    pure $ case v of
      0 -> AdobeUnknown
      1 -> AdobeYCbCr
      2 -> AdobeYCck
      _ -> AdobeUnknown

instance Binary JpgAdobeApp14 where
  get = do
    let sig = BC.pack "Adobe"
    fileSig <- getByteString 5
    when (fileSig /= sig) $
       fail "Invalid Adobe APP14 marker"
    version <- getWord16be
    when (version /= 100) $
       fail $ "Invalid Adobe APP14 version " ++ show version
    JpgAdobeApp14 version
                  <$> getWord16be
                  <*> getWord16be <*> get

  put (JpgAdobeApp14 v f0 f1 t) = do
    putByteString $ BC.pack "Adobe"
    putWord16be v
    putWord16be f0
    putWord16be f1
    put t


data JpgFrameHeader = JpgFrameHeader
    { jpgFrameHeaderLength   :: !Word16
    , jpgSamplePrecision     :: !Word8
    , jpgHeight              :: !Word16
    , jpgWidth               :: !Word16
    , jpgImageComponentCount :: !Word8
    , jpgComponents          :: ![JpgComponent]
    }
    deriving (Eq, Show, Generic)
instance NFData JpgFrameHeader


instance SizeCalculable JpgFrameHeader where
    calculateSize hdr = 2 + 1 + 2 + 2 + 1
                      + sum [calculateSize c | c <- jpgComponents hdr]

data JpgComponent = JpgComponent
    { componentIdentifier       :: !Word8
      -- | Stored with 4 bits
    , horizontalSamplingFactor  :: !Word8
      -- | Stored with 4 bits
    , verticalSamplingFactor    :: !Word8
    , quantizationTableDest     :: !Word8
    }
    deriving (Eq, Show, Generic)
instance NFData JpgComponent

instance SizeCalculable JpgComponent where
    calculateSize _ = 3

data JpgImage = JpgImage { jpgFrame :: [JpgFrame] }
    deriving (Eq, Show, Generic)
instance NFData JpgImage

data JpgScanSpecification = JpgScanSpecification
    { componentSelector :: !Word8
      -- | Encoded as 4 bits
    , dcEntropyCodingTable :: !Word8
      -- | Encoded as 4 bits
    , acEntropyCodingTable :: !Word8

    }
    deriving (Eq, Show, Generic)
instance NFData JpgScanSpecification

instance SizeCalculable JpgScanSpecification where
    calculateSize _ = 2

data JpgScanHeader = JpgScanHeader
    { scanLength :: !Word16
    , scanComponentCount :: !Word8
    , scans :: [JpgScanSpecification]

      -- | (begin, end)
    , spectralSelection    :: (Word8, Word8)

      -- | Encoded as 4 bits
    , successiveApproxHigh :: !Word8

      -- | Encoded as 4 bits
    , successiveApproxLow :: !Word8
    }
    deriving (Eq, Show, Generic)
instance NFData JpgScanHeader

instance SizeCalculable JpgScanHeader where
    calculateSize hdr = 2 + 1
                      + sum [calculateSize c | c <- scans hdr]
                      + 2
                      + 1

data JpgQuantTableSpec = JpgQuantTableSpec
    { -- | Stored on 4 bits
      quantPrecision     :: !Word8

      -- | Stored on 4 bits
    , quantDestination   :: !Word8

    , quantTable         :: MacroBlock Int16
    }
    deriving (Eq, Show, Generic)
instance NFData JpgQuantTableSpec

class SizeCalculable a where
    calculateSize :: a -> Int

-- | Type introduced only to avoid some typeclass overlapping
-- problem
newtype TableList a = TableList [a]

instance (SizeCalculable a, Binary a) => Binary (TableList a) where
    put (TableList lst) = do
        putWord16be . fromIntegral $ sum [calculateSize table | table <- lst] + 2
        mapM_ put lst

    get = TableList <$> (getWord16be >>= \s -> innerParse (fromIntegral s - 2))
      where innerParse :: Int -> Get [a]
            innerParse 0    = return []
            innerParse size = do
                onStart <- fromIntegral <$> bytesRead
                table <- get
                onEnd <- fromIntegral <$> bytesRead
                (table :) <$> innerParse (size - (onEnd - onStart))

instance SizeCalculable JpgQuantTableSpec where
    calculateSize table =
        1 + (fromIntegral (quantPrecision table) + 1) * 64

instance Binary JpgQuantTableSpec where
    put table = do
        let precision = quantPrecision table
        put4BitsOfEach precision (quantDestination table)
        forM_ (VS.toList $ quantTable table) $ \coeff ->
            if precision == 0 then putWord8 $ fromIntegral coeff
                             else putWord16be $ fromIntegral coeff

    get = do
        (precision, dest) <- get4BitOfEach
        coeffs <- replicateM 64 $ if precision == 0
                then fromIntegral <$> getWord8
                else fromIntegral <$> getWord16be
        return JpgQuantTableSpec
            { quantPrecision = precision
            , quantDestination = dest
            , quantTable = VS.fromListN 64 coeffs
            }

data JpgHuffmanTableSpec = JpgHuffmanTableSpec
    { -- | 0 : DC, 1 : AC, stored on 4 bits
      huffmanTableClass       :: !DctComponent
      -- | Stored on 4 bits
    , huffmanTableDest        :: !Word8

    , huffSizes :: !(VU.Vector Word8)
    , huffCodes :: !(V.Vector (VU.Vector Word8))
    }
    deriving (Eq, Show, Generic)
instance NFData JpgHuffmanTableSpec

instance SizeCalculable JpgHuffmanTableSpec where
    calculateSize table = 1 + 16 + sum [fromIntegral e | e <- VU.toList $ huffSizes table]

instance Binary JpgHuffmanTableSpec where
    put table = do
        let classVal = if huffmanTableClass table == DcComponent
                          then 0 else 1
        put4BitsOfEach classVal $ huffmanTableDest table
        mapM_ put . VU.toList $ huffSizes table
        forM_ [0 .. 15] $ \i ->
            when (huffSizes table ! i /= 0)
                 (let elements = VU.toList $ huffCodes table V.! i
                  in mapM_ put elements)

    get = do
        (huffClass, huffDest) <- get4BitOfEach
        sizes <- replicateM 16 getWord8
        codes <- forM sizes $ \s ->
            VU.replicateM (fromIntegral s) getWord8
        return JpgHuffmanTableSpec
            { huffmanTableClass =
                if huffClass == 0 then DcComponent else AcComponent
            , huffmanTableDest = huffDest
            , huffSizes = VU.fromListN 16 sizes
            , huffCodes = V.fromListN 16 codes
            }

instance Binary JpgImage where
    put (JpgImage { jpgFrame = frames }) =
        putWord8 0xFF >> putWord8 0xD8 >> mapM_ putFrame frames
            >> putWord8 0xFF >> putWord8 0xD9

    get = do
        skipUntilFrames
        frames <- parseFrames
        -- let endOfImageMarker = 0xD9
        {-checkMarker commonMarkerFirstByte endOfImageMarker-}
        return JpgImage { jpgFrame = frames }

skipUntilFrames :: Get ()
skipUntilFrames = do
    let startOfImageMarker = 0xD8
    checkMarker commonMarkerFirstByte startOfImageMarker
    eatUntilCode

eatUntilCode :: Get ()
eatUntilCode = do
    code <- getWord8
    unless (code == 0xFF) eatUntilCode

takeCurrentFrame :: Get B.ByteString
takeCurrentFrame = do
    size <- getWord16be
    getByteString (fromIntegral size - 2)

putFrame :: JpgFrame -> Put
putFrame (JpgAdobeAPP14 adobe) =
    put (JpgAppSegment 14) >> putWord16be 14 >> put adobe
putFrame (JpgJFIF jfif) =
    put (JpgAppSegment 0) >> putWord16be (14+2) >> put jfif
putFrame (JpgExif exif) = putExif exif
putFrame (JpgAppFrame appCode str) =
    put (JpgAppSegment appCode) >> putWord16be (fromIntegral $ B.length str) >> put str
putFrame (JpgExtension appCode str) =
    put (JpgExtensionSegment appCode) >> putWord16be (fromIntegral $ B.length str) >> put str
putFrame (JpgQuantTable tables) =
    put JpgQuantizationTable >> put (TableList tables)
putFrame (JpgHuffmanTable tables) =
    put JpgHuffmanTableMarker >> put (TableList $ map fst tables)
putFrame (JpgIntervalRestart size) =
    put JpgRestartInterval >> put (RestartInterval size)
putFrame (JpgScanBlob hdr blob) =
    put JpgStartOfScan >> put hdr >> putLazyByteString blob
putFrame (JpgScans kind hdr) =
    put kind >> put hdr

--------------------------------------------------
----            Serialization instances
--------------------------------------------------
commonMarkerFirstByte :: Word8
commonMarkerFirstByte = 0xFF

checkMarker :: Word8 -> Word8 -> Get ()
checkMarker b1 b2 = do
    rb1 <- getWord8
    rb2 <- getWord8
    when (rb1 /= b1 || rb2 /= b2)
         (fail "Invalid marker used")

-- | Splits a Scan's compressed image data from the given ByteString,
-- returning the compressed image data and the rest of the data.
-- That the given ByteString should be the part that immediately follows an SOS tag.
--
-- As described on e.g. https://www.ccoderun.ca/programming/2017-01-31_jpeg/,
--
-- > To find the next segment after the SOS, you must keep reading until you
-- > find a 0xFF bytes which is not immediately followed by 0x00 (see "byte stuffing").
-- > Normally, this will be the EOI segment that comes at the end of the file.
--
-- where the 0xFF is the next segment's marker.
--
-- This function returns the compressed image data, not including that next segment's
-- marker on its trailing end.
extractScanContent :: L.ByteString -> (L.ByteString, L.ByteString)
extractScanContent str = aux 0
  where maxi = fromIntegral $ L.length str - 1

        aux n | n >= maxi = (str, L.empty)
              | v == 0xFF && vNext /= 0 && not isReset = L.splitAt n str
               | otherwise = aux (n + 1)
             where v = str `L.index` n
                   vNext = str `L.index` (n + 1)
                   isReset = 0xD0 <= vNext && vNext <= 0xD7

parseAdobe14 :: B.ByteString -> Maybe JpgFrame
parseAdobe14 str = case runGetStrict get str of
    Left _err -> Nothing
    Right app14 -> Just $! JpgAdobeAPP14 app14

-- | Parse JFIF or JFXX information. Right now only JFIF.
parseJF__ :: B.ByteString -> Maybe JpgFrame
parseJF__ str = case runGetStrict get str of
    Left _err -> Nothing
    Right jfif -> Just $! JpgJFIF jfif

parseExif :: B.ByteString -> Maybe JpgFrame
parseExif str
  | exifHeader `B.isPrefixOf` str =
      let
        tiff = B.drop (B.length exifHeader) str
      in
        case runGetStrict (getP tiff) tiff of
            Left _err -> Nothing
            Right (_hdr :: TiffHeader, []) -> Nothing
            Right (_hdr :: TiffHeader, ifds : _) -> Just $! JpgExif ifds
  | otherwise = Nothing
  where
    exifHeader = BC.pack "Exif\0\0"

putExif :: [ImageFileDirectory] -> Put
putExif ifds = putAll where
  hdr = TiffHeader
    { hdrEndianness = EndianBig
    , hdrOffset = 8
    }

  ifdList = case partition (isInIFD0 . ifdIdentifier) ifds of
    (ifd0, []) -> [ifd0]
    (ifd0, ifdExif) -> [ifd0 <> pure exifOffsetIfd, ifdExif]

  exifBlob = runPut $ do
    putByteString $ BC.pack "Exif\0\0"
    putP BC.empty (hdr, ifdList)

  putAll = do
    put (JpgAppSegment 1)
    putWord16be . fromIntegral $ L.length exifBlob + 2
    putLazyByteString exifBlob

skipFrameMarker :: Get ()
skipFrameMarker = do
    word <- getWord8
    when (word /= 0xFF) $ do
        readedData <- bytesRead
        fail $ "Invalid Frame marker (" ++ show word
                ++ ", bytes read : " ++ show readedData ++ ")"

-- | Parse a single frame.
--
-- Returns `Nothing` when we encounter a frame we want to skip.
--
-- This function has various quirks; consider the below with great caution
-- when using this function.
--
-- While @data JpgFrame = ... | JpgScanBlob !...` itself has strict fields,
-- this function returns a lazy `Maybe` whose `Just` constructor can contain a thunk.
-- This allows `parseFrames` (which calls `parseNextFrameLazy`) to construct
-- a lazy @[JpgFrame]@ such that the expensive byte-by-byte traversal
-- in `extractScanContent` to create a `JpgScanBlob` can be avoided if only
-- list elements before that `JpgScanBlob` are evaluated.
--
-- That means the user can write code such as
--
-- > let mbFirstScan =
-- >       case runGetOrFail (get @JPG.JpgImage) hugeImageByteString of
-- >         Right (_restBs, _offset, res) ->
-- >           find (\frame -> case frame of { JPG.JpgScans{} -> True; _ -> False }) (JPG.jpgFrame res)
--
-- with the guarantee that only the first few bytes of the @hugeImageByteString@
-- will be inspected, assuming that indeed there is at least 1 `JpgScan` in front
-- of the `JpgScanBlob` that contains the huge amnount of compressed image data.
--
-- This guarantee can be useful to e.g. quickly read just the image
-- dimensions (width, height) without traversing the compressed image data.
--
-- Also note that this `Get` parser does not correctly maintain the parser byte offset
-- (`Data.Binary.Get.bytesRead`) once a `JpgScanBlob` is returned,
-- since it uses `Data.Binary.Get.getRemainingLazyBytes` to provide:
--
-- 1. the laziness described above, and
-- 2. the ability to ignore any parser failure after the first successfully-parsed
--    `JpgScanBlob` (it is debatable whether this behaviour is a nice behaviour of this
--    library, but it is historically so and existing exposed functions do not break
--    this for backwards compatibility with existing uses of this library).
--    This fact also means that even `parseNextFrameStrict` cannot maintain
--    correct parser byte offsets.
--
-- Another quirks with this logic is that this `Get` parser advances the
-- parser offset, except when `JpgScanBlob` is encountered; in that case it
-- advances the the parser offset only to right after the SOS (Start of Scan) tag.
-- This means that `parseNextFrameLazy` returns a `JpgScanBlob _ d`, the caller
-- must call @Data.Binary.Get.skip (Data.ByteString.Lazy.length d)@
-- to advance the parser state correctly before continuing to parse.
-- `parseNextFrameLazy` cannot call `skip` itself in this case, because
-- that would require determining @length d@ and thus @d@, which again
-- would require traversing the entire compressed image data.
--
-- The drawback of the lazy `Just` is obviously is that the thunk inside it
-- will keep the captured `ByteString` alive.
-- Thus, if you do not want that, use `parseNextFrameStrict` instead.
-- Further note that if you are reading a huge JPEG image from disk strictly,
-- this will already incur a full traversal (namely creation) of the `hugeImageByteString`.
-- Thus, `parseNextFrameLazy` only provides any benefit if you use read the
-- image from disk using lazy IO (not recommended!) or something similar,
-- such as creating the `hugeImageByteString` via @mmap()@.
-- For strict disk reads that shall terminate early as in the example shown above,
-- use `Data.Binary.Get`'s incremental input interface in combination with
-- just the right amount of `parseNextFrameStrict` calls for your needs.
parseNextFrameLazy :: Get (Maybe JpgFrame)
-- TODO This function is still bad because it calls `getRemainingLazyBytes` so it cannot be composed
--      with following `Get` functions. This makes its standalone existence rather pointless.
parseNextFrameLazy = do
    kind <- get
    case kind of
        JpgEndOfImage -> return Nothing
        JpgAppSegment 0 -> parseJF__ <$> takeCurrentFrame
        JpgAppSegment 1 -> parseExif <$> takeCurrentFrame
        JpgAppSegment 14 -> parseAdobe14 <$> takeCurrentFrame
        JpgAppSegment c -> Just . JpgAppFrame c <$> takeCurrentFrame
        JpgExtensionSegment c -> Just . JpgExtension c <$> takeCurrentFrame
        JpgQuantizationTable ->
            (\(TableList quants) -> Just $! JpgQuantTable quants) <$> get
        JpgRestartInterval ->
            (\(RestartInterval i) -> Just $! JpgIntervalRestart i) <$> get
        JpgHuffmanTableMarker ->
            (\(TableList huffTables) -> Just $!
                    JpgHuffmanTable [(t, packHuffmanTree . buildPackedHuffmanTree $ huffCodes t) | t <- huffTables])
                    <$> get
        JpgStartOfScan -> trace "\n-JpgStartOfScan-\n" $ do
            scanHeader <- trace "\n-JpgStartOfScan_get-\n" $ get
            -- ~(d, _other) <- trace "\n-JpgStartOfScan_extractScanContent-\n" $ extractScanContent <$> lookAhead getRemainingLazyBytes
            -- skip (fromIntegral $ L.length d)
            -- trace "\n-JpgStartOfScan_return-\n" $! return $ Just $ JpgScanBlob scanHeader d
            remainingBytes <- lookAhead getRemainingLazyBytes
            trace "\n-JpgStartOfScan_return-\n" $
              return $ Just $ -- critically no `$!` after `Just`, see function docs.
                let (d, _other) = extractScanContent remainingBytes in JpgScanBlob scanHeader d
        _ -> Just . JpgScans kind <$> get

-- | See `parseNextFrameLazy`.
parseNextFrameStrict :: Get (Maybe JpgFrame)
parseNextFrameStrict = do
    mbFrame <- parseNextFrameLazy
    case mbFrame of
        Just !_frame -> return mbFrame
        _ -> return mbFrame

parseFrames :: Get [JpgFrame]
parseFrames = do
    mbFrame <- trace "\n-parseNextFrameLazy-\n" $ parseNextFrameLazy
    case mbFrame of
        Nothing -> trace "\n-A1-\n" $ parseFollowingFrames -- skip frame
        Just frame -> trace "\n-A2-\n" $ case frame of
            -- After we've encountered the first scan blob containing encoded image data,
            -- we accept that anything else after that is allowed to fail and we'll ignore
            -- that failure.
            -- TODO: Explain why JuicyPixel chose to use this logic.
            JpgScanBlob _scanHeader d -> trace "\n-ScanBlob-\n" $ do
                -- Note that we do this funny thing of doing `runGet` inside a `Get` parser.
                -- This is because `binary` does not allow to run a parser and catch a `fail`,
                -- (Which is what the current logic below does, namely just discarding any failing
                -- parser after the first decoded scan blob.)
                -- Doing that is emulated by calling `extractScanContent` (which makes the current
                -- `Get` consume all the way to the end of the input), and running a new `Get`
                -- on `other`.
                -- Note this will make the error offset recorded by `fail` unhelpful because it
                -- will be relative to the start of `other`, not relative to the start of the JPG.

                -- TODO: The lines
                --       ```hs
                --       JpgScanBlob _scanHeader d -> do
                --           remainingBytes <- getRemainingLazyBytes
                --           ...
                --       ```
                --       cannot be written without making `parseFrames :: Get [JpgFrame]` non-lazy (and thus slow for my use case).
                --       This is becuase `JpgScanBlob` has strict fields.
                --       This means I cannot have `parseNextFrameLazy` be in its own factored-out function.
                --
                --       This shows nicely a case where StrictData actually makes some task harder:
                --       It is now harder to refactor code that uses constructors with strict fields,
                --       because it inserts hidden `seq`s into the control flow.

                remainingBytes <- getRemainingLazyBytes
                -- `parseNextFrameLazy` describes how it does not advance the parser state
                -- for `JpgScanBlob`s, so we need to do that manually.
                return $ frame : case runGet parseFrames (L.drop (L.length d + 1) remainingBytes) of -- TODO Why `drop 1` instead of `runGet parseFollowingFrames remainingBytes` that would check that the dropped 1 Byte is really a frame marker?
                    Left _ -> []
                    Right lst -> lst

            _ -> trace "\n-NotScanBlob-\n" $ (frame :) <$> parseFollowingFrames
  where
    parseFollowingFrames = do
        skipFrameMarker
        parseFrames

-- | Parses forward, returning the first scan header encountered.
parseToFirstFrameHeader :: Get (Maybe JpgFrameHeader)
parseToFirstFrameHeader = do
    mbFrame <- parseNextFrameStrict
    case mbFrame of
        Nothing -> continueSearching
        Just frame -> case frame of
            -- If we encounter a scan blob containing compressed data before
            -- encountering the frame header that tells its dimensions, fail the parser.
            JpgScanBlob{} -> fail "parseToFirstFrameHeader: Encountered scan data blob before frame header that tells its dimensions"
            JpgScans _ frameHeader -> return $ Just $! frameHeader
            _ -> continueSearching
  where
    continueSearching = do
        skipFrameMarker
        parseToFirstFrameHeader

buildPackedHuffmanTree :: V.Vector (VU.Vector Word8) -> HuffmanTree
buildPackedHuffmanTree = buildHuffmanTree . map VU.toList . V.toList

secondStartOfFrameByteOfKind :: JpgFrameKind -> Word8
secondStartOfFrameByteOfKind = aux
  where
    aux JpgBaselineDCTHuffman = 0xC0
    aux JpgExtendedSequentialDCTHuffman = 0xC1
    aux JpgProgressiveDCTHuffman = 0xC2
    aux JpgLosslessHuffman = 0xC3
    aux JpgDifferentialSequentialDCTHuffman = 0xC5
    aux JpgDifferentialProgressiveDCTHuffman = 0xC6
    aux JpgDifferentialLosslessHuffman = 0xC7
    aux JpgExtendedSequentialArithmetic = 0xC9
    aux JpgProgressiveDCTArithmetic = 0xCA
    aux JpgLosslessArithmetic = 0xCB
    aux JpgHuffmanTableMarker = 0xC4
    aux JpgDifferentialSequentialDCTArithmetic = 0xCD
    aux JpgDifferentialProgressiveDCTArithmetic = 0xCE
    aux JpgDifferentialLosslessArithmetic = 0xCF
    aux JpgEndOfImage = 0xD9
    aux JpgQuantizationTable = 0xDB
    aux JpgStartOfScan = 0xDA
    aux JpgRestartInterval = 0xDD
    aux (JpgRestartIntervalEnd v) = v
    aux (JpgAppSegment a) = (a + 0xE0)
    aux (JpgExtensionSegment a) = a

data JpgImageKind = BaseLineDCT | ProgressiveDCT

instance Binary JpgFrameKind where
    put v = putWord8 0xFF >> put (secondStartOfFrameByteOfKind v)
    get = do
        -- no lookahead :(
        {-word <- getWord8-}
        word2 <- getWord8
        case word2 of
            0xC0 -> return JpgBaselineDCTHuffman
            0xC1 -> return JpgExtendedSequentialDCTHuffman
            0xC2 -> return JpgProgressiveDCTHuffman
            0xC3 -> return JpgLosslessHuffman
            0xC4 -> return JpgHuffmanTableMarker
            0xC5 -> return JpgDifferentialSequentialDCTHuffman
            0xC6 -> return JpgDifferentialProgressiveDCTHuffman
            0xC7 -> return JpgDifferentialLosslessHuffman
            0xC9 -> return JpgExtendedSequentialArithmetic
            0xCA -> return JpgProgressiveDCTArithmetic
            0xCB -> return JpgLosslessArithmetic
            0xCD -> return JpgDifferentialSequentialDCTArithmetic
            0xCE -> return JpgDifferentialProgressiveDCTArithmetic
            0xCF -> return JpgDifferentialLosslessArithmetic
            0xD9 -> return JpgEndOfImage
            0xDA -> return JpgStartOfScan
            0xDB -> return JpgQuantizationTable
            0xDD -> return JpgRestartInterval
            a | a >= 0xF0 -> return $! JpgExtensionSegment a
              | a >= 0xE0 -> return $! JpgAppSegment (a - 0xE0)
              | a >= 0xD0 && a <= 0xD7 -> return $! JpgRestartIntervalEnd a
              | otherwise -> fail ("Invalid frame marker (" ++ show a ++ ")")

put4BitsOfEach :: Word8 -> Word8 -> Put
put4BitsOfEach a b = put $ (a `unsafeShiftL` 4) .|. b

get4BitOfEach :: Get (Word8, Word8)
get4BitOfEach = do
    val <- get
    return ((val `unsafeShiftR` 4) .&. 0xF, val .&. 0xF)

newtype RestartInterval = RestartInterval Word16

instance Binary RestartInterval where
    put (RestartInterval i) = putWord16be 4 >> putWord16be i
    get = do
        size <- getWord16be
        when (size /= 4) (fail "Invalid jpeg restart interval size")
        RestartInterval <$> getWord16be

instance Binary JpgComponent where
    get = do
        ident <- getWord8
        (horiz, vert) <- get4BitOfEach
        quantTableIndex <- getWord8
        return JpgComponent
            { componentIdentifier = ident
            , horizontalSamplingFactor = horiz
            , verticalSamplingFactor = vert
            , quantizationTableDest = quantTableIndex
            }
    put v = do
        put $ componentIdentifier v
        put4BitsOfEach (horizontalSamplingFactor v) $ verticalSamplingFactor v
        put $ quantizationTableDest v

instance Binary JpgFrameHeader where
    get = do
        beginOffset <- fromIntegral <$> bytesRead
        frmHLength <- getWord16be
        samplePrec <- getWord8
        h <- getWord16be
        w <- getWord16be
        compCount <- getWord8
        components <- replicateM (fromIntegral compCount) get
        endOffset <- fromIntegral <$> bytesRead
        when (beginOffset - endOffset < fromIntegral frmHLength)
             (skip $ fromIntegral frmHLength - (endOffset - beginOffset))
        return JpgFrameHeader
            { jpgFrameHeaderLength = frmHLength
            , jpgSamplePrecision = samplePrec
            , jpgHeight = h
            , jpgWidth = w
            , jpgImageComponentCount = compCount
            , jpgComponents = components
            }

    put v = do
        putWord16be $ jpgFrameHeaderLength v
        putWord8    $ jpgSamplePrecision v
        putWord16be $ jpgHeight v
        putWord16be $ jpgWidth v
        putWord8    $ jpgImageComponentCount v
        mapM_ put   $ jpgComponents v

instance Binary JpgScanSpecification where
    put v = do
        put $ componentSelector v
        put4BitsOfEach (dcEntropyCodingTable v) $ acEntropyCodingTable v

    get = do
        compSel <- get
        (dc, ac) <- get4BitOfEach
        return JpgScanSpecification {
            componentSelector = compSel
          , dcEntropyCodingTable = dc
          , acEntropyCodingTable = ac
          }

instance Binary JpgScanHeader where
    get = do
        thisScanLength <- getWord16be
        compCount <- getWord8
        comp <- replicateM (fromIntegral compCount) get
        specBeg <- get
        specEnd <- get
        (approxHigh, approxLow) <- get4BitOfEach

        return JpgScanHeader {
            scanLength = thisScanLength,
            scanComponentCount = compCount,
            scans = comp,
            spectralSelection = (specBeg, specEnd),
            successiveApproxHigh = approxHigh,
            successiveApproxLow = approxLow
        }

    put v = do
        putWord16be $ scanLength v
        putWord8 $ scanComponentCount v
        mapM_ put $ scans v
        putWord8 . fst $ spectralSelection v
        putWord8 . snd $ spectralSelection v
        put4BitsOfEach (successiveApproxHigh v) $ successiveApproxLow v

{-# INLINE createEmptyMutableMacroBlock #-}
-- | Create a new macroblock with the good array size
createEmptyMutableMacroBlock :: (Storable a, Num a) => ST s (MutableMacroBlock s a)
createEmptyMutableMacroBlock = M.replicate 64 0

printMacroBlock :: (Storable a, PrintfArg a)
                => MutableMacroBlock s a -> ST s String
printMacroBlock block = pLn 0
    where pLn 64 = return "===============================\n"
          pLn i = do
              v <- block `M.unsafeRead` i
              vn <- pLn (i+1)
              return $ printf (if i `mod` 8 == 0 then "\n%5d " else "%5d ") v ++ vn

printPureMacroBlock :: (Storable a, PrintfArg a) => MacroBlock a -> String
printPureMacroBlock block = pLn 0
    where pLn 64 = "===============================\n"
          pLn i = str ++ pLn (i + 1)
            where str | i `mod` 8 == 0 = printf "\n%5d " v
                      | otherwise = printf "%5d" v
                  v = block VS.! i


{-# INLINE dctBlockSize #-}
dctBlockSize :: Num a => a
dctBlockSize = 8
