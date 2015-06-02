{-# Language DeriveDataTypeable #-}

module Graphics.Rendering.KTX
    ( ErrorCode
    , Dimensions(..)
    , GLEWException(..)
    , KTXException(..)
    , initContext
    , loadTextureN
    , loadTextureM
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Typeable (Typeable)
import Data.Word (Word32)
-- constructors were not exported until here
#if MIN_VERSION_base(4,5,0)
import Foreign.C.Types (CUInt(CUInt), CInt(CInt))
#else
import Foreign.C.Types (CUInt)
#endif
import Foreign.C.Types (CUChar)
import Foreign.C.String (CString, withCString)
import Foreign.Storable (peek)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Graphics.Rendering.OpenGL (GLsizei, GLenum, GLuint, GLboolean, TextureObject(TextureObject))

#include <ktx.h>

type ErrorCode = (#type KTX_error_code)
data Dimensions = Dimensions GLsizei GLsizei GLsizei

foreign import ccall "GL/glew.h glewInit" glewInit :: IO GLenum

foreign import ccall "ktx.h ktxLoadTextureN" ktxLoadTextureN
    :: CString
    -> Ptr GLuint
    -> Ptr GLenum
    -> Ptr Dimensions
    -> Ptr GLboolean
    -> Ptr GLenum
    -> Ptr CUInt
    -> Ptr (Ptr CUChar)
    -> IO ErrorCode

foreign import ccall "ktx.h ktxLoadTextureM" ktxLoadTextureM
    :: Ptr ()
    -> GLsizei
    -> Ptr GLuint
    -> Ptr GLenum
    -> Ptr Dimensions
    -> Ptr GLboolean
    -> Ptr GLenum
    -> Ptr CUInt
    -> Ptr (Ptr CUChar)
    -> IO ErrorCode

data GLEWException = GLEWException GLenum deriving (Eq, Ord, Typeable, Show)
instance Exception GLEWException

data KTXException = KTXException ErrorCode deriving (Eq, Ord, Typeable, Show)
instance Exception KTXException

-- | Initialise the GL context for loading textures.
-- currently this just initialises /glew/.
initContext :: IO ()
initContext = do
    e <- glewInit
    case e of
        0 -> return ()
        _ -> throwIO (GLEWException e)

-- | Load a texture in ktx format from the given file path.
-- a GL texture name will be generated and returned.
loadTextureN :: FilePath -> IO TextureObject
loadTextureN path = withCString path $ \pathStr -> do
    with 0 $ \texPtr -> with 0 $ \bindingPtr -> do
        e <- ktxLoadTextureN pathStr texPtr bindingPtr nullPtr nullPtr nullPtr nullPtr nullPtr
        t <- peek texPtr
        case e of
            0 -> return $ TextureObject t
            _ -> throwIO $ KTXException e

-- | Load a texture in ktx format from the given byte stream.
-- a GL texture name will be generated and returned.
loadTextureM :: ByteString -> IO TextureObject
loadTextureM buf = unsafeUseAsCStringLen buf $ \(ptr, len) -> do
    with 0 $ \texPtr -> with 0 $ \bindingPtr -> do
        e <- ktxLoadTextureM (castPtr ptr) (fromIntegral len) texPtr bindingPtr nullPtr nullPtr nullPtr nullPtr nullPtr
        t <- peek texPtr
        case e of
            0 -> return $ TextureObject t
            _ -> throwIO $ KTXException e
