{-# Language DeriveDataTypeable #-}

module Graphics.Rendering.KTX
    ( ErrorCode
    , Dimensions(..)
    , initContext
    , loadTextureN
    , loadTextureM
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C.Types (CUInt(CUInt), CInt(CInt), CUChar)
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

initContext :: IO ()
initContext = do
    e <- glewInit
    case e of
        0 -> return ()
        _ -> throwIO (GLEWException e)

loadTextureN :: FilePath -> IO TextureObject
loadTextureN path = withCString path $ \pathStr -> do
    with 0 $ \texPtr -> with 0 $ \bindingPtr -> do
        e <- ktxLoadTextureN pathStr texPtr bindingPtr nullPtr nullPtr nullPtr nullPtr nullPtr
        t <- peek texPtr
        case e of
            0 -> return $ TextureObject t
            _ -> throwIO $ KTXException e

loadTextureM :: ByteString -> IO TextureObject
loadTextureM buf = unsafeUseAsCStringLen buf $ \(ptr, len) -> do
    with 0 $ \texPtr -> with 0 $ \bindingPtr -> do
        e <- ktxLoadTextureM (castPtr ptr) (fromIntegral len) texPtr bindingPtr nullPtr nullPtr nullPtr nullPtr nullPtr
        t <- peek texPtr
        case e of
            0 -> return $ TextureObject t
            _ -> throwIO $ KTXException e
