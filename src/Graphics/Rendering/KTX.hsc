module Graphics.Rendering.KTX
    ( ErrorCode
    , Dimensions(..)
    , initContext
    , loadTextureN
    ) where

import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Ptr
import Graphics.Rendering.OpenGL

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

initContext :: IO ()
initContext = do
    _ <- glewInit
    return ()

loadTextureN :: FilePath -> IO (Either TextureObject ErrorCode)
loadTextureN path = withCString path $ \pathStr -> do
    with 0 $ \texPtr -> with 0 $ \bindingPtr -> do
        e <- ktxLoadTextureN pathStr texPtr bindingPtr nullPtr nullPtr nullPtr nullPtr nullPtr
        t <- peek texPtr
        return $ case e of
            0 -> Left $ TextureObject t
            _ -> Right e
