[![Hackage](https://img.shields.io/hackage/v/ktx.svg)](https://hackage.haskell.org/package/ktx) [![Build Status](https://travis-ci.org/corngood/ktx.png)](https://travis-ci.org/corngood/ktx)

Make sure you have glew and mesa-dev installed.  On Ubuntu that means:

> sudo apt-get install libglew-dev libegl1-mesa-dev

Call _initContext_ on a thread where a GL context has been bound.

_loadTextureN/M_ will load a texture in ktx format from a _FilePath_ or _ByteString_ respectively.  A new texture name will be generated and returned as _IO TextureObject_.
