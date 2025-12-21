module Game.Client.Renderer (
  Renderer(..),
  newRenderer,
  m44ToGL,
  loadImage,

  module Game.Client.Renderer.Shader,
  module Game.Client.Renderer.Vertex,
) where

import Data.Int
import Data.Word
import Data.Foldable (foldlM)
import Control.Monad.Primitive
import System.Exit
import System.IO

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Linear
import Codec.Picture
import Data.Atlas
import Data.StateVar
import Graphics.Rendering.OpenGL.GL qualified as GL
import SDL qualified

import Game.Client.Renderer.Shader
import Game.Client.Renderer.Vertex

-- TODO: we shouldn't need access to the constructor and fields, make rendering possible without
-- having to access those
data Renderer = Renderer
  { rendererWindow :: SDL.Window
  , rendererShader :: Maybe Shader
  , rendererVertexBuffer :: GL.BufferObject
  , rendererElementBuffer :: GL.BufferObject
  , rendererVertexArray :: GL.VertexArrayObject
  , rendererAtlas :: Atlas (PrimState IO)
  , rendererAtlasTexture :: GL.TextureObject
  , rendererTextures :: HashMap String (GL.TextureObject, V4 Int)
  }

newRenderer :: SDL.Window -> IO Renderer
newRenderer window = do
  [vertexBuffer, elementBuffer] <- GL.genObjectNames 2
  vertexArray <- GL.genObjectName
  texture <- GL.genObjectName
  
  maxSize <- fromIntegral <$> GL.maxTextureSize
  atlas <- create maxSize maxSize

  pure $ Renderer {
      rendererWindow = window
    , rendererShader = Nothing
    , rendererVertexBuffer = vertexBuffer
    , rendererElementBuffer = elementBuffer
    , rendererVertexArray = vertexArray
    , rendererAtlas = atlas
    , rendererAtlasTexture = texture
    , rendererTextures = HashMap.empty
    }

-- TODO: apparently M44 has Storable, so we can just pass its pointer to opengl without having to
-- use this function, so it appears it's useless (and i'm not a big fan of it too) and it shall be
-- removed from this module
m44ToGL :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
m44ToGL m = GL.newMatrix GL.ColumnMajor [
  e0, e4, e8, eC,
  e1, e5, e9, eD,
  e2, e6, eA, eE,
  e3, e7, eB, eF
  ]
  where
    V4
      (V4 e0 e1 e2 e3)
      (V4 e4 e5 e6 e7)
      (V4 e8 e9 eA eB)
      (V4 eC eD eE eF) = m

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage file = do
  dynImage <- readImage file
  case dynImage of
    Left error -> do
      hPutStrLn stderr error
      exitFailure
    Right (ImageRGBA8 image) -> pure image
    Right image -> pure $ convertRGBA8 image
