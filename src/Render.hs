
module Render(
  World(..), Light(..), Surface(..), Material(..),
  Col, col,
  Vec, vec,
  Size(..),
  renderWorld, Image,
  quantizeImage, PPM,
  ) where

import Data.Maybe as Maybe (isJust)
import Data.Word (Word8)

data World = World
  { lights :: [Light]
  , surfaces :: [Surface]
  , bgColour :: Col
  }

data Light = Light { position :: Vec, brightness :: Double }

data Surface = Sphere { center :: Vec, radius :: Double, madeof :: Material }

data Ray = Ray { orig :: Vec, direction :: Vec }

data Hit = Hit
  { distance :: Double
  , material :: Material
  , hitPoint :: Vec
  , surfaceNorm :: Vec
  }

data Material = Material
  { surfaceCol :: Col
  , diffAlbedo :: Double -- diffuse
  , specAlbedo :: Double -- specular
  , reflAlbedo :: Double -- reflective
  , specExponent :: Double
  }

data Size = Size { width :: Int, height :: Int }

newtype Image = Image [[Col]]

renderWorld :: World -> Size -> Image
renderWorld world size@Size{width,height} =
  Image $ reverse $ [ [colourAtPixel (Pixel (i,j)) | i <- [0..width-1]] | j <- [0..height-1] ]
  where
    colourAtPixel :: Pixel -> Col
    colourAtPixel pixel = castRay world 0 (eyeRay size pixel)

newtype Pixel = Pixel (Int,Int)

eyeRay :: Size -> Pixel -> Ray
eyeRay Size{width,height} (Pixel (i,j)) = Ray {orig,direction} where
  orig = vec (0,0,0)
  direction = normalise (vec (x,y,z * 1.4)) -- random hack to reduce FOV
  x = float $ i*2 - (width-1)
  y = float $ j*2 - (height-1)
  z = float $ - (width-1) -- this should give a 90 degree horizonal field of view
  float = fromIntegral

castRay :: World -> Int -> Ray -> Col
castRay world@World{lights,surfaces,bgColour} depth ray@Ray{direction=lookDir} =
  case hitsScene ray surfaces of
    Nothing -> bgColour
    Just Hit
      { hitPoint
      , material = Material {surfaceCol,diffAlbedo,reflAlbedo,specAlbedo,specExponent}
      , surfaceNorm
      } -> do

      let reflDir = reflect lookDir surfaceNorm
      let reflOrig = addVec hitPoint (scaleVec eps reflDir) where eps = 0.0001
      let reflRay = Ray reflOrig reflDir

      let reflColour =
            if depth > cutoffDepth || reflAlbedo == 0 then black else
              attenuateColour reflAlbedo $ castRay world (depth+1) reflRay
            where
              cutoffDepth = 4

      let colours = reflColour : map colourFromLight lights
      sumColours colours
      where
        colourFromLight :: Light -> Col
        colourFromLight Light{position,brightness} = do

          let lightDir = normalise (subVec position hitPoint)
          let diffComponent = clampPositve (dotProduct lightDir surfaceNorm)
          let specComponent = clampPositve (dotProduct (reflect lightDir surfaceNorm) lookDir) ** specExponent

          let shadowOrig = addVec hitPoint (scaleVec eps lightDir) where eps = 0.0001
          let shadowRay = Ray shadowOrig lightDir
          let inShadow = Maybe.isJust $ hitsScene shadowRay surfaces

          if inShadow then black else
            addColour
            (attenuateColour (brightness * diffComponent * diffAlbedo) surfaceCol)
            (attenuateColour (brightness * specComponent * specAlbedo) white)

hitsScene :: Ray -> [Surface] -> Maybe Hit
hitsScene ray surfaces = combineMaybeHits (map (hitsSurface ray) surfaces)

hitsSurface :: Ray -> Surface -> Maybe Hit
hitsSurface (Ray orig lookDir) = \case
  Sphere {center,radius,madeof=material} -> do
    let l = subVec center orig
    let tca = dotProduct l lookDir
    let d2 = dotProduct l l - tca*tca
    let r2 = radius * radius
    if d2 > r2 then Nothing else do
      let thc = sqrt (r2-d2)
      let t0 = tca - thc
      let t1 = tca + thc
      case if t0 < 0 then (if t1 < 0 then Nothing else Just t1) else Just t0
        of Nothing -> Nothing
           Just distance -> do
             let hitPoint = addVec orig (scaleVec distance lookDir)
             let surfaceNorm = normalise (subVec hitPoint center)
             Just Hit
               { distance
               , material
               , hitPoint
               , surfaceNorm
               }

combineMaybeHits :: [Maybe Hit] -> Maybe Hit
combineMaybeHits mhs =
  case [ h | Just h <- mhs ] of
    [] -> Nothing
    hits -> Just $ foldl1 combineHit hits

combineHit :: Hit -> Hit -> Hit
combineHit h1@Hit{distance=d1} h2@Hit{distance=d2} = if d1 < d2 then h1 else h2

clampPositve :: Double -> Double
clampPositve = max 0

reflect :: Vec -> Vec -> Vec
reflect incidence surfaceNorm =
  subVec incidence (scaleVec ( 2 * dotProduct incidence surfaceNorm) surfaceNorm)

-------------------------------------------------------------------------------

data Vec = Vec !Double !Double !Double

vec :: (Double,Double,Double) -> Vec
vec (x,y,z) = Vec x y z

dotProduct :: Vec -> Vec -> Double
dotProduct (Vec x y z) (Vec x' y' z') = x*x' + y*y' + z*z'

subVec :: Vec -> Vec -> Vec
subVec (Vec x y z) (Vec x' y' z') = vec (x-x',y-y',z-z')

addVec :: Vec -> Vec -> Vec
addVec (Vec x y z) (Vec x' y' z') = vec (x+x',y+y',z+z')

scaleVec :: Double -> Vec -> Vec
scaleVec m (Vec x y z) = vec (m*x,m*y,m*z)

normalise :: Vec -> Vec
normalise (Vec x y z) = vec (x',y',z') where
  x' = x / len
  y' = y / len
  z' = z / len
  len = sqrt (x*x + y*y + z*z)

----------------------------------------------------------------------

data Col = Col {r :: !Double, g :: !Double, b :: !Double}

col :: (Double,Double,Double) -> Col
col (r,g,b) = Col {r,g,b}

black, white :: Col
black = col (0,0,0)
white = col (1,1,1)

sumColours :: [Col] -> Col
sumColours = foldl1 addColour

attenuateColour :: Double -> Col -> Col
attenuateColour a (Col r g b) = col (a * r, a * g, a * b)

addColour :: Col -> Col -> Col
addColour (Col r g b) (Col r' g' b') = col (r + r', g + g', b + b')

----------------------------------------------------------------------

newtype RGB = RGB (Word8,Word8,Word8)
newtype PPM = PPM [[RGB]]

quantizeImage :: Image -> PPM
quantizeImage (Image xss) = PPM (map (map quantizeCol) xss)

quantizeCol :: Col -> RGB
quantizeCol (Col r g b) = RGB (quantize r, quantize g, quantize b)

quantize :: Double -> Word8
quantize f = fromIntegral n where n :: Int = min 255 $ truncate (f * 256)

instance Show PPM where
  show (PPM lines) =
    unlines $
    [ "P3" , unwords [show width, show height] , "255"
    ] ++ [show rgb | line <- lines, rgb <- line]
    where
      width = length (head lines)
      height = length lines

instance Show RGB where
  show (RGB (r,g,b)) = unwords [show r, show g, show b]
