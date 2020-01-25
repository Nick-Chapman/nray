
module Main (main) where

import Data.Maybe as Maybe (isJust)
import Data.Word (Word8)

main :: IO ()
main = run

run :: IO ()
run = do
  let size = Size 1024 768
  let _ = writeImage "six.ppm" sixPixelImage
  let _ = writeImage "gradient.ppm" $ gradientImage size
  writeImage "scene.ppm" $ renderScene size lighting1 scene1

writeImage :: FilePath -> Image -> IO ()
writeImage path image = do
  let ppm = show (quantizeImage image)
  putStrLn $ "writing: " <> path
  writeFile path ppm

quantizeImage :: Image -> PPM
quantizeImage (Image xss) = PPM (map (map quantizeCol) xss)

newtype Image = Image [[Col]]
newtype Pixel = Pixel (Int,Int)
data Size = Size { width :: Int, height :: Int }

sixPixelImage :: Image
sixPixelImage = Image [[red,green,blue],[yellow,white,black]]

gradientImage :: Size -> Image
gradientImage Size{width,height} =
  Image $ [ [ col(r,g,b) | i <- [0..width-1]
                         , let r = float j / float height
                         , let g = float i / float width
                         , let b = 0
                         ]
          | j <- [0..height-1]
          ]
  where
    float = fromIntegral

bgColour :: Col
bgColour = col (0.2, 0.7, 0.8)
--bgColour = Col (0.1, 0.1, 0.1)

lighting1 :: Lighting
lighting1 = Lighting
  [ Light { position = vec (-20, 20,  20), brightness = 1.5 }
  , Light { position = vec ( 30, 50, -25), brightness = 1.8 }
  , Light { position = vec ( 30, 20,  30), brightness = 1.7 }
  ]

scene1 :: Scene
scene1 = Scene
  [ Sphere { center = vec (-3,    0,   -16), radius = 2,   madeof = ivory }
  , Sphere { center = vec (-1,   -1.5, -12), radius = 2,   madeof = mirror }
  , Sphere { center = vec ( 1.5, -0.5, -18), radius = 3,   madeof = redRubber }
  , Sphere { center = vec ( 7,    5,   -18), radius = 4,   madeof = mirror }
  , Sphere { center = vec ( 0,   -1000, 0 ), radius = 995, madeof = _yellowRubber }
  ]

ivory,redRubber,_yellowRubber :: Material

ivory = Material
  { surfaceCol = col (0.4, 0.4, 0.3)
  , diffAlbedo = 0.6
  , specAlbedo = 0.3
  , reflAlbedo = 0.1
  , specExponent = 50
  }

rubber :: Col -> Material
rubber surfaceCol = Material
  { surfaceCol
  , diffAlbedo = 0.9
  , specAlbedo = 0.1
  , reflAlbedo = 0
  , specExponent = 10
  }

mirror :: Material
mirror = Material
  { surfaceCol = white
  , diffAlbedo = 0
  , specAlbedo = 10
  , reflAlbedo = 0.6
  , specExponent = 1425
  }

redRubber     = rubber $ col (0.3, 0.1, 0.1)
_yellowRubber = rubber $ col (0.5, 0.5, 0)


newtype Lighting = Lighting [Light]
newtype Scene = Scene [Surface]

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

renderScene :: Size -> Lighting -> Scene -> Image
renderScene size@Size{width,height} lighting scene =
  Image $ reverse $ [ [colourAtPixel (Pixel (i,j)) | i <- [0..width-1]] | j <- [0..height-1] ]
  where
    colourAtPixel :: Pixel -> Col
    colourAtPixel pixel = castRay 0 (eyeRay size pixel) lighting scene

eyeRay :: Size -> Pixel -> Ray
eyeRay Size{width,height} (Pixel (i,j)) = Ray {orig,direction} where
  orig = vec (0,0,0)
  direction = normalise (vec (x,y,z * 1.4)) -- random hack to reduce FOV
  x = float $ i*2 - (width-1)
  y = float $ j*2 - (height-1)
  z = float $ - (width-1) -- this should give a 90 degree horizonal field of view
  float = fromIntegral

castRay :: Int -> Ray -> Lighting -> Scene -> Col
castRay depth ray lighting scene = renderMaybeHit depth ray lighting scene $ hitsScene ray scene

hitsScene :: Ray -> Scene -> Maybe Hit
hitsScene ray (Scene surfaces) = combineMaybeHits (map (hitsSurface ray) surfaces)

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

renderMaybeHit :: Int -> Ray -> Lighting -> Scene -> Maybe Hit -> Col
renderMaybeHit depth ray lighting scene = \case
  Nothing -> bgColour
  Just Hit { material = Material {surfaceCol,diffAlbedo,reflAlbedo,specAlbedo,specExponent}
           , hitPoint
           , surfaceNorm
           } -> do

    let reflDir = reflect lookDir surfaceNorm
    let reflOrig = addVec hitPoint (scaleVec eps reflDir) where eps = 0.0001
    let reflRay = Ray reflOrig reflDir

    let reflColour =
          if depth > cutoffDepth || reflAlbedo == 0 then black else
            attenuateColour reflAlbedo $ castRay (depth+1) reflRay lighting scene
          where
            cutoffDepth = 4

    let colours = reflColour : map colourFromLight lights where (Lighting lights) = lighting
    sumColours colours
    where
      Ray _ lookDir = ray

      colourFromLight :: Light -> Col
      colourFromLight light = do
        let Light { position = lightPos, brightness } = light
        let lightDir = normalise (subVec lightPos hitPoint)
        let diffComponent = clampPositve (dotProduct lightDir surfaceNorm)
        let specComponent = clampPositve (dotProduct (reflect lightDir surfaceNorm) lookDir) ** specExponent

        let shadowOrig = addVec hitPoint (scaleVec eps lightDir) where eps = 0.0001
        let shadowRay = Ray shadowOrig lightDir
        let inShadow = Maybe.isJust $ hitsScene shadowRay scene

        if inShadow then black else
          addColour
          (attenuateColour (brightness * diffComponent * diffAlbedo) surfaceCol)
          (attenuateColour (brightness * specComponent * specAlbedo) white)

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

red,green,blue,yellow,white,black :: Col
red    = col (1, 0, 0)
green  = col (0, 1, 0)
blue   = col (0, 0, 1)
yellow = col (1, 1, 0)
white  = col (1, 1, 1)
black  = col (0, 0, 0)

sumColours :: [Col] -> Col
sumColours = foldl1 addColour

attenuateColour :: Double -> Col -> Col
attenuateColour a (Col r g b) = col (a * r, a * g, a * b)

addColour :: Col -> Col -> Col
addColour (Col r g b) (Col r' g' b') = col (r + r', g + g', b + b')

quantizeCol :: Col -> RGB
quantizeCol (Col r g b) = RGB (quantize r, quantize g, quantize b)

----------------------------------------------------------------------

quantize :: Double -> Word8
quantize f = fromIntegral n where n :: Int = min 255 $ truncate (f * 256)

newtype RGB = RGB (Word8,Word8,Word8)
newtype PPM = PPM [[RGB]]

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
