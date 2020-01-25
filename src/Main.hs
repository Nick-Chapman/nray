
module Main (main) where

import Data.Maybe as Maybe (isJust)

main :: IO ()
main = run

run :: IO ()
run = do
  let size = Size (1024,768)
  --let size = Size (512,384)
  let _ = writeImage "six.ppm" sixPixelImage
  let _ = writeImage "gradient.ppm" $ gradientImage size
  writeImage "scene.ppm" $ renderScene size lighting1 scene1

writeImage :: FilePath -> Image -> IO ()
writeImage path image = do
  let ppm = show (quantizeImage image)
  putStrLn $ "writing: " <> path
  writeFile path ppm

newtype Byte = Byte Int -- 0,255
mkByte :: Int -> Byte
mkByte n = if n < 0 || n > 255 then error $ "mkByte: " <> show n else Byte n
instance Show Byte where show (Byte n) = show n

newtype RGB = RGB (Byte,Byte,Byte) --rgb
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

newtype Intensity = Intensity Double -- 0..
mkIntensity :: Double -> Intensity
mkIntensity d = if d < 0.0 then error $ "mkIntensity: " <> show d else Intensity d

one,zero :: Intensity
one = mkIntensity 1.0
zero = mkIntensity 0.0

newtype Col = Col (Intensity,Intensity,Intensity) --rgb

red,green,blue,yellow,white,black :: Col
red    = Col (one,  zero, zero)
green  = Col (zero, one,  zero)
blue   = Col (zero, zero, one )
yellow = Col (one,  one,  zero)
white  = Col (one,  one,  one )
black  = Col (zero, zero, zero)

quantizeImage :: Image -> PPM
quantizeImage (Image xss) = PPM (map (map quantizeCol) xss)

quantizeCol :: Col -> RGB
quantizeCol (Col (r,g,b)) = RGB (quantizeIntensity r, quantizeIntensity g, quantizeIntensity b)

quantizeIntensity :: Intensity -> Byte
quantizeIntensity (Intensity f) = mkByte n where
  n = min 255 $ truncate (f * 256)

newtype Image = Image [[Col]]
newtype Size = Size (Int,Int) -- width(x), height(y)

sixPixelImage :: Image
sixPixelImage = Image [[red,green,blue],[yellow,white,black]]

gradientImage :: Size -> Image
gradientImage (Size (width,height)) =
  Image $ [ [ Col(r,g,b) | i <- [0..width-1]
                         , let r = mkIntensity $ float j / float height
                         , let g = mkIntensity $ float i / float width
                         , let b = zero
                         ]
          | j <- [0..height-1]
          ]
  where
    float = fromIntegral

bgColour :: Col
--bgColour = Col (mkIntensity 0.2, mkIntensity 0.7, mkIntensity 0.8)
bgColour = Col (mkIntensity 0.1, mkIntensity 0.1, mkIntensity 0.1)

lighting1 :: Lighting
lighting1 = Lighting
  [ Light { position = mkPoint (-20, 20,  20), brightness = mkIntensity 1.5 }
  , Light { position = mkPoint ( 30, 50, -25), brightness = mkIntensity 1.8 }
  , Light { position = mkPoint ( 30, 20,  30), brightness = mkIntensity 1.7 }
  ]

scene1 :: Scene
scene1 = Scene
  [ sphere (mkPoint (-3,   0,  -16)) 2 ivory
  , sphere (mkPoint (-1,  -1.5,-12)) 2 mirror --redRubber
  , sphere (mkPoint ( 1.5,-0.5,-18)) 3 redRubber
  , sphere (mkPoint ( 7,   5,  -18)) 4 mirror --ivory
  , sphere (mkPoint ( 0,  -1000,0)) 995 _yellowRubber
  ]

ivory,redRubber,_yellowRubber :: Material

ivory = Material
  { surfaceCol = Col(mkIntensity 0.4, mkIntensity 0.4, mkIntensity 0.3)
  , diffAlbedo = mkIntensity 0.6
  , specAlbedo = mkIntensity 0.3
  , reflAlbedo = mkIntensity 0.1
  , specExponent = 50
  }

rubber :: Col -> Material
rubber surfaceCol = Material
  { surfaceCol
  , diffAlbedo = mkIntensity 0.9
  , specAlbedo = mkIntensity 0.1
  , reflAlbedo = mkIntensity 0
  , specExponent = 10
  }

mirror :: Material
mirror = Material
  { surfaceCol = white
  , diffAlbedo = mkIntensity 0
  , specAlbedo = mkIntensity 10
  , reflAlbedo = mkIntensity 0.6
  , specExponent = 1425
  }

redRubber     = rubber $ Col(mkIntensity 0.3, mkIntensity 0.1, mkIntensity 0.1)
_yellowRubber = rubber $ Col(mkIntensity 0.5, mkIntensity 0.5, mkIntensity 0)

newtype Lighting = Lighting [Light]
data Light = Light { position :: Point, brightness :: Intensity }

newtype Scene = Scene [Surface]
newtype Surface = Surface (Ray -> Maybe Hit)
data Ray = Ray Point Direction

data Hit = Hit
  { distance :: Double
  , material :: Material
  , hitPoint :: Point
  , surfaceNorm :: Direction
  }

data Material = Material -- diff=diffuse, spec=specular
  { surfaceCol :: Col
  , diffAlbedo :: Intensity --0..1
  , specAlbedo :: Intensity --0..1
  , reflAlbedo :: Intensity --0..1
  , specExponent :: Double
  }

newtype Pixel = Pixel (Int,Int) -- i(x), j(y)
newtype Norm = Norm Vec3f
newtype Point = Point Vec3f
newtype Direction = Direction Norm

renderScene :: Size -> Lighting -> Scene -> Image
renderScene size@(Size (width,height)) lighting scene =
  Image $ reverse $ [ [colourAtPixel (Pixel (i,j)) | i <- [0..width-1]] | j <- [0..height-1] ]
  where
    colourAtPixel :: Pixel -> Col
    colourAtPixel pixel = castRay 0 (eyeRay size pixel) lighting scene

castRay :: Int -> Ray -> Lighting -> Scene -> Col
castRay depth ray lighting scene = renderMaybeHit depth ray lighting scene $ castRayScene ray scene

castRayScene :: Ray -> Scene -> Maybe Hit
castRayScene ray (Scene surfaces) = combineMaybeHits [ f ray | Surface f <- surfaces ]

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
           , hitPoint = Point hitPoint
           , surfaceNorm = Direction (Norm surfaceNorm)
           } -> do

    let reflDir = reflect lookDir surfaceNorm
    let reflOrig = addVec hitPoint (scaleVec eps reflDir) where eps = 0.0001
    let reflRay = Ray (Point reflOrig) (Direction (Norm reflDir))

    let reflColour =
          if depth > cutoffDepth then black else
            attenuateColour reflAlbedo $ castRay (depth+1) reflRay lighting scene
          where
            cutoffDepth = 4

    let colours = reflColour : map colourFromLight lights where (Lighting lights) = lighting
    sumColours colours
    where
      Ray _ (Direction (Norm lookDir)) = ray

      colourFromLight :: Light -> Col
      colourFromLight light = do
        let Light { position = Point lightPos, brightness = Intensity brightness } = light
        let lightDir = normalise (subVec lightPos hitPoint)
        let diffComponent = clampPositve (dotProduct lightDir surfaceNorm)
        let specComponent = clampPositve (dotProduct (reflect lightDir surfaceNorm) lookDir) ** specExponent
        let diffLightIntensity = mkIntensity $ (brightness*) $ diffComponent
        let specLightIntensity = mkIntensity $ (brightness*) $ specComponent

        let shadowOrig = addVec hitPoint (scaleVec eps lightDir) where eps = 0.0001
        let shadowRay = Ray (Point shadowOrig) (Direction (Norm lightDir))
        let inShadow = Maybe.isJust $ castRayScene shadowRay scene

        if inShadow then black else
          addColour
          (attenuateColour (mulI diffLightIntensity diffAlbedo) surfaceCol)
          (attenuateColour (mulI specLightIntensity specAlbedo) white)

clampPositve :: Double -> Double
clampPositve = max 0

reflect :: Vec3f -> Vec3f -> Vec3f
reflect incidence surfaceNorm =
  subVec incidence (scaleVec ( 2 * dotProduct incidence surfaceNorm) surfaceNorm)

attenuateColour :: Intensity -> Col -> Col
attenuateColour a (Col (r,g,b)) = Col (mulI a r, mulI a g, mulI a b)

sumColours :: [Col] -> Col
sumColours = foldl1 addColour --black

addColour :: Col -> Col -> Col
addColour (Col (r,g,b)) (Col (r',g',b')) = Col (addI r r', addI g g', addI b b')

mulI :: Intensity -> Intensity -> Intensity
mulI (Intensity a) (Intensity b) = Intensity (a*b)

addI :: Intensity -> Intensity -> Intensity
addI (Intensity a) (Intensity b) = Intensity (a+b)

eyeRay :: Size -> Pixel -> Ray
eyeRay (Size (width,height)) (Pixel (i,j)) = Ray origin direction where
  origin = mkPoint (0,0,0)
  direction = mkDirection (x,y,z * 1.4) -- random hack to reduce FOV
  x = float $ i*2 - (width-1)
  y = float $ j*2 - (height-1)
  z = float $ - (width-1) -- this should give a 90 degree horizonal field of view
  float = fromIntegral

mkPoint :: (Double,Double,Double) -> Point
mkPoint xyz = Point (Vec3f xyz)

mkDirection :: (Double,Double,Double) -> Direction
mkDirection xyz = Direction (mkNorm (Vec3f xyz))

mkNorm :: Vec3f -> Norm
mkNorm vec = Norm (normalise vec)

normalise :: Vec3f -> Vec3f
normalise (Vec3f (x,y,z)) = Vec3f(x',y',z') where
  x' = x / len
  y' = y / len
  z' = z / len
  len = sqrt (x*x + y*y + z*z)

sphere :: Point -> Double -> Material -> Surface
sphere (Point center) radius material = do
  Surface $ \(Ray (Point orig) (Direction (Norm lookDir))) -> do
    let l :: Vec3f = subVec center orig
    let tca :: Double = dotProduct l lookDir
    let d2  :: Double = dotProduct l l - tca*tca
    let r2  :: Double = radius*radius
    if d2 > r2 then Nothing else do
      let thc :: Double = sqrt (r2-d2)
      let t0  :: Double = tca - thc
      let t1  :: Double = tca + thc
      case if t0 < 0 then (if t1 < 0 then Nothing else Just t1) else Just t0
        of Nothing -> Nothing
           Just distance -> do
             let hit = addVec orig (scaleVec distance lookDir)
             let surfaceNorm = mkNorm (subVec hit center)
             Just Hit
               { distance
               , material
               , hitPoint = Point hit
               , surfaceNorm = Direction surfaceNorm
               }

newtype Vec3f = Vec3f (Double,Double,Double) --xyz

dotProduct :: Vec3f -> Vec3f -> Double
dotProduct (Vec3f (x,y,z)) (Vec3f (x',y',z')) = x*x' + y*y' + z*z'

subVec :: Vec3f -> Vec3f -> Vec3f
subVec (Vec3f (x,y,z)) (Vec3f (x',y',z')) = Vec3f (x-x',y-y',z-z')

addVec :: Vec3f -> Vec3f -> Vec3f
addVec (Vec3f (x,y,z)) (Vec3f (x',y',z')) = Vec3f (x+x',y+y',z+z')

scaleVec :: Double -> Vec3f -> Vec3f
scaleVec m (Vec3f (x,y,z)) = Vec3f (m*x,m*y,m*z)
