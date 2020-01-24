
module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    -- ["--something"] -> run
    _ -> run

run :: IO ()
run = do
  putStrLn "nray"
  let size = Size (1024,768)
  --let size = Size (300,200)
  writeImage "six.ppm" sixPixelImage
  writeImage "gradient.ppm" $ gradientImage size

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


newtype Intensity = Intensity Double -- 0..1
mkIntensity :: Double -> Intensity
mkIntensity d = if d < 0.0 || d >= 1.0 then error $ "mkIntensity: " <> show d else Intensity d

one,zero :: Intensity
one = mkIntensity 0.99999
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
  n = truncate (f * 256)

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
