
module Main (main) where

import Render

main :: IO ()
main = do
  let size = Size 1024 768
  writeImage "scene.ppm" $ renderWorld size world

writeImage :: FilePath -> Image -> IO ()
writeImage path image = do
  let ppm = show (quantizeImage image)
  putStrLn $ "writing: " <> path
  writeFile path ppm

world :: World
world = World
  { bgColour = col (0.2, 0.7, 0.8)
  , lighting = lighting1
  , scene = scene1
  }

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
  , Sphere { center = vec ( 0,   -1000, 0 ), radius = 995, madeof = yellowRubber }
  ]

ivory,redRubber,yellowRubber :: Material

ivory = Material
  { surfaceCol = col (0.4, 0.4, 0.3)
  , diffAlbedo = 0.6
  , specAlbedo = 0.3
  , reflAlbedo = 0.1
  , specExponent = 50
  }

redRubber    = rubber $ col (0.3, 0.1, 0.1)
yellowRubber = rubber $ col (0.5, 0.5, 0)

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

white :: Col
white = col (1, 1, 1)
