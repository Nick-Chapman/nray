
module Main (main) where

import System.Environment (getArgs)
import Render

main :: IO ()
main = do
  version <- parse <$> getArgs
  let size = Size 1024 768
  writeImage "scene.ppm" $ renderWorld (world version) size

data Version = TinyRayTraceTutorial | MyChanges deriving Eq

parse :: [String] -> Version
parse = \case
  ["--tut"] -> TinyRayTraceTutorial
  [] -> MyChanges
  args -> error (show args)

writeImage :: FilePath -> Image -> IO ()
writeImage path image = do
  let ppm = show (quantizeImage image)
  putStrLn $ "writing: " <> path
  writeFile path ppm

world :: Version -> World
world version = World
  { bgColour = col (0.2, 0.7, 0.8)
  , lights =
    [ Light { position = vec (-20, 20,  20), brightness = 1.5 }
    , Light { position = vec ( 30, 50, -25), brightness = 1.8 }
    , Light { position = vec ( 30, 20,  30), brightness = 1.7 }
    ]
  , surfaces =
    [ Sphere { center = vec (-3,    0,   -16), radius = 2,   madeof = ivory }
    , Sphere { center = vec (-1,   -1.5, -12), radius = 2,   madeof = glass }
    , Sphere { center = vec ( 1.5, -0.5, -18), radius = 3,   madeof = redRubber }
    , Sphere { center = vec ( 7,    5,   -18), radius = 4,   madeof = mirror }
    ] ++ [
      Sphere { center = vec ( 0,   -1000, 0 ), radius = 995, madeof = yellowRubber } | version == MyChanges
    ]
  }

ivory,redRubber,yellowRubber :: Material

ivory = Material
  { surfaceCol = col (0.4, 0.4, 0.3)
  , diffAlbedo = 0.6
  , specAlbedo = 0.3
  , reflAlbedo = 0.1
  , refrAlbedo = 0
  , specExponent = 50
  , refractiveIndex = 1
  }

redRubber    = rubber $ col (0.3, 0.1, 0.1)
yellowRubber = rubber $ col (0.5, 0.5, 0)

rubber :: Col -> Material
rubber surfaceCol = Material
  { surfaceCol
  , diffAlbedo = 0.9
  , specAlbedo = 0.1
  , reflAlbedo = 0
  , refrAlbedo = 0
  , specExponent = 10
  , refractiveIndex = 1
  }

mirror :: Material
mirror = Material
  { surfaceCol = white
  , diffAlbedo = 0
  , specAlbedo = 10
  , reflAlbedo = 0.6
  , refrAlbedo = 0
  , specExponent = 1425
  , refractiveIndex = 1
  }

glass :: Material
glass = Material
  { surfaceCol = col (0.6, 0.7, 0.8)
  , diffAlbedo = 0
  , specAlbedo = 0.5
  , reflAlbedo = 0.1
  , refrAlbedo = 0.8
  , specExponent = 125
  , refractiveIndex = 1.5
  }

white :: Col
white = col (1, 1, 1)
