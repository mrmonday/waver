{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Data.List (find)
import           Data.Maybe (isJust)
import           Debug.Trace (traceShow)
import           Text.Printf (printf)

import           Linear.V2 (V2(V2))
import qualified System.Random as Rand

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time)

data Action
  = DoNothing
  | Animate Double
  | StartSpacing
  | StopSpacing
  | SetupGame Rand.StdGen


eastMrBox :: MrBox
eastMrBox = MrBox
  { boxColor     = rgb 1 0 0
  , boxPos       = V2 64 64
  , boxVel       = V2 0 0
  , boxDirection = East
  }

northMrBox :: MrBox
northMrBox = MrBox
  { boxColor     = rgb 0 1 0
  , boxPos       = V2 64 64
  , boxVel       = V2 0 0
  , boxDirection = North
  }

westMrBox :: MrBox
westMrBox = MrBox
  { boxColor     = rgb 0 0 1
  , boxPos       = V2 64 64
  , boxVel       = V2 0 0
  , boxDirection = West
  }

southMrBox :: MrBox
southMrBox = MrBox
  { boxColor     = rgb 1 0 1
  , boxPos       = V2 64 64
  , boxVel       = V2 0 0
  , boxDirection = South
  }

nextMrBox :: MrBox -> MrBox
nextMrBox MrBox { .. } =
  newMrBox { boxVel = V2 0 0
           , boxPos = boxPos
           }
  where
    newMrBox = case boxDirection of
                  North -> eastMrBox
                  East -> southMrBox
                  South -> westMrBox
                  West -> northMrBox

data Direction = North | East | South | West

data MrBox = MrBox
  { boxPos   :: V2 Double
  , boxVel   :: V2 Double
  , boxColor :: Color
  , boxDirection :: Direction
  }

data Barrier = Barrier
  { barrierPos :: V2 Double
  , barrierShape :: V2 Double
  }

data Level = Level
  { barriers :: [Barrier]
  }

data Model = Model
  { mrBox :: MrBox
  , level :: Level
  }

level1 :: Level
level1 = Level
  [ Barrier { barrierPos = V2 100 100, barrierShape = V2 50 500 }
  , Barrier { barrierPos = V2 800 100, barrierShape = V2 50 500 }
  , Barrier { barrierPos = V2 100 100, barrierShape = V2 500 50 }
  , Barrier { barrierPos = V2 800 100, barrierShape = V2 500 50 }
  , Barrier { barrierPos = V2 100 700, barrierShape = V2 500 50 }
  , Barrier { barrierPos = V2 100 700, barrierShape = V2 50 500 }
  , Barrier { barrierPos = V2 800 700, barrierShape = V2 50 500 }
  , Barrier { barrierPos = V2 800 700, barrierShape = V2 500 50 }
  ]

initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { mrBox = initialMrBox
      , level = level1
      }
  , Cmd.execute Rand.newStdGen SetupGame
  )
  where
    initialMrBox =
      northMrBox { boxPos   = V2 200 200
                 , boxVel   = V2 0 0
                 }

windowDims :: V2 Int
windowDims = V2 1024 768

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { mrBox = mrBox@MrBox { .. } } StartSpacing =
    (model { mrBox = mrBox { boxVel = newVel boxDirection } }, Cmd.none)
    where
      newVel North = V2 0 10
      newVel East = V2 10 0
      newVel South = V2 0 (-10)
      newVel West = V2 (-10) 0

update model@Model { mrBox = mrBox@MrBox { .. } } StopSpacing =
    (model { mrBox = nextMrBox mrBox }, Cmd.none)

update model@Model { mrBox = mrBox@MrBox { .. } } (Animate dt) =
    (model { mrBox = mrBox { boxPos = boxPos + boxVel } }, Cmd.none)

update model@Model { .. } _ = (model, Cmd.none)


subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Keyboard.downs keyDown
  , Keyboard.ups keyUp
  , Time.fps 60 Animate
  ]

keyDown :: Keyboard.Key -> Action
keyDown Keyboard.SpaceKey = StartSpacing
keyDown _ = DoNothing

keyUp :: Keyboard.Key -> Action
keyUp Keyboard.SpaceKey = StopSpacing
keyUp _ = DoNothing

view :: Model -> Graphics SDLEngine
view model@Model { mrBox = mrBox@MrBox { .. }, level } = Graphics2D $
  --center (V2 (w / 2) (h / 2)) $ collage
  collage
    [ backdrop
    , toForm $ collage barrierShapes
    , mrBox
    --, toForm $ collage [
    --        move boxPos mrBox
    --    ]
    ]

  where
    dims@(V2 w h) = fromIntegral <$> windowDims
    backdrop = move (V2 (w/2) (h/2)) $ filled (rgb 0.13 0.13 0.13) $ rect dims

    barrierShapes = map barrier (barriers level)
    barrier b = move (barrierPos b) $ filled (rgb 0.65 0.25 0.11) $ rect (barrierShape b)

    mrBox = move boxPos $ filled boxColor $ mrBoxShape boxDirection
    mrBoxShape South = polygon $ path southCoords
    mrBoxShape North = polygon $ path northCoords
    mrBoxShape East = polygon $ path eastCoords
    mrBoxShape West = polygon $ path westCoords
    squareCoords = dumbCircle [V2 0 0, V2 0 128, V2 128 128, V2 128 0]
    westCoords = [V2 128 128, V2 128 0, V2 (128 - 110.85125168440815) 64]
    eastCoords = [V2 0 0, V2 0 128, V2 110.85125168440815 64]
    southCoords = [V2 128 128, V2 0 128, V2 64 (128 - 110.85125168440815)]
    northCoords = [V2 0 0, V2 128 0, V2 64 110.85125168440815]
    kiteCoords = dumbCircle [V2 64 0, V2 128 64, V2 64 128, V2 0 64]
    dumbCircle = fmap (+ V2 (-64) (-64))

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    , SDL.windowIsFullscreen = False
    }

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
}
