{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Data.Foldable (toList)
import           Data.List (find)
import qualified Data.Sequence as S
import           Data.Maybe (isJust)
import           Debug.Trace (traceShow, traceShowId)
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
  | PrintState
  | SetBarrier (Maybe Int)
  | MoveBarrier Component Dir
  | NewBarrier
  | RemoveBarrier
  | SetupGame Rand.StdGen

data Component = PosX | PosY | Width | Height deriving (Show)
data Dir = Up | Down deriving (Show)

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
                 deriving(Show)

data MrBox = MrBox
  { boxPos   :: V2 Double
  , boxVel   :: V2 Double
  , boxColor :: Color
  , boxDirection :: Direction
  }
  deriving(Show)

data Barrier = Barrier
  { barrierPos :: V2 Double
  , barrierShape :: V2 Double
  }
  deriving(Show)

data Level = Level
  { barriers :: S.Seq Barrier
  , mrBox :: MrBox
  }
  deriving(Show)

data Model = Model
  { level :: Level
  , editBarrier :: Int
  }
  deriving(Show)

--level1 :: Level
--level1 = Level
--  [ Barrier { barrierPos = V2 100 100, barrierShape = V2 50 500 }
--  , Barrier { barrierPos = V2 800 100, barrierShape = V2 50 500 }
--  , Barrier { barrierPos = V2 100 100, barrierShape = V2 500 50 }
--  , Barrier { barrierPos = V2 800 100, barrierShape = V2 500 50 }
--  , Barrier { barrierPos = V2 100 700, barrierShape = V2 500 50 }
--  , Barrier { barrierPos = V2 100 700, barrierShape = V2 50 500 }
--  , Barrier { barrierPos = V2 800 700, barrierShape = V2 50 500 }
--  , Barrier { barrierPos = V2 800 700, barrierShape = V2 500 50 }
--  ]
--  initialMrBox
--  where
--    initialMrBox =
--      northMrBox { boxPos   = V2 200 200
--                 , boxVel   = V2 0 0
--                 }

level1 :: Level
level1 = Level
  (S.fromList
  [ Barrier { barrierPos = V2 100 100, barrierShape = V2 50 800 }
  , Barrier { barrierPos = V2 800 100, barrierShape = V2 50 800 }
  , Barrier { barrierPos = V2 100 100, barrierShape = V2 800 50 }
  , Barrier { barrierPos = V2 800 100, barrierShape = V2 800 50 }
  , Barrier { barrierPos = V2 100 750, barrierShape = V2 800 50 }
  , Barrier { barrierPos = V2 100 750, barrierShape = V2 50 800 }
  , Barrier { barrierPos = V2 800 750, barrierShape = V2 50 800 }
  , Barrier { barrierPos = V2 800 750, barrierShape = V2 800 50 }
  ])
  initialMrBox
  where
    initialMrBox =
      northMrBox { boxPos   = V2 500 450
                 , boxVel   = V2 0 0
                 }

level2 :: Level
level2 = Level
  (S.fromList
  [ Barrier { barrierPos = V2 22.0 384.0, barrierShape = V2 50.0 770.0 }
  , Barrier { barrierPos = V2 1002.0 734.0, barrierShape = V2 50.0 770.0 }
  , Barrier { barrierPos = V2 512.0 744.0, barrierShape = V2 930.0 50.0 }
  , Barrier { barrierPos = V2 512.0 14.0, barrierShape = V2 940.0 70.0 }
  , Barrier { barrierPos = V2 1002.0 74.0, barrierShape = V2 50.0 160.0 }
  , Barrier { barrierPos = V2 312.0 284.0, barrierShape = V2 80.0 470.0 }
  , Barrier { barrierPos = V2 842.0 384.0, barrierShape = V2 270.0 70.0 }
  , Barrier { barrierPos = V2 412.0 384.0, barrierShape = V2 130.0 70.0 }
  ])
  initialMrBox
  where
    initialMrBox =
      northMrBox { boxPos = V2 80.0 120.0
                 , boxVel = V2 0.0 0.0
                 }
initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { level = level2
      , editBarrier = 0
      }
  , Cmd.execute Rand.newStdGen SetupGame
  )

windowDims :: V2 Int
windowDims = V2 1024 768

mrBoxSize :: V2 Double
mrBoxSize = V2 128 128

intersects :: Barrier -> V2 Double -> V2 Double -> Bool
intersects
  Barrier { barrierPos = V2 bx by, barrierShape = V2 bw bh }
  (V2 mx my)
  (V2 mw mh) = bx' < mx + mw &&
               bx' + bw > mx &&
               by' < my + mh &&
               bh + by' > my
              where
                bx' = bx - (bw / 2)
                by' = by - (bh / 2)

intersectsAny :: S.Seq Barrier -> V2 Double -> V2 Double -> Bool
intersectsAny bs v1 v2 = any (\b -> intersects b v1 v2) bs

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { level = level@Level { mrBox = mrBox@MrBox { .. } } } StartSpacing =
    (model { level = level { mrBox = mrBox { boxVel = newVel boxDirection } } }, Cmd.none)
    where
      newVel North = V2 0 10
      newVel East = V2 10 0
      newVel South = V2 0 (-10)
      newVel West = V2 (-10) 0

update model@Model { level = level@Level { mrBox = mrBox@MrBox { .. } } } StopSpacing =
    (model { level = level { mrBox = nextMrBox mrBox } }, Cmd.none)

update model@Model { level = level@Level { mrBox = mrBox@MrBox { .. } } } (Animate dt) =
    (model { level = level { mrBox = mrBox { boxPos = newBoxPos, boxVel = newBoxVel} } }, Cmd.none)
    where
      intersected = intersectsAny (barriers level) (boxPos + boxVel) mrBoxSize
      newBoxPos = if intersected then boxPos else boxPos + boxVel
      newBoxVel = if intersected then V2 0 0 else boxVel


update model PrintState = (traceShowId model, Cmd.none)

update model@Model { .. } (SetBarrier b) =
  (model { editBarrier = newBarrier }, Cmd.none)
  where
    newBarrier = case b of
                   Just x -> read $ show editBarrier ++ show x
                   Nothing -> 0

update model@Model { level = level@Level { .. } } (MoveBarrier comp dir) =
  (model { level = level { barriers = newBarriers } }, Cmd.none)
  where
    newBarriers = S.update (editBarrier model) (updatedBarrier comp dir) barriers
    updatedBarrier PosX Up = oldBarrier { barrierPos = V2 (x + 10) y }
    updatedBarrier PosX Down = oldBarrier { barrierPos = V2 (x - 10) y }
    updatedBarrier PosY Up = oldBarrier { barrierPos = V2 x (y - 10) }
    updatedBarrier PosY Down = oldBarrier { barrierPos = V2 x (y + 10) }
    updatedBarrier Width Up = oldBarrier { barrierShape = V2 (w + 10) h }
    updatedBarrier Width Down = oldBarrier { barrierShape = V2 (w - 10) h }
    updatedBarrier Height Up = oldBarrier { barrierShape = V2 w (h + 10) }
    updatedBarrier Height Down = oldBarrier { barrierShape = V2 w (h - 10) }
    oldBarrier@Barrier { barrierPos = barrierPos@(V2 x y), barrierShape = barrierShape@(V2 w h) } = S.index barriers (editBarrier model)

update model@Model { level = level@Level { .. } } NewBarrier =
  (model { level = level { barriers = newBarriers }, editBarrier = S.length barriers }, Cmd.none)
  where
    newBarriers = barriers S.|> Barrier
                              { barrierPos = (fromIntegral <$> windowDims) / 2
                              , barrierShape = V2 50 100
                              }

update model@Model { level = level@Level { .. } } RemoveBarrier =
  (model { level = level { barriers = newBarriers } }, Cmd.none)
  where
    newBarriers = S.take (idx - 1) barriers S.>< S.drop idx barriers
    idx = editBarrier model


update model@Model { .. } _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Keyboard.downs keyDown
  , Keyboard.ups keyUp
  , Keyboard.presses keyPress
  , Time.fps 60 Animate
  ]

keyDown :: Keyboard.Key -> Action
keyDown Keyboard.SpaceKey = StartSpacing
keyDown _ = DoNothing

keyUp :: Keyboard.Key -> Action
keyUp Keyboard.SpaceKey = StopSpacing
keyUp _ = DoNothing

keyPress :: Keyboard.Key -> Action
keyPress Keyboard.PKey = PrintState
keyPress Keyboard.Number0Key = SetBarrier $ Just 0
keyPress Keyboard.Number1Key = SetBarrier $ Just 1
keyPress Keyboard.Number2Key = SetBarrier $ Just 2
keyPress Keyboard.Number3Key = SetBarrier $ Just 3
keyPress Keyboard.Number4Key = SetBarrier $ Just 4
keyPress Keyboard.Number5Key = SetBarrier $ Just 5
keyPress Keyboard.Number6Key = SetBarrier $ Just 6
keyPress Keyboard.Number7Key = SetBarrier $ Just 7
keyPress Keyboard.Number8Key = SetBarrier $ Just 8
keyPress Keyboard.Number9Key = SetBarrier $ Just 9
keyPress Keyboard.WKey = MoveBarrier PosY Up
keyPress Keyboard.SKey = MoveBarrier PosY Down
keyPress Keyboard.DKey = MoveBarrier PosX Up
keyPress Keyboard.AKey = MoveBarrier PosX Down
keyPress Keyboard.IKey = MoveBarrier Height Up
keyPress Keyboard.KKey = MoveBarrier Height Down
keyPress Keyboard.LKey = MoveBarrier Width Up
keyPress Keyboard.JKey = MoveBarrier Width Down
keyPress Keyboard.NKey = NewBarrier
keyPress Keyboard.RKey = RemoveBarrier

keyPress Keyboard.ZKey = SetBarrier Nothing
keyPress _ = DoNothing

view :: Model -> Graphics SDLEngine
view model@Model { level = level@Level { mrBox = mrBox@MrBox { .. } } } = Graphics2D $
  --center (V2 (w / 2) (h / 2)) $ collage
  collage
    [ backdrop
    , toForm $ collage $ toList barrierShapes
    , mrBox
    --, mrBoxOutline
    --, toForm $ collage [
    --        move boxPos mrBox
    --    ]
    ]

  where
    dims@(V2 w h) = fromIntegral <$> windowDims
    backdrop = move (V2 (w/2) (h/2)) $ filled (rgb 0.13 0.13 0.13) $ rect dims

    barrierShapes = fmap barrier (barriers level)
    barrier b = move (barrierPos b) $ filled (rgb 0.65 0.25 0.11) $ rect (barrierShape b)

    mrBox = move boxPos $ filled boxColor $ mrBoxShape boxDirection
    mrBoxShape South = polygon $ path southCoords
    mrBoxShape North = polygon $ path northCoords
    mrBoxShape East = polygon $ path eastCoords
    mrBoxShape West = polygon $ path westCoords
    mrBoxOutline = outlined defaultLine $ polygon $ path mrBoxCoords
    V2 mx my = boxPos
    V2 mw mh = mrBoxSize
    mrBoxCoords = [ V2  mx        my
                  , V2 (mx + mw)  my
                  , V2 (mx + mw) (my + mh)
                  , V2  mx       (my + mh)
                  , V2  mx        my
                  ]
    westCoords = [V2 128 128, V2 128 0, V2 (128 - maxPoint) 64]
    eastCoords = [V2 0 0, V2 0 128, V2 maxPoint 64]
    southCoords = [V2 128 128, V2 0 128, V2 64 (128 - maxPoint)]
    maxPoint = 128
    eqPoint = 110.85125168440815
    northCoords = [V2 0 0, V2 128 0, V2 64 maxPoint]
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
