{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Data.Foldable (toList)
import           Data.List (find)
import qualified Data.Sequence as S
import           Data.Maybe (isJust)

import           Debug.Trace (traceShow, traceShowId)

import           Linear.V2 (V2(V2))
import           System.Process (createProcess, proc, terminateProcess)

import qualified System.Random as Rand

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Engine.SDL.Asset (withImage)
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
data SwirlDir = Cw | Ccw deriving (Show)
data Swirl = Swirl SwirlDir (V2 Double) deriving (Show)
data WinBox = WinBox (V2 Double) (V2 Double) deriving (Show)
data GameStage = Prelude | GLevel Int | Credits
data NoTurn = NoTurn (V2 Double) (V2 Double) deriving (Show)

maxLevel :: Int
maxLevel = 2

nextGameStage :: GameStage -> GameStage
nextGameStage Prelude = GLevel 1
nextGameStage (GLevel n)
  | n == maxLevel = Credits
  | otherwise = GLevel (n + 1)
nextGameStage Credits = Credits

nextLevel :: Int -> Level
nextLevel 1 = level3
nextLevel n = level2

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

nextMrBox :: SwirlDir -> MrBox -> MrBox
nextMrBox Ccw MrBox { .. } =
  newMrBox { boxVel = V2 0 0
           , boxPos = boxPos
           }
  where
    newMrBox = case boxDirection of
                  North -> eastMrBox
                  East -> southMrBox
                  South -> westMrBox
                  West -> northMrBox

nextMrBox Cw MrBox { .. } =
  newMrBox { boxVel = V2 0 0
           , boxPos = boxPos
           }
  where
    newMrBox = case boxDirection of
                  North -> westMrBox
                  East -> northMrBox
                  South -> eastMrBox
                  West -> southMrBox


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
  , swirls :: [Swirl]
  , curSwirlDir :: SwirlDir
  , swirlAngle :: Double
  , winBox :: WinBox
  , won :: Bool
  , noTurn :: Maybe NoTurn
  }
  deriving(Show)

data Model = Model
  { level :: Level
  , editBarrier :: Int
  , swirlCw :: Image SDLEngine
  , swirlCcw :: Image SDLEngine
  , splashGgj :: Image SDLEngine
  , stage :: GameStage
  }
  --deriving(Show)
instance Show Model where
  show model = show (level model) ++ show (editBarrier model)

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
  []
  Ccw
  0.0
  (WinBox (V2 1062.0 254.0) (V2 50.0 200.0))
  False
  Nothing
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
  [Swirl Cw (V2 500 150)]
  Ccw
  0.0
  (WinBox (V2 1062.0 254.0) (V2 50.0 200.0))
  False
  Nothing
  where
    initialMrBox =
      northMrBox { boxPos = V2 80.0 120.0
                 , boxVel = V2 0.0 0.0
                 }

level3 :: Level
level3 = Level
  (S.fromList
  [ Barrier { barrierPos = V2 362.0 754.0, barrierShape = V2 730.0 40.0 }
  , Barrier { barrierPos = V2 982.0 754.0, barrierShape = V2 100.0 40.0 }
  , Barrier { barrierPos = V2 1002.0 364.0, barrierShape = V2 50.0 740.0 }
  , Barrier { barrierPos = V2 492.0 24.0, barrierShape = V2 990.0 50.0 }
  , Barrier { barrierPos = V2 22.0 614.0, barrierShape = V2 50.0 250.0 }
  , Barrier { barrierPos = V2 22.0 164.0, barrierShape = V2 50.0 240.0 }
  , Barrier { barrierPos = V2 (-28.0) 384.0, barrierShape = V2 50.0 240.0 }
  , Barrier { barrierPos = V2 672.0 384.0, barrierShape = V2 50.0 230.0 }
  ])
  initialMrBox
  [ Swirl Ccw (V2 300 300)
  , Swirl Cw  (V2 450 300)
  , Swirl Cw  (V2 300 120)
  ]
  Cw
  0.0
  (WinBox (V2 822.0 784.0) (V2 230.0 30.0))
  False
  (Just $ NoTurn (V2 132 384) (V2 190 210))
  where
    initialMrBox =
      eastMrBox { boxPos = V2 30.0 320.0
                , boxVel = V2 0.0 0.0
                }

initial :: Image SDLEngine -> Image SDLEngine -> Image SDLEngine -> (Model, Cmd SDLEngine Action)
initial swirlCw swirlCcw splashGgj =
  ( Model
      { level = level2
      , editBarrier = 0
      , swirlCw = swirlCw
      , swirlCcw = swirlCcw
      , splashGgj = splashGgj
      , stage = Prelude
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

intersectsWinBox :: WinBox -> V2 Double -> V2 Double -> Bool
intersectsWinBox (WinBox pos shape) = intersects barrier
  where
    barrier = Barrier { barrierPos = pos, barrierShape = shape }

intersectsNoTurn :: NoTurn -> V2 Double -> V2 Double -> Bool
intersectsNoTurn (NoTurn pos shape) = intersects barrier
  where
    barrier = Barrier { barrierPos = pos, barrierShape = shape }

intersectsSwirl :: Swirl -> V2 Double -> V2 Double -> Bool
intersectsSwirl (Swirl _ pos) = intersects barrier
  where barrier = Barrier { barrierPos = pos + 64, barrierShape = V2 128 128 }

getSwirlDir :: SwirlDir -> [Swirl] -> V2 Double -> V2 Double -> SwirlDir
getSwirlDir curDir ss v1 v2 = foldl sd curDir ss
  where
    sd a s@(Swirl swd _) = if intersectsSwirl s v1 v2 then swd else a

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { stage = GLevel _, level = level@Level { mrBox = mrBox@MrBox { .. } } } StartSpacing =
    (model { level = level { mrBox = mrBox { boxVel = newVel boxDirection } } }, Cmd.none)
    where
      newVel North = V2 0 10
      newVel East = V2 10 0
      newVel South = V2 0 (-10)
      newVel West = V2 (-10) 0

update model@Model { stage = GLevel _, level = level@Level { mrBox = mrBox@MrBox { .. } } } StopSpacing =
    (model { level = level { mrBox = newMrBox, noTurn = Nothing } }, Cmd.none)
  where
    intersects Nothing = False
    intersects (Just noTurn) = intersectsNoTurn noTurn boxPos mrBoxSize
    newMrBox = if intersects (noTurn level) then mrBox { boxVel = V2 0 0 } else nextMrBox (curSwirlDir level) mrBox


update model@Model { stage = GLevel _, level = level@Level { mrBox = mrBox@MrBox { .. } } } (Animate dt) =
    (model { level = level { mrBox = mrBox { boxPos = newBoxPos, boxVel = newBoxVel}, swirlAngle = swirlAngle level + 1.0, curSwirlDir = swirlDir, won = wonLevel } }, Cmd.none)
    where
      intersected = intersectsAny (barriers level) (boxPos + boxVel) mrBoxSize
      swirlDir = getSwirlDir (curSwirlDir level) (swirls level) (boxPos + boxVel) (V2 128 128)
      wonLevel = intersectsWinBox (winBox level) (boxPos + boxVel) mrBoxSize
      newBoxPos = if intersected then boxPos else boxPos + boxVel
      newBoxVel = if intersected then V2 0 0 else boxVel


update model PrintState = (traceShowId model, Cmd.none)

update model@Model { stage = GLevel _, .. } (SetBarrier b) =
  (model { editBarrier = newBarrier }, Cmd.none)
  where
    newBarrier = case b of
                   Just x -> read $ show editBarrier ++ show x
                   Nothing -> 0

update model@Model { stage = GLevel _, level = level@Level { .. } } (MoveBarrier comp dir) =
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

update model@Model { stage = GLevel _, level = level@Level { .. } } NewBarrier =
  (model { level = level { barriers = newBarriers }, editBarrier = S.length barriers }, Cmd.none)
  where
    newBarriers = barriers S.|> Barrier
                              { barrierPos = (fromIntegral <$> windowDims) / 2
                              , barrierShape = V2 50 100
                              }

update model@Model { stage = GLevel _, level = level@Level { .. } } RemoveBarrier =
  (model { level = level { barriers = newBarriers } }, Cmd.none)
  where
    newBarriers = S.take (idx - 1) barriers S.>< S.drop idx barriers
    idx = editBarrier model

update model@Model { stage = GLevel n, level = level@Level { won = True } } _ =
  (model { level = nextLevel n, stage = nextGameStage (GLevel n) }, Cmd.none)

update model@Model { stage = Prelude, .. } StopSpacing =
  (model { stage = nextGameStage Prelude }, Cmd.none)

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
view model@Model { stage = Prelude, .. } = Graphics2D $
  collage
    [
      image (fromIntegral <$> windowDims) splashGgj
    , text $ Text.height 24
           $ Text.color white
           $ Text.toText "\n                                          Press space to start"
    ]
  where
    dims@(V2 w h) = fromIntegral <$> windowDims
    white = rgb 1.0 1.0 1.0

view model@Model { stage = Credits, .. } = Graphics2D $
  collage
    [ backdrop
    , move (V2 (w/2) ((h - 300)/2))
           $ text
           $ Text.height 72
           $ Text.color white
           $ Text.toText "Waver"
    , move (V2 (w/2) ((h - 120)/2))
           $ text
           $ Text.height 24
           $ Text.italic
           $ Text.color white
           $ Text.toText "Waving goodbye to the theme..."
    , move (V2 (w/2) ((h + 240)/2))
           $ text
           $ Text.height 48
           $ Text.color white
           $ Text.toText "Thank you for playing!"
    , move (V2 (w - 200) (h - 20))
           $ text
           $ Text.height 12
           $ Text.color white
           $ Text.toText "Soundtrack: Roleplay - Ladybug Castle (CC-BY 3.0)"
    , move (V2 120 (h - 20))
           $ text
           $ Text.height 12
           $ Text.color white
           $ Text.toText "Gameplay: Robert Clipsham"

    ]
  where
    dims@(V2 w h) = fromIntegral <$> windowDims
    white = rgb 1.0 1.0 1.0
    backdrop = move (V2 (w/2) (h/2)) $ filled (rgb 0.243 0.176 0.396) $ rect dims

view model@Model { stage = (GLevel n), level = level@Level { mrBox = mrBox@MrBox { .. } } } = Graphics2D $
  --center (V2 (w / 2) (h / 2)) $ collage
  collage
    [ backdrop
    , toForm $ collage $ toList barrierShapes
    , toForm $ collage swirlImages
    , mrBox
    --, image (V2 128 128) (swirlCw model)
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

    swirlImages = map mkSwirl (swirls level)
    mkSwirl (Swirl Cw pos) = move pos $ image (V2 128 128) (swirlCw model)
    mkSwirl (Swirl Ccw pos) = move pos $ image (V2 128 128) (swirlCcw model)

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

  (_, _, _, music) <- createProcess $ proc "cvlc" ["assets/rolemusic-ladybug-castle.mp3"]

  withImage engine "assets/swirl-cw.png" $ \swirlCw ->
    withImage engine "assets/swirl-ccw.png" $ \swirlCcw ->
      withImage engine "assets/splash-ggj2017.png" $ \splashGgj ->
        run engine GameConfig
        { initialFn       = initial swirlCw swirlCcw splashGgj
        , updateFn        = update
        , subscriptionsFn = subscriptions
        , viewFn          = view
        }

  terminateProcess music

