module Main where

import Data.Maybe
import PrestoDOM.Core
import PrestoDOM.Elements
import PrestoDOM.Events
import PrestoDOM.Properties
import PrestoDOM.Types

import Control.Alt (map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import DOM (DOM)
import Data.Array (head, tail, last)
import Data.Unit (Unit)
import FRP (FRP)
import FRP.Behavior (behavior)
import FRP.Behavior.Keyboard (key)
import FRP.Event.Keyboard as K
import FRP.Event.Time (interval, animationFrame)
import Prelude (show, otherwise, pure, not, negate, bind, discard, map, unit, ($), (*), (+), (-), (/), (<>), (<$>), (<*>), (==), (>=), (<=), (<), (>), (&&), (||))
import PrestoDOM.Util as U
import Control.Monad.Eff.Random (randomInt, RANDOM)


type Element = {
      x     :: Int
    , y     :: Int
    , width :: Int
    , height :: Int
  }

type Elements = Array Element

obstacleImage :: forall i a. Element -> VDom (Array (Prop i)) a
obstacleImage b = imageView
                [ width (V b.width)
                , height (V b.height)
                , imageUrl "police"
                , selectableItem "false"
                , margin ((show b.x)<>","<>(show b.y)<>",0,0")
                ]

line :: forall i a. Element -> VDom (Array (Prop i)) a
line l = frameLayout
            [ width (V 8)
            , height (V 100)
            , margin ("146,"<>(show l.y)<>",0,0")
            , background "#FFFFFF"
            ]
            []

initialObstacle :: Elements
initialObstacle = [ { x: -300, y: -100, width: 0, height: 0 } ]

initialCenterLines :: Elements
initialCenterLines = [ { x: 146, y: 500, width: 0, height: 0 },
                { x: 146, y: 300, width: 0, height: 0 },
                { x: 146, y: 100, width: 0, height: 0 },
                { x: 146, y: -100, width: 0, height: 0 } ]

obstacles :: forall t46 t47 t56.
        { obstacles :: Array
                         { x :: Int
                         , y :: Int
                         , width :: Int
                         , height :: Int
                         }
        | t56
        }
        -> VDom (Array (Prop t47)) t46
obstacles state = do 
          relativeLayout 
                    [ height (V 600)
                    , width Match_Parent
                    , clipChildren "true"
                    ]
                    ( map(\b -> obstacleImage b) state.obstacles )

centerLines :: forall t175 t176 t184.
        { centerLines :: Array
                           { x :: Int
                           , y :: Int
                           , width :: Int
                           , height :: Int
                           }
        | t184
        }
        -> VDom (Array (Prop t176)) t175
centerLines state = do 
          relativeLayout 
                    [ height (V 600)
                    , width Match_Parent
                    ]
                    ( map (\l -> line l) state.centerLines )                    

updateElement :: Element -> Int -> Int -> Element
updateElement { x, y, width, height } nx ny = { x: nx, y: ny, width, height }

moveObstacles :: Elements -> Int -> Elements
moveObstacles obstacles dy = map (\b -> updateElement b b.x (b.y + dy)) obstacles

moveLines :: Elements -> Int -> Elements
moveLines centerLines dy = map (\l -> updateElement l l.x (l.y + dy)) centerLines


addNewLines :: Elements -> Elements
addNewLines lines = (fromMaybe [{ x: 146, y: -100, width: 0, height: 0 }] $ tail lines) <> [{ x: 146, y: -100, width: 0, height: 0 }]

addNewObstacle :: forall a. Elements -> Eff ( random :: RANDOM | a ) Elements 
addNewObstacle obs = do 
              randomVal <- randomInt 20 200
              pure $ [fromMaybe { x: 120, y: -100, width: 70, height: 100 } $ last obs] <> [{ x: randomVal, y: -100, width: 70, height: 100 }]

updateTimer :: forall t1 t10 t6. t1 -> Eff t6 { | t10 }
updateTimer x = do
  state <- U.getState
  if (state.collision == 0)
    then U.updateState "distance" (state.distance + 1)
    else U.getState

restart :: forall t149. t149 -> (forall t144 t92. Eff t92 { | t144 })
restart _ = initialStates

widget :: forall t187 t188 t274.
        { centerLines :: Array
                           { x :: Int
                           , y :: Int
                           , width :: Int
                           , height :: Int
                           }
        , obstacles :: Array
                         { x :: Int
                         , y :: Int
                         , width :: Int
                         , height :: Int
                         }
        , carX :: String
        , distance :: String
        , restart :: String
        | t274
        }
        -> VDom (Array (Prop t188)) t187
widget state = linearLayout
                  [ height Match_Parent
                  , width Match_Parent
                  , gravity "center"
                  , name "rootNode"
                  , background "#000000"
                  ]
                  [ linearLayout
                    [ height (V 600)
                    , width (V 500)
                    , background "#D8D8D8"
                    , orientation "horizontal"
                    ]
                    [ frameLayout
                      [ height (V 600)
                      , width (V 300)
                      , gravity "center-bottom"
                      , background "#4E4B49"
                      ]
                      [ scrollView
                        [ height (V 600)
                        , width (V 300)
                        ]
                        [ 
                          frameLayout 
                          [ height (V 600)
                          , width (V 5)
                          , background "#FFFFFF"
                          , margin "20,0,0,0"
                          ]
                          []
                        , frameLayout 
                          [ height (V 600)
                          , width (V 5)
                          , background "#FFFFFF"
                          , margin "275,0,0,0"
                          ]
                          []
                        , (centerLines state)
                        , (obstacles state)
                        , imageView
                            [ width (V 70)
                            , height (V 100)
                            , imageUrl "car"
                            , translationX (state.carX)
                            , margin "0, 300, 0, 0"
                            , selectableItem "false"
                            ]
                        ]
                      ]
                      , linearLayout
                      [ height (V 600)
                      , width (V 200)
                      , orientation "vertical"
                      , padding "0,50,0,0"
                      , gravity "center"
                      ]
                      [ textView
                          [ height (V 50)
                          , width (V 200)
                          , text "Distance"
                          , textSize "25"
                          , gravity "center"
                          ]
                        , textView
                          [ height (V 50)
                          , width (V 200)
                          , text (state.distance <> " M")
                          , textSize "25"
                          , gravity "center"
                          ]
                        , textView
                          [ height (V 50)
                          , width (V 200)
                          , text "GAME OVER"
                          , textSize "30"
                          , color "#FF0000"
                          , gravity "center"
                          , visibility state.restart
                          ]
                        , linearLayout
                          [ name "restart"
                          , height $ V 40
                          , width $ V 150
                          , background "#444444"
                          , gravity "center"
                          , visibility "not"
                          , onClick "do"
                          , cornerRadius "5"
                          , visibility state.restart
                          ]
                          [
                            textView
                            [ width (V 150)
                            , height (V 30)
                            , text "Restart"
                            , textSize "24"
                            , color "#FFFFFF"
                            , gravity "center"
                            ]
                          ]
                      ]
                    ]
                  ]

initialStates :: forall t144 t92. Eff t92 { | t144 }
initialStates = do
  _ <- U.updateState "carX" 110
  _ <- U.updateState "key" "none"
  _ <- U.updateState "yPosition" (-100)
  _ <- U.updateState "yCenter" (-100)
  _ <- U.updateState "obstacles" initialObstacle
  _ <- U.updateState "collision" 0
  _ <- U.updateState "centerLines" initialCenterLines
  _ <- U.updateState "restart" "gone"
  U.updateState "distance" 0


main :: forall t650.
        Eff
          ( dom :: DOM
          , console :: CONSOLE
          , frp :: FRP
          , random :: RANDOM
          | t650
          )
          Unit
main = do
  --- Init State {} empty record--
  U.initializeState

  state <- initialStates
  ---- Render Widget ---
  U.render (widget state) listen

  pure unit

frameUpdate :: forall t324 t343 t485.
        t324
        -> Eff
             ( random :: RANDOM
             | t343
             )
             { | t485 }
frameUpdate x = do
  oldState <- U.getState
  if (oldState.collision == 0)
    then do
      _ <- if (oldState.yPosition >= 200) 
        then do
          newObstacle <- addNewObstacle oldState.obstacles
          _ <- U.updateState "obstacles" newObstacle
          U.updateState "yPosition" (-100)
        else
          U.updateState "yPosition" (oldState.yPosition + 5)
      _ <- U.updateState "obstacles" $ moveObstacles oldState.obstacles 5


      _ <- if (oldState.yCenter >= 100) 
        then do
          _ <- U.updateState "centerLines" $ addNewLines oldState.centerLines
          U.updateState "yCenter" (-100)
        else
          U.updateState "yCenter" (oldState.yCenter + 10)

      _ <- U.updateState "centerLines" $ moveLines oldState.centerLines 10

      let currentObstacle
            | (oldState.yPosition < 100 && oldState.yPosition + 200 <= 500) = fromMaybe { x: 120, y:0, width: 70, height: 100 } $ head oldState.obstacles
            | otherwise = fromMaybe { x: 120, y:0, width: 70, height: 100 } $ last oldState.obstacles

      _ <- if (currentObstacle.y >= 210 && currentObstacle.y <=390 && ((currentObstacle.x - oldState.carX < 0 && currentObstacle.x - oldState.carX >= -60) || (currentObstacle.x - oldState.carX > 0 && currentObstacle.x - oldState.carX <= 60)))
              then do 
                  U.updateState "collision" 1
              else U.getState

      if (oldState.key == "left" && oldState.carX > 10)
        then do U.updateState "carX" (oldState.carX - 5)
        else if (oldState.key == "right" && oldState.carX < 220) 
        then U.updateState "carX" (oldState.carX + 5)
        else U.getState

    else U.updateState "restart" "visible"

evalKeyboardRight :: forall t92 t93. Boolean -> Eff t93 { | t92 }
evalKeyboardRight right = do 
        if right == true
          then U.updateState "key" "right"
          else U.getState


evalKeyboardLeft :: forall t100 t99. Boolean -> Eff t100 { | t99 }
evalKeyboardLeft left = do 
        if left == true
          then U.updateState "key" "left"
          else U.getState

evalKeyboardUp :: forall t86 t87. Boolean -> Boolean -> Eff t87 { | t86 }
evalKeyboardUp downLeft downRight = if (not downLeft && not downRight) then do
    U.updateState "key" "none"
  else U.getState


listen :: forall t628.
        Eff
          ( frp :: FRP
          , console :: CONSOLE
          , random :: RANDOM
          | t628
          )
          (Eff
             ( frp :: FRP
             , console :: CONSOLE
             , random :: RANDOM
             | t628
             )
             Unit
          )                  
listen = do

  signalRestart <- U.signal "restart" "onClick" Nothing
  let behavior = restart <$> pure ""
  let events = signalRestart.event

  let frameBehv = frameUpdate <$> pure ""
  let timerBehav = updateTimer <$> pure ""
  let kBehvLeft = evalKeyboardLeft <$> (key 37)
  let kBehvRight = evalKeyboardRight <$> (key 39)
  let kBehvUp = evalKeyboardUp <$> (key 32) <*> (key 39)
  let distanceEvent = interval 100

  _ <- U.patch widget kBehvLeft K.down
  _ <- U.patch widget kBehvRight K.down
  _ <- U.patch widget kBehvUp K.up
  _ <- U.patch widget behavior events
  _ <- U.patch widget timerBehav distanceEvent
  U.patch widget frameBehv (animationFrame)