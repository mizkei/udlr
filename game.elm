import Color
import Graphics.Collage exposing (..)
import Graphics.Element
import Keyboard
import Text
import Time
import Window
import Random exposing (..)
import Debug exposing (log)


-- model
(width, height) = (300, 400)
(halfWidth, halfHeight) = (150, 200)
(ln1, ln2, ln3) = (-100, 0, 100)
(sep1, sep2) = (-50, 50)
(color1, color2) = (Color.rgb 255 0 0, Color.rgb 0 0 0)
colorbg = Color.rgb 192 192 192
colorLine = Color.rgb 0 0 0
size = 70

type GameState = Play | Pause | End
type Dir = Up | Down | Left | Right

type alias Moji =
  { x: Float
  , y: Float
  , dir: Dir
  , char: String
  , scolor: Color.Color
  }

type alias Game =
  { state: GameState
  , prvDir: {x : Int, y : Int}
  , mojis: List Moji
  , level: Int
  , count: Int
  , addT: Float
  , seed: Seed
  }

type alias Input =
  { space: Bool
  , dir: {x : Int, y : Int}
  , iseed: Seed
  , delta: Time.Time
  }

newMoji : Int -> Int -> Int -> Moji
newMoji x d c = 
  let
    ptX = getLineX x
    dir = getDir d
    mcolor = getMojiColor c
    vdir = makeVDir x d c
  in
    Moji ptX halfHeight vdir (getDirChar dir) mcolor

initialSeed : Signal Seed
initialSeed = Signal.map (\(t, _) -> Random.initialSeed (round t)) (Time.timestamp (Signal.constant ()))

getLineX : Int -> Float
getLineX i =
  if i == 0 then ln1
  else if i == 1 then ln2
  else ln3

getMojiColor : Int -> Color.Color
getMojiColor i =
  if i == 0 then color1 else color2

getDir : Int -> Dir
getDir i =
  if i == 0 then Up
  else if i == 1 then Down
  else if i == 2 then Left
  else Right

getDirChar : Dir -> String
getDirChar d = 
  case d of
    Up -> "上"
    Down -> "下"
    Left -> "左"
    Right -> "右"

getArrow : {x : Int, y : Int} -> Maybe Dir
getArrow {x, y} = 
  if x == 0 && y == 1 then Maybe.Just Up
  else if x == 0 && y == -1 then Maybe.Just Down
  else if x == -1 && y == 0 then Maybe.Just Left
  else if x == 1 && y == 0 then Maybe.Just Right
  else Nothing

makeVDir : Int -> Int -> Int -> Dir
makeVDir x d c =
  if c == 0 then
    if x == 0 then
      Left
    else if x == 1 then
      Down
    else
      Right
  else
    getDir d

-- update
update : Input -> Game -> Game
update {space, dir, iseed, delta} ({state, prvDir, mojis, level, count, addT, seed} as game) =
  let
    tm = delta + addT
    dt = width / (calcPassTime level)
    seedt =
      if state == Pause then iseed else seed
    newState = 
      if space && state == Pause then
        Play
      else if space && state == End then
        Pause
      else if List.any isPass mojis then
        End
      else
        state
    (newCount, dropedMojis) =
      if state == Play && not (dir.x == prvDir.x && dir.y == prvDir.y) then
        dropMojis dir count mojis
      else
        (count, mojis)
    (newSeed, newAddT, addedMojis) =
      if state == Play then
        addMojis seedt dt tm dropedMojis
      else
        (seedt, addT, dropedMojis)
    newMojis =
      if state == Play then
        updateMojis delta dt addedMojis
      else
        addedMojis
    newLevel = round <| (toFloat count) / 30 + 1
  in
    if newState == Pause then
      initialGame
    else
      { game |
          state = newState,
          prvDir = dir,
          mojis = newMojis,
          level = newLevel,
          count = newCount,
          addT = newAddT,
          seed = newSeed
      }

-- TODO: remove case nest
dropMojis : {x : Int, y : Int} -> Int -> List Moji -> (Int, List Moji)
dropMojis arw c mojis =
  let
    dir = getArrow arw
  in
    case mojis of
      [] ->
        (c, [])
      m :: ms ->
        case dir of
          Maybe.Just d ->
            if d == m.dir then
              (c + 1, ms)
            else
              (c, mojis)
          Nothing ->
            (c, mojis)

-- TODO: remove seed repeat
addMojis : Seed -> Float -> Float -> List Moji -> (Seed, Float, List Moji)
addMojis sd dt tm mojis =
  let
    (line, sd1) = generate (int 0 2) sd
    (char, sd2) = generate (int 0 3) sd1
    (lcolor, newSeed) = generate (int 0 1) sd2
    dist = tm * dt
  in
    if dist > size then
      (newSeed, 0, List.append mojis [newMoji line char lcolor])
    else
      (newSeed, tm, mojis)

updateMojis : Float -> Float -> List Moji -> List Moji
updateMojis delta dt mojis = 
  List.map (\m -> { m | y = m.y - dt * delta}) mojis

isPass : Moji -> Bool
isPass {y} =
  if y < -halfHeight then True else False

calcPassTime : Int -> Float
calcPassTime lv = (4.0 / (toFloat lv)) * 1000 + 1000


-- view
view : Game -> Graphics.Element.Element
view game =
  let
    record = "lv: " ++ (toString game.level) ++ " count: " ++ (toString game.count)
  in
    collage width height <|
      List.append 
        [ rect width height |> filled colorbg
        , rect 1 height |> filled colorLine |> moveX sep1
        , rect 1 height |> filled colorLine |> moveX sep2
        , text (Text.fromString record) |> moveY (halfHeight - 10)
        ] 
        (List.map viewMoji game.mojis)

viewMoji : Moji -> Form
viewMoji ({x, y, char, scolor}as moji) =
  text (Text.fromString char |> Text.color scolor)
    |> move (x, y)
    |> scale 4


-- main
main = Signal.map view game

game : Signal Game
game = Signal.foldp update initialGame input

initialGame = Game Pause {x = 0, y = 0} [] 1 0 0 (Random.initialSeed 123456)

delta = Signal.map Time.inMilliseconds <| Time.fps 30

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.space
      Keyboard.arrows
      initialSeed
      delta

arrowsPulse : Signal { c : Bool, x : Int, y : Int }
arrowsPulse = 
  let 
    upd ({x, y} as n) ({c, x, y} as p) =
      if p.x == n.x && p.y == n.y then
        log "false" { c = False, x = n.x, y = n.y }
      else
        log "true" { c = True, x = n.x, y = n.y }
  in
    Signal.foldp upd {c = False, x = 0, y = 0} <| Signal.sampleOn delta Keyboard.arrows
