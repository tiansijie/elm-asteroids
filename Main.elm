import Time exposing  (..)
import Keyboard
import Html.App as App
import Task
import Text
import Debug
import AnimationFrame
import Html exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import List
import Random

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

type alias Object base =
    {
      base |
        x: Float,
        y: Float,
        vx: Float,
        vy: Float,
        dirX: Float,
        dirY: Float,
        degree: Float
}


type alias Asteroid =
  Object {}


type alias SpaceShip =
  Object {a: Float}


type State = Play | Pause

type alias Model = {
  state: State,
  asteroids: List Asteroid,
  spaceship: SpaceShip,
  gameTime: Time
}


makespaceship: Float -> Float -> SpaceShip
makespaceship x y =
  {x = x, y = y, vx = 1, vy = 1, dirX = 0, dirY = 0, a = 0, degree = 0}


generateAsteroid: Random.Seed -> Asteroid
generateAsteroid seed =
  {
    x = fst (Random.step (Random.float -300 300) seed),
    y = fst (Random.step (Random.float -50 50) seed),
    vx = 30,
    vy = 30,
    dirX = 1,
    dirY = 1,
    degree = 0
  }

defaultModel: Model
defaultModel =
  {
    state = Pause,
    spaceship = makespaceship 0 0,
    asteroids = [],
    gameTime = 0
  }

init =
  (defaultModel, Cmd.none)


-- UPDATE --
-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m =
    m >= n-c && m <= n+c


within : SpaceShip -> Asteroid -> Bool
within spaceship asteroid =
    near spaceship.x 10 asteroid.x
    && near spaceship.y 10 asteroid.y


stepObj : Time -> Asteroid -> Asteroid
stepObj t ({x, y, vx, vy, dirX, dirY} as asteroid) =
  { asteroid |
    x = x + vx * dirX * t,
    y = y + vy * dirY * t
  }


updatePos: Float -> Float -> Float
updatePos pos dim =
  if pos > dim then
    -dim
  else if pos < -dim then
    dim
  else
    pos


stepSpaceShip: Time -> SpaceShip -> SpaceShip
stepSpaceShip t spaceship =
  let
    {x, y, vx, vy, dirX, dirY, a} = spaceship

    newVx = t * a + vx

    newVy = t * a + vy

    tempX = x + newVx * t * dirX

    tempY = y + newVy * t * dirY

    newX = updatePos tempX (halfWidth)

    newY = updatePos tempY (halfHeight)
  in
  { spaceship |
      x = newX,
      y = newY,
      vx = newVx,
      vy = newVy
  }


type Msg
  = SpaceShip Float Float
  | Tick Time
  | TogglePlay
  | NoOp


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model

    SpaceShip deltaDegree deltaA ->
      let
        { spaceship } = model
        a' = spaceship.a + deltaA
        degree = spaceship.degree + deltaDegree
        dirX = negate (sin degree)
        dirY = cos degree
      in
        { model
          | spaceship =
            { spaceship
              | dirX = dirX,
                dirY = dirY,
                a = a',
                degree = degree
            }
        }

    TogglePlay ->
      let
        newState =
          case model.state of
            Play -> Pause
            Pause -> Play
      in
        { model | state = newState }

    Tick delta ->
      let
        { state, spaceship, asteroids, gameTime } = model

        newState = state

        newTime = gameTime + delta

        newSpaceship =
          if state == Pause then
            spaceship
          else
            stepSpaceShip delta spaceship

        flooredTime =
          floor (inMilliseconds newTime)

        newAsteroids =
          if flooredTime % 10 == 0 then
            (generateAsteroid (Random.initialSeed flooredTime)) :: asteroids
          else
            List.map (stepObj delta) asteroids

      in
        { model |
          state = newState,
          spaceship = newSpaceship,
          asteroids = newAsteroids,
          gameTime = newTime
        }

-- VIEW --
make: SpaceShip -> Shape -> Form
make obj shape =
    shape
      |> filled white
      |> move ( obj.x, obj.y )
      |> rotate obj.degree

makeAsteroid: Asteroid -> Form
makeAsteroid obj =
  (oval 15 20)
    |> filled white
    |> move ( obj.x, obj.y )

makeList: List Asteroid -> List Form
makeList objs =
  List.map makeAsteroid objs

blackGroundColor = rgb 0 0 0

listDisplay: SpaceShip -> List Asteroid -> Element -> List Form
listDisplay spaceship asteroids times=
  List.append
    ([
      rect gameWidth gameHeight
        |> filled blackGroundColor
      , polygon [(-10.0, -10.0), (0.0, 50.0), (10.0, -10.0)]
        |> make spaceship
      , toForm times
        |> move (0, gameHeight/2 - 40)
    ]) (makeList asteroids)


txt f string =
  Text.fromString string
    |> Text.color (rgb 100 100 100)
    |> Text.monospace
    |> f
    |> leftAligned

view : Model -> Html Msg
view model =
  let
    { spaceship, asteroids, gameTime, state } = model
    times =
      txt (Text.height 50) (toString (inMilliseconds gameTime))
  in
    toHtml <|
    container 1000 700 middle <|
    collage gameWidth gameHeight (listDisplay spaceship asteroids times)




keyboardProcessor down keyCode =
  case (down, keyCode) of
    (True, 38) -> SpaceShip 0 0.5
    (True, 40) -> SpaceShip 0 -0.5

    (True, 37) -> SpaceShip 0.1 0
    (True, 39) -> SpaceShip -0.1 0

    (False, 32) -> TogglePlay
    _ -> NoOp



main =
  App.program
    { init = init
    , update = \msg m -> update msg m ! []
    , view = view
    , subscriptions =
      (\_ -> Sub.batch
        [ Keyboard.downs (keyboardProcessor True)
        , Keyboard.ups (keyboardProcessor False)
        , Keyboard.presses (keyboardProcessor True)
        , AnimationFrame.diffs (Tick<<inSeconds)
        ])
    }
