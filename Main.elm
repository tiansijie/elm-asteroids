import Time exposing  (..)
import Keyboard
import Html.App as App
import Task
import Text
import AnimationFrame
import Html exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import List
import Random

(gameWidth, gameHeight) = (800, 600)
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


type alias Bullet =
  Object {}

type alias SpaceShip =
  Object {a: Float}


type State = Play | Pause

type alias Model = {
  state: State,
  asteroids: List Asteroid,
  bullets: List Bullet,
  spaceship: SpaceShip,
  gameTime: Time,
  timeSeed: Time
}


makespaceship: Float -> Float -> SpaceShip
makespaceship x y =
  {x = x, y = y, vx = 1, vy = 1, dirX = 0, dirY = 0, a = 0, degree = 0}

makeBullet: Float -> Float -> Float -> Float -> Float -> Float -> Bullet
makeBullet x y vx vy dirX dirY =
  {x = x, y = y, vx = vx, vy = vy, dirX = dirX, dirY = dirY, degree = 0}


generateAsteroid: Int -> Asteroid
generateAsteroid seed =
  {
    x = fst (Random.step (Random.float -300 300) (Random.initialSeed seed)),
    y = fst (Random.step (Random.float -100 100) (Random.initialSeed (seed+1))),
    vx = fst (Random.step (Random.float -30 30) (Random.initialSeed seed)),
    vy = fst (Random.step (Random.float -30 30) (Random.initialSeed (seed+1))),
    dirX = fst (Random.step (Random.float -1 1) (Random.initialSeed (seed))),
    dirY = fst (Random.step (Random.float -1 1) (Random.initialSeed (seed+1))),
    degree = 0
  }

defaultModel: Model
defaultModel =
  {
    state = Play,
    spaceship = makespaceship 0 0,
    asteroids = [],
    bullets = [],
    gameTime = 0,
    timeSeed = 0
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


isCollided: SpaceShip -> List Asteroid -> Bool
isCollided spaceship asteroids =
  List.foldr (\asteroid prev -> prev || (within spaceship asteroid)) False asteroids

stepAsteroid : Time -> Asteroid -> Asteroid
stepAsteroid t ({x, y, vx, vy, dirX, dirY} as asteroid) =
  { asteroid |
    x = x + vx * dirX * t,
    y = y + vy * dirY * t
  }

stepBullet: Time -> Bullet -> Bullet
stepBullet t ({x, y, vx, vy, dirX, dirY} as bullet) =
  { bullet |
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


stepSpaceShip: Time -> SpaceShip -> List Asteroid -> SpaceShip
stepSpaceShip t spaceship asteroids=
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
  | TickTotal Time
  | ShootBullet
  | TogglePlay
  | NoOp


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model

    SpaceShip deltaDegree deltaA ->
      let
        { spaceship } = model
        a' = deltaA
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

    ShootBullet ->
      let
        { spaceship, bullets } = model
        x = spaceship.x
        y = spaceship.y
        dirX = spaceship.dirX
        dirY = spaceship.dirY
        vx = spaceship.vx + 50
        vy = spaceship.vy + 50

        newBullets =
          (makeBullet x y vx vy dirX dirY) :: bullets
      in
        { model |
          bullets = newBullets
        }

    TogglePlay ->
      let
        newState =
          case model.state of
            Play -> Pause
            Pause -> Play
      in
        { model | state = newState }

    TickTotal now ->
        { model |
           timeSeed = now
        }

    Tick delta ->
      let
        { state, spaceship, asteroids, bullets, gameTime, timeSeed } = model

        newState = state

        newTime = gameTime + delta

        tempSpaceship =
          if state == Pause then
            spaceship
          else
            stepSpaceShip delta spaceship asteroids

        flooredTime =
          floor (inMilliseconds timeSeed)

        newAsteroids =
          if flooredTime % 10 == 0 then
            (generateAsteroid flooredTime) :: asteroids
          else
            List.map (stepAsteroid delta) asteroids

        newSpaceship =
          if isCollided tempSpaceship newAsteroids then
            makespaceship 0 0
          else
            tempSpaceship

        newBullets =
            List.map (stepBullet delta) bullets

      in
        { model |
          state = newState,
          spaceship = newSpaceship,
          asteroids = newAsteroids,
          bullets = newBullets,
          gameTime = newTime
        }

-- VIEW --
make: SpaceShip -> Shape -> Form
make obj shape =
    shape
      |> filled white
      |> move ( obj.x, obj.y )
      |> rotate obj.degree

drawAsteroid: Asteroid -> Form
drawAsteroid obj =
  (oval 40 40)
    |> filled white
    |> move ( obj.x, obj.y )

drawBullet: Bullet -> Form
drawBullet obj =
  (oval 15 15)
    |> filled yellow
    |> move ( obj.x, obj.y )

blackGroundColor = rgb 0 0 0

listDisplay: SpaceShip -> List Asteroid -> List Bullet -> Element -> List Form
listDisplay spaceship asteroids bullets times=
  List.append
    ([
      rect gameWidth gameHeight
        |> filled blackGroundColor
      , polygon [(-10.0, -10.0), (0.0, 50.0), (10.0, -10.0)]
        |> make spaceship
      , toForm times
        |> move (0, gameHeight/2 - 40)
    ]) (List.append (List.map drawAsteroid asteroids) (List.map drawBullet bullets))


txt f string =
  Text.fromString string
    |> Text.color (rgb 100 100 100)
    |> Text.monospace
    |> f
    |> leftAligned

view : Model -> Html Msg
view model =
  let
    { spaceship, asteroids, bullets, gameTime, state } = model
    times =
      txt (Text.height 50) (toString (floor (inMilliseconds gameTime)))
  in
    toHtml <|
    container 1300 700 middle <|
    collage gameWidth gameHeight (listDisplay spaceship asteroids bullets times)




keyboardProcessor down keyCode =
  case (down, keyCode) of
    (True, 38) -> SpaceShip 0 20
    (True, 40) -> SpaceShip 0 -20
    (False, 38) -> SpaceShip 0 0
    (False, 40) -> SpaceShip 0 0

    (True, 37) -> SpaceShip 0.08 0
    (True, 39) -> SpaceShip -0.08 0

    (False, 32) -> ShootBullet
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
        , Time.every second TickTotal
        ])
    }
