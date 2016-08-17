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

type alias General base =
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
  General {}


type alias Bullet =
  General {}

type alias SpaceShip =
  General {a: Float}


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
    vx = fst (Random.step (Random.float -60 60) (Random.initialSeed seed)),
    vy = fst (Random.step (Random.float -60 60) (Random.initialSeed (seed+1))),
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

within : Float -> Float -> Float -> Float -> Bool
within x1 y1 x2 y2 =
    near x1 10 x2
    && near y1 10 y2

isCollided: SpaceShip -> List Asteroid -> Bool
isCollided spaceship asteroids =
  List.foldl (\asteroid prev -> prev || (within spaceship.x spaceship.y asteroid.x asteroid.y)) False asteroids

isCollidedBullet: Bullet -> List Asteroid -> List Asteroid
isCollidedBullet bullet asteroids =
  List.foldl (\asteroid prev ->
    if (within bullet.x bullet.y asteroid.x asteroid.y) then
      prev
    else
      asteroid::prev
    ) [] asteroids


stepAsteroid : Time -> Asteroid -> Asteroid
stepAsteroid t asteroid =
  let
    {x, y, vx, vy, dirX, dirY} = asteroid

    tempX = x + vx * dirX * t
    tempY = y + vy * dirY * t

    newX = updatePos tempX halfWidth
    newY = updatePos tempY halfHeight
  in
    { asteroid |
      x = newX,
      y = newY
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


stepSpaceShip: Time -> SpaceShip -> SpaceShip
stepSpaceShip t spaceship =
  let
    {x, y, vx, vy, dirX, dirY, a} = spaceship

    newVx = t * a * dirX + vx

    newVy = t * a * dirY + vy

    tempX = x + newVx * t

    tempY = y + newVy * t

    newX = updatePos tempX halfWidth

    newY = updatePos tempY halfHeight
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
              |
                dirX = dirX,
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
        vx = 350
        vy = 350

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
            stepSpaceShip delta spaceship

        millisecondTime = round ((inMilliseconds newTime) * 100)
        randomNum = fst (Random.step (Random.int 0 500) (Random.initialSeed millisecondTime))

        newAsteroids =
          if randomNum <= 2 then
            (generateAsteroid millisecondTime) :: asteroids
          else
            asteroids


        stepedAsteroid = List.map (stepAsteroid delta) newAsteroids

        newSpaceship =
          if isCollided tempSpaceship stepedAsteroid then
            makespaceship 0 0
          else
            tempSpaceship

        stepedBullets = List.map (stepBullet delta) bullets

        finalUpdated =
          List.foldl (\bullet prev ->
              let
                { prevBullets, prevAsteroids } = prev
                currAsteroids = isCollidedBullet bullet prevAsteroids
                updatedBulltes =
                  if (List.length currAsteroids) /= (List.length prevAsteroids) then
                    prevBullets
                  else
                    bullet::prevBullets
              in { prev |
                prevBullets = updatedBulltes,
                prevAsteroids = currAsteroids
              }
            ) {prevBullets = [], prevAsteroids = stepedAsteroid} stepedBullets

      in
        { model |
          state = newState,
          spaceship = newSpaceship,
          asteroids = finalUpdated.prevAsteroids,
          bullets = finalUpdated.prevBullets,
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
      txt (Text.height 50) (toString (floor (inMinutes gameTime)))
  in
    toHtml <|
    container 1300 700 middle <|
    collage gameWidth gameHeight (listDisplay spaceship asteroids bullets times)




keyboardProcessor down keyCode =
  case (down, keyCode) of
    (True, 38) -> SpaceShip 0 30
    (True, 40) -> SpaceShip 0 -30
    (False, 38) -> SpaceShip 0 0
    (False, 40) -> SpaceShip 0 0

    (True, 37) -> SpaceShip 0.3 0
    (True, 39) -> SpaceShip -0.3 0

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
