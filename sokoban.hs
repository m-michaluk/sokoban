{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.String

-- funkcje pomocnicze
-- sprawdza czy dany element znajduje się na danej liście
elemList :: Eq a => a -> [a] -> Bool
elemList _ [] = False
elemList y (x:xs) = y == x || elemList y xs

-- skleja dwie listy, np. appendList [1,2] [3,4] = [1,2,3,4]
appendList :: [a] -> [a] -> [a]
appendList [] l = l
appendList (x:xs) l = x : appendList xs l

listLength :: [a] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList f (x:xs) = if f x then x : filterList f xs else filterList f xs

-- zwraca n-ty element listy (elementy listy liczę od 1)
nth :: [a] -> Integer -> a
nth [] _ = error "index out of bounds"
nth (x:xs) n
    | n == 1 = x
    | otherwise = nth xs (n-1)

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = x && andList xs

allList :: (a -> Bool) -> [a] -> Bool
allList _ [] = True
allList f (x:xs) = f x && allList f xs

-- right fold
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f z []     = z 
foldList f z (x:xs) = f x (foldList f z xs)

-- powyższe używając fold:
elemList2 :: Eq a => a -> [a] -> Bool
elemList2 x l = foldList f False l
                where f a b = x == a || b

appendList2 :: [a] -> [a] -> [a]
appendList2 l1 l2 = foldList (:) l2 l1

listLength2 :: [a] -> Integer
listLength2 l = foldList addOne 0 l
                 where addOne i j = 1 + j

filterList2 :: (a -> Bool) -> [a] -> [a]
filterList2 p l = foldList f [] l
  where f a b
          | p a    = a:b
          | otherwise = b
          
nth2 :: [a] -> Integer -> a
nth2 [] _ = error "empty list"
nth2 xs n = foldList f x xs n
           where
               f a b k
                    | k == 1    = a
                    | otherwise = b (k-1)
               x _ = error ("index out of bounds")    
               
mapList2 :: (a -> b) -> [a] -> [b]
mapList2 f l = foldList ((:) . f) [] l
         
andList2 :: [Bool] -> Bool
andList2 l = foldList (&&) True l

allList2 :: (a -> Bool) -> [a] -> Bool
allList2 p l = foldList f True l
          where f a b
                  | b == False = False
                  | otherwise  = p a


-- Definicje obrazków wyświetlanych na planszy
player :: Picture
player = dilated 0.95 (_head & _body & _hands)
    where
        _head = solidCircle 0.25
        _body = colored red (solidRectangle 1 0.3)
        _hands = translated (-0.4) 0.25 _hand & translated 0.4 0.25 _hand
            where
                _hand = colored blue (solidRectangle 0.2 0.5)
                
wall, ground, storage, box :: Picture
ground = colored (translucent yellow) (solidRectangle 1 1)
         & colored (light yellow) (rectangle 1 1)
         
storage = colored (dark brown) (circle 0.15)
         & colored (dark green) (solidCircle 0.15)
         & ground
         
wall = bricks & colored (dark red) (rectangle 1 1) & colored red (solidRectangle 1 1)
    where
        bricks = brick
                  & translated (-1/4) (-1/3) brick
                  & translated (1/4) (-1/3) brick
                  & translated (-1/4) (1/3) brick
                  & translated (1/4) (1/3) brick
            where
                brick = colored (dark red) (rectangle 0.5 (1/3))

coloredBox :: Color -> Color -> Picture
coloredBox c1 c2 = dilated 0.8 (box c1 c2)
    where
        box c1 c2 = colored c2 (rectangle 1.15 1.15)
                    & colored c1 (thickRectangle 0.15 1 1)
                    & rotated (-pi/4) plank
                    & rotated (pi/4) plank
                    & colored c2 (solidRectangle 1 1)
                        where
                            plank = dilated 1.2 (colored c2 (rectangle 1 0.15)
                                                 & colored c1 (solidRectangle 1 0.15))
        
box = coloredBox brown (dark brown)

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

data Coord = C {
  x :: Integer,
  y :: Integer
} deriving Eq
data Direction = R | U | L | D deriving Eq

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)
     
-- zwraca obrazek przesunięty o podane współrzędne
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

-- obrazek gracza obrócony w odpowiednim kierunku
pictureOfPlayer :: Direction -> Picture
pictureOfPlayer R = rotated (-pi/2) player
pictureOfPlayer U = player
pictureOfPlayer L = rotated (pi/2) player 
pictureOfPlayer D = rotated pi player

pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes [] = blank
pictureOfBoxes (b:bs) = atCoord b (drawTile Box) & pictureOfBoxes bs

pictureOfMaze :: Maze -> Picture
pictureOfMaze (Maze c maze) = go [c] []
  where
    go [] _ = blank
    go (v:queue) visited
      | elemList v visited = go queue visited
      | otherwise = pictureOfTile & go (appendList queue (visibleTiles v maze)) (v:visited)
      where
        pictureOfTile = atCoord v (drawTile (maze v))
        visibleTiles c maze = filterList (\c -> maze c /= Blank) (mapList (\d -> adjacentCoord d c) [U, D, R, L])  

data State = S {
  stPlayer     :: Coord,
  stDir        :: Direction,
  stBoxes      :: [Coord],
  stLevel      :: Integer,
  stLevelMoves :: Integer,
  stMaze       :: Maze -- mapa poziomu bez skrzyni
}

-- nie trzeba porównywać mapy poziomu, bo ona i tak jest statyczna
instance Eq State where
  s1 == s2 = stPlayer s1 == stPlayer s2
              && stDir s1 == stDir s2
              && stBoxes s1 == stBoxes s2
              && stLevel s1 == stLevel s2
              && stLevelMoves s1 == stLevelMoves s2

type MazeMap = Coord -> Tile
data Maze = Maze {
  getInitCoord :: Coord,
  getMazeMap   :: MazeMap
}
mazes :: [Maze]
mazes = [maze1, maze2, maze3]
badMazes :: [Maze]
badMazes = [badMaze1, badMaze2, badMaze3]

neighbours :: Coord -> MazeMap -> [Coord]
neighbours c maze = filterList (\c -> t /= Blank && t /= Wall) (mapList (\d -> adjacentCoord d c) [U, D, R, L])  
         where t = maze c

-- daje wynik True wtw wszystkie wierzchołki osiągalne z początkowego są dobre
isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = bfs [initial] []
        where
          bfs [] _ = True
          bfs (v:queue) visited
                | elemList v visited = bfs queue visited
                | otherwise = isOk v && bfs (appendList queue (neighbours v)) (v:visited)

reachable :: Eq a => a -> (a -> [a]) -> a -> Bool
reachable initial neighbours v = bfs [initial] []
         where
          bfs [] _ = False
          bfs (w:queue) visited
                | elemList w visited = bfs queue visited
                | otherwise = v == w || bfs (appendList queue (neighbours w)) (w:visited)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (reachable initial neighbours) vs

-- pozycja startowa Ground lub Storage, żadna osiągalna (z pozycji startowej) nie jest Blank
isClosed :: Maze -> Bool
isClosed (Maze c maze) = startingTileOk && isGraphClosed c (\c -> neighbours c) isOk
          where
            startingTileOk = startTile == Ground || startTile == Storage
                where startTile = maze c
            isOk c = maze c /= Blank
            neighbours c = filterList (\c -> maze c /= Wall) (mapList (\d -> adjacentCoord d c) [U, D, R, L])     


-- liczba osiągalnych Storage jest niemniejsza od liczby osiągalnych skrzyń.
isSane :: Maze -> Bool       
isSane (Maze c maze)  = listLength reachableStorage >= listLength reachableBoxes
          where
            reachableFields = allReachableCoord (Maze c maze)
            reachableStorage = filterList (\c -> maze c == Storage) reachableFields 
            reachableBoxes = filterList (\c -> maze c == Box) reachableFields
            
allReachableCoord :: Maze -> [Coord]
allReachableCoord (Maze c maze) = bfs [c] []
      where
          bfs [] _ = []
          bfs (v:queue) visited
                | elemList v visited = bfs queue visited
                | otherwise = v : bfs (appendList queue (neighbours v maze)) (v:visited)

removeBoxes :: MazeMap -> MazeMap
removeBoxes m = m'
        where
          m' c = if t == Box then Ground else t
            where t = m c

-- osiągalne początkowe pozycje skrzyń
initialBoxes :: Maze -> [Coord]
initialBoxes m = filterList (\c -> (getMazeMap m) c == Box) (allReachableCoord m)

picture :: (Show a) => a -> Picture
picture = lettering . fromString . show

draw :: State -> Picture
draw s = if isWinning s then endMsg else pictureOfLevel
      where
        pictureOfLevel = atCoord (stPlayer s) (pictureOfPlayer (stDir s))
                         & pictureOfBoxes (stBoxes s)
                         & pictureOfMaze (stMaze s)
        endMsg = lettering "Poziom ukończony, liczba ruchów:"
                              & translated 0 (-1) (picture (stLevelMoves s))
       
-- wizualizacja wyników dla wszystkich poziomów
etap4 :: Picture
etap4 = pictureOfBools (mapList isOk (appendList mazes badMazes))
      where
        isOk maze = isSane maze && isClosed maze
        
pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

-- faktyczna gra, reakcje na eventy
-- stan gry dla nowego poziomu
getState :: Integer -> State
getState lvlNr = S initCoord U (initialBoxes maze) lvlNr 0 mazeWithNoBoxes
          where
            maze = nth mazes lvlNr
            initCoord = getInitCoord maze
            mazeWithNoBoxes = Maze initCoord (removeBoxes (getMazeMap maze))

initialState :: State
initialState = getState 1

startScreen :: Picture
startScreen = translated 0 3 etap4 
              & translated 0 2 (colored red (scaled 2 2.5 (lettering "SOKOBAN")))
              & (lettering "press SPACE to start")
              & translated 0 (-1.5) (scaled 0.8 0.8 (lettering "press ESC to reset"))
              & translated 0 (-2.5) (scaled 0.8 0.8 (lettering "press N to go to next level")) 
              
data SSState world = StartScreen | Running world

instance Eq s => Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False


data Activity world = Activity {
        actState  :: world,
        actHandle :: (Event -> world -> world),
        actDraw   ::(world -> Picture)
}

-- czy wygrywa dany level
isWinning :: State -> Bool
isWinning s = allList (isOnStorage maze) bs
      where
        bs = stBoxes s
        (Maze _ maze) = stMaze s
        isOnStorage maze c = maze c == Storage

handleEvent :: Event -> State -> State
handleEvent e s = if (isWinning s) then ignore e s else handleEvent' e s
          where
            ignore (KeyPress key) s
                | key == "N" = nextLvlState
            ignore _ s = s
            handleEvent' (KeyPress key) s
                | key == "Right" = goTo s R
                | key == "Up"    = goTo s U
                | key == "Left"  = goTo s L
                | key == "Down"  = goTo s D
                | key == "N"     = nextLvlState
            handleEvent' _ s     = s
            
            nextLvlState = if newLvl > listLength mazes then s else getState newLvl
            newLvl = stLevel s + 1
                
            
-- sprawdza czy gracz w danym stanie może się przemieścić w danym kierunku
-- jeśli tak to zwraca nowy stan, wpp. zwraca stary stan
goTo :: State -> Direction -> State
goTo (S from _ bs lvl lvlMoves m) d
    | boxAtTarget && canMoveBox = S targetCoord d updatedBoxes lvl (lvlMoves+1) m
    | ((targetTile == Ground || targetTile == Storage) && not boxAtTarget) = S targetCoord d bs lvl (lvlMoves+1) m
    | otherwise = S from d bs lvl lvlMoves m
    where
        targetCoord = adjacentCoord d from
        targetTile = maze targetCoord
        boxAtTarget = targetCoord `elem` bs
        targetCoordForBox = adjacentCoord d targetCoord
        targetTileForBox = if targetCoordForBox `elem` bs then Box else maze targetCoordForBox
        canMoveBox = (targetTileForBox == Ground || targetTileForBox == Storage)
        updatedBoxes = map moveBox bs
              where moveBox c = if c == targetCoord then targetCoordForBox else c
        maze = getMazeMap m
              
resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw) = Activity state0 handle' draw
    where
      handle' (KeyPress key) _ 
        | key == "Esc" = state0
      handle' e s = handle e s
        
withStartScreen :: Activity s-> Activity  (SSState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
    where
      state0' = StartScreen

      handle' (KeyPress key) StartScreen
           | key == " "                  = Running state0
      handle' _              StartScreen = StartScreen
      handle' e              (Running s) = Running (handle e s)

      draw' StartScreen = startScreen
      draw' (Running s) = draw s
      
data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s
   

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

base :: Activity State
base = Activity initialState handleEvent draw

etap5 :: IO ()
etap5 = runActivity (resetable (withUndo (withStartScreen base)))

main = etap5

-- MAZES

maze1 :: Maze
maze1 = Maze (C 0 (-2)) maze
  where
      maze (C x y)
          | abs x > 4  || abs y > 4  = Blank
          | abs x == 4 || abs y == 4 = Wall
          | x ==  2 && y <= 0        = Wall
          | x ==  3 && y <= 0        = Storage
          | x >= -2 && y == 0        = Box
          | otherwise                = Ground
          
maze2 :: Maze
maze2 = Maze (C 0 0) maze
  where
      maze (C x y)
        | abs x >3 || abs y > 3    = Blank
        | abs x == 3 || abs y == 3 = Wall
        | abs x == 2 && y == 0      = Storage
        | x == 0 && abs y == 2      = Storage
        | abs x == 1 && y == 0      = Box
        | x == 0 && abs y == 1      = Box
        | otherwise                 = Ground

maze3 :: Maze
maze3 = Maze (C 3 1) maze
  where
    maze (C x y)
      | abs x > 4 || abs y > 2          = Blank
      | abs x == 4 || abs y == 2        = Wall
      | x == 3 && y == -1               = Wall
      | x == -3 || (x == -2 && y == 1)  = Storage
      | x == 0 && y /= -1               = Box
      | x == 1 && y /= 1                = Box
      | otherwise                       = Ground          
          
-- BAD MAZES
-- miejsce docelowe otoczone ścianami
badMaze1 :: Maze
badMaze1 = Maze (C 2 0) maze
  where
      maze (C x y)
          | abs x > 3  || abs y > 1  = Blank
          | abs x == 3 || abs y == 1 = Wall
          | x == 0 && y == 0         = Wall
          | x == -1 && y == 0         = Box
          | x == -2 && y == 0        = Storage
          | otherwise                = Ground

-- skrzyń więcej niż miejsc docelowych
badMaze2 :: Maze
badMaze2 = Maze (C 0 0) maze
    where maze (C x y)
            | abs x > 4  || abs y > 2         = Blank
            | x >= -1 && x <= 4 && abs y == 1 = Wall
            | x == 4 && y == 0                = Wall
            | x == -1 && y == 0               = Wall
            | x ==  0 && y == 0               = Ground
            | (x == 1 || x == 2) && y == 0    = Box
            | x == 3 && y == 0                = Storage
            | otherwise                       = Blank      

-- nie da się przesunąć skrzyni w miejsce docelowe
badMaze3 :: Maze
badMaze3 = Maze (C 0 0) maze
  where
      maze (C x y)
        | abs x > 2 || abs y > 1   = Blank
        | abs x == 2 || abs y == 1 = Wall
        | x == -1                  = Storage
        | x == 1                   = Box
        | otherwise                = Ground
                   

