module Turtle where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Control.Monad ((>=>))


data Value = Num Float
  deriving (Show, Eq, Ord)

-- defining how the Num operators will interact with a pair of 'Value' operands
instance Num Value where
  (Num val1) + (Num val2) = Num (val1 + val2)
  (Num val1) - (Num val2) = Num (val1 - val2)
  (Num val1) * (Num val2) = Num (val1 * val2)
  abs = undefined
  signum = undefined
  fromInteger = undefined

-- same as above but for fractional
instance Fractional Value where
  (Num val1) / (Num val2) = Num (val1 / val2)
  fromRational = undefined

-- Turtle data record has been changed, it now encapsulates the following: the
-- picture it has formed until this state, the index (which frame it is in),
-- and pen state (up/down)
data Turtle = Turtle { location :: Point,
                       direction :: Float,
                       pen_colour :: Color,
                       pen_down :: Bool,
                       pic :: Picture,
                       index :: Int}

rad :: Float -> Float
rad r = r*(pi/180)

start = Turtle (0,0) 0 white True (Pictures []) 0

type Command = Turtle -> (Turtle, Picture)

--now checks if pen is down, if it is no picture is drawn
fd :: Value -> Command
fd (Num dist) t | pen_down t = (new_t, Color (pen_colour new_t) (Line [location t, location new_t]))
                | otherwise = (new_t, Blank)
                  where
                  new_t = let (x,y) = location t
                              x' = x + dist * sin (rad (direction t))
                              y' = y + dist * cos (rad (direction t))
                              in t { location = (x',y') }

rt :: Value -> Command
rt (Num angle) t = (t { direction = bound (angle + direction t) }, Blank)
                 where bound ang | ang > 360 = bound (ang - 360)
                                 | ang < 0 = bound (ang + 360)
                                 | otherwise = ang

changeCol :: Color -> Command
changeCol col t = (t { pen_colour = col }, Blank)

--changes the pen down/up state of the turtle
penDown :: Float -> Command
penDown down t | down == 1 = (t { pen_down = True }, Blank)
               | otherwise = (t { pen_down = False }, Blank)

--goes to an absolute co-ordinate, picture drawn also depends on pen state
moveTo :: (Value, Value) -> Command
moveTo (Num point1, Num point2) t | pen_down t = (new_t, Color (pen_colour new_t) (Line[location t, location new_t]))
                                  | otherwise = (new_t, Blank)
                                  where new_t = t { location = (point1,point2)}

nothing :: Command
nothing t = (t, Blank)

(+>) :: Command -> Command -> Command
(+>) c1 c2 t = let (t', p1) = c1 t
                   (t'', p2) = c2 t' in
                   (t'', Pictures [p1, p2])



windowDisplay = InWindow "Turtle" (640,480) (50,50)

--displays the picture the Turtle has drawn so far
drawer :: Turtle -> Picture
drawer turt = pic turt

--function which takes the current 'state' Turtle, and updates it according
--to which command is next.
updater :: [Command] -> ViewPort -> Float -> Turtle -> Turtle
updater commands vp seconds oldTurt | index oldTurt < (length commands) = newTurt
                                    | otherwise = oldTurt --if no more commands
                                      --to run then don't update the Turtle
                                    where
                                      ind = index oldTurt
                                      tupe = (commands!!ind) oldTurt --apply next
                                      --command to turtle resulting in tuple
                                      --(new turtle, next frame)
                                      newPics = Pictures ([pic oldTurt]++[snd tupe])
                                      --update turtle with new values
                                      newTurt = (fst tupe) {pic = newPics, index = ind +1}

runTurtle :: [Command] -> Int -> IO ()
runTurtle cmds fps = simulate
                     windowDisplay
                     black
                     fps
                     start
                     drawer
                     (updater cmds)

stringToCol :: String -> Maybe Color
stringToCol stringCol | stringCol == "green" = Just green
                      | stringCol == "blue" = Just blue
                      | stringCol == "red" = Just red
                      | stringCol == "blue" = Just blue
                      | stringCol == "yellow" = Just yellow
                      | stringCol == "cyan" = Just cyan
                      | stringCol == "magenta" = Just magenta
                      | stringCol == "rose" = Just rose
                      | stringCol == "violet" = Just violet
                      | stringCol == "azure" = Just azure
                      | stringCol == "aquamarine" = Just aquamarine
                      | stringCol == "chartreuse" = Just chartreuse
                      | stringCol == "orange" = Just orange
                      | otherwise = Nothing
