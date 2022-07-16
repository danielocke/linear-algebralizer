finalX = 10
finalY = -5

vOrange =    (rgb 0xFF 0xA5 0x00)
vGreen =    (rgb 0x45 0xD3 0xA9)
vPink  = (rgb 0xF9 0x6D 0x6B)
vPinkT  = (rgba 0xF9 0x6D 0x6B 0.65)
vSky =  (rgb 0x5F 0xDF 0xF0)
vNavy =  (rgb 0x04 0x23 0x8C)

vectColours = [vOrange,vGreen,vPink,vSky,vNavy]

--List of vectors to be operated on (+,-) and the operations
vectList = [(0,3),(2,4),(-3,-1),(4,-3),(-2,5)]
funcList = [(+),(+),(-),(-)]

projPoint = (-5,-3)
decompLine =  (-5,-3)

myShapes model inputPoint =
  [
    grid
 -- --------------------------------------------------------------
 -- Orthoganal proj of projPoint onto the line spanned by projLine
 -- --------------------------------------------------------------
  , plotLine vPinkT decompLine
  , vectLineD 10 (0,0) (orthProj inputPoint decompLine) vNavy
  , vectLineD 10 (orthProj inputPoint decompLine) inputPoint vGreen
  , vectLine 10 (0,0) inputPoint vOrange
  , plot 10 vNavy (orthProj inputPoint decompLine)
  , plot 10 vOrange inputPoint
  ]

type Msg = Tick Float GetKeyState

type alias Model = { time : Float }

cartesianProduct listX ys = case listX of 
             [] -> []
             (x::xs) -> (makeVectsFromX x ys) ++ (cartesianProduct xs ys)
             
makeVectsFromX x list = case list of
                        [] -> [] 
                        (y::ys) -> (x,y) :: (makeVectsFromX x ys)

vectLine scale (xi,yi) (xf,yf) clr = line (xi*scale,yi*scale) (xf*scale,yf*scale)
                      |> outlined (solid 2) clr
vectLineD scale (xi,yi) (xf,yf) clr = line (xi*scale,yi*scale) (xf*scale,yf*scale)
                      |> outlined (dashed 1) clr

listToFloat list = case list of
                   [] -> []   
                   ((x,y)::ls) -> (toFloat x,toFloat y) :: listToFloat ls

transform ((a,b),(c,d)) (x,y) = (a*x+b*y, c*x+d*y)

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model projPoint)

grid = group
  [
    List.map (lineX gray) 
      (List.range -5 5)
      |> group
  , List.map (lineY gray) 
      (List.range -5 5)
      |> group
  , lineX black 0
  , lineY black 0
  ]

lineX clr y =
    line (-50,toFloat y*10)(50,toFloat y*10)
      |> outlined (solid 1) clr
lineY clr x =
    line (toFloat x*10,-50)(toFloat x*10,50)
      |> outlined (solid 1) clr

plot scale clr (x,y) = 
    circle 2
      |> filled clr
      |> move (x*scale,y*scale)

yInt (x,y) m = y-m*x

plotLine clr (x,y) = if (abs y) > (abs x) 
                     then line (x*(50/y),50) (-x*(50/y),-50)
                            |> outlined (solid 2) clr
                     else line (50,y*(50/x)) (-50,-y*(50/x))
                            |> outlined (solid 2) clr
                     

plotTransf model clr scale (xi,yi) (xf,yf) = 
    circle 2
      |> filled clr
      |> move (if model.time * 1.25 < scale then model.time * 1.25 * (xf-xi) + xi*scale else xf * scale,
               if model.time * 1.25 < scale then model.time * 1.25 * (yf-yi) + yi*scale else yf * scale)

--Display a set as a string
setToString list =
  case list of 
    []          -> ""
    [(x,y)]     -> "(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")"
    ((x,y)::ls) -> "(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")" ++ "," ++ (setToString ls)

--Display the equation as a string
opToString vList fList =
  case (vList,fList) of 
    ([(x,y)],[]) -> " (" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")"
    (((x,y)::vs),(f::fs)) -> " (" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ") " ++ (funcToString f) ++ (opToString vs fs)
    _            -> ""
    
--Create a list of vectors by applying the operations to them
combVects scale start vList fList cList = case (vList,cList,fList) of
  ([v],[c],[]) -> [(plot scale c v),(vectLine scale start v c)]
  (((x1,y1)::vs),(c::cs),(f::fs)) -> [(plot scale c (x1,y1)),(vectLine scale start (x1,y1) c)]
                    ++ (combVects scale (x1,y1)
                         (case vs of
                           ((x2,y2)::ls) -> ((f x1 x2),(f y1 y2)) :: ls
                           _             -> [])
                         fs cs)
  _ -> []

--scale a vector to match the scale of the grid
scaleVec (x,y) k = (k*x,k*y)

-- Converts a basic arithmetic function to a string
funcToString f = if abs((f 5 7)-12) <= 0.01 then "+" else
                 if abs((f 7 5)-2) <= 0.01 then "-" else
                 if abs((f 6 2)-3) <= 0.01 then "/" else
                 if abs((f 6 2)-12) <= 0.01 then "*" else ""

--Dot product of two vectors
dotProd (x1,y1) (x2,y2) = x1*x2 + y1*y2

--Orthagonal projection of v onto the line spanned by u
orthProj v u = scaleVec u ((dotProd v u)/(dotProd u u))  
