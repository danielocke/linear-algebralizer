finalX = 10
finalY = -5

vOrange =    (rgb 0xFF 0xA5 0x00)
vGreen =    (rgb 0x45 0xD3 0xA9)
vPink  = (rgb 0xF9 0x6D 0x6B)
vSky =  (rgb 0x5F 0xDF 0xF0)
vNavy =  (rgb 0x04 0x23 0x8C)

vectColours = [vOrange,vGreen,vPink]

--List of vectors to be operated on (+,-) and the operations
vectList = [(0,3),(2,1),(-3,-1)]
fuList = [(+),(-)]
scList = [0.5,1.5,-2]

visualizerOps model funcList scaleList =
  [
    grid
  ,(combVects 10 (0,0) (scaleVects vectList scaleList) funcList vectColours)
    |> group
  ,text (opToString vectList funcList scaleList)
    |> centered
    |> filled black
    |> scale 0.5
    |> move (0,55)
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


listToFloat list = case list of
                   [] -> []   
                   ((x,y)::ls) -> (toFloat x,toFloat y) :: listToFloat ls

transform ((a,b),(c,d)) (x,y) = (a*x+b*y, c*x+d*y)

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (visualizerOps model fuList scList)

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

--For grid
lineX clr y =
    line (-50,toFloat y*10)(50,toFloat y*10)
      |> outlined (solid 1) clr
--For grid
lineY clr x =
    line (toFloat x*10,-50)(toFloat x*10,50)
      |> outlined (solid 1) clr

--Plot a still point
plot scale clr (x,y) = 
    circle 2
      |> filled clr
      |> move (x*scale,y*scale)

--Plot a point that moves from (xi,yi) to (xf,yf)
plotTransf model clr scale (xi,yi) (xf,yf) = 
    circle 2
      |> filled clr
      |> move (if model.time*2 < scale then model.time *2* (xf-xi) + xi*scale else xf * scale,
               if model.time*2 < scale then model.time *2* (yf-yi) + yi*scale else yf * scale)

--Display a set as a string
setToString list =
  case list of 
    []          -> ""
    [(x,y)]     -> "(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")"
    ((x,y)::ls) -> "(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")" ++ "," ++ (setToString ls)

--Display the equation as a string
opToString vList fList sList =
  case (vList,fList,sList) of 
    ([(x,y)],[],[s]) -> " " ++ (String.fromFloat(myRound s 2)) ++"(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")"
    (((x,y)::vs),(f::fs),(s::ss)) -> " " ++ (String.fromFloat(myRound s 2)) ++ "(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ") " ++ (funcToString f) ++ (opToString vs fs ss)
    _            -> ""

scaleVects vList sList = case (vList,sList) of
  ((v::vs),(s::ss)) -> (scaleVect v s) :: (scaleVects vs ss)
  _ -> []

--Create a list of vectors by applying the operations to them
combVects scale start vList fList cList = case (vList,cList,fList) of
  ([v],[c],_) -> [(plot scale c v),(vectLine scale start v c)]
  (((x1,y1)::vs),(c::cs),(f::fs)) -> [(plot scale c (x1,y1)),(vectLine scale start (x1,y1) c)]
                    ++ (combVects scale (x1,y1)
                         (case vs of
                           ((x2,y2)::ls) -> ((f x1 x2),(f y1 y2)) :: ls
                           _             -> [])
                         fs cs)
  _ -> []

--scale a vector to match the scale of the grid
scaleVect (x,y) k = (k*x,k*y)

-- Converts a basic arithmetic function to a string
funcToString f = if abs((f 5 7)-12) <= 0.01 then "+" else
              if abs((f 7 5)-2) <= 0.01 then "-" else
              if abs((f 6 2)-3) <= 0.01 then "/" else
              if abs((f 6 2)-12) <= 0.01 then "*" else ""
stringToFunc s = case s of
                   "+" -> (+)
                   "-" -> (-)
                   "*" -> (*)
                   _   -> (/)
dotProd (x1,y1) (x2,y2) = x1*x2 + y1*y2

--Orthagonal projection of v onto the line spanned by u
orthProj v u = scaleVect u ((dotProd v u)/(dotProd u u))  

myRound x n = toFloat(round (x*(10^n)))/10^n 