finalX = 10
finalY = -5

vOrange =    (rgb 0xFF 0xA5 0x00)
vGreen =    (rgb 0x45 0xD3 0xA9)
vPink  = (rgb 0xF9 0x6D 0x6B)
vSky =  (rgb 0x5F 0xDF 0xF0)
vNavy =  (rgb 0x04 0x23 0x8C)
vGreenCmp =  (rgba 0x45 0xD3 0xA9 0.75)
vSkyCmp =  (rgba 0x5F 0xDF 0xF0 0.65)
vOrangeCmp =  (rgba 0xFF 0xA5 0x00 0.65)
vPurpleCmp = (rgba 0x6A 0x0D 0xAD 0.65)

vectColours = [vOrange,vGreen,vPink,vSky,vNavy]

inputVect = (-3,-2)

--List of vectors to be operated on (+,-) and the operations
vectA = (-2,-3)
vectB = (-4,0)
vectC = (2,5)
vectD = (1,1.5)
planeSpan = [vectB,vectC]
lineSpan = [vectA,vectD]
spanClr = [vPink, vSky]
funcList = [(+)]

visualizerLine model = 
  [
    grid
  , plot 10 vOrange vectA
  , vectLine 10 (0,0) vectA vOrange
  , text ("{" ++ (setToString [vectA]) ++ "}")
        |> centered
        |> filled black
        |> scale 0.5
        |> move (0,55)
  , plotLine vOrangeCmp vectA
  ]
  
visualizerPlane model = 
  [
    grid
  , rect 100 100
        |> filled vPurpleCmp
  , List.map2 (plot 10) spanClr planeSpan
      |> group
  , List.map2 (vectLine 10 (0,0)) planeSpan spanClr
      |> group
  , text ("{" ++ (setToString planeSpan) ++ "}")
        |> centered
        |> filled black
        |> scale 0.5
        |> move (0,55)
  ]
  
visualizerLine2 model = 
  [
    grid
  , plotLine vPurpleCmp vectA
  , List.map2 (plot 10) spanClr lineSpan
      |> group
  , List.map2 (vectLine 10 (0,0)) lineSpan spanClr
      |> group
  , text ("{" ++ (setToString lineSpan) ++ "}")
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

view model = collage 192 128 (visualizerLine2 model)

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
opToString vList fList sList =
  case (vList,fList,sList) of 
    ([(x,y)],[],(s::ss)) -> " " ++ (String.fromFloat(myRound s 2)) ++"(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ")"
    (((x,y)::vs),(f::fs),(s::ss)) -> " " ++ (String.fromFloat(myRound s 2)) ++ "(" ++ (String.fromFloat x) ++ "," ++ (String.fromFloat y) ++ ") " ++ (funcToString f) ++ (opToString vs fs ss)
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
scaleVect (x,y) k = (k*x,k*y)

-- Converts a basic arithmetic function to a string
funcToString f = if abs((f 5 7)-12) <= 0.01 then "+" else
              if abs((f 7 5)-2) <= 0.01 then "-" else
              if abs((f 6 2)-3) <= 0.01 then "/" else
              if abs((f 6 2)-12) <= 0.01 then "*" else ""

--Dot product of two vectors
dotProd (x1,y1) (x2,y2) = x1*x2 + y1*y2

--Orthagonal projection of v onto the line spanned by u
orthProj v u = scaleVect u ((dotProd v u)/(dotProd u u))  

linearCombV1 (a,d) (b,e) (c,f) = (c*e-b*f)/(a*e-b*d) 
linearCombV2 (a,d) (b,e) (c,f) = (a*f-c*d)/(a*e-b*d) 

--Gets an element from a list at a specific index.
elemAt lst idx = 
  case (lst,idx) of
    ([],_) -> (0,0)
    ((x::xs),0) -> x
    ((x::xs),i) -> elemAt xs (i-1)
    
myRound x n = toFloat(round (x*(10^n)))/10^n 