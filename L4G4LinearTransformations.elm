finalX = 10
finalY = -5

inputMatrix = ((2,2),(-2,3))

gridNoInput model=
  [
    grid
    |> move (30,0)
  ,List.map2 (plot model red 10) (listToFloat vectors) (listToFloat vectors)
    |> group
    |> move (30,0)
  ,[
    text "T = "
      |> centered
      |> fixedwidth
      |> filled black
      |> move (-26,0)
   ,text "┌   ┐"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,18)
   ,text "│a b│"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,6)
   ,text "│c d│"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,-6)
   ,text "└   ┘"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,-18)
   ]
   |> group
   |> move (-50,0)
  ]

gridWithInput model ((a,b),(c,d)) =
  [
    grid
    |> move (30,0)
  ,List.map2 (plot model red 10) (listToFloat vectors) (List.map (transform ((a,b),(c,d))) (listToFloat vectors))
    |> group
    |> move (30,0)
  ,[
    text ("a = " ++ fltToStr a)
      |> fixedwidth
      |> filled black
      |> move (-26,45)
   ,text ("b = " ++ fltToStr b)
      |> fixedwidth
      |> filled black
      |> move (-26,30)
   ,text ("c = " ++ fltToStr c)
      |> fixedwidth
      |> filled black
      |> move (-26,-30)
   ,text ("d = " ++ fltToStr d)
      |> fixedwidth
      |> filled black
      |> move (-26,-45)
   ]
     |> group
     |> move (-62,0)
  ,[
    text "T = "
      |> centered
      |> fixedwidth
      |> filled black
      |> move (-26,0)
   ,text "┌   ┐"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,18)
   ,text "│a b│"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,6)
   ,text "│c d│"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,-6)
   ,text "└   ┘"
      |> centered
      |> fixedwidth
      |> filled black
      |> move (0,-18)
   ]
   |> group
   |> move (-50,0)
  ]

type Msg = Tick Float GetKeyState

type alias Model = { time : Float }

vectors = cartesianProduct (List.range -3 3) (List.range -3 3)

oneToOne = ((1,1),(1,0.5))
notOneToOne = ((0.5,0),(2,0))

onto = ((1,1),(0,2))
notOnto = ((2,0),(0,0))



cartesianProduct listX ys = case listX of 
             [] -> []
             (x::xs) -> (makeVectsFromX x ys) ++ (cartesianProduct xs ys)
             
makeVectsFromX x list = case list of
                        [] -> [] 
                        (y::ys) -> (x,y) :: (makeVectsFromX x ys)


listToFloat list = case list of
                   [] -> []   
                   ((x,y)::ls) -> (toFloat x,toFloat y) :: listToFloat ls

transform ((a,b),(c,d)) (x,y) = (a*x+b*y, c*x+d*y)

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (gridWithInput model inputMatrix)

grid = group
  [
    List.map (lineX gray) 
      (List.range -6 6)
      |> group
  , List.map (lineY gray) 
      (List.range -6 6)
      |> group
  , lineX black 0
  , lineY black 0
  ]

lineX clr y =
    line (-60,toFloat y*10)(60,toFloat y*10)
      |> outlined (solid 1) clr
lineY clr x =
    line (toFloat x*10,-60)(toFloat x*10,60)
      |> outlined (solid 1) clr
      
plot model clr scale (xi,yi) (xf,yf) = 
    if (min (abs(model.time *2* (xf-xi) + xi*scale)) (abs(xf*scale)) > 60) || (min (abs(model.time *2* (yf-yi) + yi*scale)) (abs(yf*scale)) > 60)
    then circle 0 |> filled clr
    else
    circle 2
      |> filled clr
      |> move (if model.time*2 < scale then model.time *2* (xf-xi) + xi*scale else xf * scale,
               if model.time*2 < scale then model.time *2* (yf-yi) + yi*scale else yf * scale)
      
fltToStr f = String.fromFloat ((toFloat(round (f * 100))) / 100)
