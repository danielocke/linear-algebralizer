finalX = 10
finalY = -5

myShapes model =
  [
    grid
    |> move (30,0)
  ,List.map2 (plot model red 10) (listToFloat vectors) (List.map (transform oneToOne) (listToFloat vectors))
    |> group
    |> move (30,0)
  ,[
    text "T = "
      |> centered
      |> filled black
      |> move (-26,-4)
    ,text "┌        ┐"
      |> centered
      |> filled black
      |> move (0,14)
   ,text "│0.5 0│"
      |> centered
      |> filled black
      |> move (0,2)
   ,text "│2    0│"
      |> centered
      |> filled black
      |> move (0,-10)
   ,text "└        ┘"
      |> centered
      |> filled black
      |> move (0,-22)
   ]
   |> group
   |> move (-60,0)
  ]
example model =
  [
    grid
    |> move (30,0)
  ,List.map2 (plot model red 10) (listToFloat vectors) (List.map (transform oneToOne) (listToFloat vectors))
    |> group
    |> move (30,0)
  ,[
    text "T = "
      |> centered
      |> filled black
      |> move (-26,-4)
    ,text "┌        ┐"
      |> centered
      |> filled black
      |> move (0,14)
   ,text "│1   1 │"
      |> centered
      |> filled black
      |> move (0,2)
   ,text "│1 0.5│"
      |> centered
      |> filled black
      |> move (0,-10)
   ,text "└        ┘"
      |> centered
      |> filled black
      |> move (0,-22)
   ]
   |> group
   |> move (-60,0)
  ]
counterExample model =
  [
    grid
    |> move (30,0)
  ,List.map2 (plot model red 10) (listToFloat vectors) (List.map (transform notOneToOne) (listToFloat vectors))
    |> group
    |> move (30,0)
  ,[
    text "T = "
      |> centered
      |> filled black
      |> move (-26,-4)
    ,text "┌        ┐"
      |> centered
      |> filled black
      |> move (0,14)
   ,text "│0.5 0│"
      |> centered
      |> filled black
      |> move (0,2)
   ,text "│2    0│"
      |> centered
      |> filled black
      |> move (0,-10)
   ,text "└        ┘"
      |> centered
      |> filled black
      |> move (0,-22)
   ]
   |> group
   |> move (-60,0)
  ]

type Msg = Tick Float GetKeyState

type alias Model = { time : Float }

vectors = cartesianProduct (List.range -3 3) (List.range -3 3)

oneToOne = ((1,1),(1,0.5))
notOneToOne = ((0.5,0),(2,0))


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

view model = collage 192 128 (myShapes model)

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
    circle 2
      |> filled clr
      |> move (if model.time*2 < scale then model.time *2* (xf-xi) + xi*scale else xf * scale,
               if model.time*2 < scale then model.time *2* (yf-yi) + yi*scale else yf * scale)
      
