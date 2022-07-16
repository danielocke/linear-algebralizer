type alias Theme =
    { background : E.Color
    , border : E.Color
    , text : E.Color
    , buttons : E.Color
    , visualizer : E.Color
    , hyperlinks : E.Color
    }

dark =
    { background = drkBlack, border = drkCoral, text = drkWhite, buttons = drkGrey, visualizer = drkWhite, hyperlinks = drkWhite}

light =
    { background = pureWhite, border = lgtOrange, text = lgtBlack, buttons = lgtBlue, visualizer = lgtWhite, hyperlinks = lgtBlack}

--Colour Palate
drkCoral = E.rgb255 0xF0 0x54 0x54
drkBlack = E.rgb255 0x22 0x28 0x31
drkGrey  = E.rgb255 0x53 0x53 0x53
drkWhite = E.rgb255 0xDD 0xDD 0xDD 

lgtOrange = E.rgb255 0xF9 0xA8 0x28
lgtWhite  = E.rgb255 0xEC 0xEC 0xEB
lgtBlue   = E.rgb255 0x37 0x91 0xAD
lgtBlack  = E.rgb255 0x2E 0x38 0x3F

linkColour = E.rgb255 0x63 0xB8 0xFF
pureWhite  = E.rgb255 0xFF 0xFF 0xFF

--Stores details pertaining to each theorem.
type alias Theorem =
    { 
      name : String
    , desc : String
    , link : List (String,String)
    }

--Defining theorems.
oneToOne = { name = "One-to-One Transformations", desc = "A transformation T:Rn→Rm is one-to-one if, for every vector b in Rm, the equation T(x)=b has at most one solution x in Rn.",link = [("More Info","https://textbooks.math.gatech.edu/ila/one-to-one-onto.html#:~:text=Definition(Onto%20transformations),solution%20x%20in%20R%20n%20."),("Examples","http://faculty.up.edu/wootton/discrete/section7.2.pdf"),("More Examples","https://tleise.people.amherst.edu/Math272Spring2013/PracticeProofs.pdf")]}
onto = { name = "Onto Transformations", desc = "A transformation T:Rn→Rm is onto if, for every vector b in Rm, the equation T(x)=b has at least one solution x in Rn.",link = [("More Info","https://textbooks.math.gatech.edu/ila/one-to-one-onto.html#:~:text=Definition(Onto%20transformations),solution%20x%20in%20R%20n%20."),("Examples","https://tleise.people.amherst.edu/Math272Spring2013/PracticeProofs.pdf"),("More Examples","http://faculty.up.edu/wootton/discrete/section7.2.pdf")]}
independence = { name = "Linear Independence", desc = "A set of vectors is said to be linearly dependent if there is a nontrivial linear combination of the vectors that equals the zero vector. If no such linear combination exists, then the vectors are said to be linearly independent. ",link = [("More Info","https://textbooks.math.gatech.edu/ila/linear-independence.html"),("Examples","https://www.math.uh.edu/~jiwenhe/math2331/lectures/sec1_7.pdf"),("More Examples","https://www.math.purdue.edu/files/academic/courses/2010spring/MA26200/4-5.pdf")]}
orthProj = { name = "Orthagonal Projection", desc = "An orthogonal projection is a projection for which the range and the null space are orthogonal subspaces.",link = [("More Info","https://textbooks.math.gatech.edu/ila/projections.html"),("Examples","https://cpb-us-w2.wpmucdn.com/sites.uml.edu/dist/e/98/files/2018/10/projections-rynnqo.pdf"),("More Examples","https://mast.queensu.ca/~math211/m211oh/m211oh58.pdf")]}
basis = { name = "Basis", desc = "A set B of vectors in a vector space V is called a basis if every element of V may be written in a unique way as a finite linear combination of elements of B.",link = [("More Info","https://en.wikipedia.org/wiki/Basis_(linear_algebra)"),("Examples","https://yutsumura.com/tag/basis/"),("More Examples","https://www.math.tamu.edu/~dallen/DistanceEd/Math640/chapter3/node11.html")]}
span = { name = "Span", desc = "The linear span of a set S of vectors, denoted span, is the smallest linear subspace that contains the set.",link = [("More Info","https://en.wikipedia.org/wiki/Linear_span"),("Examples","https://www.math.tamu.edu/~yvorobet/MATH304-2011C/Lect2-03web.pdf"),("More Examples","https://datacadamia.com/linear_algebra/span")]}
orthDecomp = { name = "Orthagonal Decomposition", desc = "The orthogonal decomposition of a vector  y in R^n  is the sum of a vector in a subspace  W of R^n  and a vector in the orthogonal complement W^perpindicular  to  W.",link = [("More Info","https://mathworld.wolfram.com/OrthogonalDecomposition.html#:~:text=The%20orthogonal%20decomposition%20of%20a,written%20uniquely%20in%20the%20form "),("Examples","https://services.math.duke.edu/~jdr/ila/projections.html"),("More Examples","https://julia.quantecon.org/tools_and_techniques/orth_proj.html")]}
vectOps = { name = "Vector Operations", desc = "Some vector operations include: Addition, Subtraction, Cross product.",link = [("More Info","https://www.britannica.com/science/vector-mathematics"),("Examples","https://www.math.drexel.edu/~jsa33/291Su201245/Practice/Vector_Operations_Ans.pdf"),("More Examples","https://www.math.princeton.edu/sites/default/files/2018-03/MAT201Sample%20Problems_1.pdf")]}
inverse = { name = "Inverse", desc = "The inverse of a matrix is obtained by dividing the adjugate of the given matrix by the determinant of the given matrix.",link = [("More Info","https://www.mathsisfun.com/algebra/matrix-inverse.html"),("Examples","http://faculty.up.edu/wootton/discrete/section7.2.pdf"),("More Examples","https://yutsumura.com/linear-algebra/inverse-matrices/")]}
linearTransf = { name = "Linear Transformations", desc = "A linear transformation is a function from one vector space to another that respects the underlying (linear) structure of each vector space.",link = [("More Info","https://brilliant.org/wiki/linear-transformations/"),("Examples","https://web.ma.utexas.edu/users/olenab/Fall-2011-341/341lintranssols.pdf"),("More Examples","https://courses.math.tufts.edu/math70/Worksheet%20Solutions/Chapter1Solns/sec1_9new%20-%20Sol.pdf")]}
theoremList : List Theorem
theoremList = [oneToOne, onto, independence, orthProj, basis, span, orthDecomp, vectOps, inverse, linearTransf]

type alias Model =
    { time : Float
    , startTime : Float
    , width : Int
    , height : Int
    , widgetModel : Widget.Model 
    , state : State
    , theorems : List Theorem
    , selected : Int
    , showList : Bool
    , fullscreen : Bool
    , visualizerMsg : String
    , inputA : String
    , inputB : String
    , inputC : String
    , inputD : String
    , inputE : String
    , inputVectorA : (Float,Float)
    , inputVectorB : (Float,Float)
    , inputMatrix : ((Float, Float),(Float, Float))
    , inputScales: List Float
    , inputOps: List (Float->Float->Float)
    , theme : Theme
    , showHelp : Bool
    , allowErrorMessage : Bool
    }

type State
    = Animating
    | NotAnimating

type Msg
    = WindowResize Int Int -- update the size of the window
    | ToggleList
    | ToggleTheme
    | Tick Float
    | Select Int
    | StartAnimation String 
    | TakeInputA String 
    | TakeInputB String
    | TakeInputC String
    | TakeInputD String
    | TakeInputE String
    | SubmitInputVect
    | SubmitInputMatrix
    | SubmitOrth
    | SubmitInputOps
    | ToggleVisualizerFullscreen -- Fullscreen on/off
    | SetMsg String -- Update the visualizer message used to control the display
    | ToggleHelp -- Help screen on/off


--Initializing a graphicsSVG widgit for the visual components.
initVisualizer = Widget.init 192 128 "Visualizer"

--Declaring starting values.
initialModel : Model
initialModel =
    { time = 0
    , startTime = 0
    , width = 600
    , height = 450
    , widgetModel = Tuple.first initVisualizer
    , state = NotAnimating
    , theorems = theoremList
    , selected = 0
    , showList = False
    , fullscreen  = False
    , visualizerMsg = ""
    , inputA = ""
    , inputB = ""
    , inputC = ""
    , inputD = ""
    , inputE = ""
    , inputVectorA = (6,6)
    , inputVectorB = (6,6)
    , inputMatrix = ((11,11),(11,11))
    , inputScales = []
    , inputOps = []
    , theme = dark
    , showHelp = False
    , allowErrorMessage = False
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ({ model | width = width, height = height }
            , Cmd.none)
        Tick t ->
            ({ model | time = (if model.startTime /= 0 then t else 0), startTime = (if model.startTime == 0 then t*1000 else model.startTime)}, Cmd.none)
        Select i ->
            ({ model | selected = i, state = NotAnimating, visualizerMsg = "",inputA = "", inputB = "",inputC = "",inputD = "",inputE = "", inputVectorA = (6,6),inputVectorB = (6,6), inputMatrix = ((11,11),(11,11)), allowErrorMessage = False}, Cmd.none)
        ToggleList ->
            ({ model | showList = not model.showList, showHelp = False
            }, Cmd.none)
        ToggleVisualizerFullscreen -> ({ model | fullscreen = not model.fullscreen
            }, Cmd.none)
        ToggleTheme -> ({ model | theme = if model.theme == dark then light else dark
            }, Cmd.none)
        ToggleHelp -> ({ model | showHelp = not model.showHelp, showList = False
            }, Cmd.none)
        StartAnimation s -> ({model | state = Animating, startTime = 0, visualizerMsg = s}, Cmd.none)
        TakeInputA s -> ({model | inputA = s}, Cmd.none)
        TakeInputB s -> ({model | inputB = s}, Cmd.none)
        TakeInputC s -> ({model | inputC = s}, Cmd.none)
        TakeInputD s -> ({model | inputD = s}, Cmd.none)
        TakeInputE s -> ({model | inputE = s}, Cmd.none)
        SubmitInputVect -> ({model | inputVectorA = inputToVect model.inputA model.inputB, allowErrorMessage = True}, Cmd.none)
        SubmitInputMatrix -> ({model |state = Animating, startTime = 0, inputMatrix = inputToMatrix model.inputA model.inputB model.inputC model.inputD, allowErrorMessage = True}, Cmd.none)
        SetMsg s -> ({model | visualizerMsg = s}, Cmd.none)
        SubmitOrth -> ({model |state = Animating, startTime = 0, inputVectorA = inputToVect model.inputA model.inputB, inputVectorB = inputToVect model.inputC model.inputD, allowErrorMessage = True}, Cmd.none)
        SubmitInputOps -> ({model | inputScales = inputToScale model.inputA model.inputC model.inputE, inputOps = inputToOp model.inputB model.inputD, allowErrorMessage = True}, Cmd.none)
              
view : Model -> Browser.Document Msg
view model = browserView model

--Elm UI elements.
browserView : Model -> Browser.Document Msg
browserView model =
      
      let displayPort = E.row 
                                      [
                                        E.height E.fill 
                                      , E.width (E.fillPortion 8)
                                      , E.padding 10
                                      ]
                                      [
                                        E.row 
                                          [
                                            E.height E.fill 
                                          , E.width  E.fill
                                          , Background.color model.theme.visualizer
                                          , Border.rounded 10
                                          , Border.width 5
                                          , Border.color model.theme.border
                                          , Events.onClick ToggleVisualizerFullscreen
                                          ]
                                          [
-----------------------------------------------------------------------------------------------------
                                            --Visualizer
                                            E.html (Widget.view model.widgetModel 
                                              (  
                                                case model.selected of
                                                  0 -> (case model.visualizerMsg of
                                                          "example" -> Locked3.L4G4OneToOne.example model
                                                          "counter" -> Locked3.L4G4OneToOne.counterExample model
                                                          _ -> [awaitingInputText]) 
                                                          
                                                  1 -> (case model.visualizerMsg of
                                                          "example" -> Locked3.L4G4Onto.example model
                                                          "counter" -> Locked3.L4G4Onto.counterExample model
                                                          _ -> [awaitingInputText])
                                                          
                                                  2 -> doubleVectorInputInputConditions model (Eckardta.LinIndependants.visualizerWithInput model (model.inputVectorA, model.inputVectorB))
                                                       
                                                  3 -> doubleVectorInputInputConditions model (Locked3.L4G4OrthogonalProjection.myShapes model model.inputVectorA model.inputVectorB)
                                                  
                                                  4 -> if model.inputVectorA == (6.0,6.0) 
                                                       then Locked3.L4G4Basis.visualizerNoInput model
                                                       else Locked3.L4G4Basis.visualizerWithInput model model.inputVectorA
                                                       
                                                  5 -> (case model.visualizerMsg of
                                                        "1Vect" -> Locked3.L4G4Span.visualizerLine model
                                                        "2VectInd" -> Locked3.L4G4Span.visualizerPlane model
                                                        "2VectDep" -> Locked3.L4G4Span.visualizerLine2 model
                                                        _ -> [awaitingInputText])
                                                  6 -> singleVectorInputInputConditions model (Locked3.L4G4OrthogonalDecomposition.myShapes model model.inputVectorA)
                                                  7 -> opsInputInputConditions model (Locked3.L4G4VectorOperations.visualizerOps model model.inputOps model.inputScales)
                                                  8 -> (case model.visualizerMsg of
                                                          "example1" -> Locked3.L4G4Inverses.example1 model
                                                          "example2" -> Locked3.L4G4Inverses.example2 model
                                                          _ -> [awaitingInputText])
                                                          
                                                  9 -> matrixInputInputConditions model (Locked3.L4G4LinearTransformations.gridWithInput model model.inputMatrix) ((Locked3.L4G4LinearTransformations.gridNoInput model) |> group)


                                                  _ -> []
                                              )
                                            )
                                          ]
                                        ] 
                        
                        in
      
      if model.fullscreen then
         { title = "Linear Algalizer"
          , body = 
            [
                E.layout [
                  Font.family[Font.monospace]
                ] 
              (
                    displayPort
              )]
         }
        
      else
        
          { title = "Linear Algalizer"
          , body = [E.layout [Font.family[Font.monospace]]
                      (
                        --Full screen
                        E.row
                          [
                            E.width E.fill
                          , E.height E.fill
                          ]
                          [
                            --Title bar
                            E.column
                              [ 
                                E.height E.fill
                              , E.width (E.fillPortion 2)
                              , Background.color model.theme.background
                              ]
                              [
                                --Title
                                E.row
                                  [
                                    E.height (E.fillPortion 1)
                                  , E.width E.fill
                                  , E.padding 15
                                  , Font.color model.theme.text
                                  , Font.extraBold
                                  , Font.size 24
                                  ]
                                  [
                                    E.el
                                      [
                                        E.width (E.fillPortion 20)
                                      , E.height E.fill
                                      , E.padding 10
                                      , Font.extraBold
                                      , Font.size 18
                                      ]
                                      (
                                        E.text ("LINEAR ALGEBRALIZER"  ++ 
                                        (if model.showList == False
                                         then
                                          " - " 
                                          ++ 
                                          (case (elemAt model.theorems model.selected) of
                                           Just thrm -> thrm.name
                                           Nothing -> "")
                                         else ""))
                                      )
                                    --Info bubble
                                  , E.el
                                      [
                                          E.width (E.fillPortion 1)
                                        , E.height E.fill
                                        , E.paddingXY 0 2
                                        , Font.extraBold
                                        , Font.size 24
                                        , Font.center
                                        , Border.width 5
                                        , Border.color model.theme.border
                                        , Border.rounded 100
                                        , Events.onClick ToggleHelp

                                      ]
                                        (
                                          E.text "?"
                                        )
                                  , E.el
                                      [
                                          E.width (E.fillPortion 1)
                                        , E.height E.fill
                                        , E.paddingXY 0 2
                                        , Font.extraBold
                                        , Font.size 24
                                        , Font.center
                                        , Border.width 5
                                        , Border.color model.theme.border
                                        , Border.rounded 100
                                        , Events.onClick ToggleList
                                      ]
                                      (
                                        E.text (if model.showList then ">" else "<")
                                      )
                                  ,
                                  E.el
                                      [
                                          E.width (E.fillPortion 1)
                                        , E.height E.fill
                                        , E.paddingXY 0 2
                                        , Font.extraBold
                                        , Font.size 24
                                        , Font.center
                                        , Border.width 5
                                        , Border.color model.theme.border
                                        , Border.rounded 100
                                        , Events.onClick ToggleTheme

                                      ]
                                      (
                                          E.text (if model.theme == dark then "☀" else "☽")
                                      )

                                  ]
                                --Center row
                              , E.row 
                                  [
                                    E.height (E.fillPortion 8) 
                                  , E.width E.fill
                                  ]
                                  [
                                    --Visualizer background
                                    displayPort
                                    --Description
                                  , E.el 
                                      [
                                        E.height (E.px (model.height // 14 * 8)) 
                                      , E.width (E.fillPortion 4)
                                      , E.padding 10
                                      ]
                                      (
                                        E.paragraph 
                                          [
                                            E.width E.fill
                                          , E.height E.fill
                                          , Font.color model.theme.text
                                          , Font.size 16
                                          , Font.center
                                          ]
                                          [
                                            E.text (case (elemAt model.theorems model.selected) of
                                              Nothing -> ""
                                              Just thrm -> thrm.desc)
                                          ]
                                      )
                                  ]

-----------------------------------------------------------------------------------------------------
                                -- Visualizer Button Section
                              , E.row 
                                  [
                                    E.height (E.fillPortion 3) 
                                  , E.width E.fill
                                  , E.spacing 15
                                  , E.paddingXY 10 0
                                  ]
                                  (case model.selected of
                                                  0 -> [
                                                        E.column 
                                                          [
                                                            Font.color model.theme.text
                                                          , E.padding 10
                                                          , E.spacing 5
                                                          , Font.size 14
                                                          ]
                                                          [
                                                            E.row 
                                                              [E.spacing 10]
                                                              [
                                                                E.el
                                                                  [
                                                                    Events.onClick (StartAnimation "example")
                                                                  , Border.width 2
                                                                  , Border.color model.theme.text
                                                                  , E.padding 2
                                                                  ]
                                                                  (E.text "Example")
                                                               ,E.el
                                                                  [
                                                                    Events.onClick (StartAnimation "counter")
                                                                  , Border.width 2
                                                                  , Border.color model.theme.text
                                                                  , E.padding 2
                                                                  ]
                                                                  (E.text "CounterExample")
                                                              ]
                                                          , E.text "When the points overlap, the function is not one-to-one"
                                                          ] 
                                                       ] 
                                                  1 -> [
                                                          E.column 
                                                          [
                                                            Font.color model.theme.text
                                                          , E.padding 10
                                                          , E.spacing 5
                                                          , Font.size 14
                                                          ]
                                                          [
                                                            E.row 
                                                              [E.spacing 10]
                                                              [
                                                                E.el
                                                                  [
                                                                    Events.onClick (StartAnimation "example")
                                                                  , Border.width 2
                                                                  , Border.color model.theme.text
                                                                  , E.padding 2
                                                                  ]
                                                                  (E.text "Example")
                                                               ,E.el
                                                                  [
                                                                    Events.onClick (StartAnimation "counter")
                                                                  , Border.width 2
                                                                  , Border.color model.theme.text
                                                                  , E.padding 2
                                                                  ]
                                                                  (E.text "CounterExample")
                                                              ]
                                                          , E.text "When points are confined to a line it's not onto"
                                                          ]
                                                       ]
                                                  2 -> [
                                                        E.row
                                                        [
                                                          Font.color model.theme.text
                                                        , Font.size 12
                                                        ]
                                                        [
                                                          E.text "Check if Vector ("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputA s)
                                                          , text = model.inputA
                                                          , placeholder = Just (Input.placeholder [] (E.text "x"))
                                                          , label = Input.labelHidden "Vector x"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputB s)
                                                          , text = model.inputB
                                                          , placeholder = Just (Input.placeholder [] (E.text "y"))
                                                          , label = Input.labelHidden "Vector y"
                                                          }
                                                        , E.text ") is linearly independent from Vector ("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputC s)
                                                          , text = model.inputC
                                                          , placeholder = Just (Input.placeholder [] (E.text "x"))
                                                          , label = Input.labelHidden "Line x"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputD s)
                                                          , text = model.inputD
                                                          , placeholder = Just (Input.placeholder [] (E.text "y"))
                                                          , label = Input.labelHidden "Line y"
                                                          }
                                                        , E.text ")"
                                                        , E.el
                                                          [
                                                            Font.color model.theme.text
                                                          , Border.width 2 
                                                          , E.padding 4
                                                          , Border.color model.theme.text
                                                          , Events.onClick SubmitOrth
             
                                                          ]
                                                          (E.text "GO!")
                                                          ]
                                                       ]
                                                  3 -> [
                                                        E.row
                                                        [
                                                          Font.color model.theme.text
                                                        , Font.size 12
                                                        ]
                                                        [
                                                          E.text "Project ("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputA s)
                                                          , text = model.inputA
                                                          , placeholder = Just (Input.placeholder [] (E.text "x"))
                                                          , label = Input.labelHidden "Vector x"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputB s)
                                                          , text = model.inputB
                                                          , placeholder = Just (Input.placeholder [] (E.text "y"))
                                                          , label = Input.labelHidden "Vector y"
                                                          }
                                                        , E.text ") onto the line spanned by ("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputC s)
                                                          , text = model.inputC
                                                          , placeholder = Just (Input.placeholder [] (E.text "x"))
                                                          , label = Input.labelHidden "Line x"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputD s)
                                                          , text = model.inputD
                                                          , placeholder = Just (Input.placeholder [] (E.text "y"))
                                                          , label = Input.labelHidden "Line y"
                                                          }
                                                        , E.text ")"
                                                        , E.el
                                                          [
                                                            Font.color model.theme.text
                                                          , Border.width 2 
                                                          , E.padding 4
                                                          , Border.color model.theme.text
                                                          , Events.onClick SubmitOrth
             
                                                          ]
                                                          (E.text "GO!")
                                                          ]
                                                       ]
                                                  4 -> [
                                                        E.row
                                                        [
                                                          Font.color model.theme.text
                                                        , Font.size 12
                                                        ]
                                                        [
                                                          E.text "Enter a vector to see how it can be formed by the basis vectors: ("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputA s)
                                                          , text = model.inputA
                                                          , placeholder = Just (Input.placeholder [] (E.text "x"))
                                                          , label = Input.labelHidden "Vector x"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputB s)
                                                          , text = model.inputB
                                                          , placeholder = Just (Input.placeholder [] (E.text "y"))
                                                          , label = Input.labelHidden "Vector y"
                                                          }
                                                        , E.text ")"
                                                        , E.el
                                                          [
                                                            Font.color model.theme.text
                                                          , Border.width 2 
                                                          , E.padding 4
                                                          , Border.color model.theme.text
                                                          , Events.onClick SubmitInputVect
             
                                                          ]
                                                          (E.text "GO!")
                                                        ]
                                                       ]
                                                  5 -> [
                                                        E.column 
                                                          [
                                                            Font.color model.theme.text
                                                          , E.padding 10
                                                          , E.spacing 5
                                                          , Font.size 14
                                                          ]
                                                          [
                                                            E.text "See how the span (highlighted region) is affected by a set of vectors:"
                                                          , E.row 
                                                              [E.spacing 10]
                                                              [
                                                                E.el
                                                                  [
                                                                    Font.color model.theme.text
                                                                  , Border.width 2 
                                                                  , E.padding 4
                                                                  , Border.color model.theme.text
                                                                  , Events.onClick (SetMsg "1Vect")
                                                                  , Font.size 14
                                                                  ]
                                                                (E.column []
                                                                  [ 
                                                                    E.text "1 Vector"
                                                                  , E.text " "
                                                                  ]
                                                                )
                                                               ,E.el
                                                                  [
                                                                    Font.color model.theme.text
                                                                  , Border.width 2
                                                                  , E.padding 4
                                                                  , Border.color model.theme.text
                                                                  , Events.onClick (SetMsg "2VectInd")
                                                                  , Font.size 14
                                                                  ]
                                                                  (E.column []
                                                                    [ 
                                                                      E.text "2 Vectors"
                                                                    , E.text "(Independent)"
                                                                    ]
                                                                  )
                                                               ,E.el
                                                                  [
                                                                    Font.color model.theme.text
                                                                  , Border.width 2
                                                                  , E.padding 4
                                                                  , Border.color model.theme.text
                                                                  , Events.onClick (SetMsg "2VectDep")
                                                                  , Font.size 14
                                                                  ]
                                                                  (E.column []
                                                                    [ 
                                                                      E.text "2 Vectors"
                                                                    , E.text "(Dependent)"
                                                                    ]
                                                                  )
                                                                ]   
                                                              ]
                                                        ]
                                                  6 -> [
                                                        E.row
                                                        [
                                                          Font.color model.theme.text
                                                        , Font.size 12
                                                        ]
                                                        [
                                                          E.text "Enter a vector to see the orthongonal decompostion with the set W = {(-5,-6)}: "
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputA s)
                                                          , text = model.inputA
                                                          , placeholder = Just (Input.placeholder [] (E.text "x"))
                                                          , label = Input.labelHidden "Vector x"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputB s)
                                                          , text = model.inputB
                                                          , placeholder = Just (Input.placeholder [] (E.text "y"))
                                                          , label = Input.labelHidden "Vector y"
                                                          }
                                                        , E.text ")"
                                                        , E.el
                                                          [
                                                            Font.color model.theme.text
                                                          , Border.width 2 
                                                          , E.padding 4
                                                          , Border.color model.theme.text
                                                          , Events.onClick SubmitInputVect
             
                                                          ]
                                                          (E.text "GO!")
                                                        ]
                                                       ]
                                                  7 -> [
                                                        E.column
                                                        [
                                                          Font.color model.theme.text
                                                        , Font.size 12
                                                        ]
                                                        [
                                                           E.text "Fill in the blanks with scalars (must be smaller than or equal to 1) and (+/-) operations to see vector operations in action: "
                                                        ,  E.row
                                                            [
                                                              Font.color model.theme.text
                                                            , Font.size 12
                                                            ]
                                                            [
                                                              Input.text
                                                              [
                                                                Font.color drkBlack
                                                              , Border.width 2
                                                              , E.padding 4
                                                              , Border.color drkWhite
                                                              , E.width (E.px 30)
                                                              ]
                                                              {
                                                                onChange = (\s -> TakeInputA s)
                                                              , text = model.inputA
                                                              , placeholder = Just (Input.placeholder [] (E.text "C1"))
                                                              , label = Input.labelHidden "Constant 1"
                                                              }
                                                            , E.text "(0,3) "
                                                            , Input.text
                                                              [
                                                                Font.color drkBlack
                                                              , Border.width 2
                                                              , E.padding 4
                                                              , Border.color drkWhite
                                                              , E.width (E.px 30)
                                                              ]
                                                              {
                                                                onChange = (\s -> TakeInputB s)
                                                              , text = model.inputB
                                                              , placeholder = Just (Input.placeholder [] (E.text "+/-"))
                                                              , label = Input.labelHidden "Operation 1"
                                                              }
                                                            , E.text " "
                                                            , Input.text
                                                              [
                                                                Font.color drkBlack
                                                              , Border.width 2
                                                              , E.padding 4
                                                              , Border.color drkWhite
                                                              , E.width (E.px 30)
                                                              ]
                                                              {
                                                                onChange = (\s -> TakeInputC s)
                                                              , text = model.inputC
                                                              , placeholder = Just (Input.placeholder [] (E.text "C2"))
                                                              , label = Input.labelHidden "Constant 2"
                                                              }
                                                            , E.text "(2,1) "
                                                            , Input.text
                                                              [
                                                                Font.color drkBlack
                                                              , Border.width 2
                                                              , E.padding 4
                                                              , Border.color drkWhite
                                                              , E.width (E.px 30)
                                                              ]
                                                              {
                                                                onChange = (\s -> TakeInputD s)
                                                              , text = model.inputD
                                                              , placeholder = Just (Input.placeholder [] (E.text "+/-"))
                                                              , label = Input.labelHidden "Operation 2"
                                                              }
                                                            , E.text " "
                                                            , Input.text
                                                              [
                                                                Font.color drkBlack
                                                              , Border.width 2
                                                              , E.padding 4
                                                              , Border.color drkWhite
                                                              , E.width (E.px 30)
                                                              ]
                                                              {
                                                                onChange = (\s -> TakeInputE s)
                                                              , text = model.inputE
                                                              , placeholder = Just (Input.placeholder [] (E.text "C3"))
                                                              , label = Input.labelHidden "Constant 3"
                                                              }
                                                            , E.text "(-3,-1) "
                                                            , E.el
                                                              [
                                                                Font.color model.theme.text
                                                              , Border.width 2 
                                                              , E.padding 4
                                                              , Border.color model.theme.text
                                                              , Events.onClick SubmitInputOps
             
                                                              ]
                                                              (E.text "GO!")
                                                            ]
                                                        ]
                                                      ]
                                                        
                                                  8 -> [
                                                        E.column 
                                                          [
                                                            Font.color model.theme.text
                                                          , E.padding 10
                                                          , E.spacing 5
                                                          , Font.size 14
                                                          ]
                                                          [
                                                            E.row 
                                                              [E.spacing 10]
                                                              [
                                                                E.el
                                                                  [
                                                                    Events.onClick (StartAnimation "example1")
                                                                  , Border.width 2
                                                                  , Border.color model.theme.text
                                                                  , E.padding 2
                                                                  ]
                                                                  (E.text "Example 1")
                                                               ,E.el
                                                                  [
                                                                    Events.onClick (StartAnimation "example2")
                                                                  , Border.width 2
                                                                  , Border.color model.theme.text
                                                                  , E.padding 2
                                                                  ]
                                                                  (E.text "Example 2")
                                                              ]
                                                          , E.text "See how the red transformation and the inverse (blue) transformation relate"
                                                          ] 
                                                       ]
                                                  9 -> [
                                                        E.row
                                                        [
                                                          Font.color model.theme.text
                                                        , Font.size 12
                                                        ]
                                                        [
                                                          E.text "Enter a matrix of the form ((a,b),(c,d)): (("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputA s)
                                                          , text = model.inputA
                                                          , placeholder = Just (Input.placeholder [] (E.text "a"))
                                                          , label = Input.labelHidden "Matrix a"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputB s)
                                                          , text = model.inputB
                                                          , placeholder = Just (Input.placeholder [] (E.text "b"))
                                                          , label = Input.labelHidden "Matrix b"
                                                          }
                                                        , E.text "),("
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputC s)
                                                          , text = model.inputC
                                                          , placeholder = Just (Input.placeholder [] (E.text "c"))
                                                          , label = Input.labelHidden "Matrix c"
                                                          }
                                                        , E.text ","
                                                        , Input.text
                                                          [
                                                            Font.color drkBlack
                                                          , Border.width 2
                                                          , E.padding 4
                                                          , Border.color drkWhite
                                                          , E.width (E.px 30)
                                                          ]
                                                          {
                                                            onChange = (\s -> TakeInputD s)
                                                          , text = model.inputD
                                                          , placeholder = Just (Input.placeholder [] (E.text "d"))
                                                          , label = Input.labelHidden "Matrix d"
                                                          }
                                                        , E.text ")) "  
                                                        , E.el
                                                          [
                                                            Font.color model.theme.text
                                                          , Border.width 2 
                                                          , E.padding 4
                                                          , Border.color model.theme.text
                                                          , Events.onClick SubmitInputMatrix            
                                                          ]
                                                          (E.text "TRANSFORM!")
                                                        ]
                                                       ]
                                                  _ -> [])

                                --Link section
                              , E.column 
                                  [
                                    E.height (E.fillPortion 1) 
                                  , E.width E.fill
                                  
                                  ]
                                  [
                                    E.row 
                                      [
                                        E.height E.fill
                                      , E.width E.fill
                                      , Background.color model.theme.border
                                      ]
                                      (getLinks (case (elemAt model.theorems model.selected) of
                                          Nothing -> []
                                          Just({link})-> link))
                                  ]
                                --Bottom row
                              , E.column 
                                  [
                                    E.height (E.fillPortion 1) 
                                  , E.width E.fill
                                  ]
                                  []
                              ]

                          , if model.showList then
                            --Theorem selector
                            E.row
                              [
                                E.height E.fill
                              , E.width (E.fillPortion 1) 
                              , Background.color model.theme.background
                              ]
                              [
                                E.el
                                  [
                                    E.width  E.fill
                                  ]
                                  (
                                    E.indexedTable
                                      [
                                        Font.color model.theme.text
                                      , E.height (E.px model.height)
                                      , Font.size 24
                                      , E.spacing 5
                                      , E.scrollbarY
                                      ]
                                      {
                                        data    = model.theorems
                                      , columns = 
                                          [
                                            {
                                              header = E.none
                                            , width = E.fill
                                            , view = \idx theorem -> E.el
                                                          [ E.width (E.px 100)
                                                          , E.height (E.px 40)
                                                          , E.paddingXY 0 7
                                                          , Font.center
                                                          , if (model.selected == idx) then Background.color model.theme.border
                                                                                       else Background.color model.theme.buttons
                                                          , Font.color model.theme.text
                                                          , Border.rounded 5
                                                          , Events.onClick (Select idx)
                                                          ]
                                                          (E.text theorem.name) 

                                            }
                                          ]
                                      }
                                  )
                              ]
                          else
                               E.none
                          , 
                          if model.showHelp then 
                            E.paragraph 
                           
                                          [
                                            E.width E.fill
                                          , E.height E.fill
                                          , Font.color model.theme.text
                                          , Font.size 18
                                          , Font.center
                                          , Background.color model.theme.buttons
                                          ]
                                          [
                                            E.text "Welcome to the LINEAR ALGEBRALIZER where we specialize in ... you guessed it, linear algebra! and ... dark mode! We also offer a boring light mode if you so wish to use. Moving on to the good stuff, this website visualizes different linear algebra theorems so that it is easy to grasp theorems. There are many examples to choose and learn from! In addition, there are links to other resources which you may look at if further explanation is needed. To select a topic please press the < or > buttons in the top right to expand/hide the list of theorems. More over, if you want to see the graphs on a bigger screen, just click the graph! It'll expand out and if you want to make it smaller, just click it again."
                                          ]

                          else
                            E.none
                          ]

                      )
                  ]
          }





-----------------------------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-----------------------------------------------------------------------------------------------------
doubleVectorInputInputConditions model toShow =
   if model.allowErrorMessage then
       if model.inputVectorA == (7.0,7.0) || (model.inputVectorB == (7.0,7.0))
       then [errorMessage "Too Big" "One of the inputted vectors has a component that exceeds magnitude 5" model]
       else if model.inputVectorA == (6.0,6.0) || model.inputVectorB == (6.0,6.0) 
       then [errorMessage "No Input" "There is at least one empty or non numerical submitted component" model]
       else toShow
    else
      [awaitingInputText]
      
singleVectorInputInputConditions model toShow =
   if model.allowErrorMessage then
       if model.inputVectorA == (7.0,7.0)
       then [errorMessage "Too Big" "One of the inputted component exceeds magnitude 5" model]
       else if model.inputVectorA == (6.0,6.0)
       then [errorMessage "No Input" "There is at least one empty or non numerical submitted component" model]
       else toShow
    else
      [awaitingInputText]
      
matrixInputInputConditions model toShow ifFailShow =
   if model.allowErrorMessage then
       if model.inputMatrix == ((12,12),(12,12))
       then [errorMessage "Too Big" "One of the inputted component exceeds magnitude 10" model]
       else if model.inputMatrix == ((11,11),(11,11))
       then [errorMessage "No Input" "There is at least one empty or non numerical submitted component" model]
       else toShow
    else
      [ifFailShow]
opsInputInputConditions model toShow =
   if model.allowErrorMessage then
       if model.inputScales == [0]
       then [errorMessage "Too Big" "One of the inputted scalars exceeds magnitude 1" model]
       else if model.inputOps == [] || model.inputScales == []
       then [errorMessage "No Input" "There is at least one empty or invalid submitted component" model]
       else toShow
    else
      [awaitingInputText]

awaitingInputText = text "Awaiting input..." |> centered |> filled darkGray

--error message 
errorMessage head txt model =
      group [
      text head
        |> centered
        |> filled red
        |> scale 2
        |> move (0, 0)

      ,text txt
        |> centered
        |> filled red
        |> scale 0.5
        |> move (0, -10)
      ]

-- Boolean inverter
not x = if x then False else True

-- Creates link elements from a list of urls.
getLinks links = case links of
                  [] -> []
                  ((l,u)::ls) -> (E.newTabLink
                                   [
                                     E.padding 10
                                   , Font.color pureWhite
                                   , Font.underline
                                   , Font.size 16
                                   ]
                                   {
                                     url = u
                                   , label = E.text l
                                   }) :: getLinks ls
                  
--Gets an element from a list at a specific index.
elemAt lst idx = 
  case (lst,idx) of
    ([],_) -> Nothing
    ((x::xs),0) -> Just x
    ((x::xs),i) -> elemAt xs (i-1) 

--Converts string input to a vector.
inputToVect xStr yStr = 
  case (String.toFloat xStr, String.toFloat yStr) of
    (Nothing,_) -> (6,6) --denotes no input
    (_,Nothing) -> (6,6)
    (Just x,Just y) -> if abs x > 5 || abs y > 5 then (7,7) else (x,y) --7, 7 denotes too large input
    
inputToOp str1 str2 = case (str1,str2) of
  ("+","+") -> [(+),(+)]
  ("-","-") -> [(-),(-)]
  ("+","-") -> [(+),(-)]
  ("-","+") -> [(-),(+)]
  _         -> []
  
inputToScale s1 s2 s3 = case (String.toFloat s1, String.toFloat s2,String.toFloat s3) of
  (Just a,Just b,Just c) -> if a > 1||b > 1||c > 1 then [0] else [a,b,c]
  _                      -> []
  

--Converts string input to a matrix.
inputToMatrix aStr bStr cStr dStr = 
  case ((String.toFloat aStr, String.toFloat bStr),(String.toFloat cStr, String.toFloat dStr)) of
    ((Just a,Just b),(Just c, Just d)) -> if abs a > 10 || abs b > 10 || abs c > 10 || abs d > 10 then ((12,12),(12,12)) else ((a,b),(c,d))
    _ -> ((11,11),(11,11))
    
main : Program () Model Msg
main =
    Browser.document
        { init = \ flags -> (initialModel, Task.perform ( \ vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height)) Dom.getViewport)
        , view = view
        , update = update
        , subscriptions = \ model -> case model.state of 
                                        NotAnimating -> Browser.onResize WindowResize
                                        Animating ->
                                            Sub.batch 
                                                [ Browser.onResize WindowResize
                                                , Browser.onAnimationFrame (Time.posixToMillis 
                                                                            >> toFloat 
                                                                            >> (\ t -> 0.001 * (t - model.startTime)) 
                                                                            >> Tick)
                                                ]
        }