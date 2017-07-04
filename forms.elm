import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (..)
import Regex exposing (..)

main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model = 
    { name : String
    , password : String
    , passwordAgain : String 
    , age : String 
    , errorMsg: List String }

model : Model
model = Model "" "" "" "" []

-- UPDATE

type Msg = 
    Name String | Password String | 
    PasswordAgain String | 
    Age String | ErrorMsg (List String)

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name -> 
            { model | name = name }
        Password password -> 
            { model | password = password }
        PasswordAgain password -> 
            { model | passwordAgain = password} 
        Age age ->
            { model | age = age }
        ErrorMsg errorMsg ->
            { model | errorMsg = errorMsg }

-- VIEW

view : Model -> Html Msg
view model = 
    div [ style [ ("padding", "10px")
                , ("margin", "0 auto")
                , ("margin-top", "100px")
                , ("border", "1px solid black")
                , ("border-radius", "5px")
                , ("width", "200px")    
                ]
        ]
        [ input [ style [ ("margin", "5px") ], type_ "text", placeholder "Name", onInput Name ] []
        , input [ style [ ("margin", "5px") ], type_ "password", placeholder "Password", onInput Password ] []
        , input [ style [ ("margin", "5px") ], type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , input [ style [ ("margin", "5px") ], type_ "text", placeholder "Age", onInput Age ] []
        , button [ style [ ("margin", "5px") ], onClick (ErrorMsg (viewValidation model)) ] [ text "Submit"] 
        , showValidation model
        ]

viewValidation : Model -> List String
viewValidation model =
    let message = 
        if model.password == model.passwordAgain then ""
        else "Passwords do not match"
    in
    let formatOK =
        if Regex.contains (regex "[A-Z]+[a-z]+[0-9]+") model.password then ""
        else "Need uppercase, lowercase, number"
    in
    let lengthOK = 
        if String.length model.password >= 8 then ""
        else "Minimum length: 8" 
    in 
    let ageOK = 
        if (Result.withDefault 0 (String.toInt model.age)) > 0 then "" 
        else "Age invalid"
    in [ message
       , formatOK
       , lengthOK
       , ageOK
       ]

showValidation : Model -> Html Msg
showValidation model =
    div [] (List.map divify model.errorMsg) 

divify : String -> Html Msg
divify string = div [ style [("color", "red"), ("font-size", "10px"), ("padding", "5px")] ] [ text string ]
