module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JD exposing (field, Decoder, int, string)
import Http

-- HTTP


getMovies : Cmd Msg
getMovies =
  let
    url = "https://scotch-mvplayer-api.herokuapp.com/api/v1"
  in
    Http.send NewMovies (Http.get url movieListDecoder)


movieDecoder : Decoder Movie
movieDecoder =
  JD.map4 Movie
    (field "name" string)
    (field "trailer" string)
    (field "poster" string)
    (field "year" string)

movieListDecoder : Decoder (List Movie)
movieListDecoder =
  JD.list movieDecoder

-- MODEL

type alias Movie =
    {
        name: String,
        trailer: String,
        poster: String,
        year: String
    }

type alias Model =
    {
        movies: List Movie,
        selectedMovie: Maybe Movie
    }

init : ( Model, Cmd Msg )   
init = 
    ( Model [] Nothing, getMovies)

-- UPDATE

type Msg = SelectMovie (Maybe Movie)
    | NewMovies (Result Http.Error (List Movie))



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewMovies (Ok movies) ->
            ({ model | movies = movies, selectedMovie = List.head movies}, Cmd.none)
        NewMovies (Err _) ->
            (model, Cmd.none)
        SelectMovie movie ->
            ({ model | selectedMovie = movie }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

player: Model -> Html Msg
player model =
    case model.selectedMovie of
        Just movie ->
            iframe [ class "movie-player", src movie.trailer, width 520, height 325 ] []
        Nothing ->
            div [ class "empty-player" ] []

movie:  Model -> Movie -> Html Msg
movie model movie =
    li 
        [ class "movie-item", style [("backgroundImage", "url(" ++ movie.poster ++ ")")], onClick (SelectMovie (Just movie))] []

movieList: Model -> Html Msg
movieList model =        
    model.movies
        |> List.map (movie model)
        |>
            ul [ class "movie-list" ]


view: Model -> Html Msg
view model = 
    div [ class "movieplayer-wrapper" ]
        [   
            player model,
            movieList model
        ]

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }