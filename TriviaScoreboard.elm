module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Set exposing (..)
import String exposing (..)
import Tuple exposing (..)


-- TODO: Block non-numbers from being entered as scores
-- Seal Team Six/The Dream Team/The A-Team/Team America World Police/Double Team
-- model


type alias Model =
    { teamName : String
    , teamToEdit : String
    , score : String
    , toggleInstructions : Bool
    , teamsAndScores : List (List String)
    }


initModel : Model
initModel =
    { teamName = ""
    , teamToEdit = ""
    , toggleInstructions = True
    , score = ""
    , teamsAndScores = []
    }



-- update


type Msg
    = EnterTeamName String
    | SelectTeamToEdit String
    | AddTeam
    | UpdateScoreString String
    | EnterAddScore
    | EnterEditScore
    | SortByTeamName
    | SortByTotalScore
    | DeleteTeam
    | ToggleInstructions


getNth : Int -> List String -> String
getNth n entry =
    let
        value =
            List.drop (n - 1) entry |> List.head
    in
        case value of
            Just value ->
                value

            Nothing ->
                ""


getNucleus : List (List String) -> List String
getNucleus entry =
    let
        result =
            List.head entry
    in
        case result of
            Just result ->
                result

            Nothing ->
                []


convertStringToInt : String -> Int
convertStringToInt s =
    let
        value =
            String.toInt s
    in
        case value of
            Ok value ->
                value

            _ ->
                0


getRank : Int -> Dict Int String -> String
getRank total ranks =
    let
        rank =
            Dict.get total ranks
    in
        case rank of
            Just rank ->
                rank

            _ ->
                ""


calculateTotal : String -> Int
calculateTotal scoreString =
    scoreString
        |> String.split " "
        |> List.map convertStringToInt
        |> List.sum


updateScore : String -> String -> String -> List (List String) -> List (List String)
updateScore appendOrOverwrite scoreString teamToEdit teamsAndScores =
    let
        remainingTeams =
            teamsAndScores
                |> List.filter (\e -> (getNth 1 e) /= teamToEdit)

        teamToUpdate =
            teamsAndScores
                |> List.filter (\e -> (getNth 1 e) == teamToEdit)
                |> getNucleus

        spaceOrNot =
            if List.length teamToUpdate == 1 then
                ""
            else
                " "

        replacedEntry =
            [ [ teamToEdit, scoreString ] ]

        appendedEntry =
            [ [ teamToEdit
              , (getNth 2 teamToUpdate)
                    ++ spaceOrNot
                    ++ scoreString
              ]
            ]
    in
        case appendOrOverwrite of
            "overwritescores" ->
                List.append remainingTeams replacedEntry

            "deleteteam" ->
                remainingTeams

            _ ->
                List.append remainingTeams appendedEntry


blockDuplicateTeam : String -> List (List String) -> List (List String)
blockDuplicateTeam teamName teamsAndScores =
    let
        listOfTeams =
            List.map (\e -> getNth 1 e) teamsAndScores
    in
        if List.member teamName listOfTeams then
            teamsAndScores
        else
            List.append teamsAndScores [ [ teamName ] ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnterTeamName s ->
            { model | teamName = s }

        AddTeam ->
            { model
                | teamsAndScores =
                    blockDuplicateTeam model.teamName
                        model.teamsAndScores
            }

        SelectTeamToEdit s ->
            { model | teamToEdit = s }

        UpdateScoreString s ->
            { model | score = s }

        EnterEditScore ->
            { model
                | teamsAndScores =
                    updateScore "overwritescores"
                        model.score
                        model.teamToEdit
                        model.teamsAndScores
            }

        EnterAddScore ->
            { model
                | teamsAndScores =
                    updateScore "addscore"
                        model.score
                        model.teamToEdit
                        model.teamsAndScores
            }

        SortByTeamName ->
            { model | teamsAndScores = List.sort model.teamsAndScores }

        SortByTotalScore ->
            { model
                | teamsAndScores =
                    List.sortBy
                        (\e -> getNth 2 e |> calculateTotal)
                        model.teamsAndScores
                        |> List.reverse
            }

        DeleteTeam ->
            { model
                | teamsAndScores =
                    updateScore "deleteteam"
                        ""
                        model.teamToEdit
                        model.teamsAndScores
            }

        ToggleInstructions ->
            { model | toggleInstructions = not model.toggleInstructions }



-- view


makeDropdown : List (List String) -> List (Html Msg)
makeDropdown teamsAndScores =
    teamsAndScores
        |> List.map (\e -> getNth 1 e)
        |> List.sort
        |> List.map (\s -> option [ value s ] [ text s ])


pageStyle =
    style
        [ ( "padding-left", "20px" )
        , ( "font-family", "Helvetica" )
        , ( "padding-top", "10px" )
        , ( "margin", "auto" )
        , ( "width", "90%" )
        ]


mainTableStyle =
    style
        [ ( "width", "100%" )
        ]


scoreTableStyle =
    style
        [ ( "width", "100%" )
        ]


scoreTdStyle =
    style
        [ ( "vertical-align", "top" )
        , ( "padding-top", "10px" )
        ]


controlTdStyle =
    style
        [ ( "width", "15%" )
        , ( "padding-right", "15px" )
        , ( "vertical-align", "top" )

        --         , ("a")
        ]


resultsTdStyleOne =
    style
        [ ( "border", "1px solid black" )
        , ( "padding", "8px" )
        , ( "background-color", "#00ffff" )
        ]


resultsTdStyleZero =
    style
        [ ( "border", "1px solid black" )
        , ( "padding", "8px" )
        , ( "background-color", "#00ff00" )
        ]


resultsTdStyleOneBold =
    style
        [ ( "border", "1px solid black" )
        , ( "padding", "8px" )
        , ( "background-color", "#00ffff" )
        , ( "font-weight", "bold" )
        ]


resultsTdStyleZeroBold =
    style
        [ ( "border", "1px solid black" )
        , ( "padding", "8px" )
        , ( "background-color", "#00ff00" )
        , ( "font-weight", "bold" )
        ]


sortButtonStyle =
    [ ( "border-color", "#337ab7" )
    , ( "background-color", "#337ab7" )
    , ( "color", "#ffffff" )
    , ( "margin-top", "6px" )
    ]


addButtonStyle =
    [ ( "border-color", "#008000" )
    , ( "background-color", "#008000" )
    , ( "color", "#ffffff" )
    , ( "margin-top", "6px" )
    ]


deleteButtonStyle =
    [ ( "border-color", "#d9534f" )
    , ( "background-color", "#d9534f" )
    , ( "color", "#ffffff" )
    , ( "margin-top", "6px" )
    ]


displayRow : Dict Int String -> ( Int, List String ) -> Html Msg
displayRow ranks indexedEntry =
    let
        idx =
            first indexedEntry

        entry =
            second indexedEntry

        resultTdStyle =
            if (rem idx 2) == 0 then
                resultsTdStyleZero
            else
                resultsTdStyleOne

        teamName =
            getNth 1 entry

        score =
            getNth 2 entry

        total =
            score |> calculateTotal

        rankToUse =
            getRank total ranks

        resultTotalTdStyle =
            if (rem idx 2) == 0 then
                resultsTdStyleZeroBold
            else
                resultsTdStyleOneBold
    in
        tr []
            (List.append
                (List.append
                    [ td [ resultTdStyle ] [ Html.text rankToUse ]
                    , td [ resultTdStyle ] [ Html.text teamName ]
                    ]
                    (List.map (\t -> td [ resultTdStyle ] [ Html.text t ])
                        (score |> String.split " ")
                    )
                )
                [ td [ resultTotalTdStyle ] [ Html.text (toString total) ]
                ]
            )


makeRankDict : List (List String) -> Dict Int String
makeRankDict teamsAndScores =
    teamsAndScores
        |> List.map (\x -> getNth 2 x)
        |> List.map calculateTotal
        |> Set.fromList
        |> Set.toList
        |> List.sort
        |> List.reverse
        |> List.indexedMap (,)
        |> List.map (\e -> ( second e, (toString ((first e) + 1)) ))
        |> Dict.fromList


showTable : List (List String) -> Html Msg
showTable teamsAndScores =
    let
        ranks =
            makeRankDict teamsAndScores
    in
        Html.table [ scoreTableStyle ]
            (List.map (displayRow ranks) <| List.indexedMap (,) teamsAndScores)


showEmptyScoreTable : List (List String) -> Html Msg
showEmptyScoreTable teamsAndScores =
    if (not (List.isEmpty teamsAndScores)) then
        showTable teamsAndScores
    else
        p [] []


instructions =
    """
This is a dynamic scoreboard app for bar trivia, charity trivia, and other trivia games or games with scores in rounds, written in the <A HREF="http://elm-lang.org">Elm programming language</A> by <A HREF="http://www.szcz.org">Mitchell Szczepanczyk</A>.  Please take a moment to read these directions before using this app.

You can hide these instructions and view your data by clicking the "Hide / Show Instructions" button
on the left.  You can use this button to switch views between your scoreboard and these instructions.

Do NOT refresh NOR close this page, or you will lose ALL the data you input.
(Likewise, you can clear all the data saved in this this page simply by refreshing this page.)

This app assumes that there are one or more teams with distinct names that have scores divided by rounds.

You can enter a distinct team name by typing the name of a team in the "Enter Team Name" text field and clicking
the "Add Team" button.  The scoreboard will then update with the team rank, team name, and total score
(listed in <b>bold</b>).  The dropdown selector on the left will also update with the new team's name.

To add a round score for a given team, select the desired team from the dropdown menu, then type in the number of
points in the round for that team in the text field labelled "Edit / Add Score", and click "Add Score".
The score should now appear in the scoreboard, and the total score should be updated.

To edit scores or update ALL the scores for a given team, select the desired team from the dropdown menu,
type in the full list of the round scores, separated by spaces,
in the "Edit / Add Score" text field and click "Edit All Scores".  WARNING: This action
will replace ALL the scores for that team in the scoreboard and DESTROY any scores previously included,
so use this with caution.

To delete a team and that team's scores from the scoreboard, select the desired team from the dropdown menu
and click "Remove Team".  WARNING: Deleting a team is an irreversible action, so use this with caution.

You may sort the data in the scoreboard, either by team name in alphabetical order, or by total score in
descending order, by clicking either of the buttons labelled "Sort by total score" or "Sort by team name".

The source code for this page is in Github.

      """


view : Model -> Html Msg
view model =
    div [ pageStyle ]
        [ Html.table [ mainTableStyle ]
            [ tr []
                [ td [ controlTdStyle ]
                    [ button
                        [ Html.Attributes.type_ "button"
                        , onClick ToggleInstructions
                        , Html.Attributes.style addButtonStyle
                        ]
                        [ Html.text "Hide / Show Instructions" ]
                    , br [] []
                    , hr [] []
                    , input [ placeholder "Enter team name", onInput EnterTeamName ] []
                    , br [] []
                    , button
                        [ Html.Attributes.type_ "button"
                        , onClick AddTeam
                        , Html.Attributes.style addButtonStyle
                        ]
                        [ Html.text "Add Team" ]
                    , br [] []
                    , hr [] []
                    , select [ onInput SelectTeamToEdit ] (makeDropdown model.teamsAndScores)
                    , br [] []
                    , br [] []
                    , input [ placeholder "Edit / Add score", onInput UpdateScoreString ] []
                    , br [] []
                    , button
                        [ Html.Attributes.type_ "button"
                        , onClick EnterAddScore
                        , Html.Attributes.style addButtonStyle
                        ]
                        [ Html.text "Add score" ]
                    , br [] []
                    , button
                        [ Html.Attributes.type_ "button"
                        , onClick EnterEditScore
                        , Html.Attributes.style deleteButtonStyle
                        ]
                        [ Html.text "Edit all scores" ]
                    , br [] []
                    , button
                        [ Html.Attributes.type_ "button"
                        , onClick DeleteTeam
                        , Html.Attributes.style deleteButtonStyle
                        ]
                        [ Html.text "Remove Team" ]
                    , hr [] []
                    , button
                        [ Html.Attributes.type_ "button"
                        , onClick SortByTotalScore
                        , Html.Attributes.style sortButtonStyle
                        ]
                        [ Html.text "Sort by total score" ]
                    , br [] []
                    , button
                        [ Html.Attributes.type_ "button"
                        , onClick SortByTeamName
                        , Html.Attributes.style sortButtonStyle
                        ]
                        [ Html.text "Sort by team name" ]
                    , hr [] []
                    ]
                , td [ scoreTdStyle ]
                    [ h2 [] [ text "Trivia Scoreboard" ]

                    -- , p [] [ text (toString model) ]
                    , if model.toggleInstructions == True then
                        Markdown.toHtml [] instructions
                      else
                        showEmptyScoreTable model.teamsAndScores
                    ]
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
