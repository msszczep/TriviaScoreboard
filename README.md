# TriviaScoreboard
A dynamic scoreboard app for trivia games or other games with scores in rounds.

- - -

This is a dynamic scoreboard app for bar trivia, charity trivia, and other trivia games or games with scores in rounds,
written in the <A HREF="http://elm-lang.org">Elm programming language</A> by
<A HREF="http://www.szcz.org">Mitchell Szczepanczyk</A>.  Please take a moment to read these directions
before using this app.

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
