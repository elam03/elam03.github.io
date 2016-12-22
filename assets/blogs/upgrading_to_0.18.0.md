Just finished porting over my site to Elm 0.18.0.

Upgrade Process
---------------

Everything went relatively smoothly considering all the smaller syntax changes. I followed [0.18.0 upgrade guide](https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md)

### Http/Cmd/Task Troubles ###
The toughest part was figuring out the Http/Json things I load for my site. I back-tracked to a small Http example and carefully went through the example slowly.

Once I got a good handle on what functions to call and the expected parameter list, it was straightforward. The new way to process requests simplifies the steps and makes the code look a little cleaner. The failure is built into the update message and can be handled nicer now.

What it looks like now:

*0.18.0*
```elm
type Msg
    = Refresh (Result Http.Error SummaryData)

...

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Refresh (Ok summaryData) ->
            ( Model model.file summaryData "Refresh Achieved!"
            , Cmd.none
            )

        Refresh (Err _) ->
            (model, Cmd.none)

...

getData : String -> Cmd Msg
getData location =
    Http.get location decodeData
        |> Http.send Refresh
```

Once I figured out the message needed to change and be handled in update, everything fell into place.


Bug with elm-make
-----------------

I did find an interesting bug and commented on it here:
["A dot in repository name breaks elm-make"](https://github.com/elm-lang/elm-make/issues/106)

Conclusion
----------

Elm 0.18.0 looks good. `elm-reactor` works better now. I might try to use it more over NPM's `http-server`. I'll continue to keep an eye out for more Elm updates!
