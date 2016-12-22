I just finished upgrading my site to 0.17. The task seemed daunting after reading [farewell-to-frp](http://elm-lang.org/blog/farewell-to-frp). I'll admit the title mis-led me a bit, I thought perhaps Evan was leaving Elm for something else.

Some of my initial thoughts on Elm 0.17

* The new subscriptions system sounds exciting and the websocket example [link](http://elm-lang.org/examples/websockets) looks amazingly simple!
* Signals are gone?! I had just started to get semi-comfortable with them.
* Easier to use? I hope so!

Upgrade Process
---------------

I followed [0.17.0 upgrade guide](https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md) for upgrading and went through my files 1-by-1. Upgrading wasn't as bad as I thought it was going to be.

I typically use save-commands to run 'elm-make src/Main.elm --output=gen/main.js' everytime I save an elm file; however, for this upgrade process, I manually 'elm-make'd. I use Atom as my editor and had to disable a few other packages (linter-elm-make) since they haven't been upgraded as well.

Most of the upgrading involved re-naming. Action -> Msg, Effects -> Cmd, Html -> Html Msg, StartApp -> Html.app, etc...

### Http/Cmd/Task Troubles ###
Http was my biggest refactor. I had to explicitly create failure Msgs to handle http failures.

In 0.16 when 'http getting', I was converting that task into an Action with a 'maybe', using an Effect -> Task conversion; 0.17 seems to remove that Effect -> Task conversion, so I was forced to use Task.perform, for which I needed to create the additional failure Msg.


*0.16.0*
```elm
type Msg
    = ...
    | LoadBlogMarkdown (Maybe String)
    | ...

getContent location =
    Http.getString location
        |> Task.toMaybe
        |> Task.map LoadBlogMarkdown
        |> Effect.task
```

*0.17.0*
```elm
type Msg
    = ...
    | LoadBlogMarkdown String
    | FetchFail Http.Error
    | ...

getContent location =
    Http.getString location
        |> Task.perform FetchFail LoadBlogMarkdown
```

I'm still unsure if this explicitly handling of error cases is a good thing or not. In retrospect, the code is slightly cleaner, removing the 'maybe' earlier and having it short-circuit to it's own dedicated Cmd.

### Elm Reactor Troubles ###
The other big issue I encountered was serving my website. I was typically using 'elm reactor' to serve my index.html, but it appears to not serve html anymore. I'm sure that will be remedied soon.

### Subscription Troubles ###
I struggled a little bit with the new subscription system, but it's mostly because I didn't any great examples to view yet! The main sore spot was combining all of the subscriptions from the sub-modules together into Main. I eventually got it and it makes a lot of sense! Just had to pass the right model into the subscription function and map it back out as the Msg in main.

```elm
type Msg
    = NoOp
    | CityscapeMsgs Cityscape.Msg
    | ...

...

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ...
        , model.cityscape
            |> Cityscape.subscriptions
            |> Sub.map (\a -> CityscapeMsgs a)
        , ...
        ]

main =
    Html.program
        { ...
        , subscriptions = subscriptions
        , ...
        }
```

### Other Subscription Troubles ###
The last pieces I had to refactor were the keyboard/mouse/size changes I handled inside of my Cityscape. I had to install a few extra packages that wasn't quite obvious:

* elm-lang/keyboard
* elm-lang/mouse
* elm-lang/window

Then I worked on the refactor itself. I was able to simplify my input handling quite a bit with the new subscription system. It might also have been the fact that when I first worked on Cityscape I was still learning the ropes...

Conclusion
----------

I have finished my refactoring and will continue to add little features and blog my learnings. Hopefully you have found nugget of useful information from here! And if you haven't then... maybe I will at least at some more entertaining pieces of information! Anyways, elm on!
