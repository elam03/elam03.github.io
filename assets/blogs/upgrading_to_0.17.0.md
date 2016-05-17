Upgrading to Elm 0.17
============

**5/17/2016**

I just finished upgrading my site to 0.17. The task seemed daunting after reading [farewell-to-frp](http://elm-lang.org/blog/farewell-to-frp). I'll admit the title mis-led me a bit, I thought perhaps Evan was leaving Elm for something else.

Some of my initial thoughts on Elm 0.17

* The new subscriptions system sounds exciting and the websocket example [link](http://elm-lang.org/examples/websockets) looks amazingly simple!
* Signals are gone?! I had just started to get semi-comfortable with them.
* Easier to use? I hope so!

Upgrading Process
-----------------

I followed [0.17.0 upgrade guide](https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.17.md) for upgrading and went through my files 1-by-1. Upgrading wasn't as bad as I thought it was going to be.

I typically use save-commands to run 'elm-make src/Main.elm --output=gen/main.js' everytime I save an elm file; however, for this upgrade process, I manually 'elm-make'd. I use Atom as my editor and had to disable a few other packages (linter-elm-make) since they haven't been upgraded as well.

Most of the upgrading involved re-naming. Action -> Msg, Effects -> Cmd, Html -> Html Msg, StartApp -> Html.app, etc...

### Http/Cmd/Task Issues ###
Http was my biggest refactor. I had to explicitly create failure Msgs to handle http failures.

In 0.16 when 'http getting', I was converting that task into an Action with a 'maybe', using an Effect -> Task conversion; 0.17 seems to remove that Effect -> Task conversion, so I was forced to use Task.perform, for which I needed to create the additional failure Msg.

0.16.0
~~~
type Msg
    = ...
    | LoadBlogMarkdown (Maybe String)
    | ...

getContent location =
    Http.getString location
        |> Task.toMaybe
        |> Task.map LoadBlogMarkdown
        |> Effect.task
~~~

0.17.0
~~~
type Msg
    = ...
    | LoadBlogMarkdown String
    | FetchFail Http.Error
    | ...

-- 0.17.0
getContent location =
    Http.getString location
        |> Task.perform FetchFail LoadBlogMarkdown
~~~

I'm still unsure if this explicitly handling of error cases is a good thing or not. In retrospect, the code is slightly cleaner, removing the 'maybe' earlier and having it short-circuit to it's own dedicated Cmd.

### Elm Reactor Issues ###
The other big issue I encountered was serving my website. I was typically using 'elm reactor' to serve my index.html, but it appears to not serve html anymore. I'm sure that will be remedied soon.
