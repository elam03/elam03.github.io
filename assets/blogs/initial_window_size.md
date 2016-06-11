I've had a hard time deciphering what tasks are for and when to use them, but recently I used a task for a nice concrete example which will be helpful to others I hope!

Problem
-------
My cityscape needs window dimensions to do some initialization. I wanted to adapt cityscape to use the initial window dimensions rather than using a fixed size.

Solution
--------
I found this in the Window package. [link](http://package.elm-lang.org/packages/elm-lang/window/1.0.0/Window)

~~~elm
size : Task x Size
~~~

Great! So how do I use it? And why is it a Task[^Task]?

### How to use ###

In my cityscape's init function (which returns an initialized Model and Cmd Msg), instead of not executing any command, I tell it to perform this task and map the fail/success Msgs to cityscape's update function.

Before:
~~~elm
import Window exposing (Size)

type Msg
    = Size Window.Size
    | ...

init : (Model, Cmd Msg)
init =
    let
        model = ...
    in
        ( model, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        ...
        Size s ->
            ( { model
              | windowWidth = s.width
              , windowHeight = s.height
              }
            , Cmd.none
            )
        ...
~~~

After:
~~~elm
import Task

...

init : (Model, Cmd Msg)
init =
    let
        model = ...
    in
        ( model, Task.perform Size Size Window.size )
~~~

I already had the Size Msg to handle window resizes, so I just 'perform' the Window.size task and map it to that Size Msg.

### Summary ###
So you perform a task with (Task.perform) and it will return a Msg that should map into your application/module's Msg.


[^Task]:
Tasks: The basic convention is that Tasks are executions that are done in javascript land (http and websockets are other examples of executions that need to be done in javascript land and not in elm native).
