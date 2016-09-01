elam03.github.io
================

http://elam03.github.io

This is the repo for my site. I am currently interested in learning elm and this is the place for me to apply all the cool stuff I learn!

Get Started
-----------

### Install Elm 0.17.1 ###
This site was created with Elm 0.17.1. You can follow the instructions to download and install Elm from  their main site here:

[Elm Install Instructions](http://elm-lang.org/install)

### Download/Clone the Repo ###
You can clone or download this repo from this:

> git clone https://github.com/elam03/elam03.github.io.git


### Install Package Dependencies ###
The full list of dependencies can be found in elm-package.json, but with a clean repo, you can just run:

> elm package install -y elm-lang/core


And it will scan and install the rest of the dependencies for you.

### Build ###
To build the site, you should compile `src/Main.elm` to `gen/main.js` with this:

> elm make src/Main.elm --output=gen/main.js

### Run Locally ###
Finally to run locally on your machine, you can use `http-server`, `elm reactor`, or anything that serves html! There is an `index.html` that contains the embedded compiled main code.

*Note that `elm reactor` is not currently serving html at the moment in 0.17.0, so make sure you are at elm 0.17.1.*
