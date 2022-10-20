# Installing Chess

Follow the instructions below to install Chess. Make sure that you installed OCaml and OPAM in same method as was requested in CS 3110. Detailed instructions on how to do that can be found [here](https://cs3110.github.io/textbook/chapters/preface/install.html#). 

0. Make sure your OCaml and OPAM are up to date; your OCaml version should be version 4.14.0 or later. If not, please update OCaml. Then, make sure your OPAM is up to date as well. You can do this by running in Terminal<br>
<br>
`opam update` <br>
<br>
Wait for it to prompt for you to input something else. You can then upgrade all your packages by running<br>
<br>
`opam upgrade`<br>
<br>

1. Now download `chess.zip`, then place it into a directory of your choice. Navigate into that directory. Now, unzip the folder by running<br>
<br>
`unzip chess.zip`<br>
<br>
There will now be a folder named `chess` that will appear in your directory. Navigate into that `chess` folder by running `cd chess`. From there, we need to build all pertinent modules for running successfully by running<br>
<br>`make build`<br>
<br>
Make sure you are inside the `chess` folder!

 2. We now need to install ANSITerminal. When you are currently in the `chess` folder, run<br>
<br>
`opam install ANSITerminal`<br>
<br>

Now you're all done with the installation process! You can proceed to play our Chess game by simply typing in `make play`. If you're unfamiliar with chess, [here](https://www.chess.com/learn-how-to-play-chess) are the rules on how to play. Good luck, and most importantly have fun!