# Installing Chess

Follow the instructions below to install Chess. Make sure that you installed OCaml and OPAM in the same method as was requested in CS 3110. Detailed instructions on how to do that can be found [here](https://cs3110.github.io/textbook/chapters/preface/install.html#). 

0. Make sure your OCaml and OPAM are up to date; your OCaml version should be version 4.14.0 or later. If not, please update OCaml. Then, make sure your OPAM is up to date as well. To do this, open your command line and run <br/> <br/> `opam update` <br/><br/> You can also upgrade all your packages by running <br/><br/>`opam upgrade`<br/><br/>
1. Download `chess.zip` (if it isn't named that in the installation, simply rename it). In your home folder, create a new directory named `chess`, which can be done by running `mkdir ~/chess`. Move the zip file into that directory, and then navigate into that directory by running `cd ~/chess`. Once inside the `chess` directory, unzip the folder by running
<br/><br/> `unzip chess.zip`<br/> <br/> Now, we need to build all pertinent modules for running the game successfully by running 
<br/><br/>
`make build`<br/>
<br/> 
2. We now need to install ANSITerminal in order for the colors on the chessboard to work. When you are currently in the `chess` folder, run <br/> <br/> `opam install ANSITerminal`<br/> <br/>

Now you're all done with the installation process! You can proceed to play our Chess game by simply running `make play`. If you're unfamiliar with chess, [here](https://www.chess.com/learn-how-to-play-chess) are the rules on how to play. Good luck, and most importantly have fun!

