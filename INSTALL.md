# Installing Chess

Follow the instructions below to install Chess. Make sure that you installed OCaml and OPAM in same method as was requested in CS 3110. Detailed instructions on how to do that can be found [here](https://cs3110.github.io/textbook/chapters/preface/install.html#). 

0. Make sure your OCaml and OPAM are up to date; your OCaml version should be version 4.14.0 or later. If not, please update OCaml so that it is up to date. Then, make sure your OPAM is up to date as well. You can do this by doing<br>
<br>
`opam update` <br>
<br>
Wait for it to prompt for you to input something else. You want to upgrade all your packages by doing<br>
<br>
`opam upgrade`<br>
<br>

1. Create a folder in which all code in this `chess.zip` file will be stored. Let's say you want to store this into a file named 3110_chess. For Mac, you can do this by simply doing: <br>
<br>
`mkdir ~/3110_chess` <br>
<br>
where `~` is your home directory. What this will do is it will create a folder name 3110_chess in whatever your home directory is for your computer.

2. Move the zip file into the folder you just created. If you are on a Mac or Linux machine, then the easiest way wuld be to drag and drop. If you're using a Windows computer, good luck.<br>
Then, move into the same directory as your file path. You can do this by doing:<br>
<br>
`cd ~/3110_chess`
<br>
Now unzip the file. There will now be a folder named "chess" that was previously in the `.zip` file that had just been unzipped. Navigate into that chess file.

3. We now need to install ANSITerminal. When you are currently in the same directory as the folder you just created, run<br>
<br>
`opam install ANSITerminal`
<br>
<br>

Now you're all done with the installation process! You can proceed to play our Chess game by simply typing in `make play`. If you're unfamiliar with chess, [here](https://www.chess.com/learn-how-to-play-chess) are the rules on how to play. Good luck, and most importantly have fun!