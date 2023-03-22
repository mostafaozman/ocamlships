Users need to install the OCaml Graphics library to play this game.

To install it open your command line interface and run:

opam install graphics


It will ask you a series of prompts, you want to say yes to all of the prompts
(you can simply type 'y' as a response)

It might take a moment, but eventually It will finish and you will need to run:

eval $(opam env)

to reconfigure your new opam package to its enviroment. Alternatively 
restart your device. (For Mac users you would 
have to reboot your system for the package to work properly)

Check if it the package was successfully installed by running the following 
lines in utop:

#require "graphics";;

open Graphics;;

open_graph " 480x270";;

The result should result in an empty window popping up with the specified 
dimensions in the open_graph command.

For Windows users: there are fonts not installed in wsl by default so you have to install
this xfonts library inorder to be able to see the font properly and play the game

sudo apt-get install -y xfonts-base

After running these commands you should be all set to play Aleph Null's 
Battle Ship game.

Have fun !!