# Settlers of Catan MS2 Installation and Running Instructions

Welcome to our MS2 prototype for Settler's of Catan! In this version of our project, two players should be able to initialize a game and setup their initial pieces! Follow the instructions below to install and run the project.

---

## Installation

To install this project please run `git pull git@github.coecis.cornell.edu:asb323/CS-3110-Project.git`. Move into the project directory, and run `dune build`. At this point our project should be setup and ready to run.

## Running

In order to run this game, please run `dune exec bin/main.exe` within the directory. This should start our game. Players one and two will be prompted to put down their pieces in order: Player 1, Player 2, Player 2, Player 1. If you try to put down two pieces in the same position, this will not be allowed. At the end of setup, players hands and positions will be printed.