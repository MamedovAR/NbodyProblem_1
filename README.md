# NBodyProblem
Simple Haskell implementation of a Barnes-Hut galaxy simulator.
When I was writing this program, I was looking at n_body_problem.py - many comments were taken from here. Original file is here.
When the program start it calculate shots and put some of points in console than it visualize all shots in window.

# Downloading and launching
$ git clone https://github.com/MamedovAR/NbodyProblem_1
$ cd NbodyProblem_1

or from Dockerhub:
$ sudo docker run mamedovar/nbodyproblem

Build with cabal:
$ cabal build

Build with stack:
$ stack build
$ stack install

From .tar.gz:
$ tar xvzf dist-newstyle/sdist/NBodyProblem-0.1.0.0.tar.gz
$ cd dist-newstyle/sdist/NBodyProblem-0.1.0.0
$ stack build

# Options
$ NBodyProblem-exe --help
Simple Haskell implementation of a Barnes-Hut galaxy simulator.

./NBodyProblem [OPTION] [OUTPUT]

  -h, --help - Show supported options.
	-o, --out OUTPUT - write all iterations in one csv-file.
