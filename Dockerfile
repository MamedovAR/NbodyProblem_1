FROM haskell

WORKDIR /app

COPY . /app

RUN bash script.sh
RUN apt-get update && apt install -y \
    python3-pip \
    python3-numpy \
#     libgtk2.0-dev \
#     libcairo2-dev \
#     libpango1.0-dev \
#     libglib2.0-dev \
#     libx11-dev \
#     libgl1-mesa-dev \
#     libglu1-mesa-dev \
#     freeglut3-dev

# ENV DISPLAY=:0

#RUN stack setup
#RUN stack install gtk2hs-buildtools
RUN cabal update
RUN cabal test
RUN cabal build
# "Xvfb", "-ac", ":0", "-screen", "0", "1024x768x16", "&&", 
CMD ["/app/dist-newstyle/build/x86_64-linux/ghc-9.4.5/NBodyProblem-0.1.0.0/x/NBodyProblem-exe/build/NBodyProblem-exe/NBodyProblem-exe"]

