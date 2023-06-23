FROM fpco/stack-build

WORKDIR /app

COPY . /app

RUN apt-get update && apt-get install -y \
    python3-pip \
    python3-numpy \
	libgtk2.0-dev \
	libcairo2-dev \
	libpango1.0-dev \
	libglib2.0-dev \
	libx11-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    freeglut3-dev

ENV DISPLAY=:0

RUN stack update
RUN stack install gtk2hs-buildtools
RUN stack test
RUN cabal build

CMD ["Xvfb", "-ac", ":0", "-screen", "0", "1024x768x16", "&&", "stack", "exec", "app/NbodyProblem1"]
