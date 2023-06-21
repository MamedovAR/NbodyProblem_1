FROM ubuntu

WORKDIR .

COPY .

RUN apt-get update && apt-get install -y \
    haskell-stack \
    python3 \
    python3-pip \
    python3-numpy \
    libghc-quickcheck-dev \
    libghc-glut-dev \
    libghc-opengl-dev \
    libghc-hmatrix-dev

RUN stack setup
RUN stack install QuickCheck
RUN stack install GLUT
RUN stack install OpenGL
RUN stack install hmatrix
RUN stack build

CMD ["stack", "exec", "app/NbodyProblem1"]
