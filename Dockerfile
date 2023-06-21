FROM haskell

WORKDIR /app

COPY . /app

RUN apt-get update && apt-get install -y \
    python3-pip \
    python3-numpy

RUN stack update
RUN stack install QuickCheck
RUN stack build

CMD ["stack", "exec", "app/NbodyProblem1"]
