FROM ubuntu:20.04
WORKDIR /build
RUN apt update && apt install software-properties-common -y
RUN add-apt-repository ppa:linuxuprising/libpng12 && apt update
RUN DEBIAN_FRONTEND=noninteractive \
    TZ=USA \
    apt install build-essential git libboost-all-dev libpng12-0 libfreetype6-dev -y
COPY ./*.cpp ./*.hpp ./*.h ./.git ./libpngwriter.so /build/
COPY ./Makefile /build/
RUN make

FROM ubuntu:20.04
WORKDIR /app
RUN apt update && apt install software-properties-common -y
RUN add-apt-repository ppa:linuxuprising/libpng12 && apt update
RUN DEBIAN_FRONTEND=noninteractive \
    TZ=USA \
    apt install python3 sysstat screen libboost-all-dev libpng12-0 libfreetype6-dev -y
COPY --from=0 /build/mod2 .
COPY ./*.py ./*.ini ./*.so /app/
RUN mkdir /app/data
CMD ["python3", "launch_implicit_explicit_water.py"]
