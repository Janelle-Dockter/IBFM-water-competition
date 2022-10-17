FROM ubuntu:20.04
WORKDIR /build
RUN apt-get update && apt-get install software-properties-common -y
RUN add-apt-repository ppa:linuxuprising/libpng12 && apt-get update
RUN DEBIAN_FRONTEND=noninteractive \
    TZ=USA \
    apt-get install build-essential git libboost-all-dev libpng12-0 libfreetype6-dev -y
COPY ./*.cpp ./*.hpp ./*.h ./.git ./libpngwriter.so /build/
COPY ./Makefile /build/
RUN make

FROM ubuntu:20.04
WORKDIR /app
RUN apt-get update && apt-get install python3 sysstat screen -y
COPY --from=0 /build/mod2 .
COPY ./*.py ./*.ini ./*.so /app/
CMD ["python3", "launch_implicit_explicit_water.py"]
