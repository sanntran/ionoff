# copy this file to build/docker
cp -R ../../build/docker/* .
cp -R ../../ionoff.server/target ./
docker build -t "ionoff:latest" .
rm -rf ./target