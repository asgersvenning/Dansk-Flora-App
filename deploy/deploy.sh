cd deploy/
test=false
if [ "$1" == "test" ]; then
    test=true
fi
docker build -f Dockerfile_base --progress=plain -t learndfv_base .
docker build -f Dockerfile --progress=plain -t learndfv:latest . --build-arg test=$test --no-cache
docker run -p 80:80 learndfv:latest
