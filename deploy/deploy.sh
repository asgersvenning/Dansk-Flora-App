cd deploy/

docker build -f Dockerfile_base --progress=plain -t learndfv_base .
docker build -f Dockerfile --progress=plain -t learndfv:latest .
docker run -p 80:80 learndfv:latest
