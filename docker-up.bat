docker build -t wbws:latest .
docker run -d -p 8080:80 --env-file .env wbws:latest 
