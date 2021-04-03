docker build -t wbws:latest .
docker run -d -p 8080:8080 --env-file .env wbws:latest 
