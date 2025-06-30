# R Shiny Application

This project contains an R Shiny application designed for data analysis and visualization. The application allows users to upload patient data and select various parameters for analysis.

## Getting Started

### Prerequisites

- Docker installed on your machine.

### Building the Docker Image

To build the Docker image for the Shiny application, navigate to the project directory and run the following command:

```
docker build --platform linux/amd64 -f ./Dockerfile -t shiny-app .
```

### Running the Docker Container

After building the image, you can run the Docker container with the following command:

```
docker run -it -v ${PWD}/app:/srv/app/ -p 3838:3838 shiny-app 
```

This command maps port 3838 of the container to port 3838 on your host machine.

### Accessing the Application

Once the container is running, you can access the Shiny application by opening a web browser and navigating to:

```
http://localhost:3838
```
