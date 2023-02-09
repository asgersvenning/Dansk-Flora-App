To deploy the application as a Google Cloud Run service, first open the Cloud Shell in the Google Cloud Console. Then, run the following commands:
    
    docker pull docker.io/asvenning/learndfv:ready

    docker tag docker.io/asvenning/learndfv:init gcr.io/dummydfv/learndfv:ready
    
    docker push gcr.io/dummydfv/learndfv:ready
    
    gcloud run deploy learndfv \
      --image gcr.io/dummydfv/learndfv:ready \
      --platform managed \
      --region europe-west1 \
      --allow-unauthenticated \
      --port 80 

OBS: "dummydfv" is merely a placeholder for the name of the google cloud project. Replace it with the name of your project.