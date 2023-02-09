To deploy the application as a Google Cloud Run service, first open the Cloud Shell in the Google Cloud Console. Then, run the following commands:
    
    docker pull docker.io/asvenning/learndfv:init

    docker tag docker.io/asvenning/learndfv:init gcr.io/dummydfv/learndfv:init
    
    docker push gcr.io/dummydfv/learndfv:init
    
    gcloud run deploy learndfv \
      --image gcr.io/dummydfv/learndfv:init \
      --platform managed \
      --region europe-west1 \
      --allow-unauthenticated \
      --port 80 

OBS: "dummydfv" is merely a placeholder for the name of the google cloud project. Replace it with the name of your project.