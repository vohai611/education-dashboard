FROM rocker/shiny-verse
RUN mkdir /app
COPY ./edu-dashboard /app/edu-dashboard
RUN Rscript /app/edu-dashboard/install-packages.R
EXPOSE 3838 
CMD Rscript /app/edu-dashboard/runapp.R
