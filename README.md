# Indoor localization using Wi-fi signal strength

- Pre-processing: 
  - Normalization by row(phone model)
  - Excluding irregular values
  - From patterns in errors, exclude phone models that were trolling the system
  - Aggregate records of wi-fi strength in the same coordinate and average them, for more accurate numbers
  
- Multi-level modelling -> Building (prediction) %>% Floor %>% Long & Lat (k-NN with 3 neighbours)

*Wi-fi pptx -> Presentation of the project to the management team

      

