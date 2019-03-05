# Indoor localization using Wi-fi signal strength

- Pre-processing: 
  - Normalization by row (not to overfit for phone models)
  - Excluding irregular values (outside of normal Wi-fi strength range)
  - From patterns in errors, exclude phone models that were trolling the system
  - Aggregate records of wi-fi strength in the same coordinate and average them -> Avoid volatily in Wi-fi strength
  
- **Multi-level modelling** -> Building (prediction) %>% Floor %>% Long & Lat (k-NN with 3 neighbours)

*Wi-fi pptx -> Presentation of the project to the management team

      

