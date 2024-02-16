
# Read CSV file
buffer_pfeuil_0 <- utils::read.csv(file = here::here("data", "raw-data", "20240124_table_buffertest.csv"), sep = ";" )



library(tidyverse)

buffer_pfeuil <- tidyr::pivot_longer(data = buffer_pfeuil_0
                                     , cols = 1:3
                                     , names_to = "buffer"
                                     , values_to = "pfeuil"  )

buffer_pfeuil$buffer[buffer_pfeuil$buffer == 'tabl500_pfeuil_500'] <- "tampon_500"
buffer_pfeuil$buffer[buffer_pfeuil$buffer == 'tabl250_pfeuil_250'] <- "tampon_250"
buffer_pfeuil$buffer[buffer_pfeuil$buffer == 'tabl100_pfeuil_100'] <- "tampon_100"


buffer_pfeuil |> 
  ggplot( aes(x = pfeuil,  color = buffer, fill = buffer)) +
  geom_density(alpha = 0.2, position = "identity") +
  theme_minimal()
  
buffer_pfeuil |> 
  ggplot(aes(x = pfeuil, color = buffer, fill = buffer)) +
  geom_histogram(alpha = 0.2, position = "identity", bins = 30) +
  facet_grid(cols = vars(buffer))+
  theme_minimal()



bufferseq <- c("tampon_500", "tampon_250","tampon_100")
means <- tapply(buffer_pfeuil$pfeuil, buffer_pfeuil$buffer, mean)
std_dev <- tapply(buffer_pfeuil$pfeuil, buffer_pfeuil$buffer, sd)
  
ggplot() +
    geom_line(data = buffer_pfeuil, aes(x = factor(buffer, levels = bufferseq), y = pfeuil, group = number), colour = "grey70") +
    geom_point(data = buffer_pfeuil, aes(x = buffer, y = pfeuil, color = as.factor(number)), alpha = 0.5) +
    geom_point(data = data.frame(buffer = names(means), pfeuil = means),
               aes(x = factor(buffer), y = pfeuil),
               color = "red",
               size = 4) +
    geom_line(data = data.frame(buffer = names(means), pfeuil = means),
              aes(x = factor(buffer), y = pfeuil, group = NA), color = "red", linewidth = 1)+
    geom_errorbar(data = data.frame(buffer = names(means), pfeuil = means, std_dev = std_dev),
                  aes(x = factor(buffer), ymin = pfeuil - std_dev, ymax = pfeuil + std_dev),
                  width = 0.1,
                  color = "red") +
    theme_minimal()
  