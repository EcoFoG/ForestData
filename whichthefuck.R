

attach(data)
which(CircCorr != Circ & CodeCorr ==0) %>% length/ nrow(data) 
which(CircCorr != Circ & CodeCorr !=0) %>% length/ nrow(data) 
which(CircCorr == Circ & CodeCorr ==0) %>% length/ nrow(data) 
which(CircCorr == Circ & CodeCorr !=0) %>% length/ nrow(data) 


data %>% 
  filter(CircCorr != Circ & CodeCorr ==0) %>% 
  # head(20)
  mutate(A = CircCorr / Circ) %>% 
  filter(A != 1.0000) %>% 
  select(A) %>% 
  summary()
