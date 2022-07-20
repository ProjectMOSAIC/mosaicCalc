library(tuneR)
setWavPlayer("afplay") # for OS-X
cello <- readWave("cello.wav")
violin <- readWave("violin.wav")
Cello <- tibble::tibble(
  y = cello@left,
  t = (1:length(y) )/cello@samp.rate
)
Cello_seg <- Cello %>% filter(t >=0.501, t<=0.583)
Violin <- tibble::tibble(
  y = violin@left,
  t = (1:length(y) )/violin@samp.rate
)
Violin_seg <- Violin %>% filter(t >=0.401, t<=0.4835)

save(Cello, Cello_seg, Violin, Violin_seg, file="../../../data/Notes.rda")

