library(tuneR)
setWavPlayer("afplay") # for OS-X
OH_name <- "o.wav"
EE_name <- "y.wav"
OH <- readWave(OH_name)
# OH Formants: 406, 727, 2090
EE <- readWave(EE_name)
# EE Formants: 294, 2343, 3251
Oh_sound <- tibble::tibble(
  y = OH@left,
  t = (1:length(y) )/OH@samp.rate
)
Vowel_oh <- Oh_sound %>% filter(t >=0.1513, t <= 0.20)
OH_seg2 <- Oh_sound %>% filter(t >=0.15, t <= 0.166)

Ee_sound <- tibble::tibble(
  y = EE@left,
  t = (1:length(y) )/EE@samp.rate
)
Vowel_ee<- Ee_sound %>% filter(t >=0.3521, t <= 0.4058)
save(Vowel_ee, Vowel_oh, Ee_sound, Oh_sound, file="../../../data/Vowels.rda")




EE_seg2 <- Ee_sound %>% filter(t >=0.3521, t <= 0.359)

gf_line(y ~ t, data = EE_seg) %>% gf_line(y ~ t, data = EE_seg2, color="red")

amps <- function(sig, freqs = 298) {
  Res <- numeric(length(freqs))
  windowed <- sig$y
  for (k in 1:length(freqs)) {
    cpart <- sum(windowed * cos(2*pi*freqs[k]*sig$t))/16000
    spart <- sum(windowed * sin(2*pi*freqs[k]*sig$t))/16000
    Res[k] <- sqrt(cpart^2 + spart^2)
  }

  tibble::tibble(
    freq = freqs,
    amp = Res
  )
}

EE_spec <- amps(EE_seg, 24*(1:200))

EE_spec2 <- amps(EE_seg2, 24*(1:200))
OH_spec <- amps(OH_seg, 24*(1:200) )
OH_spec2 <- amps(OH_seg2, 24*(1:200))
gf_line(amp ~ freq, data = EE_spec) %>% gf_line(amp ~ freq, data = OH_spec, color="blue")

gf_line(amp ~ freq, data = OH_spec)

