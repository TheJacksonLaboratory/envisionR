compute_circadian_fft = function(activity, zeitgeber_offset = 0) {

  if(any(is.na(activity))) {
    activity = zoo::na.approx(activity, na.rm = FALSE)
  }

  if(any(is.na(activity))) {
    activity = zoo::na.fill(activity, "extend")
  }

  n = length(activity)

  # FFT
  fft_result = fft(activity)
  power = Mod(fft_result)^2 / n
  freq = (0:(n-1)) / n
  period = 1 / (freq * 60)  # period in hours (assuming 1-min resolution)

  # Circadian band (20-28 hours)
  circ_idx = which(period >= 20 & period <= 28)

  # 24h component specifically
  freq_24h = 1 / (24 * 60)
  idx_24h = which.min(abs(freq - freq_24h))

  # Extract phase for acrophase
  phase = Arg(fft_result[idx_24h])
  acrophase = -1 * phase / (2 * pi) * 24
  if(acrophase < 0) acrophase = acrophase + 24

  # Adjust for start time
  acrophase_real = (acrophase + zeitgeber_offset) %% 24

  # Return named vector (important for summarize!)
  tibble(
    circadian_power = sum(power[circ_idx]),
    power_24h = power[idx_24h],
    normalized_power = sum(power[circ_idx]) / sum(power),
    peak_period = period[circ_idx][which.max(power[circ_idx])],
    acrophase = acrophase_real,
    amplitude = 2 * Mod(fft_result[idx_24h]) / n,
    n_imputed = sum(is.na(activity))  # track how many were imputed
  )
}
