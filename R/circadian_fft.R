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

circadian_results = activity |>
  dplyr::filter(start >= lubridate::ymd_hms("2025-09-12 06:00:00", tz = metadata$tzone) &
                  start < lubridate::ymd_hms("2025-09-26 06:00:00", tz = metadata$tzone)) |>
  group_by(animal_id, cage_name, group_name, sex) |>
  summarize(
    compute_circadian_fft(activity_animal_cm_s_min, 0),
    .groups = "drop"
  )

circadian_results |>
  dplyr::group_by(group_name, sex) |>
  dplyr::mutate(sex = str_to_title(sex),
                cage_num = paste0("c", dense_rank(cage_name))) |>
  ggplot(aes(x = acrophase,
             y = group_name,
             color = group_name)) +
  geom_boxplot(outlier.colour = NA) +
  ggbeeswarm::geom_beeswarm(aes(shape = cage_num),
                            cex = 2,
                            priority = "random",
                            method = "hex",
                            dodge.width =  0.8,
                            size = 1.5,
                            alpha = 0.6) +
  scale_x_continuous(labels = function(x) paste0("ZT",x, ifelse(x > 12, "\n(dark)","\n(light)")))+
  labs(y = "Handling Technique",
       title = "Acrophase Distribution by Intervention") +
  facet_wrap(. ~ sex, ncol = 1) +
  envisionJAX::theme_jax_pub() +
  ggokabeito::scale_color_okabe_ito(name = "Technique") +
  xlab("Acrophase in Zeitgeber Time, hours\n(ZT0 = lights-on at 06:00; ZT12 = lights-off at 18:00)") +
  theme(legend.position = "bottom")

circadian_results |>
  dplyr::group_by(group_name, sex) |>
  dplyr::mutate(sex = str_to_title(sex),
                cage_num = paste0("c", dense_rank(cage_name))) |>
  ggplot(aes(x = group_name,
             y = normalized_power,
             color = group_name)) +
  geom_boxplot(outlier.colour = NA) +
  ggbeeswarm::geom_beeswarm(aes(shape = cage_num),
                            cex = 2,
                            priority = "random",
                            method = "hex",
                            dodge.width =  0.8,
                            size = 1.5,
                            alpha = 0.6) +
  scale_y_continuous() +
  labs(y = "Handling Technique",
       title = "Acrophase Distribution by Intervention") +
  facet_wrap(. ~ sex, ncol = 1) +
  envisionJAX::theme_jax_pub() +
  ggokabeito::scale_color_okabe_ito(name = "Technique") +
  xlab("Acrophase in Zeitgeber Time, hours\n(ZT0 = lights-on at 06:00; ZT12 = lights-off at 18:00)") +
  theme(legend.position = "bottom")
