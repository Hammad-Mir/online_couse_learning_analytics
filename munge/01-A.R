# Example preprocessing script.
cyber.security.1_enrolments <- cyber.security.1_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

cyber.security.2_enrolments <- cyber.security.2_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

cyber.security.3_enrolments <- cyber.security.3_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

cyber.security.4_enrolments <- cyber.security.4_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

cyber.security.5_enrolments <- cyber.security.5_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

cyber.security.6_enrolments <- cyber.security.6_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

cyber.security.7_enrolments <- cyber.security.7_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))