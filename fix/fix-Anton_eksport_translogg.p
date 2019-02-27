output to value("translog.log").
  export "Eksport av translogg startet." today string(time,"HH:MM:SS").
output close.

output to value("batchlog.d").
for each BatchLogg where RegistrertDato >= 01/01/2008:
  export BatchLogg.
end.
output close.

output to value("translog.d").
for each TransLogg where Dato >= 01/01/2008:
  export Translogg.
end.
output close.

output to value("translog.log").
  export "Eksport av translogg ferdig." today string(time,"HH:MM:SS").
output close.
