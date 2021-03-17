for each artbas:
  find vargr of artbas no-lock no-error.
  if not available vargr then
    do:
      output to value("mangler-varegruppe.err") no-echo.
      export artbas.artikkelnr artbas.vg artbas.lopnr.
      output close.
    end.
  else
    assign
      ArtBas.BongTekst = vargr.VgBeskr.
end.
