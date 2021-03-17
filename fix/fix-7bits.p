FOR EACH LevBAs:
    ASSIGN
      LevBas.levNamn = REPLACE(LevBAs.levNamn, "~|", "ö")
      LevBas.levNamn = REPLACE(LevBAs.levNamn, "~{", "æ")
      LevBas.levNamn = REPLACE(LevBAs.levNamn, "~}", "å")
      LevBas.levNamn = REPLACE(LevBAs.levNamn, "~\", "Ö")
      LevBas.levNamn = REPLACE(LevBAs.levNamn, "~[", "Æ")
      LevBas.levNamn = REPLACE(LevBAs.levNamn, "~]", "Å")
      .
END.
