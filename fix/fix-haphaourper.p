FOR EACH HappyHourPeriode
  BY HappyHourPeriode.HapHourPerId DESCENDING:

    ASSIGN
        HappyHourPeriode.HapHourPerId = HappyHourPeriode.HapHourPerId + 10
        .
    DISPLAY
        HappyHourPeriode.HapHourId
        HappyHourPeriode.HapHourPerId
        .

END.
