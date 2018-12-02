/************************************************************
    Program:  byggstlinje.i
    Created:  TN   19 Mar 100
Description:

Last change:  TN    8 Nov 100   11:55 pm
************************************************************/

STATOPPRETTELSE:
DO:

AARSLOOPEN:
DO wLoop1 = wAAr1 TO YEAR(TODAY):

  /* innev‘rende †r */
  IF wLoop1 = YEAR(TODAY) then
    DO:
      assign
        wFra = 1
        wTil = wPerLin2.
      /* Sikrer at det ikke sp›rres etter fremtiden. */
      CASE wPerId:
        WHEN "AAR" then
          DO:
            /* OK */
          END.
        WHEN "MANED" then
          DO:
            IF wTil > MONTH(TODAY) then
              assign
                wPerLin2 = MONTH(TODAY)
                wTil     = MONTH(TODAY).
          END.
        WHEN "UKE" then
          DO:
            RUN weeknum.p (TODAY, OUTPUT wWeekNum).
            wWeekNum = INT(SUBSTRING(STRING(wWeekNum,"999999"),5,2)).
            IF wTil > wWeekNum then
              assign
                wPerLin2 = wWeekNum
                wTil     = wWeekNum.
          END.
        WHEN "DAG" then
          DO:
            IF wTil > (TODAY - DATE(1,1,YEAR(TODAY)) + 1) then
              assign
                wPerLin2 = (TODAY - DATE(1,1,YEAR(TODAY)) + 1).
                wTil     = (TODAY - DATE(1,1,YEAR(TODAY)) + 1).
          END.
      END CASE.
    END.

  /* Siste †r, hvis forespurt periode ligger i tidligere †r. */
  IF wLoop1 = wAar2 then
    assign
      wFra = 1
      wTil = (if wPerId = "AAR"
                then 1
              else if wPerId = "MANED"
                then 12
              else if wPerId = "UKE"
                then 53
              else 366).
  /* F›rste †r. */
  else IF wLoop1 = wAAr1 then
    assign
      wFra = wPerLin1
      wTil = (if wPerId = "AAR"
                then 1
              else if wPerId = "MANED"
                then 12
              else if wPerId = "UKE"
                then 53
              else 366).
  /* mellomliggende †r. */
  ELSE assign
    wFra = 1
    wTil = (if wPerId = "AAR"
                then 1
              else if wPerId = "MANED"
                then 12
              else if wPerId = "UKE"
                then 53
              else 366).

  PERIODELINJE:
  DO wLoop2 = wFra TO wTil:
    FOR EACH StLinje NO-LOCK where
      StLinje.DataObjekt = wDataObjekt and
      StLinje.StTypeId   = wStTypeId and
      StLinje.PerId      = wPerId and
      StLinje.AAr        = wLoop1 and
      StLinje.PerLinNr   = wLoop2 AND
      StLinje.Butik      > 0
      {&Butikk}:

      CREATE tStLinje.
      BUFFER-COPY StLinje TO tStLinje.

      /* Er det statistikk for varegruppe som hentes, skal HG informasjon legges p†. */
      CASE wStTypeId:
        WHEN "VAREGR" then
          assign
            tStLinje.Hg = wHg.
      END CASE.
    END.
  END. /* PERIODELINJE */

END. /* AARSLOOPEN */

/* For † f† korrekte lagerverdier, m det finnes poster for siste periode. */
/* Benytter wTil verdi fra forrige blokken.                               */
if wSettLager THEN /* TEST */
LAGERLOOP:
FOR EACH Lager NO-LOCK where
  Lager.ArtikkelNr = int(wDataObjekt) and
  Lager.Butik > 0:

  if wButik = 0 THEN. /* Gj›r ingenting. */
  ELSE if NOT CAN-DO(STRING(wButik),STRING(Lager.Butik)) then
    NEXT LAGERLOOP.

  if not CAN-FIND(first tStLinje where
              tStLinje.StTypeId   = wStTypeId and
              tStLinje.PerId      = wPerId and
              tStLinje.DataObjekt = wDataObjekt and
              tStLinje.Diverse    = "" and
              tStLinje.Butik      = Lager.Butik and
              tStLinje.AAr        = wAAr2 and
              tStLinje.PerLinNr   = wPerlin2) then
    DO:
      CREATE tStLinje.
      assign
        tStLinje.StTypeId   = wStTypeId
        tStLinje.PerId      = wPerId
        tStLinje.DataObjekt = wDataObjekt
        tStLinje.Diverse    = ""
        tStLinje.Butik      = Lager.Butik
        tStLinje.Aar        = wAAr2
        tStLinje.PerLinNr   = wPerlin2.
/*
MESSAGE "TypeId:" tStLinje.StTypeId  skip
        "PerId:"      tStLinje.PerId     skip
        "DataObjekt"  tStLinje.DataObjekt skip
        "Diverse:"      tStLinje.Diverse    skip
        "Butik:"      tStLinje.Butik      skip
        "Aar:"      tStLinje.AAr        skip
        "PerLin"      tStLinje.PerLinNr skip
        "LAger:" LAger.LagAnt
        VIEW-AS ALERT-BOX.
*/
    END.
END. /* LAGERLOOP */
END. /* STATOPPRETTELSE */

