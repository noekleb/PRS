/* Bruk av parametere:
   entry(1,"¤"): Fra registreringsdato, bestilling
   entry(2,"¤"): Til registreringsdato, bestilling
   entry(3,"¤"): Dersom denne er ulik blank skal ikke antall telles dersom ordre er sendt
----------------------------------------------------------------------------------------------------------------------*/   

DEF INPUT  PARAM irVarebehLinje   AS ROWID NO-UNDO.
DEF INPUT  PARAM icBestHodeFilter AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId      AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue          AS CHAR NO-UNDO.

DEF VAR fSumKost  AS DEC NO-UNDO.
DEF VAR dRegFra   AS DATE NO-UNDO.
DEF VAR dRegTil   AS DATE NO-UNDO.

IF icBestHodeFilter NE "" THEN DO:
  IF ENTRY(1,icBestHodeFilter,"¤") NE "" THEN
    dRegFra = DATE(ENTRY(1,icBestHodeFilter,"¤")).
  IF ENTRY(2,icBestHodeFilter,"¤") NE "" THEN
    dRegTil = DATE(ENTRY(2,icBestHodeFilter,"¤")).
END.

FOR FIRST VarebehLinje FIELDS()
    WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK,
    FIRST VarebehHode FIELDS(VareBehType) NO-LOCK OF VarebehLinje,
          EACH VareBehBestHode NO-LOCK OF VarebehLinje
          WHERE (IF dRegFra NE ? THEN VarebehBestHode.RegistrertDato GE dRegFra ELSE TRUE) 
            AND (IF dRegTil NE ? THEN VarebehBestHode.RegistrertDato LE dRegTil ELSE TRUE):
  IF VarebehHode.VarebehType = 2 THEN
    fSumKost = fSumKost + VarebehBestHode.VerdiLevert.
  ELSE FOR FIRST BestHode FIELDS(TotInnkjVerdi BestStat) NO-LOCK OF VarebehBestHode:
    IF NOT (NUM-ENTRIES(icBestHodeFilter,"¤") > 2 AND ENTRY(3,icBestHodeFilter,"¤") NE "" AND BestHode.BestStat > 3) THEN 
      fSumKost = fSumKost + BestHode.TotInnkjVerdi.
  END.
END.
ocValue = STRING(fSumKost).
