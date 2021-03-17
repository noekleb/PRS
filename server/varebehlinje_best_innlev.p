/* Hent innleveringslinjer eller bestillingslinjer for vareh.bok butikk
   Hvis bestilling så hentes verdier direkte fra bestilling, ellers fra varebehbestlinje
   
   Parameter: VarebehNr,ArtikkelNr,HodeLinjeId (fra varebehbesthode)


----------------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOK              AS LOG NO-UNDO.
DEF VAR hBuffMenu        AS HANDLE NO-UNDO.
DEF VAR httBuffer        AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.

{incl/tt_varebehbestlinje.i}
hTempTable = BUFFER tt_VarebehBestLinje:TABLE-HANDLE.

DEF VAR fVarebehNr    AS DEC NO-UNDO.
DEF VAR fArtikkelNr   AS DEC NO-UNDO.
DEF VAR iHodeLinjeId  AS INT NO-UNDO.

ASSIGN fVarebehNr    = DEC(ENTRY(1,icParam))
       fArtikkelNr   = DEC(ENTRY(2,icParam))
       iHodeLinjeId  = INT(ENTRY(3,icParam))
       .

FIND VarebehHode WHERE VarebehHode.VarebehNr = fVarebehNr NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Finner ikke varehåndteringsbok " + ENTRY(1,icParam) + CHR(10) + PROGRAM-NAME(1).
  RETURN.
END.

FIND FIRST VarebehLinje OF VarebehHode NO-LOCK 
     WHERE VarebehLinje.ArtikkelNr = fArtikkelNr
     NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Finner ikke artikkel " + ENTRY(2,icParam) + " i varehåndteringsbok " + ENTRY(1,icParam) + CHR(10) + PROGRAM-NAME(1).
  RETURN.
END.
    
FIND FIRST VarebehBestHode OF VarebehLinje NO-LOCK
     WHERE VarebehBestHode.HodeLinjeId = iHodeLinjeId
     NO-ERROR.
IF NOT AVAIL VarebehBestHode THEN DO:
  ocReturn = "Finner ikke best/innlevering for artikkel " + ENTRY(2,icParam) + " i varehåndteringsbok " + ENTRY(1,icParam) + CHR(10) + PROGRAM-NAME(1).
  RETURN.
END.

/* Bestilling: */
IF VarebehHode.VarebehType = 1 THEN 
  FOR EACH BestStr NO-LOCK
      WHERE BestStr.BestNr = VarebehBestHode.BestNr:
    CREATE TT_VareBehBestLinje.
    BUFFER-COPY VarebehBestHode TO TT_VareBehBestLinje.
    BUFFER-COPY BestStr TO TT_VareBehBestLinje.
    ASSIGN TT_VareBehBestLinje.storl = TRIM(TT_VareBehBestLinje.storl)
           TT_VareBehBestLinje.BestiltButikkNr = BestStr.Butik.
  END.

ELSE FOR EACH VareBehBestLinje OF VarebehBestHode NO-LOCK:
  CREATE TT_VareBehBestLinje.
  BUFFER-COPY VarebehBestLinje TO TT_VareBehBestLinje.
END.

DELETE OBJECT hTempTable.
