/* get_KOrdreHode.p
   Purpose: Hengte fakturaer for print
   Parameters: entry(1,icParam,"|"): Liste
               entry(2,icParam,"|"): WHERE betingelse
-------------------------------------------------------------------------*/              
 
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR cIDliste   AS CHAR  NO-UNDO.
DEF VAR cDummy     AS CHAR   NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.

IF ENTRY(2,icParam,"|") = "UTLEV" THEN
    ASSIGN ENTRY(2,icParam,"|") = "".

IF icSessionId NE "validsession" THEN
  {incl/validatesession.i}

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    1,
    0,
    "",
    "KOrdreHode",
    IF ENTRY(2,icParam,"|") = "" THEN
      "WHERE false, NO-LOCK"
    ELSE ENTRY(2,icParam,"|"),
    "",
    "",
    OUTPUT TABLE-HANDLE ohTempTable,
    OUTPUT cDummy,
    OUTPUT ocReturn)
    .

IF ENTRY(2,icParam,"|") = "" THEN DO:
  ASSIGN httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE
         cIDliste  = ENTRY(1,icParam,"|").
 
  DO ix = 1 TO NUM-ENTRIES(cIDliste):
    FOR FIRST KOrdreHode NO-LOCK
        WHERE KOrdreHode.KOrdre_Id = DEC(ENTRY(ix,cIDliste)):

      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-COPY(BUFFER KOrdreHode:HANDLE).
      FIND Kunde OF KOrdreHode NO-LOCK NO-ERROR.
      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INTEGER(KOrdreHode.ButikkNr) NO-ERROR.

      IF AVAILABLE Butiker THEN 
      DO:
        IF TRIM(httBuffer:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) = '' THEN 
        DO:
          httBuffer:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE = Butiker.ButNamn.
        END.
      END.

      IF AVAIL Kunde THEN DO:
        IF TRIM(httBuffer:BUFFER-FIELD("DeresRef"):BUFFER-VALUE) = '' THEN 
        DO:
          httBuffer:BUFFER-FIELD("DeresRef"):BUFFER-VALUE = Kunde.KontNavn.
        END.
        IF TRIM(httBuffer:BUFFER-FIELD("KontTelefon"):BUFFER-VALUE) = '' THEN 
        DO:
          httBuffer:BUFFER-FIELD("KontTelefon"):BUFFER-VALUE = IF Kunde.KontTelefon <> "" THEN Kunde.KontTelefon ELSE Kunde.KontMobilTlf.
        END.
        IF TRIM(httBuffer:BUFFER-FIELD("KontNavn"):BUFFER-VALUE) = '' THEN 
        DO:
          httBuffer:BUFFER-FIELD("KontNavn"):BUFFER-VALUE = Kunde.KontNavn.
        END.
        IF TRIM(httBuffer:BUFFER-FIELD("Adresse1"):BUFFER-VALUE) = '' THEN 
        DO:
          FIND Post WHERE Post.Postnr = KOrdreHode.PostNr NO-LOCK NO-ERROR.
          httBuffer:BUFFER-FIELD("Adresse1"):BUFFER-VALUE = Kunde.Adresse1.
          httBuffer:BUFFER-FIELD("Adresse2"):BUFFER-VALUE = Kunde.Adresse2.
          httBuffer:BUFFER-FIELD("PostNr"):BUFFER-VALUE   = Kunde.PostNr.
          httBuffer:BUFFER-FIELD("PostSted"):BUFFER-VALUE = IF AVAIL Post THEN Post.Beskrivelse ELSE KOrdreHode.PostSted.
        END.
        IF TRIM(httBuffer:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE) = '' THEN 
        DO:
          FIND Post WHERE Post.Postnr = KOrdreHode.FaktPostNr NO-LOCK NO-ERROR.
          httBuffer:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE = Kunde.FaktAdresse1.
          httBuffer:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE = Kunde.FaktAdresse2.
          httBuffer:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE   = Kunde.FaktPostNr.
          httBuffer:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE = IF AVAIL Post THEN Post.Beskrivelse ELSE KOrdreHode.FaktPostSted.
        END.
        IF TRIM(httBuffer:BUFFER-FIELD("LevAdresse1"):BUFFER-VALUE) = '' THEN 
        DO:  
          FIND Post WHERE Post.Postnr = KOrdreHode.LevPostNr NO-LOCK NO-ERROR.
          httBuffer:BUFFER-FIELD("LevAdresse1"):BUFFER-VALUE = Kunde.LevAdresse1.
          httBuffer:BUFFER-FIELD("LevAdresse2"):BUFFER-VALUE = Kunde.LevAdresse2.
          httBuffer:BUFFER-FIELD("LevPostNr"):BUFFER-VALUE   = Kunde.LevPostNr.
          httBuffer:BUFFER-FIELD("LevPostSted"):BUFFER-VALUE = IF AVAIL Post THEN Post.Beskrivelse ELSE KOrdreHode.LevPostSted.
        END.
      END.
    END.
  END.
END.

DELETE OBJECT ohTempTable.
