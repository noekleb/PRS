/* Lagre bestillingsrader (antall pr butikk pr str) 
   Parametere: 
               - VarebehNr,CLButikkNr,HodeLinjeId,levuke,"best" eller "innlev",Direkte lev (yes/no)
                 ELLER (ved generering fra messe):
               - VarebehNr,CLButikkNr,,LevUke,"genbest",ArtikkelNr,LevNr
                 
               - temp-tabell med nye bestillings eller varemottaksrader (TT_VareBehBestLinje)
                 Tabellen inneholder bestillinger for kun en artikkel.
   
   Opprettet: 15.10.04 av BHa   
   Endret:    10.11.05 av BHa
               - Hvis kalt fra genvarebehmesse_best.p (generering fra messeregistrering)
                 så inneholder temptabellen også rader med inndelinger.
                 Dette har ingen konsekvens her egentlig men sjekk create_bestilling.p    
               - Parameter 4, levuke inneholder en dato hvis kallet kommer fra butvarebehhode.w
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR httTable      AS HANDLE NO-UNDO.
DEF VAR hVBBLbuffer   AS HANDLE NO-UNDO.
DEF VAR oiBatchNr     AS INT    NO-UNDO.
DEF VAR iWeekNum      AS INT    NO-UNDO.
DEF VAR ofAntLevert   AS DEC    NO-UNDO.
DEF VAR ofVerdiLevert AS DEC    NO-UNDO.
DEF VAR iLevUke       AS INT    NO-UNDO.

DEF VAR hBuffVarebehBestHode AS HANDLE NO-UNDO.
hBuffVarebehBestHode = BUFFER VarebehBestHode:HANDLE.

IF NUM-ENTRIES(icParam) < 6 THEN DO:
  ocReturn = "Invalid parameters for " + PROGRAM-NAME(1) + CHR(10) + "(invalid number of params)".
  RETURN.
END.

IF ENTRY(5,icParam) NE "best" AND ENTRY(5,icParam) NE "genbest" THEN DO:
  DEF VAR hBuffInnlev AS HANDLE NO-UNDO.
  DEF TEMP-TABLE ttVarebehBestLinje LIKE VarebehBestLinje
      FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr.
  
  hBuffInnlev = BUFFER ttVarebehBestLinje:HANDLE.
END.

hVBBLbuffer = BUFFER VarebehBestLinje:HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

OppdaterBestilling:
DO TRANSACTION ON ERROR UNDO, LEAVE:

  IF ENTRY(5,icParam) = "genbest" THEN DO:
    FIND FIRST Artbas WHERE ArtBas.Artikkelnr = DEC(ENTRY(6,icParam)) NO-LOCK NO-ERROR.
    FIND FIRST StrType OF ArtBas NO-LOCK.

    CREATE VarebehBestHode.
    ASSIGN VarebehBestHode.ArtikkelNr     = DEC(ENTRY(6,icParam))
           VarebehBestHode.levnr          = DEC(ENTRY(7,icParam))
           VarebehBestHode.CLbutikkNr     = INT(ENTRY(2,icParam))
           VarebehBestHode.VarebehNr      = DEC(ENTRY(1,icParam))
           VarebehBestHode.godkjent       = TRUE
           VarebehBestHode.AlfaFordeling  = StrType.AlfaFordeling
           VarebehBestHode.LevUke         = INT(ENTRY(4,icParam))
           VarebehBestHode.DirekteLev     = TRUE
           .

    RUN create_varebehbesthode.p 
        (hBuffVarebehBestHode,
         "VarebehNr,CLbutikkNr",
         ENTRY(1,icParam) + "|" + ENTRY(2,icParam),
         icSessionId,
         OUTPUT ocReturn).
    
    IF ocReturn NE "" THEN DO:
      MESSAGE "Feil i oppretting av temporært bestillingshode (create_varebehbesthode.p)" SKIP
              ocReturn SKIP
              PROGRAM-NAME(1)
              VIEW-AS ALERT-BOX ERROR.
      UNDO, LEAVE.
    END.
    ELSE DO:
      ENTRY(3,icParam) = STRING(VarebehBestHode.HodeLinjeId).
      hQuery:GET-FIRST().
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        ihBuffer:BUFFER-FIELD("HodeLinjeId"):BUFFER-VALUE = VarebehBestHode.HodeLinjeId.
        hQuery:GET-NEXT().
      END.
    END.
  END.
  ELSE DO:
    FIND FIRST VarebehBestHode 
         WHERE VarebehBestHode.VarebehNr   = DEC(ENTRY(1,icParam)) 
           AND VarebehBestHode.CLbutikkNr  = INT(ENTRY(2,icParam)) 
           AND VarebehBestHode.HodeLinjeId = INT(ENTRY(3,icParam))
         EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL VarebehBestHode THEN DO:
      ocReturn = "Bestillingshode for varehåndteringsbok ikke tilgjengelig for oppdatering".
      RETURN.
    END.
    ELSE IF ENTRY(5,icParam) = "best" THEN DO:
      RUN weeknum.p (DATE(ENTRY(4,icParam)),OUTPUT iLevUke).
      ASSIGN VarebehBestHode.LevDato    = DATE(ENTRY(4,icParam))
             VarebehBestHode.LevUke     = iLevUke
             VarebehBestHode.DirekteLev = LOGICAL(ENTRY(6,icParam)).
    END.
    ELSE DO:
      RUN Weeknum.p (TODAY,OUTPUT iWeekNum).
      ASSIGN VarebehBestHode.LevDato = TODAY
             VarebehBestHode.LevUke  = iWeekNum.
    END.
  END.

  /* Erstatter innholdet i VarebehBestLinje med nye registreringer: */

  FOR EACH VarebehBestLinje 
      WHERE VarebehBestLinje.VarebehNr   = DEC(ENTRY(1,icParam))
        AND VarebehBestLinje.CLButikkNr  = INT(ENTRY(2,icParam))
        AND VarebehBestLinje.HodeLinjeId = INT(ENTRY(3,icParam))
      EXCLUSIVE-LOCK:
    DELETE VarebehBestLinje.
  END.

  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    hVBBLbuffer:BUFFER-CREATE().
    hVBBLbuffer:BUFFER-COPY(ihBuffer).

    IF ENTRY(5,icParam) NE "best" AND ENTRY(5,icParam) NE "genbest" THEN DO:
      hBuffInnlev:BUFFER-CREATE().
      hBuffInnlev:BUFFER-COPY(ihBuffer).
      hBuffInnlev:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = VarebehBestHode.ArtikkelNr.
    END.

    hQuery:GET-NEXT().
  END.

  IF ENTRY(5,icParam) = "best" OR ENTRY(5,icParam) = "genbest" THEN do:
    RUN create_bestilling.p (DEC(ENTRY(1,icParam)),  /* VarebehNr */
                             INT(ENTRY(2,icParam)),  /* CL Butikk */
                             INT(ENTRY(3,icParam)),  /* HodeLinjeId */
                             ENTRY(5,icParam),       
                             OUTPUT ocReturn
                             ).
    IF ocReturn NE "" THEN
      MESSAGE "Feil i oppretting av bestilling (create_bestilling.p)" SKIP
              ocReturn SKIP
              PROGRAM-NAME(1)
              VIEW-AS ALERT-BOX ERROR.
    
  END.
  ELSE DO:

    RUN create_innlevering.p (hBuffInnlev,
                             OUTPUT oiBatchNr,
                             OUTPUT ofAntLevert,
                             OUTPUT ofVerdiLevert,
                             OUTPUT ocReturn
                             ).
    IF oiBatchNr NE 0 THEN
      ASSIGN VarebehBestHode.BestNr = oiBatchNr
             VarebehBestHode.AntLevert = ofAntLevert
             VarebehBestHode.VerdiLevert = ofVerdiLevert
             .
  END.
  IF ocReturn NE "" AND NOT ocReturn BEGINS "etikett" THEN 
    UNDO, LEAVE OppdaterBestilling.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" OR ocReturn BEGINS "etikett" THEN 
  obOk = TRUE.

