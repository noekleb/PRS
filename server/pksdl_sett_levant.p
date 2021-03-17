/* Sett levert antall for valgte linjer på pakkseddel
   Parameter:  0 eller "antlevert"
   Opprettet: 09.08.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fPkSdlId        AS DEC    NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO.
DEF VAR cBestNrListe    AS CHAR   NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.
DEF VAR fDbKr           AS DEC    NO-UNDO.
DEF VAR fDb%            AS DEC    NO-UNDO.
DEF VAR fMvaKr          AS DEC    NO-UNDO.
DEF VAR fRab1Kr         AS DEC    NO-UNDO.
DEF VAR fRab1%          AS DEC    NO-UNDO.
DEF VAR fEuroKurs       AS DEC    NO-UNDO.
DEF VAR fFrakt%         AS DEC    NO-UNDO.
DEF VAR fFrakt          AS DEC    NO-UNDO.
DEF VAR fVarekost       AS DEC    NO-UNDO.
DEF VAR fInnkjopsPris   AS DEC    NO-UNDO.
DEF VAR fPris           AS DEC    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR iCurrBestNr     AS INT    NO-UNDO.
DEF VAR iMottaksId      AS INT    NO-UNDO INIT 1.
DEF VAR cButliste       AS CHAR   NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE MottaksId = 0").
hQuery:QUERY-OPEN().

DO ON ERROR UNDO, LEAVE:

  hQuery:GET-FIRST().

  IF ihBuffer:AVAIL THEN 
    FIND FIRST PkSdlHode NO-LOCK
         WHERE PkSdlHode.PkSdlId = DEC(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
         NO-ERROR.
  
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    IF PkSdlHode.PkSdlOpphav = 2 THEN DO:
      hQuery:GET-NEXT().
      NEXT.
    END.

    FIND PkSdlLinje EXCLUSIVE-LOCK 
         WHERE ROWID(PkSdlLinje) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
         NO-ERROR.
    IF NOT AVAIL PkSdlLinje THEN DO:
      ocReturn = "Pakkseddel-linje ikke tilgjengelig for oppdatering".
      UNDO, LEAVE.
    END.
    IF icParam = "0" THEN
      PkSdlLinje.AntLevert = 0.
    ELSE 
      PkSdlLinje.AntLevert = PkSdlLinje.Antall.

    PkSdlLinje.AntRest = PkSdlLinje.Antall - PkSdlLinje.AntLevert.

    hQuery:GET-NEXT().
  END.
  
END.

DELETE OBJECT hQuery NO-ERROR.

obOk = ocReturn = "".
