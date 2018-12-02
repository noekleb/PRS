/* Fordeling av fraktkostnad pakkseddel - fordeles ihht varekost
   Parameter:  PkSdlId
   Opprettet: 24.08.07 av BHa     
   Endret:    16.10.07 av BHa
              Fordeler ut fra sum frakt i PkSdlHode ihht til innkjøpspris         
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fAnt          AS DECIMAL NO-UNDO.
DEF VAR hQuery        AS HANDLE  NO-UNDO.
DEF VAR fSumInnkjPris AS DEC     NO-UNDO.
DEF VAR fPkSdlId      AS DEC     NO-UNDO.
DEF VAR bOk           AS LOG     NO-UNDO.


DEF TEMP-TABLE ttPris
    FIELD PkSdlId     AS DEC
    FIELD Artikkelnr  AS DEC
    FIELD fSumInk     AS DEC
    .

ASSIGN fPkSdlId = DECIMAL(ENTRY(1,icParam,";"))
       NO-ERROR.

FIND PkSdlHode NO-LOCK
     WHERE PkSdlHode.PkSdlId = fPkSdlId
     NO-ERROR.
IF NOT AVAIL PkSdlHode THEN DO:
  ocReturn = "Finner ikke pakkseddel " + icParam + " Program: " + PROGRAM-NAME(1).
  RETURN.
END.

ihBuffer = BUFFER PkSdlPris:HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE PkSdlId = " + ENTRY(1,icParam,";")).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  fAnt = 0.
  FOR EACH PkSdlLinje NO-LOCK
      WHERE PkSdlLinje.PkSdlId    = PkSdlPris.PkSdlId
        AND PkSdlLinje.ArtikkelNr = PkSdlPris.ArtikkelNr
      :
    fAnt = fAnt + PkSdlLinje.AntLevert.        
  END.
  IF fAnt > 0 THEN DO:
    CREATE ttPris.
    ASSIGN ttPris.PkSdlId    = PkSdlPris.PkSdlId
           ttPris.ArtikkelNr = PkSdlPris.ArtikkelNr
           ttPris.fSumInk    = fAnt * PkSdlPris.InnkjopsPris
           fSumInnkjPris     = fSumInnkjPris + fAnt * PkSdlPris.InnkjopsPris.
  END.
  hQuery:GET-NEXT(). 
END.

IF NOT CAN-FIND(FIRST ttPris) THEN DO:
  ocReturn = "Ingen varer er innlevert".   
  RETURN.
END.

PrisOppdatering:
DO TRANSACTION ON ERROR UNDO, LEAVE:
  FOR EACH ttPris:
    FIND PkSdlPris EXCLUSIVE-LOCK  
         WHERE PkSdlPris.PkSdlId    = ttPris.PkSdlId
           AND PkSdlPris.ArtikkelNr = ttPris.ArtikkelNr
         NO-ERROR.
    IF AVAIL PkSdlPris THEN DO:        
      ASSIGN PkSdlPris.OverstyrPris = YES
             PkSdlPris.NyFrakt      = PkSdlHode.SumFrakt * ttPris.fSumInk / fSumInnkjPris.
      bOk = ihBuffer:FIND-FIRST("WHERE PkSdlId = " + ENTRY(1,icParam,";") + " AND ArtikkelNr = " + STRING(PkSdlPris.ArtikkelNr)) NO-ERROR.
      IF bOk THEN
        RUN pksdlpris_post_update.p (ihBuffer,"",icSessionId,OUTPUT ocReturn).
      ELSE DO:
        ocReturn = "Finner ikke pakkseddel-pris " + icParam + " Program: " + PROGRAM-NAME(1).
        UNDO, LEAVE PrisOppdatering.
      END.
    END.
    ELSE DO:
      ocReturn = "Pris kunne ikke oppdateres for artikkel " + STRING(ttPris.ArtikkelNr) + ". Program: " + PROGRAM-NAME(1).
      UNDO, LEAVE PrisOppdatering.
    END.
  END.
END.

obOK = ocReturn = "".


