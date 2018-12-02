/* Finn totalt antall og verdi (varekost) for artikkel på pakkseddel
   Parameter:  <PkSdlId>;<artikkelnr>
   Opprettet: 06.12.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fPkSdlId        AS DEC    NO-UNDO.
DEF VAR fArtikkelNr     AS DEC    NO-UNDO.
DEF VAR iButikkNr       AS INT    NO-UNDO.
DEF VAR fAntall         AS DEC    NO-UNDO.
DEF VAR fVerdi          AS DEC    NO-UNDO.
DEF VAR fAntRest        AS DEC    NO-UNDO.
DEF VAR fVerdiRest      AS DEC    NO-UNDO.

ASSIGN fPkSdlId    = DEC(ENTRY(1,icParam,"|"))
       fArtikkelNr = DEC(ENTRY(2,icParam,"|"))
       iButikkNr   = INT(ENTRY(3,icParam,"|"))
       .

FIND PkSdlPris NO-LOCK 
     WHERE PkSdlPris.PkSdlId = fPkSdlId
       AND PkSdlPris.ArtikkelNr = fArtikkelNr
     NO-ERROR.
IF NOT AVAIL PkSdlPris THEN DO:
  ocReturn = "Finner ikke pakklisteid " + STRING(fPkSdlId) + " - programfeil".
  RETURN.
END.
  

FOR EACH PkSdlLinje OF PkSdlPris NO-LOCK
    WHERE PkSdlLinje.AntLevert > 0
      AND (IF iButikkNr NE 0 THEN PkSdlLinje.ButikkNr = iButikkNr ELSE TRUE)
    :
  ASSIGN fAntall  = fAntall + PkSdlLinje.AntLevert
         fAntRest = fAntRest + PkSdlLinje.AntRest
         .
END.
  
ASSIGN obOk = YES
       ocReturn = STRING(fAntall) + "|" + STRING(fAntall * PkSdlPris.NyVarekost) + "|"
                + STRING(fAntRest) + "|" + STRING(fAntRest * PkSdlPris.NyVarekost)
                  .

