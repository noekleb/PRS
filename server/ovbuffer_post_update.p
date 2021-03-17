/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbuffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields       AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldValues  AS CHAR  NO-UNDO.   
DEFINE VARIABLE lVVareKost AS DECIMAL NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cFelt AS CHARACTER NO-UNDO.

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

FIND OvBuffer WHERE OvBuffer.BuntNr = INT(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE) AND 
  OvBuffer.LinjeNr = INT(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE) 
  NO-LOCK NO-ERROR.
IF NOT AVAIL OvBuffer THEN DO:
  ocValue = "".
  RETURN.
END.
ELSE DO:
  IF icAction = 'CREATE' THEN 
  OPPRETT:
  DO:
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
    FIND Lager NO-LOCK WHERE 
      Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
      Lager.Butik = INT(ihBuffer:BUFFER-FIELD("ButikkNrFra"):BUFFER-VALUE) NO-ERROR.
    IF AVAILABLE Lager AND Lager.VVarekost <> ? AND Lager.VVarekost > 0 THEN 
      lVVareKost = Lager.VVarekost.
    ELSE IF AVAILABLE ArtPris THEN 
      lVVareKost = ArtPris.VareKost[1].
       
    FIND FIRST OvBuffer EXCLUSIVE-LOCK WHERE 
      OvBuffer.BuntNr = INT(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE) AND 
      OvBuffer.ArtikkelNr = ArtBas.ArtikkelNr AND 
      OvBuffer.Storl = ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE AND 
      OvBuffer.ButikkNrFra = INT(ihBuffer:BUFFER-FIELD("ButikkNrFra"):BUFFER-VALUE) AND 
      OvBuffer.ButikkNrTil = INT(ihBuffer:BUFFER-FIELD("ButikkNrTil"):BUFFER-VALUE) NO-ERROR.
    IF NOT AVAILABLE OvBuffer THEN
    DO: 
      CREATE OvBuffer.
      ASSIGN 
        OvBuffer.BuntNr      = INT(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE)
        OvBuffer.LinjeNr     = INT(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE)
        OvBuffer.ButikkNrFra = INT(ihBuffer:BUFFER-FIELD("ButikkNrFra"):BUFFER-VALUE)  
        OvBuffer.ButikkNrTil = INT(ihBuffer:BUFFER-FIELD("ButikkNrTil"):BUFFER-VALUE) 
        .
    END.
    ELSE DO:        
      ASSIGN 
        Ovbuffer.ArtikkelNr     = ArtBas.ArtikkelNr
        Ovbuffer.Vg             = ArtBas.Vg
        Ovbuffer.LopNr          = ArtBas.LopNr
        Ovbuffer.VareKost       = lVVareKost
        Ovbuffer.Mva%           = IF AVAILABLE ArtPris THEN ArtPris.Mva%[1] ELSE 0
        OvBuffer.TilStorl       = OvBuffer.Storl
      .
    END.
  END. /* OPPRETT */
  ELSE 
  OPPDATER:
  DO:
    IF cFelt = '' THEN 
      LEAVE OPPDATER.
    DO iLoop = 1 TO NUM-ENTRIES(cFields).
      cFelt = ENTRY(iLoop,cFields).
      CASE cFelt:
/* TN 16/6-20 Dette håndteres automatisk av jukebox. Det er bare når felt som ikke ligger i datasett det skal gjøres noe.      */
/*        WHEN 'Avvis' THEN                                                                                                    */
/*          DO:                                                                                                                */
/*            ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE = 'OrgAntall=' + ENTRY(iLoop,cFieldValues,'|').                    */
/*          END.                                                                                                               */
/*        WHEN 'AngreAvvis' THEN                                                                                               */
/*          DO:                                                                                                                */
/*            IF NUM-ENTRIES(ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE,'=') = 2 THEN                                       */
/*            DO:                                                                                                              */
/*              ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE = INT(ENTRY(2,ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE,'=')).*/
/*              ihBuffer:BUFFER-FIELD("Merknad"):BUFFER-VALUE = ''.                                                            */
/*            END.                                                                                                             */
/*          END.                                                                                                               */
/*        WHEN 'Antall' THEN ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE = INT(ENTRY(iLoop,cFieldValues,'|')).                */
      END CASE.
    END.
  END. /* OPPDATER */
END.
