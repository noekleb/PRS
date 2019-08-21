DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icAction    AS CHARACTER   NO-UNDO.  /* Create or Update */
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFields       AS CHARACTER NO-UNDO.  /* Last modified field */
DEFINE VARIABLE cFieldValues  AS CHARACTER NO-UNDO.   
DEFINE VARIABLE iPkSdlLinjeId AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOk2 AS LOG NO-UNDO.
DEFINE VARIABLE bSkipJBoxInit AS LOG NO-UNDO.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE) NO-ERROR.

/*MESSAGE 'pksdllinje_post_update.p test-2' SKIP*/
/*'   cFields ' cFields SKIP                    */
/*'   cFieldValues ' cFieldValues               */
/*VIEW-AS ALERT-BOX.                            */

/* Legger på linjenr. */
IF ihBuffer:BUFFER-FIELD("PksdlLinjeId"):BUFFER-VALUE = 0 THEN 
DO:
  FIND LAST bufPkSdlLinje NO-LOCK WHERE 
    bufPkSdlLinje.PkSdlId = ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE USE-INDEX PkSdlLinje NO-ERROR.
  IF AVAILABLE bufPkSdlLinje THEN 
    ihBuffer:BUFFER-FIELD("PksdlLinjeId"):BUFFER-VALUE = bufPkSdlLinje.PkSdlLinjeId + 1.
  ELSE  
    ihBuffer:BUFFER-FIELD("PksdlLinjeId"):BUFFER-VALUE = 1.
  ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("PksdlLinjeId"):BUFFER-VALUE.
END.

IF DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) = "Antall" THEN
  ASSIGN
    ihBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE. 

/* Beregner rest. */ 
ihBuffer:BUFFER-FIELD("AntRest"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE     
  - ihBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE. 

/* Sjekker og eventuelt legger opp pris. */
RUN pksdllinje_opprett_pris.p (STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + '|' + STRING(ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE),
                               ?,
                               '',
                               OUTPUT ocReturn,
                               OUTPUT bOk2
                               ).

FIND ArtBas NO-LOCK WHERE 
  ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
IF AVAILABLE ArtBas THEN 
BERIK_RECORD: 
DO:
  IF AVAILABLE ArtBas AND STRING(ihBuffer:BUFFER-FIELD("SalgsEnhet"):BUFFER-VALUE) = '' THEN 
    ASSIGN
      ihBuffer:BUFFER-FIELD("SalgsEnhet"):BUFFER-VALUE = ArtBas.Salgsenhet
      .      
END. /* BERIK_RECORD */

