DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icAction    AS CHARACTER   NO-UNDO.  /* Create or Update */
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFields       AS CHARACTER NO-UNDO.  /* Last modified field */
DEFINE VARIABLE cFieldValues  AS CHARACTER NO-UNDO.   
DEFINE VARIABLE iPkSdlLinjeId AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOk2 AS LOG NO-UNDO.
DEFINE VARIABLE bSkipJBoxInit AS LOG NO-UNDO.
DEFINE VARIABLE fiKamp% AS DECIMAL FORMAT "->>>9.9" NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.

{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

{syspara.i 150 1 2 iButNr INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButNr NO-ERROR.

ASSIGN 
  bTest      = TRUE
  cLogg      = 'kampanjelinje_post_update' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE) NO-ERROR.

ASSIGN 
  fiKamp% = DECIMAL(ENTRY(2,cFieldValues,'|')) 
  .

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cFields.....: ' + cFields 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cFieldValues: ' + cFieldValues 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Artikkelnr..: ' + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Profilnr....: ' + STRING(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    fiKamp%.....: ' + STRING(fiKamp%) 
    ).    
END.

FIND ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
IF AVAILABLE ArtBas THEN
BERIK_RECORD:
DO:
  IF AVAILABLE ArtBas THEN
    ASSIGN
      ihBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE = ArtBas.Vg
      ihBuffer:BUFFER-FIELD("LopNr"):BUFFER-VALUE = ArtBas.LopNr
      .
/*  FIND ArtPris NO-LOCK WHERE                                                                                       */
/*    ArtPris.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND                                 */
/*    ArtPris.ProfilNr   = INT(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) NO-ERROR.                             */
/*  IF AVAILABLE ArtPris THEN                                                                                        */
/*    ASSIGN                                                                                                         */
/*      ihBuffer:BUFFER-FIELD("VareKost"):BUFFER-VALUE = ArtPris.VareKost[1]                                         */
/*      ihBuffer:BUFFER-FIELD("Pris_1"):BUFFER-VALUE = ROUND(ArtPris.Pris[1] - ((ArtPris.Pris[1] * fiKamp%) / 100),2)*/
/*      .                                                                                                            */
END. /* BERIK_RECORD */

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    ETTER: ' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Vg........: ' + STRING(ihBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    LopNr.....: ' + STRING(ihBuffer:BUFFER-FIELD("LopNr"):BUFFER-VALUE) 
    ).    
END.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
