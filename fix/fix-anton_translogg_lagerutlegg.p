&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :


    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

def var cFilNavn as char no-undo.

DEF STREAM Ut.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */



RUN LeggUtVaretrans.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LeggUtVaretrans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggUtVaretrans Procedure 
PROCEDURE LeggUtVaretrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    /* ----------------- */
      1 · butnr         Parameterstyrt
      2 . ean           Scannes eller tastes
      3 · dato          Registreringsdato
      4 · tid           Registreringstidspunkt. Lagres som sekunder etter midnatt.
      5 · loggnr        0
      6 · transtype     Velges blant lovlige verdier (1 – 7 og 9)
      7 . transtekst    Varetekst
      8 · brukerid      Forhåndsvalgt ved oppstart registreringssekvens
      9 · antall        Tastes (default 1)                             
     10 · kostpris      0 (kan tastes ved spesielle behov)             
     11 · salggsum      0 (kan tastes ved spesielle behov)             
     12 · nylagant      0                                              
     13 · gmlagant      0                                              
     
     Filnavn – varetran.butnr (butnr er uformatert, butnr = 1 à varetran.1, butnr = 11 à varetran.11)
     Eksempelfil – varetran.1

    /* Fileksempel */
    1 54491229 01/12/04 19565 0 5 "" "1" 45 9.90 9.90 0 0
    1 7044610022420 03/12/04 17839 0 7 "" "1" 1 0.00 0.00 0 0
    1 7031580812045 03/12/04 17866 0 7 "" "1" 1 0.00 0.00 0 0
    1 7031580812045 03/12/04 17870 0 7 "" "1" 1 0.00 0.00 0 0
    
  174 0221932340013 26/01/09 64887 0 7 "Wilson Squashballer"   8 0 0 0 0
  174 7391762593682 26/01/09 64888 0 7 "Platinum Zorro"   1 0 0 0 0
  174 0299580052640 26/01/09 64888 0 7 "INNEBANDYKØLLE ZETA 96CM/L"   2 0 0 0 0
  174 0221791970017 26/01/09 64888 0 7 "Alpha 100 CM/R"   1 0 0 0 0
  174 7391762582297 26/01/09 64888 0 7 "INNEBANDYKØLLE ZETA 81CM/L"   4 0 0 0 0
    
                         

------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr  AS INT  NO-UNDO.


FOR EACH bUTIKER NO-LOCK:

  ASSIGN
      piLinjeNr  = 1.
      cFilNavn   = 'VARETRAN_AntonLager.' + STRING(Butiker.Butik)
      .
      
      
  output stream ut to value(cFilNavn).
  LESERLINJER:
  for each ArtLag no-lock where ArtLag.Butik = Butiker.Butik:
    if ArtLag.LagAnt = 0 then next.
    
    find ArtBas no-lock where
      ArtBAs.ArtikkelNR = ArtLag.ArtikkelNr no-error.
    if not available ArtBas then next.
    if ArtBas.OPris then next.
    
    find first Strekkode no-lock where
      Strekkode.ArtikkelNr = ArtLag.ArtikkelNr and
      Strekkode.StrKode    = ArtLAg.StrKode no-error.
    if not available Strekkode then
      next.
    
    put stream Ut unformatted
      string(ArtLag.Butik)   ' ' 
      Strekkode.Kode    ' ' 
      today    ' ' 
      time    ' ' 
      '0'    ' ' 
      '7'    
      ' "' + trim(ArtBas.Beskr) + '" '
      '"Gurre"' ' ' 
      string(ArtLag.LagAnt) ' ' 
      '0.00'    ' ' 
      '0.00'    ' '
      '0'    ' '
      '0' 
      skip.
  END. /* LESERLINJER */
  output stream ut close.

END. /* Butiker */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

