&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fix-ryns_gavetilgode_konvertering.p
    Purpose     :

    Syntax      :

    Description : Leser inn filen og omformer den til en INV<NNN..>.<ButNr> fil.

    Author(s)   : Tom Nøkleby
    Created     : 25/8-13
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE iTotAntLinjer AS INT  NO-UNDO.
DEFINE VARIABLE cLinje        AS CHAR NO-UNDO.
DEFINE VARIABLE cFilNavn      AS CHAR NO-UNDO.
DEFINE VARIABLE cVPIFil       AS CHAR NO-UNDO.
DEFINE VARIABLE ctmpKatalog   AS CHAR NO-UNDO.
DEFINE VARIABLE pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF VAR cGenEan       AS CHAR NO-UNDO.
DEFINE VARIABLE cDefVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGenInterleave AS CHARACTER NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEFINE TEMP-TABLE ttGT
   /*  1 */ FIELD ID AS CHARACTER 
   /*  2 */ FIELD MedlemsNr AS CHARACTER 
   /*  3 */ FIELD KundeNr AS CHARACTER 
   /*  4 */ FIELD Navn AS CHARACTER 
   /*  5 */ FIELD Telefon AS CHARACTER 
   /*  6 */ FIELD GyldigFraDato AS CHARACTER 
   /*  7 */ FIELD GyldigTilDato  AS CHARACTER
   /*  8 */ FIELD Dato  AS CHARACTER
   /*  9 */ FIELD Tid  AS CHARACTER
   /* 10 */ FIELD TYPE  AS CHARACTER
   /* 11 */ FIELD Belop  AS CHARACTER
   /* 12 */ FIELD ButikkNr  AS CHARACTER
   /* 13 */ FIELD KasseNr AS CHARACTER
   /* 14 */ FIELD BongNr AS CHARACTER
   /* 15 */ FIELD Selger AS CHARACTER
   /* 16 */ FIELD DatoInnlost AS CHARACTER
   /* 17 */ FIELD TidInnlost AS CHARACTER
   /* 18 */ FIELD ButikkInnlost AS CHARACTER
   /* 19 */ FIELD KasseNrInnlost AS CHARACTER
   /* 20 */ FIELD BongNrInnlost AS CHARACTER
   /* 21 */ FIELD SelgerInnlost AS CHARACTER
    INDEX Id TYPE Id. 

DEF BUFFER bttGT FOR ttGT.

{windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
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

ASSIGN
    cFilNavn = '.\kom\in\PRSpfxuser_respt.csv'.

{syspara.i 2 4 8 cGenEan}

ASSIGN
    ctmpKatalog = '.\kom\in\' /*SESSION:TEMP-DIRECTORY*/
    cVPIFil     = "PRSGavTil" + STRING(TODAY,"99-99-9999") + "-" + STRING(TIME,"HH:MM:SS") + ".csv"
    cVPIFil     = REPLACE(cVPIFil,":","-")
    .

RUN LesInnFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksportINVFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportINVFil Procedure 
PROCEDURE EksportInvFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* MESSAGE ctmpKatalog + cVPIUtFil SKIP   */
/*     iAntLinjer                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF iAntLinjer > 0 THEN
DO:
    OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + '\' + cVPIFil) NO-ECHO.                    
    EKSPORTFIL:                    
    FOR EACH ttGT NO-LOCK:

        PUT STREAM UtVPI UNFORMATTED  
          ttGT.ID ';' 
          ttGT.MedlemsNr ';' 
          ttGT.KundeNr ';' 
          ttGT.Navn ';' 
          ttGT.Telefon ';' 
          ttGT.GyldigFraDato ';' 
          ttGT.GyldigTilDato ';'
          ttGT.Dato ';'
          ttGT.Tid ';'
          ttGT.TYPE ';'
          ttGT.Belop ';'
          ttGT.ButikkNr ';'
          ttGT.KasseNr ';'
          ttGT.BongNr ';'
          ttGT.Selger ';'
          ttGT.DatoInnlost ';'
          ttGT.TidInnlost ';'
          ttGT.ButikkInnlost ';'
          ttGT.KasseNrInnlost ';'
          ttGT.BongNrInnlost ';'
          ttGT.SelgerInnlost
        SKIP.  

    END. /* EKSPORTFIL */
    MESSAGE 'PRSGavTil fil er generert: ' + ctmpKatalog + '\' + cVPIFil
        VIEW-AS ALERT-BOX.
    OUTPUT STREAM UtVpi CLOSE.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + cFilNavn skip
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       


------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStrl AS CHARACTER NO-UNDO.      
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttGT TRANSACTION:
      DELETE ttGT.
  END.
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    assign
      iAntLinjer = iAntLinjer + 1      
      .

    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 22 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
        NEXT LESERLINJER.
    END.

    /* Tomme linjer fra Excel */
    IF pcLinje BEGINS ";;;;;:" THEN
        NEXT LESERLINJER.
    /* OVerskriftsrad 2 */
    IF pcLinje BEGINS "recnum" THEN
        NEXT LESERLINJER.

    /* Innløste skal ikke med */
    IF TRIM(ENTRY(14,pcLinje,';')) <> '' THEN
        NEXT LESERLINJER.

    CREATE ttGT.

    ASSIGN
        ttGT.ID             = TRIM(ENTRY(2,pcLinje,';')) 
        ttGT.MedlemsNr      = '' /*TRIM(ENTRY(2,pcLinje,';'))*/ 
        ttGT.KundeNr        = '' /*TRIM(ENTRY(2,pcLinje,';'))*/ 
        ttGT.Navn           = TRIM(ENTRY(3,pcLinje,';')) 
        ttGT.Telefon        = TRIM(ENTRY(4,pcLinje,';')) 
        ttGT.GyldigFraDato  = TRIM(ENTRY(5,pcLinje,';')) 
        ttGT.GyldigTilDato  = ''
        ttGT.Dato           = TRIM(ENTRY(5,pcLinje,';'))
        ttGT.Tid            = ''
        ttGT.TYPE           = TRIM(ENTRY(6,pcLinje,';'))
        ttGT.Belop          = TRIM(ENTRY(7,pcLinje,';'))
        ttGT.ButikkNr       = TRIM(ENTRY(10,pcLinje,';'))
        ttGT.KasseNr        = '1'
        ttGT.BongNr         = TRIM(ENTRY(12,pcLinje,';'))
        ttGT.Selger         = TRIM(ENTRY(13,pcLinje,';'))
        ttGT.DatoInnlost    = TRIM(ENTRY(14,pcLinje,';'))
        ttGT.TidInnlost     = ''
        ttGT.ButikkInnlost  = TRIM(ENTRY(15,pcLinje,';'))
        ttGT.KasseNrInnlost = '1'
        ttGT.BongNrInnlost  = TRIM(ENTRY(17,pcLinje,';'))
        ttGT.SelgerInnlost  = TRIM(ENTRY(18,pcLinje,';'))
        .
                
     IF ttGT.TYPE = 'P' THEN
         ttGT.TYPE = '1'.
     IF ttGT.TYPE = 'T' THEN
         ttGT.TYPE = '2'.

     STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Eksporterer til VPI fil. */
  RUN EksportInvFil.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  repeat:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */
