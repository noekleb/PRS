&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : gantPkSdlKontrollMottatt.p
    Purpose     : Leser en excel fil med pakkseddelnr, og sjekker om pakkseddlene er mottatt.

    Syntax      : 

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 28/03/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iAntLinjer  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTotAntLinjer AS INTEGER  NO-UNDO.
DEFINE VARIABLE cLinje        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilNavn      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtFilNavn AS CHARACTER NO-UNDO.

DEFINE VARIABLE piLinjeNr AS INTEGER  NO-UNDO.
DEFINE VARIABLE pcLinje   AS CHARACTER NO-UNDO.
DEFINE VARIABLE piAntFeil AS INTEGER  NO-UNDO. 

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cExcelFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSheets AS INTEGER NO-UNDO.

DEFINE VARIABLE iProdGroupId AS INTEGER NO-UNDO.
DEFINE VARIABLE iPMGCode AS INTEGER NO-UNDO.
DEFINE VARIABLE iPSGCode AS INTEGER NO-UNDO.
DEFINE VARIABLE iProductTypeCode AS INTEGER NO-UNDO.
DEFINE VARIABLE iActCode AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE STREAM InnFil.
DEFINE STREAM UtFil.

DEFINE BUFFER bStrType FOR StrType.
DEFINE BUFFER bStrTStr FOR StrTStr.


DEFINE TEMP-TABLE ttStrType LIKE StrType
  INDEX StrTypeIdXX StrTypeId.
DEFINE TEMP-TABLE ttStrTStr LIKE StrTstr
  INDEX StrTstrXX StrTypeId SoStorl.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INTEGER
  FIELD Tekst   AS CHARACTER
  .
/*{windows.i}         */
/*{incl/devmode.i}    */
/*{incl/custdevmode.i}*/
/*{AssignRutiner.i}   */

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).


{syspara.i 1 1 52 cFilNavn}
ASSIGN
    cLogg    = "PkSdlKontroll" + REPLACE(STRING(TODAY),'/','')
/*    cFilNavn = "\PkSdlKontroll.xls"*/
    cFilNavn = 'c:\home\lindbak\ankommet'
    cFilNavn = cFilNavn + "\PkSdlKontroll.xlsx" /* Har Exce360 på min maskin. Får derfor ikke kjørt excel direkte. */
    cUtFilNavn = REPLACE(cFilNavn,'Kontroll','KontrollResultat')
    .

/* Er det en excel fil, skal den kovnerteres til csv. */
IF CAN-DO('xls,xlsx',ENTRY(NUM-ENTRIES(cFilNavn,'.'),cFilNavn,'.')) THEN 
DO:
    cExcelFilNavn = cFilNavn.
    rStandardFunksjoner:konvExcel2csv(cExcelFilNavn,'',OUTPUT cFilNavn, OUTPUT iSheets).
    RUN bibl_loggDbFri.p (cLogg,
        '  Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + '.'
        ).    
END.

RUN LesInnFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
 
&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE b2Ok AS LOG NO-UNDO.
DEFINE VARIABLE iStrKode AS INTEGER NO-UNDO.

/*  RUN TellOppLinjer.*/
  RUN bibl_logg.p (cLogg, 'Leser inn fil: ' + cFilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

  ASSIGN
      piLinjeNr  = 0
      pcLinje    = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
      
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  OUTPUT STREAM UtFil TO VALUE(cUtFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      pcLinje    = REPLACE(pcLinje,CHR(9),';')
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
    IF TRIM(ENTRY(1,pcLinje,';')) = '' THEN 
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4;5" THEN
        NEXT LESERLINJER.
    
    /* Skipper tomme linjer. */
    IF TRIM(ENTRY(1,pcLinje,';')) = '' THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 1 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
        NEXT LESERLINJER.
    END.

    FOR EACH PkSdlHode NO-LOCK WHERE 
        PkSdlHode.PkSdlNr = TRIM(ENTRY(1,pcLinje,';')):
            
        FIND FIRST PkSdlMottak OF PkSdlHode NO-LOCK NO-ERROR.
         
        PUT STREAM UtFil UNFORMATTED
            ENTRY(1,pcLinje,';') ';'
            PkSdlHode.PkSdlStatus ';'
            PkSdlHode.SendtDato ';'
            (IF AVAILABLE PkSdlMottak THEN STRING(PkSdlMottak.MottattDato) ELSE '')
            SKIP. 
    END.    
    
    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  OUTPUT STREAM UtFil CLOSE.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

