&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSFedasGrpInnles.p
    Purpose     : Innlesning av FEDAS varegrupper

    Syntax      : 

    Description : Leser inn data i FEDAS gruppe tabellene. 

    Author(s)   : Tom Nøkleby
    Created     : 26/09/2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.

DEF VAR piLinjeNr AS INT  NO-UNDO.
DEF VAR pcLinje   AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO. 

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

DEF STREAM InnFil.

DEFINE BUFFER bStrType FOR StrType.
DEFINE BUFFER bStrTStr FOR StrTStr.


DEFINE TEMP-TABLE ttStrType LIKE StrType
  INDEX StrTypeIdXX StrTypeId.
DEFINE TEMP-TABLE ttStrTStr LIKE StrTstr
  INDEX StrTstrXX StrTypeId SoStorl.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}
{incl/devmode.i}
{incl/custdevmode.i}
{AssignRutiner.i}

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



FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cLogg    = cLogg + REPLACE(STRING(TODAY),'/','')
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

/* Er det en excel fil, skal den kovnerteres til csv. */
IF CAN-DO('xls,xlsx',ENTRY(NUM-ENTRIES(cFilNavn,'.'),cFilNavn,'.')) THEN 
DO:
    cExcelFilNavn = cFilNavn.
    PUBLISH 'visStatusMsg' ('Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + '.').    
    rStandardFunksjoner:konvExcel2csv(cExcelFilNavn,'',OUTPUT cFilNavn, OUTPUT iSheets).
    RUN bibl_loggDbFri.p (cLogg,
        '  Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + '.'
        ).    
    PUBLISH 'visStatusMsg' ('Filkonvertering ferdig.').    
END.

RUN LesInnFil.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

IF CAN-FIND(FIRST ttError) THEN
  RUN ErrorLogg.

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

FIND LAST StrKonv NO-LOCK USE-INDEX StrKode NO-ERROR.
IF AVAILABLE StrKonv THEN 
  iStrKode = StrKonv.StrKode + 1.
  ELSE iStrKode = 0.

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p (cLogg, 'Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

  ASSIGN
      piLinjeNr  = 0
      pcLinje    = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
      
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
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
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 10 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 10 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.

    /* NB: 0 er tillatte verdier hos FEDAS. */
    ASSIGN 
        iProdGroupId     = INT(TRIM(ENTRY(1,pcLinje,';')))
        iPMGCode         = INT(TRIM(ENTRY(6,pcLinje,';'))) 
        iPSGCode         = INT(TRIM(ENTRY(8,pcLinje,';'))) 
        iProductTypeCode = INT(TRIM(ENTRY(2,pcLinje,';'))) 
        iActCode         = INT(TRIM(ENTRY(4,pcLinje,';')))
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Ugyldige tall i integer felt) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    FIND FedasActCode EXCLUSIVE-LOCK WHERE 
        FedasActCode.ActCode = iActCode NO-ERROR NO-WAIT.
    IF NOT AVAILABLE FedasActCode AND NOT LOCKED FedasActCode THEN 
    DO:
        CREATE FedasActCode.
        ASSIGN 
            FedasActCode.ActCode  = iActCode
            FedasActCode.Activity = TRIM(ENTRY(5,pcLinje,';'))
            .
    END.
    
    FIND FedasProduct_Group EXCLUSIVE-LOCK WHERE 
        FedasProduct_Group.ProdGroupId = iProdGroupId NO-ERROR NO-WAIT.
    IF NOT AVAILABLE FedasProduct_Group AND NOT LOCKED FedasProduct_Group THEN 
    DO:
        CREATE FedasProduct_Group.
        ASSIGN
            FedasProduct_Group.ProdGroupId          = iProdGroupId
            FedasProduct_Group.ProdGroupDescription = TRIM(ENTRY(10,pcLinje,';'))
            FedasProduct_Group.PMGCode         = iPMGCode
            FedasProduct_Group.PSGCode         = iPSGCode
            FedasProduct_Group.ProductTypeCode = iProductTypeCode
            FedasProduct_Group.ActCode         = iActCode
            .
    END.
    
    FIND FedasProduct_Main_Group EXCLUSIVE-LOCK WHERE 
        FedasProduct_Main_Group.PMGCode = iPMGCode NO-ERROR NO-WAIT.
    IF NOT AVAILABLE FedasProduct_Main_Group AND NOT LOCKED FedasProduct_Main_Group THEN 
    DO:
        CREATE FedasProduct_Main_Group.
        ASSIGN
            FedasProduct_Main_Group.PMGCode            = iPMGCode
            FedasProduct_Main_Group.Product_Main_Group = TRIM(ENTRY(7,pcLinje,';')) 
        .
    END.

    FIND FedasProduct_Sub_Group EXCLUSIVE-LOCK WHERE 
        FedasProduct_Sub_Group.PSGCode = iPSGCode NO-ERROR NO-WAIT.
    IF NOT AVAILABLE FedasProduct_Sub_Group AND NOT LOCKED FedasProduct_Sub_Group THEN 
    DO:
        CREATE FedasProduct_Sub_Group.
        ASSIGN 
            FedasProduct_Sub_Group.PSGCode           = iPSGCode
            FedasProduct_Sub_Group.Product_Sub_Group = TRIM(ENTRY(9,pcLinje,';'))
            .
    END.    
    
    FIND FedasProduct_Type EXCLUSIVE-LOCK WHERE 
        FedasProduct_Type.ProductTypeCode = iProductTypeCode NO-ERROR NO-WAIT.
    IF NOT AVAILABLE FedasProduct_Type AND NOT LOCKED FedasProduct_Type THEN 
    DO:
        CREATE FedasProduct_Type.
        ASSIGN 
            FedasProduct_Type.ProductTypeCode = iProductTypeCode
            FedasProduct_Type.ProductType     = TRIM(ENTRY(3,pcLinje,';'))
            .
    END. 
    
    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

