&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSStrTypSGIDHOInnles.p
    Purpose     : Innlesning av data til størrelsestyperegister

    Syntax      : xPRSStrTypSGIDHOInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i størrelsestyperegisteret. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 21/09/2018
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

/* Leser alle arkene i excel arket. */
DO iLoop = 1 TO iSheets:
/*    RUN LesInnFil. */
/*    RUN PosterData.*/
END.

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
  RUN bibl_logg.p (cLogg, 'xPRSStrTypeInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

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
    IF NUM-ENTRIES(pcLinje,";") < 6 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 6 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    CREATE ttStrType.

    RUN AssignInt(1,OUTPUT ttStrType.StrTypeId, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(4,OUTPUT ttStrType.Hg, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(5,OUTPUT ttStrType.AvdelingNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
                            
    /*
    RUN AssignDec(17,OUTPUT ttLevBas.Rab1%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    */
    
    ASSIGN 
      ttStrType.KortNavn     = ENTRY(2,pcLinje,';')           
      ttStrType.Beskrivelse  = ENTRY(3,pcLinje,';')           
    NO-ERROR. 
    
    /* Legger opp størrelsene */
    DO piLoop = 6 TO NUM-ENTRIES(pcLinje,';'):
      IF piLoop = 6 AND TRIM(ENTRY(PiLoop,pcLinje,';')) = '' THEN 
        ENTRY(PiLoop,pcLinje,';') = ' 1'.
      IF TRIM(ENTRY(PiLoop,pcLinje,';')) = '' THEN 
        NEXT.
      CREATE ttStrTStr.
      ASSIGN 
        ttStrTStr.StrTypeId = ttStrType.StrTypeId
        ttStrTStr.SeqNr     = piLoop - 5
        ttStrTStr.SoStorl   = TRIM(TRIM(ENTRY(PiLoop,pcLinje,';')),'"')
        .
      RUN bibl_fixstorl.p (ttStrTStr.SoStorl,?,'',OUTPUT ttStrTStr.SoStorl,OUTPUT bOk).
      IF NOT CAN-FIND(StrKonv WHERE 
                      StrKonv.Storl = ttStrTStr.SoStorl) THEN 
      DO:
        CREATE StrKonv.
        ASSIGN 
          iStrKode        = iStrKode + 1
          StrKonv.StrKode = iStrKode 
          StrKonv.Storl   = ttStrTStr.SoStorl
          .
      END.
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

 
&IF DEFINED(EXCLUDE-PosterData) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterData Procedure
PROCEDURE PosterData:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  
  FOR EACH ttStrType TRANSACTION:
    FIND StrType EXCLUSIVE-LOCK WHERE 
      StrType.StrTypeId = ttStrType.StrTypeId NO-ERROR.
    IF NOT AVAILABLE StrType THEN 
    DO:
      CREATE StrType.
      ASSIGN
        StrType.StrTypeId = ttStrType.StrTypeId.
    END.

    IF ttStrType.KortNavn <> ''    THEN StrType.KortNavn    = ttStrType.KortNavn.          
    IF ttStrType.Beskrivelse <> '' THEN StrType.Beskrivelse = ttStrType.Beskrivelse.           
    IF ttStrType.Hg <> 0           THEN StrType.Hg          = ttStrType.Hg.          
    IF ttStrType.AvdelingNr <> 0   THEN StrType.AvdelingNr  = ttStrType.AvdelingNr.          

    /* Tar bort eventuelle gamle størrelser */
    FOR EACH StrTStr OF StrType EXCLUSIVE-LOCK:
      DELETE StrTStr.
    END.
    
    /* Legger opp størrelser */
    FOR EACH ttStrTStr WHERE 
      ttStrTStr.StrTypeId = ttStrType.StrTypeId
      BREAK BY ttStrTStr.StrTypeId
            BY ttStrTStr.SeqNr:
      IF NOT CAN-FIND(FIRST StrTStr WHERE 
        StrTStr.StrTypeId = ttStrTStr.StrTypeId AND 
        StrTStr.SeqNr     = ttStrTStr.SeqNr) THEN 
      DO: 
          CREATE StrTStr.
          ASSIGN 
            StrTStr.StrTypeId = ttStrTStr.StrTypeId
            StrTStr.SeqNr     = ttStrTStr.SeqNr
            StrTStr.SoStorl   = ttStrTStr.SoStor
            .
      RELEASE StrTStr.
      END.  
    END.
    
    RUN settStrTypeFelt.p (StrType.StrTypeID).
    RELEASE StrType.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

