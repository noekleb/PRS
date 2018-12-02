&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSMappingInnles.p
    Purpose     : Innlesning av data til mapping registeret

    Syntax      : xPRSMappingInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i ImpKonv.  

    Author(s)   : Tom Nøkleby
    Created     : 25/09/2013
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

DEF STREAM InnFil.

DEFINE BUFFER bImpKonv FOR ImpKonv.
DEFINE TEMP-TABLE ttImpKonv LIKE ImpKonv
  INDEX ImpKonvDefNr EDB-System Tabell EksterntId InterntId.

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

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.
RUN PosterData.

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

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p ('PRSMappingimport', 'xPRSMappingInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

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
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    /*
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.
    */
    
    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 5 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 4 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    /* Skipper linjer med for få entries. */
    /*
    IF TRIM(ENTRY(1,pcLinje,';')) = '' OR 
       TRIM(ENTRY(2,pcLinje,';')) = '' OR 
       TRIM(ENTRY(3,pcLinje,';')) = '' OR 
       TRIM(ENTRY(5,pcLinje,';')) = ''  
    THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Kolonne 1, 2, 3 og 4 må være utfyllt) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    */
    
    CREATE ttImpKonv.

    /*
    RUN AssignDec(17,OUTPUT ttImpKonv.Rab1%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(1,OUTPUT ttImpKonv.ImpKonv, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    */
    
    ASSIGN 
      ttImpKonv.EDB-System = ENTRY(1,pcLinje,';')
      ttImpKonv.Tabell     = ENTRY(2,pcLinje,';')
      ttImpKonv.EksterntId = ENTRY(3,pcLinje,';')
      ttImpKonv.Merknad    = TRIM(ENTRY(4,pcLinje,';'))
      ttImpKonv.InterntId  = ENTRY(5,pcLinje,';')
    NO-ERROR.                

    /* Det har vært feil i en eller flere kolonner */
    IF b2Ok = FALSE THEN 
      NEXT LESERLINJER.
      
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
  
  FOR EACH ttImpKonv TRANSACTION:
    FIND FIRST ImpKonv EXCLUSIVE-LOCK WHERE 
        ImpKonv.EDB-System = ttImpKonv.EDB-System AND 
        ImpKonv.Tabell     = ttImpKonv.Tabell AND 
        ImpKonv.InterntId  = ttImpKonv.InterntId AND  
        ImpKonv.eksterntId = ttImpKonv.EksterntId 
        NO-ERROR.
    IF NOT AVAILABLE ImpKonv THEN 
    DO:
      CREATE ImpKonv.
      ASSIGN
          ImpKonv.EDB-System = ttImpKonv.EDB-System  
          ImpKonv.Tabell     = ttImpKonv.Tabell 
          ImpKonv.InterntId  = ttImpKonv.InterntId  
          ImpKonv.EksterntId = ttImpKonv.EksterntId 
          ImpKonv.EDato      = TODAY 
          impKonv.ETid       = TIME  
          .
    END.
    ELSE DO:
        ASSIGN 
          ImpKonv.InterntId  = ttImpKonv.InterntId  
          ImpKonv.EDato      = TODAY 
          impKonv.ETid       = TIME  
            .
    END.

    IF TRIM(ENTRY(4,pcLinje,';')) <> '' THEN ImpKonv.Merknad =  ttImpKonv.Merknad.          
    
    RELEASE ImpKonv.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

