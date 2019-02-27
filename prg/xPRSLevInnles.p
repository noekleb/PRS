&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSLevInnles.p
    Purpose     : Innlesning av data til leverandørregister

    Syntax      : xPRSLevInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i leverandørregisteret. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 28/07/2013
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

DEFINE BUFFER bLevBas FOR LevBas.
DEFINE TEMP-TABLE ttLevBas LIKE LevBas
  INDEX LevnrDef LevNr.

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
  RUN bibl_logg.p ('PRSLevImport', 'xPRSLevInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

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
    IF pcLinje BEGINS "1;2;3;4;5" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 35 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 35 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
        NEXT LESERLINJER.
    END.
    
    CREATE ttLevBas.

    RUN AssignInt(1,OUTPUT ttLevBas.LevNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    RUN AssignDec(17,OUTPUT ttLevBas.Rab1%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(18,OUTPUT ttLevBas.Frakt%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(19,OUTPUT ttLevBas.DivKost%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(20,OUTPUT ttLevBas.Rab2%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
                        
    /*
    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    */
    
    ASSIGN 
      ttLevBas.KjedeAvtale       = CAN-DO('Ja,Yes,J,1,True',ENTRY(16,pcLinje,';'))            
      ttLevBas.levnamn           = ENTRY(2,pcLinje,';')           
      ttLevBas.levadr            = ENTRY(3,pcLinje,';')       
      ttLevBas.levponr           = ENTRY(4,pcLinje,';')       
      ttLevBas.levpadr           = ENTRY(5,pcLinje,';')       
      ttLevBas.levtel            = ENTRY(6,pcLinje,';')       
      ttLevBas.telefax           = ENTRY(7,pcLinje,';')       
      ttLevBas.Telex             = ENTRY(8,pcLinje,';')       
      ttLevBas.E_MailLev         = ENTRY(9,pcLinje,';')       
      ttLevBas.levland           = ENTRY(10,pcLinje,';')       
      ttLevBas.EgetKundeNrHosLev = ENTRY(11,pcLinje,';')       
      ttLevBas.kommentar[1]      = ENTRY(12,pcLinje,';')       
      ttLevBas.kommentar[2]      = ENTRY(13,pcLinje,';')       
      ttLevBas.kommentar[3]      = ENTRY(14,pcLinje,';')       
      ttLevBas.kommentar[4]      = ENTRY(15,pcLinje,';')
      ttLevBas.levkon            = ENTRY(21,pcLinje,';')                   
      ttLevBas.koadr             = ENTRY(22,pcLinje,';')
      ttLevBas.koponr            = ENTRY(23,pcLinje,';')
      ttLevBas.kopadr            = ENTRY(24,pcLinje,';')
      ttLevBas.kotel             = ENTRY(25,pcLinje,';') 
      ttLevBas.kotelefax         = ENTRY(26,pcLinje,';')
      ttLevBas.kotelex           = ENTRY(27,pcLinje,';')
      ttLevBas.E_MailKontakt     = ENTRY(28,pcLinje,';')
      ttLevBas.koland            = ENTRY(29,pcLinje,';')
      ttLevBas.ReklAdresse1      = ENTRY(30,pcLinje,';')
      ttLevBas.ReklAdresse2      = ENTRY(31,pcLinje,';')       
      ttLevBas.ReklPostBoks      = ENTRY(32,pcLinje,';')       
      ttLevBas.ReklPostNr        = ENTRY(33,pcLinje,';')       
      ttLevBas.valkod            = ENTRY(43,pcLinje,';')       
      ttLevBas.Notat             = ENTRY(35,pcLinje,';') 
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
  
  FOR EACH ttLevBas TRANSACTION:
    FIND LevBas EXCLUSIVE-LOCK WHERE 
      LevBas.LevNr = ttLevBas.LevNr NO-ERROR.
    IF NOT AVAILABLE LevBas THEN 
    DO:
      CREATE LevBas.
      ASSIGN
        LevBas.LevNr = ttLevBas.LevNr.
    END.

    IF TRIM(ENTRY(2,pcLinje,';')) <> '' THEN LevBas.levnamn           =  ttLevBas.levnamn.          
    IF TRIM(ENTRY(3,pcLinje,';')) <> '' THEN LevBas.levadr            =  ttLevBas.levadr.           
    IF TRIM(ENTRY(4,pcLinje,';')) <> '' THEN LevBas.levponr           =  ttLevBas.levponr.          
    IF TRIM(ENTRY(5,pcLinje,';')) <> '' THEN LevBas.levpadr           =  ttLevBas.levpadr.          
    IF TRIM(ENTRY(6,pcLinje,';')) <> '' THEN LevBas.levtel            =  ttLevBas.levtel.           
    IF TRIM(ENTRY(7,pcLinje,';')) <> '' THEN LevBas.telefax           =  ttLevBas.telefax.          
    IF TRIM(ENTRY(8,pcLinje,';')) <> '' THEN LevBas.Telex             =  ttLevBas.Telex.            
    IF TRIM(ENTRY(9,pcLinje,';')) <> '' THEN LevBas.E_MailLev         =  ttLevBas.E_MailLev.        
    IF TRIM(ENTRY(10,pcLinje,';')) <> '' THEN LevBas.levland           =  ttLevBas.levland.          
    IF TRIM(ENTRY(11,pcLinje,';')) <> '' THEN LevBas.EgetKundeNrHosLev =  ttLevBas.EgetKundeNrHosLev.
    IF TRIM(ENTRY(12,pcLinje,';')) <> '' THEN LevBas.kommentar[1]      =  ttLevBas.kommentar[1].     
    IF TRIM(ENTRY(13,pcLinje,';')) <> '' THEN LevBas.kommentar[2]      =  ttLevBas.kommentar[2].     
    IF TRIM(ENTRY(14,pcLinje,';')) <> '' THEN LevBas.kommentar[3]      =  ttLevBas.kommentar[3].     
    IF TRIM(ENTRY(15,pcLinje,';')) <> '' THEN LevBas.kommentar[4]      =  ttLevBas.kommentar[4].     
    IF TRIM(ENTRY(16,pcLinje,';')) <> '' THEN LevBas.KjedeAvtale       =  ttLevBas.KjedeAvtale.      
    IF TRIM(ENTRY(17,pcLinje,';')) <> '' THEN LevBas.Rab1%             =  ttLevBas.Rab1%.            
    IF TRIM(ENTRY(18,pcLinje,';')) <> '' THEN LevBas.Frakt%            =  ttLevBas.Frakt%.           
    IF TRIM(ENTRY(19,pcLinje,';')) <> '' THEN LevBas.DivKost%          =  ttLevBas.DivKost%.         
    IF TRIM(ENTRY(20,pcLinje,';')) <> '' THEN LevBas.Rab2%             =  ttLevBas.Rab2%.            
    IF TRIM(ENTRY(21,pcLinje,';')) <> '' THEN LevBas.levkon            =  ttLevBas.levkon.           
    IF TRIM(ENTRY(22,pcLinje,';')) <> '' THEN LevBas.koadr             =  ttLevBas.koadr.            
    IF TRIM(ENTRY(23,pcLinje,';')) <> '' THEN LevBas.koponr            =  ttLevBas.koponr.           
    IF TRIM(ENTRY(24,pcLinje,';')) <> '' THEN LevBas.kopadr            =  ttLevBas.kopadr.           
    IF TRIM(ENTRY(25,pcLinje,';')) <> '' THEN LevBas.kotel             =  ttLevBas.kotel.            
    IF TRIM(ENTRY(26,pcLinje,';')) <> '' THEN LevBas.kotelefax         =  ttLevBas.kotelefax.        
    IF TRIM(ENTRY(27,pcLinje,';')) <> '' THEN LevBas.kotelex           =  ttLevBas.kotelex.          
    IF TRIM(ENTRY(28,pcLinje,';')) <> '' THEN LevBas.E_MailKontakt     =  ttLevBas.E_MailKontakt.    
    IF TRIM(ENTRY(29,pcLinje,';')) <> '' THEN LevBas.koland            =  ttLevBas.koland.           
    IF TRIM(ENTRY(30,pcLinje,';')) <> '' THEN LevBas.ReklAdresse1      =  ttLevBas.ReklAdresse1.     
    IF TRIM(ENTRY(31,pcLinje,';')) <> '' THEN LevBas.ReklAdresse2      =  ttLevBas.ReklAdresse2.     
    IF TRIM(ENTRY(32,pcLinje,';')) <> '' THEN LevBas.ReklPostBoks      =  ttLevBas.ReklPostBoks.     
    IF TRIM(ENTRY(33,pcLinje,';')) <> '' THEN LevBas.ReklPostNr        =  ttLevBas.ReklPostNr.       
    IF TRIM(ENTRY(34,pcLinje,';')) <> '' THEN LevBas.valkod            =  ttLevBas.valkod.           
    IF TRIM(ENTRY(35,pcLinje,';')) <> '' THEN LevBas.Notat             =  ttLevBas.Notat.            
    
    RELEASE LevBas.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

