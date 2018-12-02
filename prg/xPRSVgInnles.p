&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSVgInnles.p
    Purpose     : Innlesning av data til varegruppesregister

    Syntax      : xPRSVgInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i varegrupperegisteret. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 30/07/2013
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

DEFINE BUFFER bVarGr FOR VarGr.
DEFINE TEMP-TABLE ttVarGr LIKE VarGr
  FIELD AvdelingNr AS INTEGER 
  FIELD AvdelingNavn AS CHARACTER 
  FIELD HgBeskr AS CHARACTER 
  INDEX VareGrDefId Vg.

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
  RUN bibl_logg.p ('PRSVgImport', 'xPRSSEnhInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

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
    IF pcLinje BEGINS "1;2;3;4" THEN
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
    
    CREATE ttVarGr.

    RUN AssignInt(5,OUTPUT ttVarGr.Vg, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(1,OUTPUT ttVarGr.AvdelingNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(3,OUTPUT ttVarGr.Hg, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignInt(7,OUTPUT ttVarGr.MomsKod, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDec(8,OUTPUT ttVarGr.Kost_Proc, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    /*
    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    */
    
    ASSIGN 
      ttVarGr.AvdelingNavn = ENTRY(2,pcLinje,';')           
      ttVarGr.HgBeskr      = ENTRY(4,pcLinje,';')           
      ttVarGr.VgBeskr      = ENTRY(6,pcLinje,';')           
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
  
  FOR EACH ttVarGr TRANSACTION:
    FIND Avdeling EXCLUSIVE-LOCK WHERE 
      Avdeling.AvdelingNr = ttVarGr.AvdelingNr NO-ERROR.
    IF NOT AVAILABLE Avdeling THEN 
    DO:
      CREATE Avdeling.
      ASSIGN Avdeling.AvdelingNr = ttVargr.AvdelingNr.
    END.
    IF TRIM(ENTRY(2,pcLinje,';')) <> '' THEN Avdeling.AvdelingNavn =  ttVarGr.AvdelingNavn.          
    
    FIND HuvGr EXCLUSIVE-LOCK WHERE 
      HuvGr.Hg = ttVarGr.Hg NO-ERROR.
    IF NOT AVAILABLE HuvGr THEN 
    DO:
      CREATE HuvGr.
      ASSIGN
        HuvGr.Hg         = ttVarGr.Hg
        HuvGr.AvdelingNr = ttVarGr.AvdelingNr.
    END.
    IF TRIM(ENTRY(4,pcLinje,';')) <> '' THEN HuvGr.HgBeskr =  ttVarGr.HgBeskr.          
    
    FIND VarGr EXCLUSIVE-LOCK WHERE 
      VarGr.Vg = ttVarGr.Vg NO-ERROR.
    IF NOT AVAILABLE VarGr THEN 
    DO:
      CREATE VarGr.
      ASSIGN
        VarGr.Vg = ttVarGr.Vg.
    END.

    IF TRIM(ENTRY(3,pcLinje,';'))  <> '' THEN VarGr.Hg                 =  ttVarGr.Hg.          
    IF TRIM(ENTRY(6,pcLinje,';'))  <> '' THEN VarGr.VgBeskr            =  ttVarGr.VgBeskr.          
    IF TRIM(ENTRY(7,pcLinje,';'))  <> '' THEN VarGr.MomsKod            =  ttVarGr.MomsKod.          
    IF TRIM(ENTRY(8,pcLinje,';'))  <> '' THEN VarGr.Kost_Proc          =  ttVarGr.Kost_Proc.          
    IF TRIM(ENTRY(9,pcLinje,';'))  <> '' THEN VarGr.TillatLokalePriser =  ttVarGr.TillatLokalePriser.          
    IF TRIM(ENTRY(10,pcLinje,';')) <> '' THEN VarGr.Bonus_Givende      =  ttVarGr.Bonus_Givende.          
    
    FOR EACH Kategori:
      IF NOT CAN-FIND(VgKat WHERE 
                      VgKat.Vg    = ttVarGr.Vg AND 
                      VgKat.VgKat = Kategori.KatNr AND 
                      VgKat.KatNr = Kategori.KatNr) THEN 
      DO:
        CREATE VgKat.
        ASSIGN
            VgKat.Vg    = ttVarGr.Vg  
            VgKat.VgKat = Kategori.KatNr
            VgKat.KatNr = Kategori.KatNr 
            .
      END.
    END.
    
    RELEASE Avdeling.
    RELEASE HuvGr.
    RELEASE VarGr.
    IF AVAILABLE VgKat THEN RELEASE VgKat.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

