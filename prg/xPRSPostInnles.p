&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSPostInnles.p
    Purpose     : Innlesning av data til postregister

    Syntax      : xPRSPostInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i postregisteret. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 28/07/2014
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
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE ix   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTxt AS CHARACTER NO-UNDO.

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE BUFFER bPost FOR Post.
DEFINE TEMP-TABLE ttPost LIKE Post
  FIELD KommuneNavn AS CHARACTER
  INDEX PostDefNr PostNr.

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
    cLogg = 'xPRSPostInnles' + REPLACE(STRING(TODAY),'/','') + '.log'
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    .

RUN bibl_loggDbFri.p (cLogg, 'Start LesInnFil'). 
RUN LesInnFil.

RUN bibl_loggDbFri.p (cLogg, 'Start PosterData'). 
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

RUN bibl_loggDbFri.p (cLogg, 'Slutt.'). 

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
  RUN bibl_logg.p ('PRSVaremImport', 'xPRSPostInnles.p: Leser inn fil: ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

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
    IF NUM-ENTRIES(pcLinje,";") < 5 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 5 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
      RUN bibl_loggDbFri.p (cLogg, ttError.Tekst).       
      NEXT LESERLINJER.
    END.
    
    CREATE ttPost.

    RUN AssignInt(1,OUTPUT ttPost.PostNr, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.

    /*
    RUN AssignDec(17,OUTPUT ttPost.Rab1%, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    RUN AssignDate(24,OUTPUT ttButiker.ApningsDato, OUTPUT bOk).
    IF bOk = FALSE THEN b2Ok = FALSE.
    */
    
    ASSIGN 
      ttPost.Beskrivelse    = ENTRY(2,pcLinje,';')           
      ttPost.Kommnr         = TRIM(ENTRY(3,pcLinje,';'))
      ttPost.KommuneNavn    = ENTRY(4,pcLinje,';')      
      ttPost.FylkesNr       = SUBSTRING(STRING(INT(ttPost.Kommnr),"9999"),1,2)   
    NO-ERROR.                
    IF ERROR-STATUS:ERROR THEN 
      DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
          cTxt = '** Feil i assign ttPost: ' + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).      
          RUN bibl_loggDbFri.p (cLogg, cTxt).       
      END.       /* Det har vært feil i en eller flere kolonner */
    IF b2Ok = FALSE THEN
    DO: 
      cTxt = '** b20k = false.'.
      RUN bibl_loggDbFri.p (cLogg, cTxt). 
      NEXT LESERLINJER.
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
  
  FOR EACH ttPost TRANSACTION:
    FIND Post EXCLUSIVE-LOCK WHERE 
      Post.PostNr = ttPost.PostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN 
    DO:
      CREATE Post.
      ASSIGN
        Post.PostNr = ttPost.PostNr.
    END.
    ASSIGN 
        Post.Beskrivelse = ttPost.Beskrivelse
        Post.KommNr      = ttPost.KommNr
        Post.FylkesNr =  ttPost.FylkesNr  
        .

    IF Post.KommNr = '' THEN 
      Post.KommNr = '1'.
    IF Post.FylkesNr = '' THEN 
      Post.FylkesNr = '1'.        
    
    FIND Kommune EXCLUSIVE-LOCK WHERE 
        Kommune.KommNr   = Post.KommNr NO-ERROR.
    IF NOT AVAILABLE Kommune THEN  
        DO:
            CREATE Kommune.
            ASSIGN 
            Kommune.FylkesNr    = Post.FylkesNr  
            Kommune.KommNr      = Post.KommNr
            Kommune.Beskrivelse = ttPost.KommuneNavn 
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO: 
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    cTxt = '** Feil i create kommune: ' + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).      
                    RUN bibl_loggDbFri.p (cLogg, cTxt).       
                END.       /* Det har vært feil i en eller flere kolonner */
                DELETE Kommune.
            END.
            ELSE RELEASE Kommune.
        END.  
    ELSE Kommune.Beskrivelse = ttPost.KommuneNavn.

    cTxt = STRING(Post.PostNr) + ' ' + Post.Beskrivelse + '.'.  
    RUN bibl_loggDbFri.p (cLogg, cTxt).       
           
    IF NOT CAN-FIND(Fylke WHERE
        Fylke.FylkesNr = Post.FylkesNr) THEN 
        DO:
            CREATE Fylke.
            ASSIGN 
            Fylke.FylkesNr = Post.FylkesNr
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DELETE Fylke.
            ELSE RELEASE Fylke.
        END.
    
    RELEASE Post.
    
  END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

