&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE TT_Kjedebut
    FIELD Butikknr LIKE butiker.butik
    FIELD Hk AS LOGICAL
    FIELD ButikkNavn AS CHAR
    FIELD Teamnr   LIKE butiker.butik
    FIELD orgnr LIKE Butiker.OrganisasjonsNr
    .
DEFINE BUFFER bTT_Kjedebut FOR TT_Kjedebut.

DEF STREAM Inn.

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

RUN LesInnTT.
RUN SkapaBut.
RUN SkapaButTeam.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnTT Procedure 
PROCEDURE LesInnTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cLinje AS CHAR NO-UNDO.
    DEF VAR cFilNavn AS CHAR NO-UNDO.
    
    ASSIGN cFilNavn = "c:\appdir\91d\skotex\imp\Sport1Kjedebutikker.skv".
    INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
    REPEAT:
        IMPORT STREAM Inn UNFORMATTED cLinje.

        CREATE TT_Kjedebut.
        ASSIGN
            TT_Kjedebut.ButikkNr      = INT(ENTRY(5,cLinje,";"))
            TT_Kjedebut.ButikkNavn    = TRIM(ENTRY(6,cLinje,";"))
            TT_Kjedebut.Hk            = ENTRY(20,cLinje,";") = "J"
            TT_Kjedebut.Teamnr        = IF TRIM(ENTRY(21,cLinje,";")) = "" THEN
                               TT_Kjedebut.ButikkNr ELSE INT(ENTRY(21,cLinje,";"))
            TT_Kjedebut.orgnr         = TRIM(ENTRY(4,cLinje,";"))
            NO-ERROR.
    END.
    IF AVAIL TT_Kjedebut AND TT_Kjedebut.butikknr = 0 THEN
        DELETE TT_Kjedebut.
    ELSE
        RELEASE TT_Kjedebut.
    INPUT STREAM Inn CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Skapabut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Skapabut Procedure 
PROCEDURE Skapabut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_Kjedebut BY butikknr:
        IF NOT CAN-FIND(Butiker WHERE Butiker.butik = TT_Kjedebut.butikknr) THEN DO:
            FIND KjedensButikker WHERE KjedensButikker.butikknr = TT_Kjedebut.butikknr NO-LOCK.
            FIND Post WHERE Post.PostNr = KjedensButikker.PostNr NO-LOCK NO-ERROR.
            CREATE Butiker.
            ASSIGN Butiker.Butik           = KjedensButikker.ButikkNr
                   Butiker.ButNamn         = KjedensButikker.ButikkNavn
                   Butiker.BuAdr           = KjedensButikker.Adresse1
                   Butiker.BuPonr          = KjedensButikker.PostNr
                   Butiker.BuPadr          = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
                   Butiker.BuKon           = KjedensButikker.Kontaktperson
                   Butiker.BuTel           = KjedensButikker.Telefon
                   Butiker.ProfilNr        = 1
                   Butiker.OrganisasjonsNr = TT_Kjedebut.orgnr
                   Butiker.LanButikk       = FALSE
                   Butiker.Sentrallager    = TT_Kjedebut.Hk.
        END.      
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaButTeam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaButTeam Procedure 
PROCEDURE SkapaButTeam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTeamNr AS INTEGER    NO-UNDO.
    FOR EACH TT_Kjedebut BY butikknr:
        CREATE ButikkTilgang.
        ASSIGN ButikkTilgang.Butik   = TT_Kjedebut.butikknr
               ButikkTilgang.BrGrpNr = 1.
    END.

    FIND LAST ButikkTeam WHERE ButikkTeam.BrGrpNr = 1 AND ButikkTeam.TeamTypeId = 1 NO-LOCK NO-ERROR.
    IF AVAIL ButikkTeam THEN
        ASSIGN iTeamNr = ButikkTeam.TeamNr.
    FOR EACH TT_Kjedebut WHERE Hk = TRUE BY TT_Kjedebut.butikknr:
        FIND Butiker WHERE Butiker.Butik = TT_Kjedebut.butikknr NO-LOCK.
        CREATE ButikkTeam.
        ASSIGN iTeamNr                = iTeamNr + 1
               ButikkTeam.BrGrpNr     = 1
               ButikkTeam.TeamTypeId  = 1
               ButikkTeam.TeamNr      = iTeamNr
               ButikkTeam.Beskrivelse = ButNamn
               ButikkTeam.Notat       = "Autogen".
        CREATE ButikkKobling.
        ASSIGN ButikkKobling.BrGrpNr    = 1
               ButikkKobling.TeamTypeId = 1
               ButikkKobling.TeamNr     = ButikkTeam.TeamNr
               ButikkKobling.Butik      = TT_Kjedebut.butikknr.
        FOR EACH bTT_Kjedebut WHERE bTT_Kjedebut.Teamnr = TT_Kjedebut.butikknr AND bTT_Kjedebut.Hk = FALSE BY bTT_Kjedebut.butikknr:
            CREATE ButikkKobling.
            ASSIGN ButikkKobling.BrGrpNr    = 1
                   ButikkKobling.TeamTypeId = 1
                   ButikkKobling.TeamNr     = ButikkTeam.TeamNr
                   ButikkKobling.Butik      = bTT_Kjedebut.butikknr.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

