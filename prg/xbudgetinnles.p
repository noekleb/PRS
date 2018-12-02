&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xpreembudgetinnles.p
    Purpose     :

    Syntax      :

    Description : Läser in budgetfiler från Preemstationerna.

    Author(s)   : Kenneth Olausson
    Created     : 02/11-10
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
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE cReturnStatus AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
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

&IF DEFINED(EXCLUDE-linjeOK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD linjeOK Procedure 
FUNCTION linjeOK RETURNS LOGICAL
  ( INPUT cFillinje AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 19.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
FOR EACH tt_Error:
  DELETE tt_Error.
END.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

RETURN.

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
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR piLoop AS INT NO-UNDO.
  DEFINE VARIABLE cFillinje AS CHARACTER   NO-UNDO.
  
  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
      
  /* Läser in filen */
  INPUT FROM VALUE(cFilnavn).
  REPEAT:
      IMPORT UNFORMATTED cFillinje.
      iAntLinjer = iAntLinjer + 1.
      IF linjeOK(cFillinje) THEN DO:
          FIND prBudget WHERE prBudget.aar     = INT(ENTRY(1,cFillinje,";")) AND
                              prBudget.station = INT(ENTRY(2,cFillinje,";")) AND
                              prBudget.hg      = INT(ENTRY(3,cFillinje,";")) NO-ERROR.
          IF NOT AVAIL prBudget THEN DO:
              CREATE prBudget.
              ASSIGN prBudget.aar     = INT(ENTRY(1,cFillinje,";"))
                     prBudget.station = INT(ENTRY(2,cFillinje,";"))
                     prBudget.hg      = INT(ENTRY(3,cFillinje,";")).
          END.
          ASSIGN prBudget.sumfgaar = DECI(ENTRY(4,cFillinje,";"))
                 prBudget.p01      = DECI(ENTRY(5,cFillinje,";"))
                 prBudget.p02      = DECI(ENTRY(6,cFillinje,";"))
                 prBudget.p03      = DECI(ENTRY(7,cFillinje,";"))
                 prBudget.p04      = DECI(ENTRY(7,cFillinje,";"))
                 prBudget.p05      = DECI(ENTRY(8,cFillinje,";"))
                 prBudget.p06      = DECI(ENTRY(10,cFillinje,";"))
                 prBudget.p07      = DECI(ENTRY(11,cFillinje,";"))
                 prBudget.p08      = DECI(ENTRY(12,cFillinje,";"))
                 prBudget.p09      = DECI(ENTRY(13,cFillinje,";"))
                 prBudget.p10      = DECI(ENTRY(14,cFillinje,";"))
                 prBudget.p11      = DECI(ENTRY(15,cFillinje,";"))
                 prBudget.p12      = DECI(ENTRY(16,cFillinje,";")).
      END.
  END.
  INPUT CLOSE.
  
  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-linjeOK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION linjeOK Procedure 
FUNCTION linjeOK RETURNS LOGICAL
  ( INPUT cFillinje AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTst AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lReturn AS LOGICAL INIT TRUE NO-UNDO.

  IF NUM-ENTRIES(cFillinje,";") <> 16 THEN
      lReturn = FALSE.
  ELSE DO ii = 1 TO NUM-ENTRIES(cFillinje,";"):
      CASE ii:
          WHEN 1 OR WHEN 2 OR WHEN 3 THEN DO:
              iTst = INT(ENTRY(ii,cFillinje,";")) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  lReturn = FALSE.
                  LEAVE.
              END.
          END.
          OTHERWISE DO:
              dTst = DECI(ENTRY(ii,cFillinje,";")) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  lReturn = FALSE.
                  LEAVE.
              END.
          END.
      END CASE.
  END.
  RETURN lReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

