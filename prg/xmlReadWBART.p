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

DEFINE INPUT  PARAMETER cFilename AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER dArtikkelnr AS DECIMAL  NO-UNDO.
DEFINE OUTPUT PARAMETER lAktiver    AS LOGICAL  NO-UNDO.
DEFINE OUTPUT PARAMETER lPubliser   AS LOGICAL  NO-UNDO.
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hWebButArt AS HANDLE NO-UNDO.
DEFINE VARIABLE hWebButArtVerdi AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE hArtbas AS HANDLE     NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER  NO-UNDO.

/* Setter til ukjent. Ukjent verdi vil ikke bli oppdatert mot artikkel.  */
/* Kun verdier som er satt ved xml import vil da bli oppdatert artikkel. */
ASSIGN
  lAktiver  = ?
  lPubliser = ?
  .

{xwbartinnles.i}

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
         HEIGHT             = 15.71
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* cFilename = "posevent.xml". */
DEFINE VARIABLE cRet_val AS CHARACTER  NO-UNDO.

IF ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".") = "xml" THEN DO:
    RUN LesInnFil.
    cRet_Val = RETURN-VALUE.

    DELETE OBJECT hDoc NO-ERROR.
    DELETE OBJECT hRoot NO-ERROR.
    DELETE OBJECT hArtbas NO-ERROR.
    DELETE OBJECT hWebButArt NO-ERROR.
    DELETE OBJECT hWebButArtVerdi NO-ERROR.
END.
ELSE
    cRet_val = "ERROR".

IF cRet_val = "ERROR" THEN 
    RETURN "ERROR".
ELSE 
    RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Ändrad hantering av terminering skift. Gammal kod i LesInnFilOld 17/6-07      
------------------------------------------------------------------------------*/
DEF VAR ii AS INT NO-UNDO.
DEF VAR lOk AS LOGICAL NO-UNDO.
CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hWebButArt.
CREATE X-NODEREF hWebButArtVerdi.
CREATE X-NODEREF hArtbas.

lOk = hDoc:LOAD ("file",cFileName, FALSE) NO-ERROR.
IF lOK = FALSE THEN DO:
    RETURN "ERROR".
END.
IF NOT hDoc:GET-DOCUMENT-ELEMENT (hRoot) THEN
    RETURN "ERROR".
cName = hRoot:NAME NO-ERROR.

/* IF hRoot:NAME <> "POSBOFile" THEN */
IF cName <> "ArtBas" THEN
    RETURN "ERROR".
REPEAT i = 1 TO hRoot:NUM-CHILDREN:
    lOK = hRoot:GET-CHILD (hArtbas,i) NO-ERROR. /* Alla element til kvitton */
    IF NOT lOK THEN NEXT.
    ASSIGN
        dArtikkelNr = ?
        lPubliser   = ?
        lAktiver    = ?.
    cNAME = hArtbas:ATTRIBUTE-NAMES.
    IF cName = "Artikkelnr" THEN
        ASSIGN dArtikkelnr = DECI(hArtbas:GET-ATTRIBUTE ("Artikkelnr")).

    REPEAT j = 1 TO hArtbas:NUM-CHILDREN:
      lOK = hArtbas:GET-CHILD (hWebButArt,j).
      IF NOT lOK THEN NEXT.
      /* skip any null values */
      IF hWebButArt:NUM-CHILDREN < 1 THEN
          NEXT.
      IF hWebButArt:NAME = "WebButikkArtikkel" THEN DO:
          /* Här tar vi hand om alla element */
          REPEAT ii = 1 TO hWebButArt:NUM-CHILDREN:
            lOK = hWebButArt:GET-CHILD (hWebButArtVerdi,ii).
            IF NOT lOK THEN
                NEXT.
            lAktiver = hWebButArtVerdi:NODE-VALUE = "yes".
          END.
      END.
      IF hWebButArt:NAME = "Publiser" THEN DO:
          /* Här tar vi hand om alla element */
          REPEAT ii = 1 TO hWebButArt:NUM-CHILDREN:
            lOK = hWebButArt:GET-CHILD (hWebButArtVerdi,ii).
            IF NOT lOK THEN
                NEXT.
            lPubliser = hWebButArtVerdi:NODE-VALUE = "yes".
          END.
      END.
    END.

    /* Oppretter tmpPost */
    IF dArtikkelNr <> ? AND
        CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr) THEN
    DO:
        CREATE tmpArtBas.
        ASSIGN
            tmpArtBas.ArtikkelNr = dArtikkelNr
            tmpArtBas.Publiser   = lPubliser
            tmpArtBas.Aktiver    = lAktiver
            .
    END.
END.
/* Delete the objects we created. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

