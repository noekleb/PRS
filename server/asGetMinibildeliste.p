&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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
DEFINE TEMP-TABLE ttFile NO-UNDO
    FIELD FILENAME   AS CHAR FORMAT "x(60)"
    FIELD FILEPATH   AS CHAR FORMAT "x(50)"
    FIELD FileObject AS BLOB .

DEFINE INPUT  PARAMETER iButikkNr AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iKasseNr AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER dMinDat AS DATE        NO-UNDO.

DEFINE OUTPUT  PARAMETER TABLE FOR ttFile.
DEFINE OUTPUT  PARAMETER lStatus AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cBildekatalog AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSysHId AS INTEGER     NO-UNDO.

DEFINE VARIABLE cLastPicDate AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dLastPicDate AS DATE        NO-UNDO.
DEFINE TEMP-TABLE tt_minibilder NO-UNDO
    FIELD cBildenavn AS CHAR
    FIELD cPathBildenavn AS CHAR
.

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
ASSIGN iSysHId = 400.
RUN InitVar.
RUN GetBildFilList.
RUN FillBlobs.
RUN LagreSyspara.
/* OUTPUT TO "CLIPBOARD".    */
/*                           */
/* FOR EACH tt_minibilder.   */
/*     EXPORT tt_minibilder. */
/* END.                      */

OUTPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FillBlobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillBlobs Procedure 
PROCEDURE FillBlobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* OUTPUT TO "C:\tmp\filer.txt". */
/* FOR EACH tt_minibilder:       */
/*     EXPORT tt_Minibilder.     */
/* END.                          */
/* OUTPUT CLOSE.                 */
FOR EACH tt_minibilder:
    IF SEARCH(tt_minibilder.cPathBildenavn) <> ? THEN DO:
        CREATE ttFile.
        ASSIGN ttfile.FILENAME = tt_minibilder.cBildeNavn.
        COPY-LOB FILE tt_minibilder.cPathBildenavn TO ttfile.FileObject NO-CONVERT. 
    END.
    ELSE
        DELETE tt_minibilder.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetBildFilList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBildFilList Procedure 
PROCEDURE GetBildFilList :
DEFINE VARIABLE search-dir AS CHARACTER.
DEFINE VARIABLE cfile-name AS CHARACTER FORMAT "x(30)" LABEL "File". 
DEFINE VARIABLE cFilePath AS CHARACTER   NO-UNDO.
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "x(4)" LABEL "Attributes".
DEFINE VARIABLE cFullname AS CHARACTER   NO-UNDO.
/* dMinDat = dMinDat - 1. */



FOR EACH bilderegister WHERE bilderegister.dato > dLastPicDate NO-LOCK.
    cFile-name = "mini" + STRING(bilderegister.bildnr) + ".jpg".
    cFullname = cBildekatalog + "\" + cfile-name.
    IF SEARCH(cFullname) <> ? THEN DO:
        CREATE tt_minibilder.
        ASSIGN tt_minibilder.cBildenavn      = cfile-name
               tt_minibilder.cPathBildenavn  = cFullname.
        RELEASE tt_minibilder.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitVar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVar Procedure 
PROCEDURE InitVar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {syspara.i 10 1 2 cBildekatalog}
    cBildekatalog = RIGHT-TRIM(cBildekatalog,"\").

    FIND Syspara WHERE SysPara.SysHId = iSysHId   AND 
                       SysPara.SysGr  = iButikknr AND
                       SysPara.ParaNr = iKasseNr  NO-LOCK NO-ERROR.
    IF AVAIL syspara THEN
        cLastPicDate = SysPara.Parameter1.
    ELSE 
        cLastPicDate = "19900101".
    dLastPicDate = DATE(INT(SUBSTR(cLastPicDate,5,2)),INT(SUBSTR(cLastPicDate,7,2)),INT(SUBSTR(cLastPicDate,1,4))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        dLastPicDate = DATE(1,1,1990).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreSyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreSyspara Procedure 
PROCEDURE LagreSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND SysHode WHERE SysHode.SysHId = iSysHId NO-LOCK NO-ERROR.
    IF AVAIL SysHode THEN DO:
        FIND SysGruppe WHERE SysGruppe.SysHId = iSysHId AND
                             SysGruppe.SysGr  = iButikkNr NO-LOCK NO-ERROR.
        IF NOT AVAIL SysGruppe THEN DO:
            CREATE SysGruppe.
            ASSIGN SysGruppe.SysHId = iSysHId
                   SysGruppe.SysGr  = iButikkNr
                   SysGruppe.Beskrivelse = "Butik: " + STRING(iButikkNr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE SysGruppe.
                FIND SysGruppe WHERE SysGruppe.SysHId = iSysHId AND
                                     SysGruppe.SysGr  = iButikkNr NO-LOCK NO-ERROR.
            END.
            ELSE
                FIND CURRENT SysGruppe NO-LOCK NO-ERROR.
        END.
    END.
    IF AVAIL SysGruppe THEN DO:
        cLastPicDate = STRING(YEAR(TODAY - 1),"9999") + STRING(MONTH(TODAY - 1),"99") + STRING(DAY(TODAY - 1),"99").
        FIND Syspara WHERE SysPara.SysHId = iSysHId   AND 
                           SysPara.SysGr  = iButikknr AND
                           SysPara.ParaNr = iKasseNr  NO-ERROR.
        IF NOT AVAIL SysPara THEN DO:
            CREATE SysPara.
            ASSIGN SysPara.SysHId = iSysHId  
                   SysPara.SysGr  = iButikknr
                   SysPara.ParaNr = iKasseNr 
                   SysPara.Beskrivelse = "Kassa: " + STRING(iKasseNr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE SysPara.
                RETURN.
            END.
        END.
        IF AVAIL SysPara THEN DO:
            ASSIGN SysPara.parameter1 = cLastPicDate.
            FIND CURRENT SysPara NO-LOCK.
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ORG_GetBildFilList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ORG_GetBildFilList Procedure 
PROCEDURE ORG_GetBildFilList :
DEFINE VARIABLE search-dir AS CHARACTER.
DEFINE VARIABLE cfile-name AS CHARACTER FORMAT "x(30)" LABEL "File". 
DEFINE VARIABLE cFilePath AS CHARACTER   NO-UNDO.
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "x(4)" LABEL "Attributes".

/* dMinDat = dMinDat - 1. */

INPUT FROM OS-DIR(cBildekatalog).

REPEAT:
   IMPORT cfile-name cFilePath attr-list.
   FILE-INFO:FILE-NAME = cFilePath.
   IF FILE-INFO:FILE-MOD-DATE < dMinDat THEN
       NEXT.
   IF NOT cfile-name BEGINS "mini" THEN
       NEXT.
   ELSE IF attr-list <> "F" THEN
       NEXT.
   ELSE IF FILE-INFO:FILE-SIZE = 0 THEN
       NEXT.
   ELSE IF NOT (NUM-ENTRIES(cfile-name,".") = 2 AND ENTRY(2,cfile-name,".") = "jpg") THEN
                NEXT.
   CREATE tt_minibilder.
   ASSIGN tt_minibilder.cBildenavn      = cfile-name
          tt_minibilder.cPathBildenavn  = cfilepath.
   RELEASE tt_minibilder.
END.
INPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

