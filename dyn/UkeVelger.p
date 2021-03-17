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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR idFrom  AS DATE NO-UNDO INIT 07/01/06.
  DEF VAR idTo    AS DATE NO-UNDO INIT 03/01/07.
  DEF VAR ocWeek  AS CHAR NO-UNDO.
&ELSE
  DEF INPUT  PARAM idFrom AS DATE NO-UNDO.
  DEF INPUT  PARAM idTo   AS DATE NO-UNDO.
  DEF OUTPUT PARAM ocWeek AS CHAR NO-UNDO.
&ENDIF

DEF VAR ix AS INT NO-UNDO.

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
   Other Settings: CODE-ONLY
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


DEF VAR cLookupValue  AS CHAR NO-UNDO INIT "cShortWeek".
DEF VAR cReturnValues AS CHAR NO-UNDO.
DEF VAR bOK           AS LOG  NO-UNDO.

/* See also procedure setLookupAttributes */

RUN JBoxDLookup.w ("_file"
                   + ";+dFrom|DATE|99/99/9999||Fra dato"
                   + ";+dTo|DATE|99/99/9999||Til dato"
                   + ";+cShortWeek|CHARACTER|xxxx||Uke"
                   + ";+!iWeeknum|INTEGER|>>>>>9||Uke"
                  ,"WHERE false"
                  ,INPUT-OUTPUT cLookupValue).


IF cLookupValue NE "" THEN 
  ocWeek = cLookupValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-setLookupAttributes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes Procedure 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

DEF VAR dCurr     AS DATE   NO-UNDO.
DEF VAR iWeek     AS INT    NO-UNDO.
DEF VAR hBuffer   AS HANDLE NO-UNDO.

hBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).

DO ix = 0 TO idTo - idFrom:
  dCurr = idFrom + ix.

  IF WEEKDAY(dCurr) = 2 THEN DO:
    RUN weeknum.p (dCurr,OUTPUT iWeek).
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("iWeeknum"):BUFFER-VALUE = iWeek
           hBuffer:BUFFER-FIELD("cShortWeek"):BUFFER-VALUE = SUBSTR(STRING(iWeek),5) + SUBSTR(STRING(iWeek),3,2)
           hBuffer:BUFFER-FIELD("dFrom"):BUFFER-VALUE = dCurr
           hBuffer:BUFFER-FIELD("dTo"):BUFFER-VALUE   = dCurr + 6
           .
  END.
END.

DYNAMIC-FUNCTION("setAttribute",ihBrowse,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"sortmap","cShortWeek;iWeekNum").
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"nocolumnsearch","cShortWeek").
/* DYNAMIC-FUNCTION("setAttribute",ihBrowse,"querysort","dFrom").  */
DYNAMIC-FUNCTION("setCurrentObject",ihBrowse).
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

