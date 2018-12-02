&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : mypage_lib.p
    Purpose     : Library functions for mypage service interface

    Syntax      : To start:
                  RUN mypage_lib.p PERSIST SET hMyPageLib.

    Description :

    Author(s)   : brynjar@chemistry.no
    Created     : 21.oct.14
    Notes       :
    Modified    : Brynjar 20.mars.15: Lagt inn test på lengde av charData før cNodeValue oppdateres
                  i prosedyre "Characters"
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR hDoc            AS HANDLE NO-UNDO.
DEF VAR hSax            AS HANDLE NO-UNDO.
DEF VAR hText           AS HANDLE NO-UNDO.
DEF VAR hFormat         AS HANDLE NO-UNDO.
DEF VAR iNodeIdx        AS INT    NO-UNDO.
DEF VAR iLevel          AS INT    NO-UNDO.
DEF VAR cIndent         AS CHAR   NO-UNDO INIT "  ". /* "~t".  */
DEF VAR cActiveNode     AS CHAR   NO-UNDO.
DEF VAR iLogHeaderId    AS INT    NO-UNDO.
DEF VAR iEventLogId     AS INT    NO-UNDO.
DEF VAR cSessionId      AS CHAR   NO-UNDO.
DEF VAR iLoginSessionId AS INT    NO-UNDO.

DEFINE VARIABLE mb64Data AS MEMPTR NO-UNDO.
DEF VAR bUseMptr         AS LOG NO-UNDO.

{ttXML.i}

DEF TEMP-TABLE ttDoc
    FIELD hDoc          AS HANDLE
    FIELD cDocName      AS CHAR
    .

DEF TEMP-TABLE ttNodes
    FIELD hDoc          AS HANDLE
    FIELD hNode         AS HANDLE
    FIELD hParentNode   AS HANDLE
    FIELD iLevel        AS INT
    FIELD cElement      AS CHAR
    FIELD cViewType     AS CHAR /* block / line */
    FIELD cNodeValue    AS CHAR
    FIELD iNodeIdx      AS INT
    .

DEF TEMP-TABLE ttDumpNodes
    FIELD iLevel        AS INT
    FIELD cElement      AS CHAR
    FIELD cViewType     AS CHAR /* block / line */
    FIELD cNodeValue    AS CHAR
    FIELD iNodeIdx      AS INT
    .
DEF VAR httDumpNodes AS HANDLE NO-UNDO.
httDumpNodes = BUFFER ttDumpNodes:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getErrorDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getErrorDesc Procedure 
FUNCTION getErrorDesc RETURNS CHARACTER
  ( INPUT iiMessageNr AS INT,INPUT icMessageType AS CHAR,INPUT icMessageTxt AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getErrorMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getErrorMsg Procedure 
FUNCTION getErrorMsg RETURNS CHARACTER
  ( INPUT iiMessageNr AS INT,INPUT icMessageType AS CHAR,INPUT icMessageTxt AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoginSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoginSessionId Procedure 
FUNCTION getLoginSessionId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ON 'close':U OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(hText)   THEN DELETE OBJECT hText.
  IF VALID-HANDLE(hFormat) THEN DELETE OBJECT hFormat.

  FOR EACH ttDoc:
    DELETE OBJECT ttDoc.hDoc NO-ERROR.
  END.
  IF VALID-HANDLE(hDoc) THEN DELETE OBJECT hDoc.

  FOR EACH ttNodes:
    DELETE OBJECT ttNodes.hNode NO-ERROR.
  END.

  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addNode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addNode Procedure 
PROCEDURE addNode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icParentElement AS CHAR NO-UNDO.
DEF INPUT PARAM icViewType      AS CHAR NO-UNDO.
DEF INPUT PARAM icElement       AS CHAR NO-UNDO.
DEF INPUT PARAM icValue         AS CHAR NO-UNDO.

DEF VAR iLevel       AS INT    NO-UNDO.
DEF VAR hParentNode  AS HANDLE NO-UNDO.
DEF VAR rbNode       AS ROWID  NO-UNDO.

IF icValue = ? THEN icValue = "".

DEF BUFFER bttNodes FOR ttNodes.

FOR EACH ttNodes
    WHERE ttNodes.cElement = icParentElement
    BY ttNodes.iNodeIdx DESC:
  rbNode = ROWID(ttNodes).
  LEAVE.
END.

IF rbNode NE ? THEN DO:
  FIND FIRST bttNodes 
       WHERE ROWID(bttNodes) = rbNode.
  ASSIGN iLevel = ttNodes.iLevel + 1
         hParentNode = bttNodes.hNode
         .
END.

CREATE ttNodes.
ASSIGN ttNodes.hDoc        = hDoc
       ttNodes.hParentNode = hParentNode
       ttNodes.iLevel      = iLevel
       ttNodes.cViewType   = icViewType
       ttNodes.cNodeValue  = icValue
       ttNodes.cElement    = icElement
       iNodeIdx            = iNodeIdx + 1
       ttNodes.iNodeIdx    = iNodeIdx
       .
CREATE X-NODEREF ttNodes.hNode.

IF icViewType = "block" THEN DO:
  IF iLevel > 0 THEN DO: /* Create child block node */
    hDoc:CREATE-NODE(ttNodes.hNode,icElement,"ELEMENT").

    hDoc:CREATE-NODE(hFormat, ?, "TEXT").
    hFormat:NODE-VALUE = cIndent.
    hParentNode:APPEND-CHILD(hFormat).

    hParentNode:APPEND-CHILD(ttNodes.hNode).
    hDoc:CREATE-NODE(hFormat, ?, "TEXT").
    hFormat:NODE-VALUE = "~n" + FILL(cIndent,iLevel).
    ttNodes.hNode:APPEND-CHILD(hFormat).

    hDoc:CREATE-NODE(hFormat, ?, "TEXT").
    hFormat:NODE-VALUE = "~n" + FILL(cIndent,iLevel - 1). 
    hParentNode:APPEND-CHILD(hFormat).
  END.
  ELSE DO: /* Create the root block node */        
    hDoc:CREATE-NODE(ttNodes.hNode, icElement, "ELEMENT").
    hDoc:APPEND-CHILD(ttNodes.hNode).
    hDoc:CREATE-NODE(hFormat, ?, "TEXT").
    hFormat:NODE-VALUE = "~n".    /* new line */
    ttNodes.hNode:APPEND-CHILD(hFormat).
  END.
END.
ELSE DO:
  hDoc:CREATE-NODE(hFormat, ?, "TEXT").
  hFormat:NODE-VALUE = cIndent.
  hParentNode:APPEND-CHILD(hFormat).

  hDoc:CREATE-NODE(ttNodes.hNode, icElement, "ELEMENT").
  hDoc:CREATE-NODE(hText, ?, "TEXT").
  IF bUseMptr THEN DO:
    hText:MEMPTR-TO-NODE-VALUE(mb64Data).
    bUseMptr = NO.
  END.
  ELSE hText:NODE-VALUE = icValue.
  ttNodes.hNode:APPEND-CHILD(hText).
  hParentNode:APPEND-CHILD(ttNodes.hNode).
  /* since the parent is "block"  type append a new line to close it neatly  */                      
  hDoc:CREATE-NODE(hFormat, ?, "TEXT").
  hFormat:NODE-VALUE = "~n" + FILL(cIndent,iLevel - 1).
  hParentNode:APPEND-CHILD(hFormat).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Characters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Characters Procedure 
PROCEDURE Characters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Brynjar 20.mars.15: Lagt inn test på lengde av charData før cNodeValue oppdateres
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER charData AS LONGCHAR NO-UNDO.
DEFINE INPUT PARAMETER numChars AS INTEGER NO-UNDO.

CREATE ttNodes.
ASSIGN ttNodes.hDoc         = hSax
       ttNodes.cElement     = cActiveNode
       ttNodes.cNodeValue   = IF LENGTH(charData) < 32000 THEN charData ELSE ""
       iNodeIdx             = iNodeIdx + 1
       ttNodes.iNodeIdx     = iNodeIdx       
       ttNodes.iLevel       = iLevel
       .

END PROCEDURE.

/*
    FIELD hDoc          AS HANDLE
    FIELD hNode         AS HANDLE
    FIELD hParentNode   AS HANDLE
    FIELD iLevel        AS INT
    FIELD cElement      AS CHAR
    FIELD cViewType     AS CHAR /* block / line */
    FIELD cNodeValue    AS CHAR
    FIELD iNodeIdx      AS INT

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createRequest Procedure 
PROCEDURE createRequest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiKlientNr          AS INT  NO-UNDO.
DEF INPUT  PARAM iiMedlemsnr         AS INT  NO-UNDO.
DEF INPUT  PARAM icRequestType       AS CHAR NO-UNDO.
DEF INPUT  PARAM icServiceProgName   AS CHAR NO-UNDO.
DEF INPUT  PARAM ibCreateRequestInfo AS LOG  NO-UNDO.
DEF OUTPUT PARAM oiRequestId         AS INT64 NO-UNDO.
DEF OUTPUT PARAM oiRequestInfoId     AS INT64 NO-UNDO.

DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR ix     AS INT    NO-UNDO.

IF iiMedlemsnr = ? THEN iiMedlemsnr = 0.

/*
FIND FIRST RequestType NO-LOCK
     WHERE RequestType.cExternalRequestType = icRequestType
     NO-ERROR.
IF NOT AVAIL RequestType THEN 
  RUN create_eventlog.p ("request_not_defined","Request type " + icRequestType + " not defined","","","","",iiKlientNr,iiMedlemsNr,"",INPUT-OUTPUT iLogHeaderId,OUTPUT iEventLogId).
ELSE IF RequestType.cProgram NE icServiceProgName THEN
  RUN create_eventlog.p ("requesttype_caller","Request type " + icRequestType + " called but not defined for " + icServiceProgName,"","","","",iiKlientNr,iiMedlemsNr,"",INPUT-OUTPUT iLogHeaderId,OUTPUT iEventLogId).

IF iLoginSessionId = 0 AND iiMedlemsnr NE 0 THEN
  RUN create_eventlog.p ("request_failure","Session id could not be validated for request","","","","",iiKlientNr,iiMedlemsNr,"",INPUT-OUTPUT iLogHeaderId,OUTPUT iEventLogId).
ELSE DO ON ERROR UNDO,LEAVE:
  CREATE Request. 
  ASSIGN Request.dtRequest       = NOW
         Request.iRequestTypeId  = (IF AVAIL RequestType THEN RequestType.iRequestTypeId ELSE 0)
         Request.iLoginSessionId = iLoginSessionId
         Request.KlientNr        = iiKlientNr
         Request.Medlemsnr       = iiMedlemsnr
         Request.bRequestOK      = YES
         oiRequestId             = Request.iRequestId
         .
  IF ibCreateRequestInfo THEN DO:
    CREATE RequestInfo.
    ASSIGN RequestInfo.iRequestId = Request.iRequestId
           RequestInfo.KlientNr   = Request.KlientNr
           RequestInfo.Medlemsnr  = Request.Medlemsnr
           oiRequestInfoId        = RequestInfo.iRequestInfoId
           .
  END.
END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creTestLoginSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creTestLoginSession Procedure 
PROCEDURE creTestLoginSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiKlientnr  AS INT  NO-UNDO.
DEF INPUT  PARAM iiMedlemsnr AS INT  NO-UNDO.
DEF OUTPUT PARAM ocSessionId AS CHAR NO-UNDO.

/*
CREATE LoginSession.
ASSIGN LoginSession.cSessionId = "TEST" + STRING(NOW)
       LoginSession.dtCreated  = NOW
       LoginSession.iLoginTime = TIME
       LoginSession.KlientNr   = iiKlientnr
       LoginSession.Medlemsnr  = iiMedlemsnr
       .

ocSessionId = LoginSession.cSessionId.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDocs Procedure
PROCEDURE deleteDocs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE ttDoc.
EMPTY TEMP-TABLE ttNodes.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-dumpXmlNodes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpXmlNodes Procedure 
PROCEDURE dumpXmlNodes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icDocName AS CHAR NO-UNDO.

FIND FIRST ttDoc 
     WHERE ttDoc.cDocName = icDocName
     NO-ERROR.
IF AVAIL ttDoc THEN DO:
  EMPTY TEMP-TABLE ttDumpNodes.  
  FOR EACH ttNodes
      WHERE ttNodes.hDoc = ttDoc.hDoc
      :
    CREATE ttDumpNodes.
    BUFFER-COPY ttNodes TO ttDumpNodes.
  END.
  IF SESSION:CLIENT-TYPE = "APPSERVER" THEN DO:
    OUTPUT TO VALUE (".\" + icDocName + ".csv").
    FOR EACH ttDumpNodes:
      EXPORT DELIMITER ";" ttDumpNodes.
    END.
    OUTPUT CLOSE.
  END.
  ELSE RUN toExcelViaFile.p (httDumpNodes,0).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndElement Procedure 
PROCEDURE EndElement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER namespaceURI AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER localName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER qName AS CHARACTER NO-UNDO.

iLevel = iLevel - 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNodeValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNodeValue Procedure 
PROCEDURE getNodeValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icDocName   AS CHAR NO-UNDO.
DEF INPUT  PARAM icNodeName  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocNodeValue AS CHAR NO-UNDO.

FIND FIRST ttDoc
     WHERE ttDoc.cDocName = icDocName
     NO-ERROR.
IF AVAIL ttDoc THEN DO:
  FIND FIRST ttNodes 
       WHERE ttNodes.hDoc = ttDoc.hDoc
         AND ttNodes.cElement = icNodeName
       NO-ERROR.
  IF AVAIL ttNodes THEN
    ocNodeValue = ttNodes.cNodeValue.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTtXML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTtXML Procedure 
PROCEDURE getTtXML :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icDocName   AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE FOR ttXML.

FOR EACH ttXML WHERE
    ttXML.cDocName = icDocName:
  DELETE ttXML.
END.

FIND FIRST ttDoc 
     WHERE ttDoc.cDocName = icDocName 
     NO-ERROR.
IF AVAIL ttDoc THEN
  FOR EACH ttNodes
      WHERE ttNodes.hDoc = ttDoc.hDoc:
    CREATE ttXML.
    BUFFER-COPY ttNodes TO ttXML
           ASSIGN ttXML.cDocName = icDocName
           . 
  END.
ELSE MESSAGE "No xml exists for " icDocName VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getXmlHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getXmlHandle Procedure 
PROCEDURE getXmlHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icDocName AS CHAR NO-UNDO.
DEF OUTPUT PARAM ohDoc     AS HANDLE NO-UNDO.

FIND FIRST ttDoc 
     WHERE ttDoc.cDocName = icDocName
     NO-ERROR.
IF AVAIL ttDoc THEN 
  ohDoc = ttDoc.hDoc.
ELSE 
  MESSAGE "Could not find xml document " icDocName
          VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initXMLdoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initXMLdoc Procedure 
PROCEDURE initXMLdoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocName AS CHAR NO-UNDO.

FIND FIRST ttDoc 
     WHERE ttDoc.cDocName = icDocName
     NO-ERROR.
IF NOT AVAIL ttDoc THEN DO:
  CREATE ttDoc.
  ttDoc.cDocName = icDocName.
  CREATE X-DOCUMENT ttDoc.hDoc.
  hDoc = ttDoc.hDoc.
END.
ELSE DO:
  MESSAGE icDocName " already defined"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

IF NOT VALID-HANDLE(hText) THEN
  CREATE X-NODEREF hText.
IF NOT VALID-HANDLE(hFormat) THEN
  CREATE X-NODEREF hFormat.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loadXML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadXML Procedure 
PROCEDURE loadXML :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocName AS CHAR NO-UNDO.
DEF INPUT PARAM ilcDoc    AS LONGCHAR NO-UNDO.

FIND FIRST ttDoc 
     WHERE ttDoc.cDocName = icDocName
     NO-ERROR.
IF AVAIL ttDoc THEN DO:
  MESSAGE icDocName " already defined" VIEW-AS ALERT-BOX.
  RETURN.
END.

CREATE ttDoc.

CREATE SAX-READER ttDoc.hDoc NO-ERROR.

IF VALID-HANDLE(hSax) THEN DELETE OBJECT hSax.

ASSIGN hSax = ttDoc.hDoc
       ttDoc.cDocName = icDocName
       iNodeIdx = 0
       .

hSax:HANDLER = THIS-PROCEDURE NO-ERROR.
hSax:SET-INPUT-SOURCE("longchar",ilcDoc) NO-ERROR.
hSax:SAX-PARSE() NO-ERROR.   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActiveDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setActiveDoc Procedure 
PROCEDURE setActiveDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocName AS CHAR NO-UNDO.

FIND FIRST ttDoc 
     WHERE ttDoc.cDocName = icDocName
     NO-ERROR.
IF AVAIL ttDoc THEN 
  hDoc = ttDoc.hDoc.
ELSE
  MESSAGE icDocName " not defined" VIEW-AS ALERT-BOX.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseMptr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUseMptr Procedure 
PROCEDURE setUseMptr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM imMptr AS MEMPTR NO-UNDO.

SET-SIZE(mb64data) = 0.

ASSIGN mb64Data = imMptr
       bUseMptr = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartElement Procedure 
PROCEDURE StartElement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER namespaceURI AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER localName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER qName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER attributes AS HANDLE NO-UNDO.

ASSIGN cActiveNode = qName
       iLevel      = iLevel + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validateLoginSession) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateLoginSession Procedure 
PROCEDURE validateLoginSession :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAM icLoginSession AS CHAR NO-UNDO.
DEFINE INPUT  PARAM iiKlientNr     AS INT  NO-UNDO.
DEFINE INPUT  PARAM iiMedlemsnr    AS INT  NO-UNDO.
DEFINE OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.

DEF VAR cEventLogText AS CHAR NO-UNDO.

/*
FIND FIRST CodeValue NO-LOCK
     WHERE CodeValue.cCodeType = "CheckLoginSessionId"
       AND CodeValue.cCodeValue = "no" 
     NO-ERROR.
IF AVAIL CodeValue THEN DO:
*/

/*   FIND FIRST LoginSession NO-LOCK                                                                                                                                                      */
/*        WHERE LoginSession.KlientNr = iiKlientnr                                                                                                                                        */
/*          AND LoginSession.Medlemsnr = iiMedlemsnr                                                                                                                                      */
/*        NO-ERROR.                                                                                                                                                                       */
/*   RUN create_eventlog.p ("weak_session_validation","Login session checked for client/member only","","","","",iiKlientNr,iiMedlemsNr,"",INPUT-OUTPUT iLogHeaderId,OUTPUT iEventLogId). */

/*
END.
ELSE */
IF icLoginSession = '20141114' THEN DO:
  RUN create_eventlog.p ("validatesession_skipped",cEventLogText,"","","","",iiKlientNr,iiMedlemsNr,"",INPUT-OUTPUT iLogHeaderId,OUTPUT iEventLogId).
  RETURN.
END.
ELSE IF icLoginSession = "" 
     OR icLoginSession = "?" 
     OR icLoginSession = ? THEN DO:

  ASSIGN ocReturn = "Invalid session"
         cEventLogText = ocReturn + " (login session id empty )"
         .
  RETURN.
END.

/*     MESSAGE "icLoginSession unknown =  " icLoginSession = ? SKIP. */
/*
FIND FIRST LoginSession NO-LOCK
        WHERE LoginSession.cSessionId = icLoginSession
          AND TRIM(LoginSession.cSessionId) NE ""
          AND LoginSession.cSessionId NE "?"
          AND LoginSession.cSessionId NE ?
        NO-ERROR.

IF NOT AVAIL LoginSession THEN DO:
  ASSIGN ocReturn = "Invalid session"
         cEventLogText = ocReturn + " (login session " + icLoginSession + " not found)"
         .
END.
ELSE IF LoginSession.Medlemsnr NE iiMedlemsnr OR LoginSession.KlientNr NE iiKlientNr THEN
  ASSIGN ocReturn = "Invalid session"
         cEventLogText = ocReturn + " (client/member id mismatch)"
         .

IF ocReturn NE "" THEN
  RUN create_eventlog.p ("validatesession_failed",cEventLogText,"","","","",iiKlientNr,iiMedlemsNr,"",INPUT-OUTPUT iLogHeaderId,OUTPUT iEventLogId).
ELSE 
  ASSIGN iLoginSessionId = LoginSession.iLoginSessionId
         cSessionId      = LoginSession.cSessionId
         .
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getErrorDesc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getErrorDesc Procedure 
FUNCTION getErrorDesc RETURNS CHARACTER
  ( INPUT iiMessageNr AS INT,INPUT icMessageType AS CHAR,INPUT icMessageTxt AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
IF R-INDEX(STRING(iiMessageNr),"0") = 1 THEN 
  RETURN icMessageTxt + " (" + STRING(iiMessageNr) + ")".
ELSE DO:
  FIND FIRST CodeValue NO-LOCK
        WHERE CodeValue.cCodeType = icMessageType
          AND CodeValue.cCodeValue = STRING(iiMessageNr)
        NO-ERROR.
  IF AVAIL CodeValue AND CodeValue.cMisc2 NE "" THEN
    RETURN CodeValue.cMisc2 + " (" + STRING(iiMessageNr) + ")".
  ELSE 
    RETURN icMessageTxt + " (" + STRING(iiMessageNr) + ")".
END.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getErrorMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getErrorMsg Procedure 
FUNCTION getErrorMsg RETURNS CHARACTER
  ( INPUT iiMessageNr AS INT,INPUT icMessageType AS CHAR,INPUT icMessageTxt AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
IF iiMessageNr = 0 AND icMessageTxt NE "" THEN
  RETURN icMessageTxt + " (0)".
ELSE DO:
  FIND FIRST CodeValue NO-LOCK
        WHERE CodeValue.cCodeType = icMessageType
          AND CodeValue.cCodeValue = STRING(iiMessageNr)
       NO-ERROR.
  IF AVAIL CodeValue AND CodeValue.cDescription NE "" THEN
    RETURN CodeValue.cDescription + " (" + STRING(iiMessageNr) + ")".
  ELSE DO:
    FIND FIRST CodeValue NO-LOCK
          WHERE CodeValue.cCodeType = icMessageType
            AND CodeValue.cCodeValue = "0"
          NO-ERROR.
    IF AVAIL CodeValue THEN
      RETURN CodeValue.cDescription + " (" + STRING(iiMessageNr) + ")".
  END.
  RETURN "Beklager! Vi kan for øyeblikket ikke behandle din forespørsel, vennligst prøv igjen litt senere. (" + STRING(iiMessageNr) + ")".       
END.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoginSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoginSessionId Procedure 
FUNCTION getLoginSessionId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  RETURN iLoginSessionId.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

