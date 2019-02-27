&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : transaction.p
    Purpose     : General create/update/delete function that handles one or many updates to one or many buffers (tables). 


    Syntax      :

    Description :

    Author(s)   : Brynjar Hasle, Chemistry as www.chemistry.no
    Created     : sept.03
    Notes       : Customise the include-files in main-block for session-validation,
                  auto-assign-values and validation-exception-list
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Definitions  ************************** */


DEF INPUT PARAM                     icSessionId      AS CHAR NO-UNDO. 
DEF INPUT PARAM                     ibReturnAll      AS LOG  NO-UNDO.
DEF INPUT PARAM                     icParam          AS CHAR NO-UNDO.
DEF INPUT PARAM TABLE-HANDLE        httTransAction. 
DEF INPUT-OUTPUT PARAM TABLE-HANDLE httTransRecord.
DEF OUTPUT PARAM                    ocReturn         AS CHAR NO-UNDO.
DEF OUTPUT PARAM                    obOk             AS LOG  NO-UNDO.
DEF INPUT PARAM                     icDebugFile      AS CHAR NO-UNDO.

DEF VAR ix                AS INT NO-UNDO.
DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR bDebug            AS LOG NO-UNDO.

DEF VAR hBuffTransAction  AS HANDLE NO-UNDO.
DEF VAR hBuffTransRecord  AS HANDLE NO-UNDO.
DEF VAR hTransQuery       AS HANDLE NO-UNDO.
DEF VAR cTransBuffAct     AS CHAR NO-UNDO.
DEF VAR hTransBuffers     AS HANDLE NO-UNDO EXTENT 100.
DEF VAR ixTransBuffAct    AS INT NO-UNDO.
DEF VAR cCurrBuffer       AS CHAR NO-UNDO.
DEF VAR hCurrBuffer       AS HANDLE NO-UNDO.
DEF VAR hField            AS HANDLE NO-UNDO.
DEF VAR cCurrAction       AS CHAR NO-UNDO.
DEF VAR cQueryString      AS CHAR NO-UNDO.
DEF VAR cFindField        AS CHAR NO-UNDO.
DEF VAR cFindString       AS CHAR NO-UNDO.
DEF VAR cValueFields      AS CHAR NO-UNDO EXTENT 100. 
DEF VAR cCurrValueFields  AS CHAR NO-UNDO.
DEF VAR cCurrValues       AS CHAR NO-UNDO.
DEF VAR cUpdValidateCrit  AS CHAR NO-UNDO EXTENT 100. /* buffer1;field11,field12..|buffer2;field21,field22.. calculated once pr buffer/action */
DEF VAR cCurrUpdValCrit   AS CHAR NO-UNDO.            /* Current value of table above */
DEF VAR cCurrUpdValues    AS CHAR NO-UNDO.            /* Corresponding field values for cCurrUpdValCrit */
DEF VAR cValidationType   AS CHAR NO-UNDO EXTENT 100.
DEF VAR cCurrValType      AS CHAR NO-UNDO.
DEF VAR cPrimaryKeyFields AS CHAR NO-UNDO EXTENT 100.
DEF VAR cCurrPrimKeyFlds  AS CHAR NO-UNDO.
DEF VAR cErrorHandl       AS CHAR NO-UNDO EXTENT 100.
DEF VAR cCurrErrorHandl   AS CHAR NO-UNDO.
DEF VAR cCreateProc       AS CHAR NO-UNDO EXTENT 100.
DEF VAR cCurrCreateProc   AS CHAR NO-UNDO.
DEF VAR cPostUpdProc      AS CHAR NO-UNDO EXTENT 100.
DEF VAR cCurrPostUpdProc  AS CHAR NO-UNDO.
DEF VAR cTransContext     AS CHAR NO-UNDO.
DEF VAR bSupprDynDelete   AS LOG  NO-UNDO.
DEF VAR cReturnParam      AS CHAR NO-UNDO.
DEF VAR httPreTrans       AS HANDLE NO-UNDO.
DEF VAR hBuffPreTrans     AS HANDLE NO-UNDO.
DEF VAR hJbAPI            AS HANDLE NO-UNDO.
DEF VAR cExternalFields   AS CHAR  NO-UNDO.

DEF STREAM sDebug.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-assignExternalBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignExternalBuffer Procedure 
FUNCTION assignExternalBuffer RETURNS CHARACTER
  ( INPUT ihBuffer    AS HANDLE,
    INPUT icExtFields AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-assignStringValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignStringValue Procedure 
FUNCTION assignStringValue RETURNS CHARACTER
  ( INPUT icFieldName   AS CHAR,
    INPUT icUpdateValue AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dynDeleteValidate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dynDeleteValidate Procedure 
FUNCTION dynDeleteValidate RETURNS CHARACTER
  (INPUT ihBuffer    AS HANDLE,
   INPUT icPKfields  AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dynFkValidate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dynFkValidate Procedure 
FUNCTION dynFkValidate RETURNS CHARACTER
  (INPUT icUpdValCrit   AS CHAR,
   INPUT icValueFields  AS CHAR,
   INPUT icCurrValues   AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompanyId Procedure 
FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContext Procedure 
FUNCTION getContext RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentAction Procedure 
FUNCTION getCurrentAction RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentValueFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentValueFields Procedure 
FUNCTION getCurrentValueFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentValues Procedure 
FUNCTION getCurrentValues RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDate Procedure 
FUNCTION getDate RETURNS DATE
  (INPUT icDate AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExternalFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExternalFields Procedure 
FUNCTION getExternalFields RETURNS CHARACTER
  ( INPUT icTable AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInputParam Procedure 
FUNCTION getInputParam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLanguageCode Procedure 
FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOutputParam Procedure 
FUNCTION getOutputParam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPreTransBuffer Procedure 
FUNCTION getPreTransBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  (INPUT ihBuffer AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUpdValidateCrit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUpdValidateCrit Procedure 
FUNCTION getUpdValidateCrit RETURNS CHARACTER
  (INPUT icBuffer      AS CHAR,
   INPUT icValueFields AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue Procedure 
FUNCTION getValue RETURNS CHARACTER
  ( INPUT icField AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setContext Procedure 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icTransContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOutputParam Procedure 
FUNCTION setOutputParam RETURNS LOGICAL
  ( INPUT ocReturnParam AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPreTransBuffer Procedure 
FUNCTION setPreTransBuffer RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startASlib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startASlib Procedure 
FUNCTION startASlib RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SuppressDynDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SuppressDynDelete Procedure 
FUNCTION SuppressDynDelete RETURNS LOGICAL
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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 22.95
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
IF icDebugFile NE "" THEN DO:
  bDebug = TRUE.
  OUTPUT STREAM sDebug TO VALUE(icDebugFile).
END.

/* Customise these: */
{incl/validatesession.i}
{incl/autoassigncreate.i}
{incl/autoassignupdate.i}
{incl/valexceptionlist.i} 

RUN processTransaction.


DO ix = 1 TO 100:
  IF VALID-HANDLE(hTransBuffers[ix]) THEN
    DELETE OBJECT hTransBuffers[ix].
  ELSE LEAVE.
END.

IF NOT ibReturnAll THEN DO:
  ix = 0.
  hTransQuery:GET-FIRST(). 
  REPEAT WHILE NOT hTransQuery:QUERY-OFF-END:
    ix = ix + 1.
    IF ix > 1 THEN
      hBuffTransRecord:BUFFER-DELETE.
    hTransQuery:GET-NEXT(). 
  END.
END.
DELETE OBJECT hTransQuery.

IF VALID-HANDLE(hBuffPreTrans) THEN DO:
  DELETE OBJECT hBuffPreTrans NO-ERROR.
  DELETE OBJECT httPreTrans NO-ERROR.
END.

obOk = ocReturn = "".

IF cReturnParam NE "" THEN
  ocReturn = ocReturn + "¤" + cReturnParam.

IF cOrgDateFormat NE "" THEN SESSION:DATE-FORMAT    = cOrgDateFormat.
IF cOrgNumFormat  NE "" THEN SESSION:NUMERIC-FORMAT = cOrgNumFormat.

IF bDebug THEN
  OUTPUT STREAM sDebug CLOSE.

DELETE OBJECT httTransAction.
DELETE OBJECT httTransRecord.

IF VALID-HANDLE(hJbAPI) THEN DO:
  DYNAMIC-FUNCTION("setPreTransBuffer" IN hJbAPI,?).  
  DELETE PROCEDURE hJbAPI.
END.

CATCH eAnyError AS Progress.Lang.ERROR:  
  IF ocReturn  = "" THEN  
    ocReturn = "Error in transaction." + chr(10)     
             + (IF eAnyError:GetMessage(1) NE "" THEN eAnyError:GetMessage(1) + " (" + STRING(eAnyError:GetMessageNum(1)) + ")" ELSE "").
END CATCH.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-processTransaction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processTransaction Procedure 
PROCEDURE processTransaction :
DEF VAR cReturnFields   AS CHAR NO-UNDO.
DEF VAR cReturnValues   AS CHAR NO-UNDO.
DEF VAR cTemp           AS CHAR NO-UNDO.
DEF VAR hDBfield        AS HANDLE NO-UNDO.
DEF VAR hPostUpdProc    AS HANDLE NO-UNDO.

hBuffTransAction = httTransAction:DEFAULT-BUFFER-HANDLE.
hBuffTransRecord = httTransRecord:DEFAULT-BUFFER-HANDLE.

hPostUpdProc = hBuffTransAction:BUFFER-FIELD("cTransActionPostUpdProc") NO-ERROR.

CREATE QUERY hTransQuery.
hTransQuery:SET-BUFFERS(hBuffTransRecord,hBuffTransAction).
hTransQuery:QUERY-PREPARE("FOR EACH " + hBuffTransRecord:NAME + 
                           ", FIRST " + hBuffTransAction:NAME + 
                           " WHERE ttTransAction.iTransActionId = ttTransRecord.iTransActionId" +
                           " BY ttTransRecord.iTransRecordId").
hTransQuery:QUERY-OPEN().

hTransQuery:GET-FIRST().
UpdateBlock:
DO TRANSACTION ON ERROR UNDO, THROW:
  DO WHILE NOT hTransQuery:QUERY-OFF-END:

    ASSIGN cReturnValues   = ""
           bSupprDynDelete = NO.

    ASSIGN cCurrBuffer = hBuffTransAction:BUFFER-FIELD("cTransActionBuffer"):BUFFER-VALUE
           cCurrAction = hBuffTransAction:BUFFER-FIELD("cTransAction"):BUFFER-VALUE.

    IF NOT CAN-DO(cTransBuffAct,cCurrBuffer + cCurrAction) THEN DO:
      CREATE BUFFER hCurrBuffer FOR TABLE cCurrBuffer.
      ASSIGN ixTransBuffAct                  = ixTransBuffAct + 1
             cTransBuffAct                   = cTransBuffAct + cCurrBuffer + cCurrAction + ","
             hTransBuffers[ixTransBuffAct]   = hCurrBuffer
             cValueFields[ixTransBuffAct]    = hBuffTransAction:BUFFER-FIELD("cTransActionValueFields"):BUFFER-VALUE 
             cCurrValueFields                = cValueFields[ixTransBuffAct]
             cValidationType[ixTransBuffAct] = ENTRY(1,hBuffTransAction:BUFFER-FIELD("cTransActionValidation"):BUFFER-VALUE)
             cCurrValType                    = cValidationType[ixTransBuffAct]
             cErrorHandl[ixTransBuffAct]     = hBuffTransAction:BUFFER-FIELD("cTransActionErrorHandl"):BUFFER-VALUE
             cCurrErrorHandl                 = cErrorHandl[ixTransBuffAct]
             cCreateProc[ixTransBuffAct]     = IF NUM-ENTRIES(STRING(hBuffTransAction:BUFFER-FIELD("cTransActionValidation"):BUFFER-VALUE)) > 1 THEN
                                                 SUBSTR(ENTRY(2,hBuffTransAction:BUFFER-FIELD("cTransActionValidation"):BUFFER-VALUE),2)
                                               ELSE ""
             cCurrCreateProc                 = cCreateProc[ixTransBuffAct]
             cPostUpdProc[ixTransBuffAct]    = IF VALID-HANDLE(hPostUpdProc) THEN hPostUpdProc:BUFFER-VALUE ELSE ""
             cCurrPostUpdProc                = cPostUpdProc[ixTransBuffAct]
             .

      /* Detrm bffs and flds for upd val */
      IF cCurrValType NE "NO" AND NOT cCurrValType BEGINS "=" AND cCurrAction NE "delete" THEN DO:
        ASSIGN cUpdValidateCrit[ixTransBuffAct]  = getUpdValidateCrit(cCurrBuffer,cCurrValueFields)
               cCurrUpdValCrit                   = cUpdValidateCrit[ixTransBuffAct]
               NO-ERROR.
        IF ERROR-STATUS:ERROR AND bDebug THEN
          PUT STREAM sDebug UNFORMATTED SKIP(1)
              "Buffer: " cCurrBuffer SKIP
              "Error from getUpdateValidateCrit: " ERROR-STATUS:GET-MESSAGE(1) SKIP(1).
      END.
       
      IF cCurrAction NE "update" THEN DO:
        ASSIGN cPrimaryKeyFields[ixTransBuffAct] = getPrimaryKeyFields(hCurrBuffer)    
               cCurrPrimKeyFlds                  = cPrimaryKeyFields[ixTransBuffAct]
               NO-ERROR.
        IF ERROR-STATUS:ERROR AND bDebug THEN
          PUT STREAM sDebug UNFORMATTED SKIP(1)
              "Buffer: " cCurrBuffer SKIP
              "Error from getPrimaryKeyFields: " ERROR-STATUS:GET-MESSAGE(1) SKIP(1).
      END.
      ELSE cCurrPrimKeyFlds = "".
    END.
    ELSE 
      ASSIGN hCurrBuffer      = hTransBuffers[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrValueFields = cValueFields[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrUpdValCrit  = cUpdValidateCrit[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrValType     = cValidationType[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrPrimKeyFlds = cPrimaryKeyFields[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrErrorHandl  = cErrorHandl[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrCreateProc  = cCreateProc[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             cCurrPostUpdProc = cPostUpdProc[LOOKUP(cCurrBuffer + cCurrAction,cTransBuffAct)]
             .
  
    IF bDebug THEN
      PUT STREAM sDebug UNFORMATTED SKIP(1)
          "Buffer: " cCurrBuffer 
          "    Action: " cCurrAction "  " cCurrErrorHandl
          "    Criteria: " hBuffTransAction:BUFFER-FIELD("cTransActionCriteria"):BUFFER-VALUE 
          "    Codepage: " SESSION:CPINTERNAL SKIP
          "Key fields: " cCurrPrimKeyFlds SKIP
          "Fields: " cCurrValueFields SKIP
          "Values: " hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE SKIP
          "Val.proc: " cCurrValType SKIP
          "Create proc: " cCurrCreateProc SKIP
          "Post update proc: " cCurrPostUpdProc SKIP
          .

    /* Cre or find: */

    IF cCurrAction = "create" THEN DO:
      bOk = hCurrBuffer:BUFFER-CREATE() NO-ERROR.
      IF bOk THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cCurrPrimKeyFlds):
          IF CAN-DO(cCurrValueFields,ENTRY(ix,cCurrPrimKeyFlds)) THEN 
            ocReturn = assignStringValue(ENTRY(ix,cCurrPrimKeyFlds),
                                         ENTRY(LOOKUP(ENTRY(ix,cCurrPrimKeyFlds),cCurrValueFields),hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE,CHR(1))).
        END.
        IF cCurrCreateProc NE "" THEN DO:
          IF SEARCH(cCurrCreateProc) = ? AND SEARCH(SUBSTR(cCurrCreateProc,1,LENGTH(cCurrCreateProc) - 1) + "r") = ? THEN 
            ocReturn = "Couldn't find server create program: " + cCurrCreateProc.
          ELSE DO:
            RUN VALUE(cCurrCreateProc) (hCurrBuffer,icSessionId,OUTPUT ocReturn) NO-ERROR.
            IF ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER(1) = 3230 THEN DO:
              RUN VALUE(cCurrCreateProc) (hCurrBuffer,cCurrValueFields,REPLACE(hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE,CHR(1),"|"),icSessionId,OUTPUT ocReturn) NO-ERROR.
              IF ERROR-STATUS:ERROR AND ocReturn = "" THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
            END.
            ELSE IF ERROR-STATUS:ERROR AND ocReturn = "" THEN 
              ocReturn = ERROR-STATUS:GET-MESSAGE(1).   
          END.
        END.

        IF bDebug THEN
          PUT STREAM sDebug UNFORMATTED "ocReturn: " ocReturn SKIP.
        IF ocReturn NE "" THEN DO:
          IF cCurrErrorHandl = "ignore" THEN DO:
            IF bDebug THEN
              PUT STREAM sDebug UNFORMATTED ocReturn SKIP
                  "Buffer: " cCurrBuffer ". Action: " cCurrAction " skipped." SKIP
                  "-----------".
            hTransQuery:GET-NEXT().
            NEXT.
          END.
          ELSE bOk = FALSE.
        END.
      END.
      ELSE ocReturn = "Error in create " + cCurrBuffer.
    END.

    ELSE DO:
      cQueryString = hBuffTransAction:BUFFER-FIELD("cTransActionCriteria"):BUFFER-VALUE.
      CASE cQueryString:
        WHEN "rowid" THEN 
          bOk = hCurrBuffer:FIND-BY-ROWID(TO-ROWID(hBuffTransRecord:BUFFER-FIELD("cTransRecordRowid"):BUFFER-VALUE),EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
        WHEN "unique" THEN DO:
          cFindString = "WHERE ".
          DO ix = 1 TO NUM-ENTRIES(hBuffTransAction:BUFFER-FIELD("cTransActionIdFields"):BUFFER-VALUE):
            ASSIGN cFindField = ENTRY(ix,hBuffTransAction:BUFFER-FIELD("cTransActionIdFields"):BUFFER-VALUE).
                   cFindString = cFindString + cFindField + " = " +
                                 (IF hCurrBuffer:BUFFER-FIELD(cFindField):DATA-TYPE = "character" THEN
                                    QUOTER(ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE,CHR(1)))
                                  ELSE IF hCurrBuffer:BUFFER-FIELD(cFindField):DATA-TYPE = "decimal" THEN
                                     "DEC('" + ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE,CHR(1)) + "')"
                                  ELSE IF hCurrBuffer:BUFFER-FIELD(cFindField):DATA-TYPE = "date" THEN
                                     "DATE('" + ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE,CHR(1)) + "')"
                                  ELSE IF hCurrBuffer:BUFFER-FIELD(cFindField):DATA-TYPE = "logical" THEN
                                     "LOGICAL('" + ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE,CHR(1)) + "')"
                                  ELSE
                                    ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE,CHR(1))) +
                                 " AND "
                                 .
          END.
          cFindString = TRIM(cFindString," AND ").
          bOk = hCurrBuffer:FIND-UNIQUE(cFindString,EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
        END.
        OTHERWISE 
          bOk = FALSE.
      END CASE.
      IF NOT hCurrBuffer:AVAIL AND cCurrErrorHandl NE "avail" THEN DO:
        ocReturn = hCurrBuffer:NAME + " not available for update" + CHR(10) +
                   (IF hCurrBuffer:LOCKED THEN "Record is locked" 
                    ELSE "Record not found").
        UNDO, LEAVE UpdateBlock.
      END.

      IF NOT bOk THEN DO:
        IF cCurrErrorHandl = "avail" THEN DO:
          IF bDebug THEN
            PUT STREAM sDebug UNFORMATTED 
                "Buffer: " cCurrBuffer ". Action: " cCurrAction " skipped." SKIP
                "-----------".
          hTransQuery:GET-NEXT().
          NEXT.
        END.
        ELSE DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ocReturn = ocReturn + ERROR-STATUS:GET-MESSAGE(ix) + CHR(10).
        END.
      END.

    END.
    IF bDebug THEN DO:
      PUT STREAM sDebug UNFORMATTED 
          "Initial action: "bOK "  cFindString: " cFindString " Avail TransActBuffer: " hBuffTransAction:AVAIL SKIP.
      DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
        PUT STREAM sDebug UNFORMATTED ERROR-STATUS:GET-MESSAGE(ix) SKIP.
      END.
    END.

    /* Process record: */

    IF bOK THEN DO:
      setPreTransBuffer().
      IF cCurrAction = "delete" THEN DO:
        IF cCurrValType NE "NO" THEN DO:
          IF NOT cCurrValType BEGINS "=" THEN /* = ind. that only cust.val */
            ocReturn = dynDeleteValidate (hCurrBuffer,cCurrPrimKeyFlds).
          IF bDebug THEN
            PUT STREAM sDebug UNFORMATTED SKIP(1)
                "Standard validation: " SKIP
                "  cCurrPrimKeyFlds: " cCurrPrimKeyFlds SKIP
                "  cCurrValType: " cCurrValType "  SEARCH(SUBSTR(cCurrValType,2)): " SEARCH(SUBSTR(cCurrValType,2)) SKIP
                "  ocReturn: " ocReturn SKIP(1).
          IF (cCurrValType BEGINS "+" OR cCurrValType BEGINS "=") AND 
             ocReturn = "" AND
             (SEARCH(SUBSTR(cCurrValType,2)) NE ? OR SEARCH(SUBSTR(cCurrValType,2,LENGTH(cCurrValType) - 2) + "r") NE ?) THEN /* if both dyn and cust val the proc is pref by + */
            RUN VALUE(SUBSTR(cCurrValType,2)) (cCurrBuffer,STRING(hCurrBuffer:ROWID),icSessionId,OUTPUT ocReturn).
          ELSE IF ocReturn = "" AND (cCurrValType BEGINS "+" OR cCurrValType BEGINS "=") AND (SEARCH(SUBSTR(cCurrValType,2)) = ?  OR SEARCH(SUBSTR(cCurrValType,2,LENGTH(cCurrValType) - 2) + "r") = ?) AND ocReturn = "" THEN
            ocReturn = "Couldn't find server validation program: " + SUBSTR(cCurrValType,2).
        END.
        IF bDebug THEN
          PUT STREAM sDebug UNFORMATTED SKIP(1)
              "Post validation: " SKIP
              "  cCurrErrorHandl: " cCurrErrorHandl SKIP
              "  ocReturn: " ocReturn SKIP(1).

        IF ocReturn = "" OR cCurrErrorHandl = "ignore" THEN DO:
          IF hCurrBuffer:AVAIL AND NOT bSupprDynDelete THEN
            hCurrBuffer:BUFFER-DELETE().
          IF cCurrErrorHandl = "ignore" THEN ocReturn = "".
        END.
        ELSE
          UNDO, LEAVE UpdateBlock.
      END.
      ELSE DO:        
        IF cCurrValType NE "NO" THEN DO:
          cCurrValues = "".
          DO ix = 1 TO NUM-ENTRIES(cCurrValueFields):
            cCurrValues = cCurrValues + ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE,CHR(1)) + CHR(1).
          END.
          cCurrValues = SUBSTR(cCurrValues,1,LENGTH(cCurrValues) - 1).
          IF bDebug THEN
            PUT STREAM sDebug UNFORMATTED 
                "Start update, cCurrUpdValCrit: " cCurrUpdValCrit SKIP
                "              cCurrValueFields: " cCurrValueFields SKIP
                "              cCurrValues (replaced CHR(1) with |): " REPLACE(cCurrValues,CHR(1),"|") SKIP
                "              cCurrValType: " cCurrValType SKIP.

          IF NOT cCurrValType BEGINS "=" THEN /* = indicates that only custom validation should be used */
            ocReturn = dynFkValidate (cCurrUpdValCrit,cCurrValueFields,cCurrValues).
  
          IF (cCurrValType BEGINS "+" OR cCurrValType BEGINS "=") AND 
             ocReturn = "" AND 
             (SEARCH(SUBSTR(cCurrValType,2)) NE ? OR SEARCH(SUBSTR(cCurrValType,2,LENGTH(cCurrValType) - 2) + "r") NE ?) THEN DO:
            /* if both dynamic and custom val the procedure name should be prefixed by a + */
            RUN VALUE(SUBSTR(cCurrValType,2)) (STRING(hCurrBuffer:ROWID),icSessionId,OUTPUT ocReturn) NO-ERROR.
            IF ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER(1) = 3230 THEN 
              RUN VALUE(SUBSTR(cCurrValType,2)) (STRING(hCurrBuffer:ROWID),cCurrAction,icSessionId,OUTPUT ocReturn) NO-ERROR.
            IF ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER(1) = 3230 THEN DO:
              RUN VALUE(SUBSTR(cCurrValType,2)) (STRING(hCurrBuffer:ROWID),cCurrValueFields,REPLACE(cCurrValues,CHR(1),"|"),icSessionId,OUTPUT ocReturn) NO-ERROR.
              IF ERROR-STATUS:ERROR AND ocReturn = "" THEN
                ocReturn = ERROR-STATUS:GET-MESSAGE(1).
            END.  
            ELSE IF ERROR-STATUS:ERROR AND ocReturn = "" THEN 
              ocReturn = ERROR-STATUS:GET-MESSAGE(1).  
          END.
          ELSE IF ocReturn = "" AND (cCurrValType BEGINS "+" OR cCurrValType BEGINS "=") AND (SEARCH(SUBSTR(cCurrValType,2)) = ? OR SEARCH(SUBSTR(cCurrValType,2,LENGTH(cCurrValType) - 2) + "r") = ?) THEN
            ocReturn = "Couldn't find server validation program: " + SUBSTR(cCurrValType,2).

          IF bDebug THEN
            PUT STREAM sDebug UNFORMATTED 
                "Update validation message: " ocReturn SKIP.

          /* The validatation hook can also be used to complete any other information, by returning a list that begins with "valuelist",
            eg: "valuelist,field1|value1|field2|value2..: */
          cTemp = "".
          IF ocReturn BEGINS "valuelist" THEN DO:
            DO ix = 2 TO NUM-ENTRIES(ocReturn,"|") - 1 BY 2:
              cTemp = assignStringValue(ENTRY(ix,ocReturn,"|"),ENTRY(ix + 1,ocReturn,"|")).
              IF cTemp NE "" THEN UNDO, LEAVE UpdateBlock.
            END.
            ocReturn = cTemp.
          END.
          ELSE IF ocReturn NE "" AND cCurrErrorHandl NE "ignore" THEN
            UNDO, LEAVE UpdateBlock.
        END.

        IF ocReturn = "" OR cCurrErrorHandl = "ignore" THEN DO ix = 1 TO NUM-ENTRIES(cCurrValueFields):
          ocReturn = assignStringValue(ENTRY(ix,cCurrValueFields),ENTRY(ix,hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE,CHR(1))).
          IF ocReturn NE "" THEN UNDO, LEAVE UpdateBlock. 
        END.
        
        /* Assign auto-complete fields (could be like dCreated f.ex).
           Also prepare return values. Assign rowid, primary key fields and auto-complete fields and values to return-fields.
           Note the convetion TODAY as char value: */
        IF (ibReturnAll OR hBuffTransRecord:BUFFER-FIELD("iTransRecordId"):BUFFER-VALUE = 1) AND (ocReturn = "" OR cCurrErrorHandl = "ignore") THEN DO:
          ASSIGN hBuffTransRecord:BUFFER-FIELD("cTransRecordRowid"):BUFFER-VALUE = STRING(hCurrBuffer:ROWID)
                 cReturnFields = cCurrPrimKeyFlds.
          DO ix = 1 TO NUM-ENTRIES(cCurrPrimKeyFlds):
            cReturnValues = cReturnValues + hCurrBuffer:BUFFER-FIELD(ENTRY(ix,cCurrPrimKeyFlds)):BUFFER-VALUE + "|".
          END.  
  
          IF cCurrAction = "update" THEN DO:
            cReturnFields = cReturnFields + "," + cUpdateAutoAssign.
            DO ix = 1 TO NUM-ENTRIES(cUpdateAutoAssign):
              hField = hCurrBuffer:BUFFER-FIELD(ENTRY(ix,cUpdateAutoAssign)) NO-ERROR.
              IF VALID-HANDLE(hField) THEN DO:
                IF ENTRY(ix,cUpdateValues,"|") = "TODAY" THEN
                  hField:BUFFER-VALUE = TODAY.
                ELSE
                  AssignStringValue(hField:NAME,ENTRY(ix,cUpdateValues,"|")).
                cReturnValues = cReturnValues + STRING(hField:BUFFER-VALUE) + "|".
              END.
              ELSE cReturnValues = cReturnValues + "|".
            END.
          END.
          ELSE DO:
            cReturnFields = cReturnFields + "," + cCreateAutoAssign.
            DO ix = 1 TO NUM-ENTRIES(cCreateAutoAssign):
              IF CAN-DO(cCurrPrimKeyFlds,ENTRY(ix,cCreateAutoAssign)) THEN NEXT.
              hField = hCurrBuffer:BUFFER-FIELD(ENTRY(ix,cCreateAutoAssign)) NO-ERROR.
              IF VALID-HANDLE(hField) THEN DO:
                IF ENTRY(ix,cCreateValues,"|") = "TODAY" THEN
                  hField:BUFFER-VALUE = TODAY.
                ELSE
                  AssignStringValue(hField:NAME,ENTRY(ix,cCreateValues,"|")).
                cReturnValues = cReturnValues + STRING(hField:BUFFER-VALUE) + "|".
              END.
              ELSE cReturnValues = cReturnValues + "|".
            END.
          END.

          ASSIGN hBuffTransRecord:BUFFER-FIELD("cReturnExtraFields"):BUFFER-VALUE = TRIM(cReturnFields,",")
                 hBuffTransRecord:BUFFER-FIELD("cReturnExtraValues"):BUFFER-VALUE = SUBSTR(cReturnValues,1,LENGTH(cReturnValues) - 1)
                 .
        END.
      END.
      /* Invoke any post upd: */
      IF (ocReturn = "" OR cCurrErrorHandl = "ignore") AND cCurrPostUpdProc NE "" THEN DO:
        IF SEARCH(cCurrPostUpdProc) = ? AND SEARCH(SUBSTR(cCurrPostUpdProc,1,LENGTH(cCurrPostUpdProc) - 1) + "r") = ? THEN 
          ocReturn = "Couldn't find server post update program: " + cCurrPostUpdProc.
        ELSE DO:
          RUN VALUE(cCurrPostUpdProc) (IF cCurrAction = "DELETE" THEN hBuffPreTrans ELSE hCurrBuffer,cCurrAction,icSessionId,OUTPUT ocReturn). 
          IF ocReturn NE "" THEN
            UNDO, LEAVE UpdateBlock.
        END.
      END.
    END.
    ELSE DO:
      IF bDebug THEN 
        PUT STREAM sDebug UNFORMATTED 
          "Backing out on transaction: " SKIP
          "  Buffer: " cCurrBuffer ". Action: " cCurrAction SKIP
          "  cCurrUpdValCrit: " cCurrUpdValCrit SKIP
          "  cCurrValueFields: " cCurrValueFields SKIP
          "  cCurrValues (replaced CHR(1) with |): " cCurrValues SKIP.

      obOk = FALSE.
      IF ocReturn = "" THEN
        ocReturn = "Transaction failed".
      UNDO, LEAVE UpdateBlock.
    END.

    IF bDebug THEN
      PUT STREAM sDebug UNFORMATTED 
          "Buffer: " cCurrBuffer ". Action: " cCurrAction " done." SKIP(1)
          "STRING(hCurrBuffer:ROWID): " STRING(hCurrBuffer:ROWID) SKIP
          'hBuffTransRecord:BUFFER-FIELD("iTransRecordId"):BUFFER-VALUE: ' hBuffTransRecord:BUFFER-FIELD("iTransRecordId"):BUFFER-VALUE SKIP(1)
          'hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE: ' hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE SKIP(1)
          'hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE: ' hBuffTransRecord:BUFFER-FIELD("cTransRecordIdValues"):BUFFER-VALUE SKIP
          "----------------------------------------------" SKIP(1).


    hTransQuery:GET-NEXT().
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-assignExternalBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignExternalBuffer Procedure 
FUNCTION assignExternalBuffer RETURNS CHARACTER
  ( INPUT ihBuffer    AS HANDLE,
    INPUT icExtFields AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR cField     AS CHAR   NO-UNDO.
DEF VAR iExtent    AS INT    NO-UNDO.
DEF VAR cExtFields AS CHAR   NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR cFieldName AS CHAR   NO-UNDO.
DEF VAR cValue     AS CHAR   NO-UNDO.
DEF VAR ocReturn   AS CHAR   NO-UNDO.

IF icExtFields NE "" THEN
  cExtFields = icExtFields.
ELSE
  cExtFields = getExternalFields(ihBuffer:NAME).

DO ix = 1 TO NUM-ENTRIES(cExtFields):
  cFieldName = ENTRY(ix,cExtFields).
  cValue = getValue(cFieldName).
  IF INDEX(cFieldName,"[") > 0 THEN
    ASSIGN cField  = SUBSTR(cFieldName,1,INDEX(cFieldName,"[") - 1)
           iExtent = INT(SUBSTR(cFieldName,INDEX(cFieldName,"[") + 1,INDEX(cFieldName,"]") - INDEX(cFieldName,"[") - 1)).
  ELSE cField = cFieldName.
  
  hField = ihBuffer:BUFFER-FIELD(cField) NO-ERROR.
  
  IF VALID-HANDLE(hField) THEN DO:    
    CASE hField:DATA-TYPE:
      WHEN "character" THEN
        hField:BUFFER-VALUE[iExtent] = cValue NO-ERROR.
      WHEN "date" THEN 
        hField:BUFFER-VALUE[iExtent] = DATE(cValue) NO-ERROR.
      WHEN "decimal" THEN
        hField:BUFFER-VALUE[iExtent] = DEC(cValue) NO-ERROR.
      WHEN "integer" THEN
        hField:BUFFER-VALUE[iExtent] = INT(cValue) NO-ERROR.
      WHEN "logical" THEN 
        hField:BUFFER-VALUE[iExtent] = (IF cValue = "yes" OR cValue = "true" THEN TRUE 
                                        ELSE IF cValue = "?" THEN ?
                                        ELSE FALSE)  NO-ERROR.
      OTHERWISE DO:
        RUN jbserv_assignstringvalue.p (hField,iExtent,cValue,OUTPUT bOk).
        IF NOT bOk THEN
          ocReturn = ocReturn + (IF ocReturn NE "" THEN CHR(10) ELSE "") + "Error assigning string value " + cValue.
      END.
    END CASE.
    IF ERROR-STATUS:ERROR THEN                         
      ocReturn = ocReturn + (IF ocReturn NE "" THEN CHR(10) ELSE "") + ERROR-STATUS:GET-MESSAGE(1).   
  END.
END.

RETURN ocReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-assignStringValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignStringValue Procedure 
FUNCTION assignStringValue RETURNS CHARACTER
  ( INPUT icFieldName   AS CHAR,
    INPUT icUpdateValue AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR cField  AS CHAR   NO-UNDO.
DEF VAR iExtent AS INT    NO-UNDO.

IF INDEX(icFieldName,"[") > 0 THEN
  ASSIGN cField  = SUBSTR(icFieldName,1,INDEX(icFieldName,"[") - 1)
         iExtent = INT(SUBSTR(icFieldName,INDEX(icFieldName,"[") + 1,INDEX(icFieldName,"]") - INDEX(icFieldName,"[") - 1)).
ELSE cField = icFieldName.

hField = hCurrBuffer:BUFFER-FIELD(cField) NO-ERROR.

IF NOT VALID-HANDLE(hField) THEN DO:    
  IF bDebug THEN
    PUT STREAM sDebug UNFORMATTED 
        "Warning, Invalid field for update: " icFieldName SKIP.
  RETURN "".
END.

CASE hField:DATA-TYPE:
  WHEN "character" THEN
    hField:BUFFER-VALUE[iExtent] = icUpdateValue NO-ERROR.
  WHEN "date" THEN 
    hField:BUFFER-VALUE[iExtent] = DATE(icUpdateValue) NO-ERROR.
  WHEN "decimal" THEN
    hField:BUFFER-VALUE[iExtent] = DEC(icUpdateValue) NO-ERROR.
  WHEN "integer" THEN
    hField:BUFFER-VALUE[iExtent] = INT(icUpdateValue) NO-ERROR.
  WHEN "logical" THEN 
    hField:BUFFER-VALUE[iExtent] = (IF icUpdateValue = "yes" OR icUpdateValue = "true" THEN TRUE 
                                    ELSE IF icUpdateValue = "?" THEN ?
                                    ELSE FALSE)  NO-ERROR.
  OTHERWISE DO:
    RUN jbserv_assignstringvalue.p (hField,iExtent,icUpdateValue,OUTPUT bOk).
    IF NOT bOk THEN
      RETURN "Error assigning string value " + icUpdateValue.
  END.
END CASE.
IF ERROR-STATUS:ERROR THEN
  RETURN ERROR-STATUS:GET-MESSAGE(1).   
ELSE 
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dynDeleteValidate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dynDeleteValidate Procedure 
FUNCTION dynDeleteValidate RETURNS CHARACTER
  (INPUT ihBuffer    AS HANDLE,
   INPUT icPKfields  AS CHAR):

DEF VAR hPKfield       AS HANDLE NO-UNDO.
DEF VAR cPKvalue       AS CHAR   NO-UNDO.
DEF VAR bFK            AS LOG    NO-UNDO.
DEF VAR hFKbuffer      AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR iy             AS INT    NO-UNDO.
DEF VAR cFKbuffers     AS CHAR   NO-UNDO.
DEF VAR bOk            AS LOG    NO-UNDO.
DEF VAR cWhereString   AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR ocReturn       AS CHAR   NO-UNDO.

/* Determine PK values: */
DO ix = 1 TO NUM-ENTRIES(icPKfields):
  ASSIGN cPKvalue = ihBuffer:BUFFER-FIELD(ENTRY(ix,icPKfields)):BUFFER-VALUE
         hPKfield = ihBuffer:BUFFER-FIELD(ENTRY(ix,icPKfields))
         .
  IF hPKfield:DATA-TYPE = "CHARACTER" THEN
    cPKvalue = QUOTER(cPKvalue).
  ELSE IF hPKfield:DATA-TYPE = "DECIMAL" THEN
    cPKvalue = 'DEC("' + cPKvalue + '")'.
  ELSE IF hPKfield:DATA-TYPE = "DATE" THEN
    cPKvalue = 'DATE("' + cPKvalue + '")'.
  cWhereString = cWhereString + ENTRY(ix,icPKfields) + " = " + cPKvalue + " AND ".
END.  
cWhereString = TRIM(cWhereString," AND ").

/* Seek out FK tables and check for existence. All components of PK must be member of same index in referencing table: */
FOR EACH _field NO-LOCK
    WHERE _field._Field-Name = ENTRY(1,icPKfields),
    FIRST _file OF _field NO-LOCK
          WHERE _file._File-Name NE ihBuffer:NAME
            AND _file._tbl-type = "T":

  CREATE BUFFER hFKbuffer FOR TABLE _file._File-Name.
  DO ix = 1 TO 100:
    bFK = FALSE.
    IF hFKbuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
      DO iy = 1 TO NUM-ENTRIES(icPKfields):
        IF CAN-DO(hFKbuffer:INDEX-INFORMATION(ix),ENTRY(iy,icPKfields)) THEN DO:
          bFK = TRUE.
        END.
        ELSE DO: 
          bFK = FALSE.
          LEAVE.
        END.
      END.
      IF bFK THEN DO:
        bOK = hFKbuffer:FIND-FIRST(cWhereString,NO-LOCK) NO-ERROR.
        IF bOk THEN
          ocReturn = ocReturn + _file._File-Name + ", ".
        LEAVE.
      END.
    END.
    ELSE LEAVE.
  END.
  DELETE OBJECT hFKbuffer.
END.

RETURN TRIM(ocReturn,", ").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dynFkValidate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dynFkValidate Procedure 
FUNCTION dynFkValidate RETURNS CHARACTER
  (INPUT icUpdValCrit   AS CHAR,
   INPUT icValueFields  AS CHAR,
   INPUT icCurrValues   AS CHAR):

DEF VAR cPKfield       AS CHAR NO-UNDO.
DEF VAR cPKvalue       AS CHAR NO-UNDO.
DEF VAR hFKbuffer      AS HANDLE NO-UNDO.
DEF VAR ix             AS INT NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.
DEF VAR cFKbuffer      AS CHAR NO-UNDO.
DEF VAR bOk            AS LOG NO-UNDO.
DEF VAR cWhereString   AS CHAR NO-UNDO.
DEF VAR ocReturn       AS CHAR NO-UNDO.

IF icUpdValCrit NE "" THEN 
  ValCrit:
  DO ix = 1 TO NUM-ENTRIES(icUpdValCrit,"|"):
    ASSIGN cFKbuffer    = ENTRY(1,ENTRY(ix,icUpdValCrit,"|"),";")
           cWhereString = "WHERE "
           .
    FIND FIRST _file WHERE _file-name = cCurrBuffer NO-LOCK NO-ERROR.

    CREATE BUFFER hFKbuffer FOR TABLE cFKbuffer.
    DO iy = 1 TO NUM-ENTRIES(ENTRY(2,ENTRY(ix,icUpdValCrit,"|"),";")):
      ASSIGN cPKfield = ENTRY(iy,ENTRY(2,ENTRY(ix,icUpdValCrit,"|"),";")).
      IF LOOKUP(cPKfield,icValueFields) = 0 THEN NEXT ValCrit.
      ELSE cPKvalue = ENTRY(LOOKUP(cPKfield,icValueFields),icCurrValues,CHR(1)).

      IF cPKvalue = "0" OR cPKvalue = "" THEN DO:
        IF AVAIL _file THEN DO:
          FIND FIRST _field OF _file
               WHERE _field-name = cPKfield
               NO-LOCK NO-ERROR.
          IF AVAIL _field THEN DO:
            IF NOT _field._mandatory THEN
              NEXT ValCrit.
          END.
          ELSE NEXT ValCrit.
        END.
        ELSE NEXT ValCrit.
      END.

      IF hFKbuffer:BUFFER-FIELD(cPKfield):DATA-TYPE = "CHARACTER" THEN
        cPKvalue = QUOTER(cPKvalue).
      ELSE IF hFKbuffer:BUFFER-FIELD(cPKfield):DATA-TYPE = "DECIMAL" THEN
        cPKvalue = 'DEC("' + cPKvalue + '")'.
      ELSE IF hFKbuffer:BUFFER-FIELD(cPKfield):DATA-TYPE = "DATE" THEN
        cPKvalue = 'DATE("' + cPKvalue + '")'.
      cWhereString = cWhereString + cPKfield + " = " + cPKvalue + " AND ".
    END.
    cWhereString = TRIM(cWhereString," AND ").
    bOk = hFKbuffer:FIND-UNIQUE(cWhereString,NO-LOCK) NO-ERROR.
    DELETE OBJECT hFKbuffer.
    IF NOT bOK THEN 
      ocReturn = ocReturn + "Invalid reference: " + cFKbuffer + " " + cWhereString + CHR(10).
  END.  

RETURN TRIM(ocReturn,CHR(10)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrUserId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCodeMaster.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompanyId Procedure 
FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrCompanyId. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContext Procedure 
FUNCTION getContext RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Get context that can be shared for many transactions 
    Notes: Useful f.ex when creating both header and details in one transactin
------------------------------------------------------------------------------*/
RETURN cTransContext.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentAction Procedure 
FUNCTION getCurrentAction RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  If the update validation proc shoud act different based on the action the current action can be obtained:
            cAction = DYNAMIC-FUNCTION("getCurrentAction" IN SOURCE-PROCEDURE).
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrAction.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentValueFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentValueFields Procedure 
FUNCTION getCurrentValueFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Return the current value field names for updates 
    Notes: To invoke from validation hook:
           cValueFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE). 
------------------------------------------------------------------------------*/

RETURN cCurrValueFields. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentValues Procedure 
FUNCTION getCurrentValues RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Return the current values for an update 
    Notes: To invoke from validation hook:
           cValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE). 
           
           dec08: ONLY KEPT FOR BACKWARD COMPATIBILITY
------------------------------------------------------------------------------*/

RETURN REPLACE(cCurrValues,CHR(1),"|").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDate Procedure 
FUNCTION getDate RETURNS DATE
  (INPUT icDate AS CHAR):

icDate = TRIM(icDate,"/").
IF icDate NE ? AND icDate NE "" THEN DO:
  IF SESSION:DATE-FORMAT = "dmy" THEN
    RETURN DATE(INT(SUBSTR(icDate,4,2)),INT(SUBSTR(icDate,1,2)),INT(SUBSTR(icDate,7))).
  ELSE 
    RETURN DATE(INT(SUBSTR(icDate,1,2)),INT(SUBSTR(icDate,4,2)),INT(SUBSTR(icDate,7))).
END.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExternalFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExternalFields Procedure 
FUNCTION getExternalFields RETURNS CHARACTER
  ( INPUT icTable AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField      AS HANDLE NO-UNDO.
DEF VAR cFieldName  AS CHAR   NO-UNDO.
DEF VAR cField      AS CHAR   NO-UNDO.
DEF VAR iExtent     AS INT    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.

IF cExternalFields = "" OR icTable NE "" THEN DO:
  IF icTable = "" THEN
    hBuffer = hCurrBuffer.
  ELSE 
    CREATE BUFFER hBuffer FOR TABLE icTable NO-ERROR.
  
  IF VALID-HANDLE(hBuffer) THEN
    DO ix = 1 TO NUM-ENTRIES(cCurrValueFields):
      cFieldName = ENTRY(ix,cCurrValueFields).
  
      IF INDEX(cFieldName,"[") > 0 THEN
        ASSIGN cField  = SUBSTR(cFieldName,1,INDEX(cFieldName,"[") - 1)
               iExtent = INT(SUBSTR(cFieldName,INDEX(cFieldName,"[") + 1,INDEX(cFieldName,"]") - INDEX(cFieldName,"[") - 1)).
      ELSE cField = cFieldName.
  
      hField = hBuffer:BUFFER-FIELD(cField) NO-ERROR.
  
      cExternalFields = cExternalFields + (IF cExternalFields NE "" THEN "," ELSE "") + cFieldName.
    END.
END. 
  
IF icTable NE "" THEN
  DELETE OBJECT hBuffer NO-ERROR.

RETURN cExternalFields.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInputParam Procedure 
FUNCTION getInputParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Return the input parametere string passed from the client
           to a bl-hook procedure       
    Notes: To set the parameter on the client use the setServerTransInputParam 
           function
------------------------------------------------------------------------------*/

RETURN icParam.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLanguageCode Procedure 
FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrLanguage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOutputParam Procedure 
FUNCTION getOutputParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: If f.ex both the create and update validation hooks should pass a
           return param (by using the ¤ separator) the current return value can
           be obtained (for further append) 
    Notes:  
------------------------------------------------------------------------------*/

RETURN cReturnParam. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPreTransBuffer Procedure 
FUNCTION getPreTransBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBuffPreTrans.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  (INPUT ihBuffer AS HANDLE):

DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.

DO ix = 1 TO 100:
  IF ihBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
    IF ENTRY(2,ihBuffer:INDEX-INFORMATION(ix)) = "1" AND ENTRY(3,ihBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
      DO iy = 5 TO NUM-ENTRIES(ihBuffer:INDEX-INFORMATION(ix)) BY 2:
        cPKfields = cPKfields + ENTRY(iy,ihBuffer:INDEX-INFORMATION(ix)) + ",".
      END.
      LEAVE.
    END.
  END.
  ELSE LEAVE.
END.  
IF cPKfields = "" THEN
  DO ix = 1 TO 100:
    IF ihBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
      IF ENTRY(2,ihBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
        DO iy = 5 TO NUM-ENTRIES(ihBuffer:INDEX-INFORMATION(ix)) BY 2:
          cPKfields = cPKfields + ENTRY(iy,ihBuffer:INDEX-INFORMATION(ix)) + ",".
        END.
        LEAVE.
      END.
    END.
    ELSE LEAVE.
  END.  


RETURN TRIM(cPKfields,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUpdValidateCrit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUpdValidateCrit Procedure 
FUNCTION getUpdValidateCrit RETURNS CHARACTER
  (INPUT icBuffer      AS CHAR,
   INPUT icValueFields AS CHAR):

  DEF VAR bFK            AS LOG NO-UNDO.
  DEF VAR hFKbuffer      AS HANDLE NO-UNDO.
  DEF VAR cPKfields      AS CHAR NO-UNDO.
  DEF VAR iy             AS INT NO-UNDO.
  DEF VAR iz             AS INT NO-UNDO.
  DEF VAR cReturn        AS CHAR NO-UNDO.

  DO ix = 1 TO NUM-ENTRIES(icValueFields):
    /* Seek out FK tables. All components of FK must be member of field list: */
    
    IF CAN-DO(cValExceptionList,ENTRY(ix,icValueFields)) THEN NEXT.

    FOR EACH _field NO-LOCK
        WHERE _field._Field-Name = ENTRY(ix,icValueFields),
        FIRST _file OF _field NO-LOCK
              WHERE _file._File-Name NE icBuffer
                AND _file._tbl-type = "T":

      CREATE BUFFER hFKbuffer FOR TABLE _file._File-Name.
      DO iy = 1 TO 100:
        bFK = TRUE.
        cPKfields = "".
        IF hFKbuffer:INDEX-INFORMATION(iy) NE ? THEN DO:
          IF ENTRY(2,hFKbuffer:INDEX-INFORMATION(iy)) = "1" AND ENTRY(3,hFKbuffer:INDEX-INFORMATION(iy)) = "1" AND ENTRY(5,hFKbuffer:INDEX-INFORMATION(iy)) = ENTRY(ix,icValueFields) THEN DO:
            cPKfields = ENTRY(ix,icValueFields).
            DO iz = 7 TO NUM-ENTRIES(hFKbuffer:INDEX-INFORMATION(iy)) BY 2:
              IF CAN-DO(icValueFields, ENTRY(iz,hFKbuffer:INDEX-INFORMATION(iy))) THEN
                cPKfields = cPKfields + ENTRY(iz,hFKbuffer:INDEX-INFORMATION(iy)) + ",".
              ELSE bFK = FALSE.
            END.
            IF bFK THEN 
              cReturn = cReturn + _file._File-Name + ";" + TRIM(cPKfields,",") + "|".
            LEAVE.
          END.
        END.
        ELSE LEAVE.
      END.
      DELETE OBJECT hFKbuffer.
    END.
  END.
  RETURN TRIM(cReturn,"|").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue Procedure 
FUNCTION getValue RETURNS CHARACTER
  ( INPUT icField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF LOOKUP(icField,cCurrValueFields) = 0 THEN RETURN "".

IF cCurrValues = "" THEN
  RETURN ENTRY(LOOKUP(icField,cCurrValueFields),hBuffTransRecord:BUFFER-FIELD("cTransRecordValues"):BUFFER-VALUE,CHR(1)).
ELSE
  RETURN ENTRY(LOOKUP(icField,cCurrValueFields),cCurrValues,CHR(1)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setContext Procedure 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icTransContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Store context that can be used for multiple transactions 
    Notes: Useful f.ex when creating both header and details in one transactin
------------------------------------------------------------------------------*/
cTransContext = icTransContext.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOutputParam Procedure 
FUNCTION setOutputParam RETURNS LOGICAL
  ( INPUT ocReturnParam AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cReturnParam = ocReturnParam.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPreTransBuffer Procedure 
FUNCTION setPreTransBuffer RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hBuffPreTrans) THEN DO:
  DELETE OBJECT hBuffPreTrans NO-ERROR.
  DELETE OBJECT httPreTrans NO-ERROR.
END.

CREATE TEMP-TABLE httPreTrans.
httPreTrans:CREATE-LIKE(hCurrBuffer).
httPreTrans:TEMP-TABLE-PREPARE(hCurrBuffer:NAME).
hBuffPreTrans = httPreTrans:DEFAULT-BUFFER-HANDLE.
hBuffPreTrans:BUFFER-CREATE().
hBuffPreTrans:BUFFER-COPY(hCurrBuffer).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startASlib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startASlib Procedure 
FUNCTION startASlib RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hJbAPI) THEN
  RUN jbserv_api_for_server.p PERSIST SET hJbAPI (icSessionId).
  
SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(hJbAPI).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SuppressDynDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SuppressDynDelete Procedure 
FUNCTION SuppressDynDelete RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bSupprDynDelete = YES.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

