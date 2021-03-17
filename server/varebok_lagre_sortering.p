/* Lagre en spesialsortering for varebok
   
   Opprettet: 09.01.08 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO INIT YES.

DEF VAR cSortString   AS CHAR   NO-UNDO.
DEF VAR cDescription  AS CHAR   NO-UNDO.
DEF VAR cUser         AS CHAR   NO-UNDO.
DEF VAR rDefault      AS ROWID  NO-UNDO.
DEF VAR cDefault      AS CHAR   NO-UNDO.

ASSIGN cSortString   = ENTRY(1,icParam,"|")
       cDescription  = ENTRY(2,icParam,"|")
       cDefault      = ENTRY(3,icParam,"|")
       cUser         = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       .

FIND FIRST JBoxGenCodeType NO-LOCK
     WHERE JBoxGenCodeType.cCodeType = "VarebokSort"
     NO-ERROR.
IF NOT AVAIL JBoxGenCodeType THEN DO:
  CREATE JBoxGenCodeType.
  ASSIGN JBoxGenCodeType.cCodeType    = "VarebokSort"
         JBoxGenCodeType.cDescription = "Spesialsortering for varebøker"
         JBoxGenCodeType.cCreatedBy   = cUser
         JBoxGenCodeType.dCreated     = TODAY.
END.

FIND FIRST JBoxGenCode EXCLUSIVE-LOCK 
     WHERE JBoxGenCode.cCodeType  = JBoxGenCodeType.cCodeType
       AND JBoxGenCode.cCodeValue = cSortString
     NO-ERROR.
IF NOT AVAIL JBoxGenCode THEN DO:
  CREATE JBoxGenCode.
  ASSIGN JBoxGenCode.cCodeType     = JBoxGenCodeType.cCodeType
         JBoxGenCode.cCodeValue    = cSortString
         JBoxGenCode.cCreatedBy    = cUser
         JBoxGenCode.dCreated      = TODAY
         JBoxGenCode.cDescription  = cDescription
         JBoxGenCode.cMisc1        = cDefault
         rDefault                 = ROWID(JBoxGenCode)
         .
END.
ELSE
  ASSIGN JBoxGenCode.cDescription = cDescription
         JBoxGenCode.cModifiedBy  = cUser
         JBoxGenCode.dModified    = TODAY
         JBoxGenCode.cMisc1       = cDefault
         rDefault                 = ROWID(JBoxGenCode)
         .
IF LOGICAL(cDefault) THEN
  FOR EACH JBoxGenCode EXCLUSIVE-LOCK 
      WHERE JBoxGenCode.cCodeType = "VarebokSort"
        AND ROWID(JBoxGenCode) NE rDefault
      :
    JBoxGenCode.cMisc1 = "false".
  END.
