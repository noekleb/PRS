DEF INPUT PARAM icSessionId AS CHAR NO-UNDO.
DEF INPUT PARAM cTabList    AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM      TABLE-HANDLE httSelected.
DEF INPUT-OUTPUT PARAM      TABLE-HANDLE httAvail.
DEF INPUT-OUTPUT PARAM      TABLE-HANDLE httCriteria.
DEF OUTPUT PARAM ocReturn   AS CHAR NO-UNDO.

DEF VAR hBuffer      AS HANDLE NO-UNDO.

{incl/validatesession.i}

{incl/ttjbqueryfieldselect.i}
{incl/movetostatic.i}

hBuffer = BUFFER ttSelected:HANDLE.
MoveToStatic(httSelected,hBuffer).
hBuffer = BUFFER ttCriteria:HANDLE.
MoveToStatic(httCriteria,hBuffer).
hBuffer = BUFFER ttAvail:HANDLE.
MoveToStatic(httAvail,hBuffer).

DEF BUFFER bJBoxQEntity   FOR JBoxQEntity.

/* Start org. SaveFields */

DEF VAR iReturn  AS INT NO-UNDO.
DEF VAR ix       AS INT NO-UNDO.
DEF VAR iSeq     AS INT NO-UNDO.
DEF VAR bCreated AS LOG NO-UNDO.

DO ON ERROR UNDO, LEAVE: 
  iSeq = 1.
  FOR EACH ttSelected:
    FOR FIRST JBoxQEntity
          WHERE JBoxQEntity.iEntityId = ttSelected.iEntityId
            AND ttSelected.iEntityId > 0
              EXCLUSIVE-LOCK:
    END.
    IF AVAIL JBoxQEntity THEN
      ASSIGN JBoxQEntity.cDesc  = ttSelected.cDesc
             JBoxQEntity.cName  = ttSelected.cLabel.
    ELSE DO:
      CREATE JBoxQEntity.
      ASSIGN JBoxQEntity.iEntityId = NEXT-VALUE(Next_iEntityId)
             JBoxQEntity.cType     = "FIELD"
             JBoxQEntity.cDBtable  = ttSelected.cTabName
             JBoxQEntity.cDBField  = ttSelected.cFieldName
             JBoxQEntity.cName     = ttSelected.cLabel
             JBoxQEntity.cDataType = ttSelected.cDataType
             JBoxQEntity.cDesc     = ttSelected.cDesc
             .
    END.
    iReturn = 0.
    Loop1:
    DO ix = 1 TO NUM-ENTRIES(cTabList, ";") - 1:
      IF ENTRY (2, ENTRY(ix,cTabList,";"), "|") = ttSelected.cTabName THEN DO:
        iReturn = INT(ENTRY(1,ENTRY(ix,cTabList,";"), "|")).
        LEAVE Loop1. 
      END.
    END.
    IF iReturn = 0 THEN DO:
      ocReturn = "Finner ikke tabell " + ttSelected.cTabName .
      RETURN.
    END.
    ELSE DO:
      FIND FIRST JBoxQRelation
            WHERE JBoxQRelation.iEntityId = iReturn
              AND JBoxQRelation.iEntityIn = JBoxQEntity.iEntityId
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL JBoxQRelation THEN DO:
        IF LOCKED JBoxQRelation THEN DO:
          ocReturn = "Record is locked by another user".
          RETURN.
        END.
        ELSE DO:
          CREATE JBoxQRelation.
          ASSIGN JBoxQRelation.iEntityId = iReturn
                 JBoxQRelation.iEntityIn = JBoxQEntity.iEntityId
                 JBoxQRelation.cRelation = STRING(iSeq)
                 .
        END.
      END.
      ELSE DO:
        JBoxQRelation.cRelation = STRING(iSeq).
      END.
    END.
    iSeq = iSeq + 1.
  END. 
  
  /* Delete the deselected fields and related criteria */
  FOR EACH ttAvail:
    IF ttAvail.iEntityId > 0 THEN DO:
      FOR FIRST JBoxQEntity 
            WHERE JBoxQEntity.iEntityId = ttAvail.iEntityId
              EXCLUSIVE-LOCK:
      END.
      IF NOT AVAIL JBoxQEntity THEN
        RETURN ERROR.         /* Fang opp! */
      ELSE DO:
        FIND FIRST JBoxQRelation 
              WHERE JBoxQRelation.iEntityIn = ttAvail.iEntityId
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF LOCKED JBoxQRelation THEN DO:
          ocReturn = "JBoxQRelation is locked by another user".
          RETURN.
        END.
        ELSE IF AVAIL JBoxQRelation THEN DO:
          DELETE JBoxQRelation.
          FOR EACH JBoxQCriteria
              WHERE JBoxQCriteria.iEntityId = JBoxQEntity.iEntityId
                EXCLUSIVE-LOCK:
            DELETE JBoxQCriteria.
          END.
          DELETE JBoxQEntity.
        END.
      END.
    END.
  END.
  
  /* Delete the original JBoxQCriteria */
  DO ix = 1 TO NUM-ENTRIES(cTabList, ";"):
    FOR EACH JBoxQRelation
          WHERE JBoxQRelation.iEntityId = INT(ENTRY(1,ENTRY(ix,cTabList,";"),"|"))
            EXCLUSIVE-LOCK,
          FIRST JBoxQEntity 
            WHERE JBoxQEntity.iEntityId = JBoxQRelation.iEntityIn
              AND JBoxQEntity.cType = "FIELD"
                EXCLUSIVE-LOCK:
      FOR EACH JBoxQCriteria
            WHERE JBoxQCriteria.iEntityId = JBoxQEntity.iEntityId
              EXCLUSIVE-LOCK:
        DELETE JBoxQCriteria.
      END.
      IF JBoxQRelation.cRelation EQ "" THEN DO:
        DELETE JBoxQRelation.
        DELETE JBoxQEntity.
      END.
    END.
    FIND FIRST JBoxQRelation
      WHERE JBoxQRelation.cRelation = "" NO-ERROR.
  END.
  
  /*Save JBoxQCriteria by table*/
  DO ix = 1 TO NUM-ENTRIES(cTabList, ";") - 1:
    FOR FIRST JBoxQEntity
          WHERE JBoxQEntity.iEntityId = INT(ENTRY(1, ENTRY(ix,cTabList,";"),"|"))
              NO-LOCK:
    END.
    iSeq = 1.
    FOR EACH ttCriteria
          WHERE ttCriteria.cTable = JBoxQEntity.cDBtable:
      bCreated = FALSE.
      FOR EACH bJBoxQEntity
            WHERE bJBoxQEntity.cDBtable = ttCriteria.cTable
              AND bJBoxQEntity.cDBfield = ttCriteria.cFieldName
                NO-LOCK:
        FOR FIRST JBoxQRelation
              WHERE JBoxQRelation.iEntityId = JBoxQEntity.iEntityId
                AND JBoxQRelation.iEntityIn = bJBoxQEntity.iEntityId
                  NO-LOCK:
        END.
        IF AVAIL JBoxQRelation THEN DO:
          CREATE JBoxQCriteria.
          bCreated = TRUE.
          LEAVE.                 
        END.
      END.
      IF NOT bCreated THEN DO:
        FIND FIRST ttAvail
              WHERE ttAvail.cTabName   = ttCriteria.cTable
                AND ttAvail.cFieldName = ttCriteria.cFieldName NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          RETURN ERROR.
        CREATE bJBoxQEntity.
        ASSIGN bJBoxQEntity.iEntityId = NEXT-VALUE(Next_iEntityId)
               bJBoxQEntity.cDBtable  = ttCriteria.cTable
               bJBoxQEntity.cDBfield  = ttCriteria.cFieldName
               bJBoxQEntity.cDataType = ttCriteria.cDataType
               bJBoxQEntity.cName     = ttAvail.cLabel
               bJBoxQEntity.cDesc     = ttAvail.cDesc
               bJBoxQEntity.cType     = "FIELD"
               .
        CREATE JBoxQRelation.
        ASSIGN JBoxQRelation.iEntityId = JBoxQEntity.iEntityId
               JBoxQRelation.iEntityIn = bJBoxQEntity.iEntityId
               .
        CREATE JBoxQCriteria.
      END.
      ASSIGN JBoxQCriteria.iEntityId = JBoxQRelation.iEntityIn 
             JBoxQCriteria.iSequence = iSeq
             JBoxQCriteria.cAndOr    = ttCriteria.cAndOr
             JBoxQCriteria.cLeftPar  = ttCriteria.cLeftPar
             JBoxQCriteria.cOper     = ttCriteria.cOp
             JBoxQCriteria.cValue    = ttCriteria.cValue
             JBoxQCriteria.cRightPar = ttCriteria.cRightPar
             iSeq = iSeq + 1.
             .
      FIND FIRST JBoxQRelation
              WHERE JBoxQRelation.iEntityIn = JBoxQCriteria.iEntityId
                  NO-LOCK NO-ERROR.
    END. /*FOR EACH ttCriteria*/
  END.
END.


