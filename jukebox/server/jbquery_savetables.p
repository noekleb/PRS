DEF INPUT PARAM icSessionId AS CHAR NO-UNDO.
DEF INPUT PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT PARAM      	    TABLE-HANDLE httSelected.
DEF OUTPUT PARAM ocReturn   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocStatus   AS CHAR NO-UNDO.

DEF VAR iiEntityId   AS INT NO-UNDO.
DEF VAR cNewName     AS CHAR NO-UNDO.
DEF VAR ioReturn     AS INT NO-UNDO.
DEF VAR bOK          AS LOG NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hQuery2      AS HANDLE NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR bhBuffer     AS HANDLE NO-UNDO.
DEF VAR hBuffer2     AS HANDLE NO-UNDO.

{incl/validatesession.i}

{incl/ttjbquerytables.i}
{incl/movetostatic.i}
hBuffer = BUFFER ttSelected:HANDLE.
MoveToStatic(httSelected,hBuffer).


DEF BUFFER bttSelected    FOR ttSelected.
DEF BUFFER bJBoxQEntity   FOR JBoxQEntity.
DEF BUFFER bJBoxQRelation FOR JBoxQRelation.
DEF BUFFER bbJBoxQEntity   FOR JBoxQEntity.
DEF BUFFER bbJBoxQRelation FOR JBoxQRelation.

ASSIGN iiEntityId = INT(ENTRY(1,icParam,"|"))
       cNewName   = ENTRY(2,icParam,"|").

IF iiEntityId NE 0 AND iiEntityId NE ? THEN DO:
  RUN CheckModified (iiEntityId) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    ocStatus = ERROR-STATUS:GET-MESSAGE(1).
    RETURN.
  END.
  
  FOR EACH ttSelected:
    IF ttSelected.iEntityId GT 0 THEN DO:
      FIND FIRST JBoxQEntity
              WHERE JBoxQEntity.iEntityId = ttSelected.iEntityId
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL JBoxQEntity THEN 
        RETURN ERROR PROGRAM-NAME(1). 
    END.
    ELSE DO:
      CREATE JBoxQEntity.
      ASSIGN JBoxQEntity.iEntityId    = NEXT-VALUE(Next_iEntityId)
             JBoxQEntity.cDBtable     = TRIM(ttSelected.cName)
             JBoxQEntity.cType        = "TABLE"
             ttSelected.iEntityId = JBoxQEntity.iEntityId
             .
    END.
    JBoxQEntity.cDesc = ttSelected.cDesc.
  END.
  FOR EACH ttSelected,
        FIRST JBoxQEntity
           WHERE JBoxQEntity.iEntityId = ttSelected.iEntityId
              NO-LOCK:
    FOR FIRST JBoxQRelation
          WHERE JBoxQRelation.iEntityIn = JBoxQEntity.iEntityId
            EXCLUSIVE-LOCK:
    END.
    IF AVAIL JBoxQRelation THEN DO: 
      JBoxQRelation.cRelation = ttSelected.cRelation.   
    END.
    ELSE DO:
      CREATE JBoxQRelation.
      JBoxQRelation.iEntityIn = JBoxQEntity.iEntityId.
      JBoxQRelation.cRelation = ttSelected.cRelation.
      FIND FIRST bttSelected
              WHERE TRIM(bttSelected.cName) = ttSelected.cMotherName 
                  NO-ERROR.
      IF AVAIL bttSelected THEN
        JBoxQRelation.iEntityId = bttSelected.iEntityId.
      ELSE
        ASSIGN JBoxQRelation.iEntityId = iiEntityId
               ioReturn            = iiEntityId.  
    END.
  END.
END.
ELSE DO:
  CREATE JBoxQEntity.
  ASSIGN JBoxQEntity.iEntityId  = NEXT-VALUE(Next_iEntityId)
         JBoxQEntity.cName      = cNewName
         JBoxQEntity.cType      = "VIEW"
         JBoxQEntity.cOwner     = cCurrUserId
         JBoxQEntity.cCreatedBy = cCurrUserId
         JBoxQEntity.dCreated   = TODAY
         JBoxQEntity.iCompanyId = iCurrCompanyId
         ioReturn               = JBoxQEntity.iEntityId
         iiEntityId             = JBoxQEntity.iEntityId
         .
  FOR EACH ttSelected:
    CREATE JBoxQEntity.
    ASSIGN JBoxQEntity.iEntityId    = NEXT-VALUE(Next_iEntityId)
           JBoxQEntity.cName        = TRIM(ttSelected.cName)
           JBoxQEntity.cDBtable     = TRIM(ttSelected.cName)
           JBoxQEntity.cDesc        = ttSelected.cDesc
           JBoxQEntity.cType        = "TABLE"
           ttSelected.iEntityId = JBoxQEntity.iEntityId
           .
  END.
  FOR EACH ttSelected NO-LOCK:
    CREATE JBoxQRelation.
    ASSIGN JBoxQRelation.iEntityIn = ttSelected.iEntityId.
      FOR FIRST bttSelected
          WHERE TRIM(bttSelected.cName) = ttSelected.cMotherName
          AND ttSelected.cMotherName NE "":
      END.
      IF AVAIL bttSelected THEN
        ASSIGN JBoxQRelation.iEntityId = bttSelected.iEntityId
               JBoxQRelation.cRelation = ttSelected.cRelation
               .
      ELSE
        ASSIGN JBoxQRelation.iEntityId = ioReturn.
  END.
END.

IF ioReturn = 0 THEN ioReturn = iiEntityId.

DEF VAR ix AS INT NO-UNDO.
FIND FIRST JBoxQRelation 
  WHERE JBoxQRelation.cRelation = ""
    NO-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
    ocStatus = ocStatus + ERROR-STATUS:GET-MESSAGE(ix) + CHR(10).
  END.
ocReturn = STRING(ioReturn).


PROCEDURE CheckModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiEntId AS INT NO-UNDO.

DEF VAR riRel AS ROWID NO-UNDO.
DEF VAR riEnt AS ROWID NO-UNDO.

DO ON ERROR UNDO, LEAVE:

  FOR EACH JBoxQRelation
        WHERE JBoxQRelation.iEntityId = iiEntId
              NO-LOCK,
      FIRST JBoxQEntity
        WHERE JBoxQEntity.iEntityId = JBoxQRelation.iEntityIn
          AND JBoxQEntity.cType = "TABLE"
            NO-LOCK:

/*     IF AVAIL JBoxQRelation AND AVAIL JBoxQEntity THEN DO: */
      ASSIGN riRel = ROWID(JBoxQRelation)
             riEnt = ROWID(JBoxQEntity)
             .
      RUN CheckModified(JBoxQRelation.iEntityIn).

      FIND bJBoxQRelation
        WHERE ROWID(bJBoxQRelation) = riRel
                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL bJBoxQRelation THEN DO:
        IF LOCKED bJBoxQRelation THEN DO: 
          ocStatus = "Record is locked by another user".
          RETURN.
        END. 
      END.
      ELSE DO:
        FOR FIRST ttSelected 
              WHERE ttSelected.iEntityId = bJBoxQRelation.iEntityIn:
        END.
        IF NOT AVAIL ttSelected THEN DO: /*Slette tilh. rel., felter og beskrank. */
          FOR EACH JBoxQCriteria
                      WHERE JBoxQCriteria.iEntityId = bJBoxQRelation.iEntityIn
                          EXCLUSIVE-LOCK:
            DELETE JBoxQCriteria.
          END.
          DELETE bJBoxQRelation.
          FIND FIRST bJBoxQEntity 
              WHERE ROWID(bJBoxQEntity) = riEnt
                  EXCLUSIVE-LOCK NO-WAIT.
          IF AVAIL bJBoxQEntity THEN
            RUN DeleteEntity (bJBoxQEntity.iEntityId, 0). 
        END.
      END.
      FIND bJBoxQRelation
            WHERE bJBoxQRelation.iEntityId = iiEntityId
                NO-LOCK NO-ERROR.
/*     END. */
  END.
END.

END PROCEDURE.


PROCEDURE DeleteEntity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiEntId AS INT NO-UNDO.
DEF INPUT PARAM iiLevel  AS INT NO-UNDO.

DEF VAR riRel AS ROWID NO-UNDO.
DEF VAR riEnt AS ROWID NO-UNDO.

DO ON ERROR UNDO, LEAVE:
  FOR EACH JBoxQRelation
        WHERE JBoxQRelation.iEntityId = iiEntId
            NO-LOCK,
      FIRST JBoxQEntity
        WHERE JBoxQEntity.iEntityId = JBoxQRelation.iEntityIn
             NO-LOCK:
    ASSIGN riRel = ROWID(JBoxQRelation)
           riEnt = ROWID(JBoxQEntity).

    RUN DeleteEntity (JBoxQRelation.iEntityIn, iiLevel + 1).

    FOR FIRST bJBoxQRelation
          WHERE ROWID(bJBoxQRelation)= riRel
                EXCLUSIVE-LOCK:
      DELETE bJBoxQRelation.
    END.
    FOR FIRST bJBoxQEntity
          WHERE ROWID(bJBoxQEntity)= riEnt
                EXCLUSIVE-LOCK:
      DELETE bJBoxQEntity.
    END.
  END.
  
  FOR FIRST JBoxQEntity
        WHERE JBoxQEntity.iEntityId = iiEntId
              EXCLUSIVE-LOCK:
    FOR EACH JBoxQCriteria
            OF JBoxQEntity
            EXCLUSIVE-LOCK:
      DELETE JBoxQCriteria.
    END.
    FOR EACH JBoxQLog
            OF JBoxQEntity
            EXCLUSIVE-LOCK:
      DELETE JBoxQLog.
    END.
  END.
  IF iiLevel = 0 THEN
    DELETE JBoxQEntity.
END.

END PROCEDURE.

