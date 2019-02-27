DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO. 
DEF INPUT  PARAM iiOldEntityId AS INT  NO-UNDO.
DEF INPUT  PARAM cNewName      AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiNewEntityId AS INT NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.

DEF VAR ix          AS INT NO-UNDO.
DEF VAR bOK         AS LOG NO-UNDO.

{incl/validatesession.i}

DEF BUFFER b-Entity        FOR JBoxQEntity.
DEF BUFFER bb-Entity       FOR JBoxQEntity.
DEF BUFFER bbb-Entity      FOR JBoxQEntity.
DEF BUFFER b-Relation      FOR JBoxQRelation.
DEF BUFFER b-Criteria      FOR JBoxQCriteria.

RUN MakeCopy (iiOldEntityId,?,cNewName,"",OUTPUT oiNewEntityId).


PROCEDURE MakeCopy:
/*****************************************
******************************************/

DEF INPUT PARAM  iiOldEntityId AS INT NO-UNDO.
DEF INPUT PARAM  iiNewEntityId AS INT NO-UNDO.
DEF INPUT PARAM  cNewName      AS CHAR NO-UNDO.
DEF INPUT PARAM  cNewDesc      AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiNewEntityId AS INT NO-UNDO.

FIND JBoxQEntity WHERE JBoxQEntity.iEntityId = iiOldEntityId NO-LOCK NO-ERROR.
IF AVAIL JBoxQEntity THEN DO:
  IF cNewName NE "" THEN DO:
    CREATE b-Entity.
    BUFFER-COPY JBoxQEntity TO b-Entity
       ASSIGN b-Entity.iEntityId  = NEXT-VALUE(Next_iEntityId)
              b-Entity.cName      = cNewName
              b-Entity.cOwner     = cCurrUserId
              b-Entity.cCreatedBy = cCurrUserId
              b-Entity.dCreated   = TODAY
              iiNewEntityId       = b-Entity.iEntityId
              .
  END.
  FOR EACH JBoxQRelation OF JBoxQEntity
      NO-LOCK,
      FIRST bb-Entity 
            WHERE bb-Entity.iEntityId = JBoxQRelation.iEntityIn
            NO-LOCK:

    CREATE bbb-Entity.
    BUFFER-COPY bb-Entity TO bbb-Entity
       ASSIGN bbb-Entity.iEntityId = NEXT-VALUE(Next_iEntityId).
    
    CREATE b-Relation.
    BUFFER-COPY JBoxQRelation TO b-Relation
      ASSIGN b-Relation.iEntityId = iiNewEntityId
             b-Relation.iEntityIn = bbb-Entity.iEntityId.
    FOR EACH JBoxQCriteria OF bb-Entity
        NO-LOCK:
      CREATE b-Criteria.
      BUFFER-COPY JBoxQCriteria TO b-Criteria
        ASSIGN b-Criteria.iEntityId = bbb-Entity.iEntityId.
    END.
    RUN MakeCopy(bb-Entity.iEntityId,bbb-Entity.iEntityId,"","",OUTPUT oiNewEntityId).
  END.
  IF cNewName NE "" THEN
    oiNewEntityId      = iiNewEntityId.
END.

END PROCEDURE.
