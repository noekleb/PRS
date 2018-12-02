/* Eksport av budsjett til Excel 
   Parameter:  <PkSdlId>;<brukerid>
   Opprettet: 12.10.15 av TN              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR iSBudId  AS INT  NO-UNDO.
DEF VAR piLoop   AS INT  NO-UNDO.

DEF BUFFER tgtLokSBudHode FOR SBudHode.
DEF BUFFER bufLokSBudHode FOR SBudHode.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE true").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  FIND SBudHode NO-LOCK WHERE 
         SBudHode.SBudId = INT(ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE)
         NO-ERROR.
  IF AVAIL SBudHode THEN 
      RUN slettSBudHode.
  hQuery:GET-NEXT().
END.
DELETE OBJECT hQuery NO-ERROR.

PROCEDURE slettSBudHode:
    IF NOT AVAILABLE SBudHode THEN 
      DO:
        obOk = FALSE.
        RETURN.
      END.

    /* Sletter tellelinjene */
    FOR EACH SBudManed WHERE 
        SBudManed.SBudId = SBudHode.SBudId EXCLUSIVE-LOCK:
        FOR EACH SBudMalDag WHERE 
            SBudDag.SBudId = SBudManed.SBudId AND 
            SBudDag.AarMnd = SBudManed.AarMnd:
            DELETE SBudDag.    
        END.    
        DELETE SBudManed.
    END.

    /* Sletter SBudHode. */
    DO TRANSACTION:
      /* Sletter SBudHode for lokasjonslisten. */
      FIND CURRENT SBudHode EXCLUSIVE-LOCK.
      DELETE SBudHode.

      obOk = TRUE.
    END.
END PROCEDURE.
