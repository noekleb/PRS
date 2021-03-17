PROCEDURE Kunde_Adresse1:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
      ocValue = Kunde.Adresse1.
END PROCEDURE.

PROCEDURE Kunde_Adresse2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
      ocValue = Kunde.Adresse2.
END PROCEDURE.

PROCEDURE Kunde_PostSted:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
    DO:
      FIND Post NO-LOCK WHERE 
        Post.PostNr = Kunde.PostNr NO-ERROR.
      IF AVAILABLE Post THEN 
        ocValue = Post.Beskrivelse.
    END.
END PROCEDURE.

PROCEDURE Kunde_FaktPostSted:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
    DO:
      FIND Post NO-LOCK WHERE 
        Post.PostNr = Kunde.FaktPostNr NO-ERROR.
      IF AVAILABLE Post THEN 
        ocValue = Post.Beskrivelse.
    END.
END PROCEDURE.

PROCEDURE Kunde_HarOrdre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE lFlagg AS LOG NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
    DO:
      FIND FIRST KOrdreHode NO-LOCK WHERE 
        KOrdreHode.KundeNr = Kunde.KundeNr AND 
        KOrdreHode.LevStatus < '50' NO-ERROR.
      IF NOT AVAILABLE KOrdreHode THEN 
        FIND FIRST KOrdreHode NO-LOCK WHERE 
          KOrdreHode.KundeNr = Kunde.KundeNr AND 
          KOrdreHode.LevStatus = '55' NO-ERROR.
      IF AVAILABLE KOrdreHode THEN 
        ocValue = '3'.
      ELSE DO:
        FIND FIRST KOrdreHode NO-LOCK WHERE 
          KOrdreHode.KundeNr = Kunde.KundeNr NO-ERROR.
        IF AVAILABLE KOrdreHode THEN 
          ocValue = '2'.
      END.
      IF ocValue = '' THEN ocValue = '1'.
    END.
END PROCEDURE.

PROCEDURE Kunde_FaktAdresse1:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
      ocValue = Kunde.FaktAdresse1.
END PROCEDURE.

PROCEDURE Kunde_FaktAdresse2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irKunde  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND Kunde NO-LOCK
        WHERE ROWID(Kunde) = irKunde
        NO-ERROR.
    IF AVAILABLE Kunde THEN 
      ocValue = Kunde.FaktAdresse2.
END PROCEDURE.







