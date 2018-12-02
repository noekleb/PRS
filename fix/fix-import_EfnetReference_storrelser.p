CURRENT-WINDOW:WIDTH = 300.

DEF VAR cLinje AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cStorl AS CHAR NO-UNDO.
DEF VAR iStrKode AS INT NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.
DEF VAR iLinjeNr AS INT NO-UNDO.

DEF TEMP-TABLE bufLinje 
    FIELD LinjeNr AS INT
    FIELD Tekst   AS CHAR.

DEF BUFFER b1Linje FOR bufLinje.
DEF BUFFER b2Linje FOR bufLinje.

ASSIGN
    cFilNavn = 'kom\in\EfnetReference_sortiment900.csv'
    iLinjeNr = 0.

DEF STREAM Inn.

iLinjeNr = 0.
INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.

  cLinje   = TRIM(cLinje).
  iLinjeNr = iLinjeNr + 1.

  /* De to første linjene håndteres separat. */
  IF iLinjeNr <= 2 THEN 
  DO:
      /* Oppretter StrType */
      CREATE bufLinje.
      ASSIGN
          bufLinje.LinjeNr = iLinjeNr
          bufLinje.Tekst   = cLinje
          .
      NEXT.
  END.

  DO iLoop = 2 TO NUM-ENTRIES(cLinje,';'):
    cStorl = TRIM(ENTRY(iLoop,cLinje,';')).
    IF cStorl = '' THEN
        NEXT.
    RUN bibl_fixstorl.p (cStorl,?,'',OUTPUT ocReturn,OUTPUT obOk).
    cStorl = ocReturn.
    /* Legger opp størrelsen hvis den mangler. */
    OPPRETT_STR:
    DO TRANSACTION:
        FIND StrKonv EXCLUSIVE-LOCK WHERE
            StrKonv.Storl = cStorl NO-ERROR.
        IF AVAILABLE StrKonv THEN
        DO:
            ASSIGN
                StrKonv.Merknad = 'Efnet'.
        END.
        ELSE DO:
            FIND LAST StrKonv USE-INDEX StrKode NO-LOCK.
            iStrKode = IF AVAILABLE StrKonv 
                         THEN StrKonv.StrKode + 1
                         ELSE 1.
            CREATE StrKonv.
            ASSIGN
                StrKonv.StrKode = iStrKode
                StrKonv.Storl   = cStorl
                StrKonv.Merknad = 'Efnet'

                .
        END.
        IF AVAILABLE STrKonv THEN RELEASE StrKonv.
    END. /* OPPRETT_STR TRANSACTION*/  
  END.

  /* Oppretter StrType */
  CREATE bufLinje.
  ASSIGN
      bufLinje.LinjeNr = iLinjeNr
      bufLinje.Tekst   = cLinje
      .
END.

FIND b1Linje WHERE b1Linje.LinjeNr = 1 NO-LOCK.
FIND b2Linje WHERE b2Linje.LinjeNr = 2 NO-LOCK.

/* Tar bort gamle størrelser på størrelsestypen */
DO iLoop = 2 TO NUM-ENTRIES(b2Linje.Tekst,';') TRANSACTION:
    FIND StrType EXCLUSIVE-LOCK WHERE
        StrType.StrTypeId = INT(ENTRY(iLoop,b2Linje.Tekst,';')) NO-ERROR.
    IF AVAILABLE StrType THEN
        FOR EACH StrTSTr OF StrType EXCLUSIVE-LOCK:
          DELETE StrTStr.
        END.
END. /* TRANSACTION */


/* Leser størrelsestypene */
STRTYPELOOP:
DO iLoop = 2 TO NUM-ENTRIES(b2Linje.Tekst,';') TRANSACTION:
    FIND StrType EXCLUSIVE-LOCK WHERE
        StrType.StrTypeId = INT(ENTRY(iLoop,b2Linje.Tekst,';')) NO-ERROR.
    IF NOT AVAILABLE StrType THEN
    DO:
        CREATE StrType.
        ASSIGN
            StrType.StrTypeId   = INT(ENTRY(iLoop,b2Linje.Tekst,';'))
            StrType.Beskrivelse = ENTRY(iLoop,b1Linje.Tekst,';')
            .
    END.
    ELSE
        ASSIGN
            StrType.Beskrivelse = ENTRY(iLoop,b1Linje.Tekst,';').
    /* Tar bort alle gamle størrelser på størrelsestypen */
    FOR EACH StrTStr OF StrType:
        DELETE StrTStr.
    END.

    /* Leser alle størrelsene for størrelsestypen. */
    STRLOOP:
    FOR EACH bufLinje WHERE
        bufLinje.LinjeNr > 2
        BREAK BY bufLinje.LinjeNr:
        
        /* Legger opp størrelsen på størrelsestypen. */
        cStorl = TRIM(ENTRY(iLoop,bufLinje.Tekst,';')).
        IF cStorl <> '' THEN
        DO:
            RUN bibl_fixstorl.p (cStorl,?,'',OUTPUT ocReturn,OUTPUT obOk).
            cStorl = ocReturn.
            FIND StrTStr EXCLUSIVE-LOCK WHERE
                StrTStr.StrType = StrType.StrTypeId AND
                StrTSTr.SeqNr   = INT(ENTRY(1,bufLinje.Tekst,';')) NO-ERROR.
            IF NOT AVAILABLE StrTStr THEN
            DO:
                CREATE StrTStr.
                ASSIGN
                    StrTStr.StrTypeId = StrType.StrTypeId
                    StrTStr.SeqNr     = INT(ENTRY(1,bufLinje.Tekst,';')).
            END.
            ASSIGN
              StrTStr.SoStorl   = cStorl.
        END.
    END. /* STRLOOP */

END. /* STRTYPELOOP*/

INPUT STREAM Inn CLOSE.

DO iLoop = 2 TO NUM-ENTRIES(b2Linje.Tekst,';'):
    FIND StrType NO-LOCK WHERE
        StrType.StrTypeId = INT(ENTRY(iLoop,b2Linje.Tekst,';')) NO-ERROR.
    IF AVAILABLE StrType THEN
        RUN settStrTypeFelt.p (StrType.StrTypeID).
END.

