/* Finner størrelsestype eller genererer ny størrelsestype. */
DEFINE INPUT PARAMETER lArtikkelNr   LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER bOpprettNy    AS LOG NO-UNDO.
DEF OUTPUT PARAMETER iStrTypeId      AS INT NO-UNDO.

DEF VAR bFlagg AS LOG NO-UNDO.
DEF VAR cListe AS CHAR NO-UNDO.
DEFINE VARIABLE cSjekkListe AS CHARACTER NO-UNDO.
DEF VAR iStartStrTypeId AS INT INITIAL 900 NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.

/* Lager en liste over alle strekkoder på artikkelen. Det dise som bestemmer hvilken */
/* størrelsestype artikkelen skal ha.                                                */
FOR EACH Strekkode NO-LOCK WHERE
    Strekkode.ArtikkelNr = lArtikkelNr:
    FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
    IF StrKonv.StrKode = ? OR 
       StrKonv.Storl = ? THEN 
       NEXT.
    IF AVAILABLE StrKonv AND NOT CAN-DO(cListe,TRIM(StrKonv.Storl)) THEN
        cListe = cListe +
                (IF cListe = '' THEN '' ELSE ',') + 
                TRIM(StrKonv.Storl).
END.

/* Kommer vi fra VPI registeret, skal vi også ta med det som ligger der. */
IF iEkstVPILevNr > 0 THEN 
DO:
  FOR EACH VPIArtBas NO-LOCK WHERE 
    VPIArtBas.ArtikkelNr   = lArtikkelNr AND 
    VPIArtBas.EkstVPILevNr = iEkstVPILevNr:
    FOR EACH VPIStrekkode NO-LOCK OF VPIArtBas: 
      FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
      IF AVAILABLE StrKonv THEN 
      DO: 
        IF (StrKonv.StrKode = ? OR StrKonv.Storl = ?) THEN 
           NEXT.
        IF NOT CAN-DO(cListe,TRIM(StrKonv.Storl)) THEN
          cListe = cListe +
                  (IF cListe = '' THEN '' ELSE ',') + 
                  TRIM(StrKonv.Storl).
      END.
    END.
  END.
END.

iStrTypeId = 0.
RUN sjekkStrTyper (900,999).
IF iStrTypeId = 0 THEN
  RUN sjekkStrTyper (2,899).

/* Da legger vi opp en ny størrelsestype. */
IF iStrTypeId = 0 AND bOpprettNy THEN 
  RUN OpprettNyStrType (cListe, OUTPUT iStrTypeId).

/* Hvis ingenting annet nytter, så... */
IF iStrTypeId = 0 THEN
    iStrtypeId = 2.
ELSE 
    RUN settStrTypeFelt.p (iStrTypeId).

RETURN.



/* **********************  Internal Procedures  *********************** */


PROCEDURE OpprettNyStrType:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcListe AS     CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER piStrTypeId AS INTEGER NO-UNDO.

  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE cStorl AS CHARACTER NO-UNDO.

  MORGENGRYET:
  DO ON ERROR UNDO, LEAVE TRANSACTION:
      CREATE StrType.
      ASSIGN
        StrType.Beskrivelse = 'Generert ' + STRING(StrType.StrTypeId)
        piStrTypeId         = StrType.StrTypeId
        .
        
      /* Nummerserie er full */
      IF StrType.StrTypeId = 0 THEN 
      DO:
        IF AVAILABLE StrType THEN 
          DELETE StrType.
        piStrTypeId = 2.
        LEAVE MORGENGRYET.
      END.
      
      DO piLoop = 1 TO NUM-ENTRIES(pcListe):
        cStorl = ENTRY(piLoop,pcListe).
        DYNAMIC-FUNCTION("runproc","bibl_fixstorl.p",cStorl,?).
        cStorl = CAPS(DYNAMIC-FUNCTION("getTransactionMessage")).

        CREATE StrTStr.
        ASSIGN
            StrTStr.STrTypeId = StrType.StrTypeId
            StrTStr.SeqNr     = piLoop
            StrTStr.SoStorl   = cStorl
            .
      END.
      IF AVAILABLE Strtype THEN RELEASE StrType.
      IF AVAILABLE StrTStr THEN RELEASE StrTStr.
      RUN settStrTypeFelt.p (piStrTypeId).
  END. /* MORGENGRYET TRANSACTION */
END PROCEDURE.

PROCEDURE sjekkStrTyper:
  DEF INPUT PARAMETER fraStrTypeId AS INT NO-UNDO.
  DEF INPUT PARAMETER tilStrTypeId AS INT NO-UNDO.

    /* Sjekker Efnet størrelsestyper */
    SJEKKING:
    FOR EACH StrType NO-LOCK WHERE
        StrType.StrTypeId >= fraStrTypeId AND
        StrType.StrTypeId <= tilStrTypeId:

      /* Bygger sjekkliste */
      cSjekkListe = ''.
      FOR EACH StrTStr OF StrType NO-LOCK:
          cSjekkListe = cSjekkListe +
                  (IF cSjekkListe = '' THEN '' ELSE ',') + 
                  TRIM(StrTStr.SoStorl).
      END.

      bFlagg = TRUE.
      DO iLoop = 1 TO NUM-ENTRIES(cListe):
        IF NOT CAN-DO(cSjekkListe,ENTRY(iLoop,cListe)) THEN
        DO:
            bflagg = FALSE.
        END.
      END.
      IF bFlagg = FALSE THEN
          NEXT.
      ELSE DO:
        iStrTypeId = StrType.StrTypeId.
        LEAVE SJEKKING.
      END.
    END. /* SJEKKING */
END PROCEDURE.

