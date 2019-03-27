/* kordrehode_setLevertSpeditor.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSporingsNr AS CHARACTER NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

ASSIGN 
    cSporingsNr = ENTRY(1,icParam,'|')
    .

IF cSporingsNr <> '' THEN 
DO:
    FIND FIRST KOrdreHode NO-LOCK WHERE 
        KOrdreHode.SendingsNr = cSporingsNr AND 
        NUM-ENTRIES(KOrdreHode.EkstOrdreNr,' ') = 1 AND 
        KOrdreHode.LevStatus <= '45' NO-ERROR.
    IF AVAILABLE KOrdreHode THEN 
    DO:
        rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                                   INPUT IF KOrdreHode.LevStatus <= '45' THEN 47 ELSE INT(KOrdreHode.LevStatus)).  
        ASSIGN 
            obOk = TRUE 
            ocReturn = ''
            .
    END.
    ELSE DO:
        ASSIGN 
            obOk = FALSE 
            ocReturn = 'Finner ingen kundeordre med sporingsnummer (' + cSporingsNr + ').'
            .
    END.
END.

