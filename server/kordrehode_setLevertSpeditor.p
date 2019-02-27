/* kordrehode_setLevertSpeditor.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendingsNr AS CHARACTER NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

ASSIGN 
    cSendingsNr = ENTRY(1,icParam,'|')
    .

IF cSendingsNr <> '' THEN 
DO:
    FIND LAST KOrdreHode NO-LOCK WHERE 
        KOrdreHode.SendingsNr = cSendingsNr AND 
        KOrdreHode.LevStatus < '45' NO-ERROR.
    IF AVAILABLE KOrdreHode THEN 
    DO:
        rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                                   INPUT IF KOrdreHode.LevStatus < '45' THEN 45 ELSE INT(KOrdreHode.LevStatus)).  
        ASSIGN 
            obOk = TRUE 
            ocReturn = ''
            .
    END.
    ELSE DO:
        ASSIGN 
            obOk = FALSE 
            ocReturn = 'Finner ingen kundeordre med sendingsnr (' + cSendingsNr + ').'
            .
    END.
END.

