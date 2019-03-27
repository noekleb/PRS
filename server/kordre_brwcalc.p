DEFINE VARIABLE iCl            AS INT NO-UNDO.
DEFINE VARIABLE iUtvidetStatus AS INTEGER NO-UNDO.

{syspara.i 19 9 4 iUtvidetStatus INT}
{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.

PROCEDURE kordre_FakturaNr:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND KOrdreHode NO-LOCK
        WHERE ROWID(KOrdreHode) = irKOrdreHode
        NO-ERROR.
    IF AVAILABLE KORdreHode THEN 
    DO:
        FIND FakturaHode NO-LOCK WHERE 
            FakturaHode.Faktura_Id = KOrdreHode.Faktura_Id NO-ERROR.
        IF AVAILABLE FakturaHode THEN 
            ocValue = STRING(FakturaHode.FakturaNr).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE kordre_LevFTekst:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND KOrdreHode NO-LOCK
        WHERE ROWID(KOrdreHode) = irKOrdreHode
        NO-ERROR.
    IF AVAILABLE KORdreHode THEN 
    DO:
        FIND Leveringsform NO-LOCK WHERE 
            Leveringsform.LevFNr = KORdreHode.LevFNr NO-ERROR.
        IF AVAILABLE LeveringsForm THEN 
            ocValue = LeveringsForm.LevFormBeskrivelse.        
        ELSE ocValue = 'Ukjent'.
    END.
    ELSE ocValue = 'Ukjent'.

END PROCEDURE.

PROCEDURE kordre_ShipmentSendt:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND KOrdreHode NO-LOCK
        WHERE ROWID(KOrdreHode) = irKOrdreHode
        NO-ERROR.
    IF AVAILABLE KORdreHode THEN 
    DO:
        ocValue = ENTRY(1,STRING(KOrdreHode.ShipMentSendt),',').
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE kordre_Dummy:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    ocValue = ENTRY(1,STRING(KOrdreHode.ShipMentSendt),',').
 
END PROCEDURE.

PROCEDURE kordre_Butikk:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND KOrdreHode NO-LOCK
        WHERE ROWID(KOrdreHode) = irKOrdreHode
        NO-ERROR.
    IF AVAILABLE KORdreHode THEN 
    DO:
        FIND Butiker NO-LOCK WHERE 
            Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.
        IF AVAILABLE Butiker THEN 
            ocValue = STRING(Butiker.butik) + ' ' + Butiker.ButNamn.
        ELSE 
            ocValue = 'Ukjent'.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE kordre_LevStatus:
  DEF INPUT  PARAM irKOrdreHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND KOrdreHode NO-LOCK
        WHERE ROWID(KOrdreHode) = irKOrdreHode
        NO-ERROR.
    IF AVAILABLE KORdreHode THEN 
    DO:
        /* Ny utvidet status */
        IF iUtvidetStatus = 1 THEN 
        DO:
            FIND SysPara NO-LOCK WHERE 
                SysPara.SysHId = 19 AND 
                SysPara.SysGr  = 15 AND 
                SysPara.ParaNr = INT(KOrdreHode.LevStatus) NO-ERROR.
            IF AVAILABLE SysPara THEN 
                ocValue = STRING(SysPara.ParaNr) + ' ' + SysPara.Parameter1.
            ELSE 
                ocValue = 'Ukjent'.
        END.
        
        /* Gammel status. */
        ELSE DO:
            FIND SysPara NO-LOCK WHERE 
                SysPara.SysHId = 19 AND 
                SysPara.SysGr  = 1 AND 
                SysPara.ParaNr  = INT(KOrdreHode.LevStatus) NO-ERROR.
            IF AVAILABLE SysPara THEN 
                ocValue = STRING(SysPara.ParaNr) + ' ' + SysPara.Parameter1.
            ELSE 
                ocValue = 'Ukjent'.            
        END.
    END.
    ELSE ocValue = ''.

END PROCEDURE.



