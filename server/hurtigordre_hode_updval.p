/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=update_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val.
   
   If there's no fieldmap (viewer) set the attribute on the browse object or the browse overlay object
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEF VAR cValue     AS CHAR NO-UNDO.
DEF VAR cTilfKunde AS CHAR NO-UNDO. 

FIND KOrdreHode WHERE ROWID(KOrdreHode) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL KOrdreHode THEN DO:
  cValue = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"cBefrakter"). 
  IF cValue NE "" THEN DO:
    FIND FIRST ArtBas NO-LOCK
         WHERE ArtBas.ArtikkelNr = DECIMAL(cValue)
         NO-ERROR.
    IF AVAIL ArtBas THEN DO:
      FIND FIRST KOrdreLinje EXCLUSIVE-LOCK
           WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id
             AND KOrdreLinje.KOrdreLinjeNr = 999
           NO-ERROR.
      IF NOT AVAIL KOrdreLinje THEN DO:
      CREATE KOrdreLinje.
        ASSIGN KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id
               KOrdreLinje.KOrdreLinjeNr = 999.
      END.
      ASSIGN KOrdreLinje.VareNr    = cValue
             KOrdreLinje.Varetekst = ArtBas.Beskr
             KOrdreLinje.Antall    = 1.
      ASSIGN        
             KOrdreLinje.Pris      = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"fFrakt"))
            .
    END.
  END.
  cValue = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Kundenr"). 
  IF cValue = "" THEN DO:
    {syspara.i 19 3 1 cTilfKunde }
    IF cTilfKunde = "" THEN DO:
      IF NOT CAN-FIND(FIRST SysHode WHERE SysHode.SysHId = 19) THEN DO:
        ocReturn = "Parameterklasse (SysHode) mangler for kundeordre (19). Ordre kan ikke opprettes".
        RETURN.
      END.
      FIND FIRST Kunde NO-LOCK
           WHERE Kunde.Navn BEGINS "Tilfeldig kunde"
           NO-ERROR.
      IF NOT AVAIL Kunde THEN DO:
        CREATE Kunde.
        ASSIGN Kunde.Navn = "Tilfeldig kunde"
               .
      END.
      FIND FIRST SysGruppe NO-LOCK
           WHERE SysGruppe.SysHId = 19
             AND SysGruppe.SysGr  = 3
           NO-ERROR.
      IF NOT AVAIL SysGruppe THEN DO:
        CREATE SysGruppe.
        ASSIGN SysGruppe.SysHId = 19
               SysGruppe.SysGr  = 3
               SysGruppe.Beskrivelse = "Tilfeldig kunde"
               SysGruppe.Hjelpetekst = "Opprettet av " + PROGRAM-NAME(1)
               .
      END.
      FIND FIRST SysPara EXCLUSIVE-LOCK
           WHERE SysPara.SysHId = 19
             AND SysPara.SysGr  = 3
             AND SysPara.ParaNr = 1
           NO-ERROR.
      IF NOT AVAIL SysPara THEN DO:
        CREATE SysPara.
        ASSIGN SysPara.SysHId = 19
               SysPara.SysGr  = SysGruppe.SysGr
               SysPara.ParaNr = 1
               .
      END.
      ASSIGN SysPara.Parameter1 = STRING(Kunde.KundeNr)
             KOrdreHode.KundeNr = Kunde.KundeNr
             .
    END.
    ELSE KOrdreHode.KundeNr = INTEGER(cTilfKunde).
  END.
END.


