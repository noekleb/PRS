CURRENT-WINDOW:WIDTH = 350.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR wRecid AS RECID NO-UNDO.

DEF BUFFER bufKunde FOR Skotex.Kunde.
DEF VAR iButNr AS INT NO-UNDO.

FOR EACH InfoPOS.Kund NO-LOCK
  BREAK BY InfoPOS.Kund.KundeNr:
  
  IF FIRST-OF(InfoPOS.Kund.KundeNr) THEN 
  DO:
    iAnt = 1.
  END.
  ELSE iAnt = iant + 1.
  
  ibutNr = Kund.ButNr.
  IF iButNr = 0 THEN 
  DO:
    FIND FIRST TeamButikk NO-LOCK WHERE
      Teambutikk.TeamNr = Kund.TeamNr NO-ERROR.
    IF AVAILABLE TeamButikk THEN
      iButNr = TeamButikk.ButNr.
  END.
  
  FIND FIRST kundekort NO-LOCK WHERE 
      KundeKort.KortNr = STRING(InfoPOS.Kund.KundeNr) NO-ERROR.
  
  /* Skal bare ha kunder fra denne butikken. */
  IF iButNr <> 17 THEN 
    NEXT.
    
  /*
  DISPLAY 
      InfoPOS.kund.KundeNr
      InfoPOS.Kund.ProfNr
      InfoPOS.Kund.Butnr
      iButNr
      InfoPOS.Kund.regbutnr
      InfoPOS.Kund.TeamNr
      InfoPOS.Kund.Kundegr
      'FUNNET' WHEN AVAILABLE KundeKort
      KundeKort.KortNr WHEN AVAILABLE KundeKort
      iAnt
      WITH WIDTH 350.
  */

  /* TN Her opprettes kunder som ikke finnes fra før. */
  IF NOT CAN-FIND(FIRST KundeKort WHERE
                  KundeKort.KortNr = STRING(InfoPOS.Kund.KundeNr)) THEN 
  DO:
    FIND FIRST KundeGruppe NO-LOCK WHERE
       KundeGruppe.GruppeId > 0 NO-ERROR.
    CREATE SkoTex.Kunde.
    FIND FIRST SkoTex.KundeKort OF SkoTex.Kunde EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE SkoTex.KundeKort THEN DELETE  SkoTex.KundeKort.
  END.
  ELSE DO:
     FIND FIRST SkoTex.KundeKort WHERE
                SkoTex.KundeKort.KortNr = STRING(InfoPOS.Kund.KundeNr).
     FIND SkoTex.Kunde OF SkoTex.KundeKort.
  END.

  ASSIGN
     SkoTex.Kunde.Navn            = TRIM(TRIM(InfoPOS.Kund.navn,','))
     SkoTex.Kunde.BetType         = 2
     SkoTex.Kunde.ButikkNr        = iButNr
     SkoTex.Kunde.GruppeId        = KundeGr
     SkoTex.Kunde.SamleFaktura    = FALSE  
     SkoTex.Kunde.Fakturagebyr    = FALSE 
     SkoTex.Kunde.Purregebyr      = TRUE 
     SkoTex.Kunde.EksterntKundeNr = STRING(InfoPOS.Kund.KundeNr)
     SkoTex.Kunde.ePostAdresse    = InfoPOS.Kund.emailadr
     SkoTex.Kunde.Adresse1        = InfoPOS.Kund.adresse                 
     SkoTex.Kunde.PostNr          = STRING(InfoPOS.Kund.postnr)  
     SkoTex.Kunde.Telefon         = InfoPOS.Kund.Telefon1     
     SkoTex.Kunde.Telefaks        = InfoPOS.Kund.faxnr      
     SkoTex.Kunde.MobilTlf        = InfoPOS.Kund.Telefon2  
     SkoTex.Kunde.Aktiv           = NOT InfoPOS.Kund.Sperret
     SkoTex.Kunde.MaksKredit      = InfoPOS.Kund.limit
     SkoTex.Kunde.LevAdresse1     = InfoPOS.Kund.levadresse    
     SkoTex.Kunde.LevPostNr       = STRING(InfoPOS.Kund.LevPostNr)
     SkoTex.Kunde.FaktAdresse1    = InfoPOS.Kund.faktadresse   
     SkoTex.Kunde.FaktPostNr      = STRING(InfoPOS.Kund.faktpostnr)    
     .
    IF NOT CAN-FIND(FIRST SkoTex.KundeKort OF SkoTex.Kunde) THEN 
      RUN createKundekort.p (INPUT SkoTex.Kunde.Kundenr, INPUT STRING(InfoPOS.Kund.KundeNr), INPUT 720, OUTPUT wRecid).
END.
