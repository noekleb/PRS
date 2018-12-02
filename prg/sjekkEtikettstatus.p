/*
      RUN sjekkEtikettstatus.p (LokPrisko.ArtikkelNr, LokPrisko.ProfilNr, LokPrisKo.Opphav, LokPrisKo.VareKost, LokPrisKo.Pris, INPUT-OUTPUT LokPrisko.EtikettStatus).  
*/
DEFINE INPUT PARAMETER lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE INPUT PARAMETER iClProfilNr LIKE ArtPris.ProfilNr  NO-UNDO.
DEFINE INPUT PARAMETER bSettEtikett AS LOG                NO-UNDO.
DEFINE INPUT PARAMETER bEtiTvang    AS LOG                NO-UNDO.
DEFINE INPUT PARAMETER iProfilNr   LIKE ArtPris.ProfilNr  NO-UNDO.
DEFINE INPUT PARAMETER iType       AS  INTEGER            NO-UNDO.
DEFINE INPUT PARAMETER cOpphav     LIKE Prisko.Opphav     NO-UNDO. 
DEFINE INPUT PARAMETER lVareKost   LIKE PrisKo.VareKost   NO-UNDO.
DEFINE INPUT PARAMETER lPris       LIKE PrisKo.Pris       NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iEtikettStatus  LIKE PrisKo.EtikettStatus NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iKlargjorStatus LIKE PrisKo.EtikettStatus NO-UNDO.

DEFINE VARIABLE piEtikettStatus  AS INTEGER NO-UNDO.
DEFINE VARIABLE piKlargjorStatus AS INTEGER NO-UNDO.
DEFINE VARIABLE bOk              AS LOG     NO-UNDO.
DEFINE VARIABLE bAlltidEtikett   AS LOG NO-UNDO.
DEFINE VARIABLE cTekst           AS CHARACTER NO-UNDO.

/* Sjekker om etikettflagg alltid skal settes. */
{syspara.i 2 4 39 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bAlltidEtikett = TRUE.
ELSE
  bAlltidEtikett = FALSE. 

/* BEHANDLER tilbud, leverandørtilbud og tilbudsendring. */
IF iType <> 1 THEN 
TILBUD:
DO:
ASSIGN
  piKlargjorStatus = 1  
  piEtikettStatus  = 1   
  .
END. /* TILBUD */

ELSE 
NORMALPRIS:
DO:
  /* Behandler NORMALPRIS for HK og <Blank> */
  IF NOT CAN-DO('IPS,LOK',cOpphav) THEN
  DO:
    /* Setting av etikettflagg på¨hk ved etikettvang. */
    IF iClProfilNr = iProfilNr THEN 
    HK:
    DO:     
      IF bEtiTvang THEN
      DO:
        /* Hvis det er overstyrt at hk skal ha etikettflagg satt, setter vi det her. */
        IF bSettEtikett THEN piEtikettStatus = 1.
        /* Ellers skal også etikett skrives ut på hk - hvis det ligger salg på artikkelen eller prisen er endret. */
        ELSE DO: 
          RUN sjekksalg (INPUT-OUTPUT bOk).
          IF bOk THEN piEtikettStatus = 0.
          ELSE piEtikettStatus = 1.
        END. 
/*
MESSAGE 'TEST' 'bSettEtikett' bSettEtikett SKIP
'bEtiTvang' bEtiTvang SKIP
'piEtikettStatus'piEtikettStatus SKIP
'iClProfilNr' iClProfilNr 'iProfilNr' iProfilNr
VIEW-AS ALERT-BOX.
*/        
      END.
      /* Er det hk og ikke etikettvang, skal hk ikke ha etikett uavhengig av overstyringsflagget. */   
      ELSE ASSIGN piEtikettStatus  = 1. 
    END. /* HK */
    
    ELSE 
    BUTIKK:
    DO:
      /* Er det etikettvang, skal det skrives etikett hvis det er salg på den eller lokal pris er endret. */
      IF bEtiTvang THEN
      DO:
          RUN sjekksalg (INPUT-OUTPUT bOk).
          IF bOk THEN piEtikettStatus = 0.
          ELSE piEtikettStatus = 1.
      END.
      /* Er det ikke etikettvang, skal butikken ikke ha etikett. */   
      ELSE ASSIGN piEtikettStatus  = 1. 
    END. /* BUTIKK */
  END. 
  
  /* Behandler NORMALPRIS for 'IPS og LOK' */
  ELSE DO: 
      /* Er det etikettvang, skal det skrives etikett hvis det er salg på den eller lokal pris er endret. */
      IF bEtiTvang THEN
      DO:
          RUN sjekksalg (INPUT-OUTPUT bOk).
          IF bOk THEN piEtikettStatus = 0.
          ELSE piEtikettStatus = 1.
      END.
      /* Er det ikke etikettvang, skal butikken ikke ha etikett. */   
      ELSE ASSIGN piEtikettStatus  = 1. 
  END. 

  /* Er det ikke etikett, kan priskøposten behandles automatisk. */
  IF piEtikettStatus  = 1 THEN 
    ASSIGN piKlargjorStatus = 1.
  ELSE   
    ASSIGN piKlargjorStatus = 0.

END. /* NORMALPRIS */
/* Oppdaterer recorden. */
ASSIGN
  iKlargjorStatus = piKlargjorStatus  
  iEtikettStatus  = piEtikettStatus   
  .
/*
MESSAGE 'TEST-klar' skip
        'iKlargjorStatus' iKlargjorStatus skip
        'iEtikettStatus' iEtikettStatus
'iClProfilNr' iClProfilNr 'iProfilNr' iProfilNr
VIEW-AS ALERT-BOX.
*/
/* **********************  Internal Procedures  *********************** */

PROCEDURE sjekkSalg:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
  
    pbOk = FALSE.
    
    IF bAlltidEtikett THEN 
    DO:
        pbOk = TRUE.
        RETURN.
    END.
 
    /* Sjekker om det finnes salg på artikkelen på den aktuelle profilen. */
    /* Alle butikkene i profilen må sjekkes.                              */
    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK WHERE 
      Butiker.ProfilNr = iProfilNr:
    
      IF Butiker.ApningsDato > TODAY OR 
       Butiker.ApningsDato = ? OR 
       Butiker.NedlagtDato <> ? OR 
       Butiker.harButikksystem = FALSE THEN 
       NEXT BUTIKKLOOP.
 
      /* Er det salg på artikkelen for butikken skal den stoppe i priskø. */
      IF CAN-FIND (FIRST TransLogg WHERE
                       TransLogg.ArtikkelNr  = lArtikkelNr AND
                       TransLogg.Dato        > TODAY - (24 * 31) AND 
                       TransLogg.Tid        >= 0 AND
                       TransLogg.Butik       = Butiker.Butik AND
                       TransLogg.TTId        = 1) THEN 
      DO:
        pbOk = TRUE.
        LEAVE BUTIKKLOOP.
      END.
      ELSE DO:
        /* Vi har funnet salg. Da skal vi sjekke om inn eller utpris er endret. */
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = lArtikkelNr AND
          ArtPris.ProfilNr   = iProfilNr  NO-ERROR.
        /* Finnes lokal artpris, og det er prisendring skal priskøposten stoppe. */
        IF AVAILABLE ArtPris THEN 
        DO:
          IF ArtPris.VareKost[1] <> lVareKost OR ArtPris.Pris[1] <> lPris THEN
          DO: 
            pbOk = TRUE.
            LEAVE BUTIKKLOOP.
          END.
        END.
      END.
    END. /* BUTIKKLOOP */
END PROCEDURE.
