/*------------------------------------------------------------------------------
  Purpose:     oppdVPIArtBas.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iHKVareId AS INT  NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.
DEF VAR bHK       AS LOG  NO-UNDO.
DEF VAR iLopNr    AS INT  NO-UNDO.
DEF VAR bHKDirekteOppd    AS LOG NO-UNDO.
DEF VAR iUkjentEkstVPILev AS INT NO-UNDO.
DEF VAR ocReturn    AS CHAR NO-UNDO.
DEF VAR obOK        AS LOG NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  
ON CREATE OF VPIArtBas OVERRIDE 
  DO:  
  END.
ON WRITE OF VPIArtBas OVERRIDE 
  DO:  
  END.
ON DELETE OF VPIArtBas OVERRIDE 
  DO:  
  END.
ON CREATE OF VPIStrekkode OVERRIDE 
  DO:  
  END.
ON WRITE OF VPIStrekkode OVERRIDE 
  DO:  
  END.
ON DELETE OF VPIStrekkode OVERRIDE 
  DO:  
  END.
  
{tmptt_VPIArtBas.i &SHARED=SHARED}
{tmptt_VPIStrekkode.i &SHARED=SHARED}

DEF BUFFER bVPIStrekkode FOR VPIStrekkode.

IF NOT CAN-FIND(FIRST TT_VPIArtBas) THEN
  RETURN.

/* Korr ekstvpilevnr for ukjent vare */
{syspara.i 22 10 3 iUkjentEkstVPILev INT}
/* HK installasjon. */
{syspara.i 1 1 18 cTekst}
IF CAN-DO("yes,1,ja,true",cTekst) THEN
    bHk = TRUE.
  ELSE
    bHk = FALSE.

/* Direkteoppdatering av info fra HK. */
{syspara.i 50 16 2 cTekst}
IF CAN-DO("yes,1,ja,true",cTekst) THEN
    bHKDirekteOppd = TRUE.
  ELSE
    bHKDirekteOppd = FALSE.

/* Behandler sletteposter */
RUN behandleSletteposter.

/* Behandler Ny/endre poster */
RUN behandleNyeEndrePoster.







/* **********************  Internal Procedures  *********************** */


PROCEDURE behandleNyeEndrePoster:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
NY-ENDRE:
FOR EACH TT_VPIArtBas WHERE
  TT_VPIArtBas.RecType = 1:
  FIND VPIArtBas EXCLUSIVE-LOCK WHERE
    VPIArtBas.EkstVPILevNr = TT_VPIArtBas.EkstVPILevNr AND
    VPIArtBas.VareNr       = TT_VPIArtBas.VareNr NO-ERROR.
  /* Ser til at det finnes et VPIDatasett å henge opp dataene på. */
  IF NOT CAN-FIND(FIRST VPIDataSett WHERE
                  VPIDataSett.EkstVPILevNr = TT_VPIArtBas.EkstVPILevNr)  THEN
  DO:
    CREATE VPIDataSett.
    ASSIGN
      VPIDataSett.EkstVPILevNr   = TT_VPIArtBas.EkstVPILevNr
      VPIDataSett.DataSettStatus = 3 /* Mottatt */
      VPIDataSett.ImportDato     = TODAY
      VPIDataSett.ImportKl       = TIME
      .
    RELEASE VPIDataSett.
  END.

  /* Legger inn artikkelinformasjonen */
  IF NOT AVAILABLE VPIArtBas THEN
    CREATE VPIArtBas.
  BUFFER-COPY TT_VPIArtBas TO VPIArtBas.

  /* Varer som kommer fra hk SKAL være lagerstyrt. */
  /* TN 25/8-11                                    */
  IF VPIArtBas.OPris = FALSE THEN 
    VPIArtBas.Lager = TRUE.

  /* Varer som kommer fra HK er stykkvarer hvis de ikke er markert med vekt. */
  /* TN 13/9-11                                                              */
  VPIArtBas.ArtSlag = 0.

  /* Her tar vi bort priser og strekkoder. De blir oppdatert senere. */
  /* Kommer i samme fil som artikkelinformasjonen.                   */
  FOR EACH VPIArtPris OF VPIArtBas:
    DELETE VPIArtPris.
  END.
  FOR EACH VPIStrekkode OF VPIArtBas:
    DELETE VPISTrekkode.
  END.
  FOR EACH VPIErstattningsvare WHERE
      VPIErstattningsVare.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
      VPIErstattningsVare.VareNr       = VPIArtBas.VareNr:
      DELETE VPIErstattningsVare.
  END.
  FOR EACH VPIPakkeLinje WHERE
      VPIPakkeLinje.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
      VPIPakkeLinje.VareNr       = VPIArtBas.VareNr:
      DELETE VPIPakkeLinje.
  END.
  FOR EACH VPIAltLevBas WHERE
      VPIAltLevBas.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
      VPIAltLevBas.VareNr       = VPIArtBas.VareNr:
      DELETE VPIAltLevBAs. 
  END.
  FOR EACH VPIArtBestPkt OF VPIArtBas:
      DELETE VPIArtBestPkt.
  END.
  FOR EACH VPIBildeRegister WHERE
      VPIBildeRegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
      VPIBildeRegister.VareNr       = VPIArtBas.VareNr:
      FOR EACH VPIBildeData WHERE
          VPIBildeData.EkstVPILevNr = VPIBildeRegister.EkstVPILevNr AND
          VPIBildeData.VareNr       = VPIBildeRegister.VareNr:
          DELETE VPIBildeData.
      END.
      DELETE VPIBildeRegister.
  END.
  /* Henter ut strekkoder for denne artikkelen (De gamle ble slettet lengre oppe. */
  HENT_STREKKODE:
  FOR EACH TT_VPIStrekkode WHERE
    TT_VPIStrekkode.RecType = 1 AND
    TT_VPIStrekkode.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND 
    TT_VPIStrekkode.VareNr       = VPIArtBas.VareNr:

    FIND VPIStrekkode EXCLUSIVE-LOCK WHERE
      VPIStrekkode.EkstVPILevNr = TT_VPIStrekkode.EkstVPILevNr AND
      VPISTrekkode.VareNr       = TT_VPIStrekkode.VareNr AND
      VPIStrekkode.Kode         = TT_VPIStrekkode.Kode NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIStrekkode THEN
      CREATE VPIStrekkode.
    BUFFER-COPY TT_VPIStrekkode TO VPIStrekkode.
    
    /* Sjekker om størrelsen en endret på strekkoden. Er den det, skal størrelsen også */
    /* endres på strekkoden lokalt. Og... lageret skal følge med.                      */
    FIND Strekkode NO-LOCK WHERE
      Strekkode.ArtikkelNr = DECIMAL(VPIStrekkode.VareNr) AND
      Strekkode.Kode       = VPIStrekkode.Kode NO-ERROR.
    IF AVAILABLE Strekkode THEN 
    FLYTT_LAGER:
    DO:
      /* Flytter lager */
      IF bHk = FALSE AND Strekkode.StrKode <> VPIStrekkode.StrKode THEN 
        RUN bibl_flytt_lager_str.p (Strekkode.Kode, VPISTrekkode.StrKode).
    END. /* FLYTT_LAGER */
    IF AVAILABLE VPIStrekkode THEN RELEASE VPIStrekkode.
    
  END. /* HENT_STREKKODE */

  /* Logging av VPI korreksjonsposter fra HK i ELogg. */
  /* Disse skal senere behandles av ELoggServer.      */
  VPIKORRPOST:
  DO:
      /*
      /* Korreksjonsposter som kommer fra HK på varer som var 'ukjent' fra ERP */
      /* når de ble lest inn via pakkseddel, skal ikke logges i elogg.         */
      IF VPIArtBas.EkstVPILevNr = iUkjentEkstVPILev THEN
          LEAVE VPIKORRPOST.
          
      /* Varer med åpen pris skal ikke behandles her. */
      IF VPIArtBas.OPris THEN 
          LEAVE VPIKORRPOST.

      /* I butikk - Logger korreksjonsposter som skal behandles.   */
      /* Dette kjøres når HK's korreksjonsmelding mottas i butikk. */
      IF bHk = FALSE AND
        CAN-FIND(ArtBas NO-LOCK WHERE
                 ArtBas.ArtikkelNr = dec(VPIArtBas.KorrArtikkelNr) AND
                 VPIArtBas.KorrArtikkelNr > 0) THEN 
          RUN logg_vpiartbas_i_elogg.p (RECID(VPIArtBas),"KORRHK").
          
      /* HK mottar VPI med lokale artikler fra butikk. */    
      ELSE IF bHK AND VPIArtBas.EkstVPILevNr > 999999 THEN 
      LOKALE_ARTIKLER_FRA_BUTIKK:
      DO:
        /* Finnes EAN koden på en artikkel på hk, skal den kobles med en gang. */
        STREKKODESJEKK:
        FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
          /* Ugyldige tegn i kode, hopper vi over. */
          ASSIGN lDec = DECIMAL(VPIStrekkode.Kode) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN NEXT STREKKODESJEKK.
          
          /* Blanke skal vi ikke ha. */
          IF (lDec = 0 OR lDec = ?) THEN NEXT STREKKODESJEKK.
        
          /* PLU nummer skal vi ikke ha. */
          IF lDec <= 9999999 THEN NEXT STREKKODESJEKK.
          
          /* Bedriftsinterne EAN koder skal vi ikke ha. */
          IF (LENGTH(VPIStrekkode.Kode) = 13 AND SUBSTRING(VPIStrekkode.Kode,1,2) = '02') THEN NEXT STREKKODESJEKK. 
        
          /* Ser om strekkoden er kjent på HK */
          FIND Strekkode NO-LOCK WHERE Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
            
          /* Koble og sende korreksjonsmelding til butikk. */
          IF AVAILABLE Strekkode AND Strekkode.ArtikkelNr <> DECIMAL(VPIArtBas.VareNr) THEN 
          KOBLE_TIL_BUTIKK:
          DO:
            ASSIGN VPIArtBas.KorrArtikkelNr = Strekkode.ArtikkelNr
                   VPIArtBas.KorrStatus     = 10.
            /* Sender korreksjon til butikk. */
            RUN korrvpiartbas_send.p (STRING(VPIArtBas.EkstVPILevNr) + '|' + VPIArtBas.VareNr + '|',
                                      ?,'',OUTPUT ocReturn,OUTPUT obOk).
            LEAVE STREKKODESJEKK.
          END. /* KOBLE_TIL_BUTIKK */
          /* Artikkelen har gyldige koder og skal opprettes på HK med lokalt artikkelnr. */
          ELSE 
          OPPRETTE_LOKAL_ARTIKKEL_PA_HK:
          DO:
            IF LENGTH(TRIM(VPISTrekkode.Kode)) = 13 AND
               SUBSTRING(TRIM(VPISTrekkode.Kode),1,2) <> '02' THEN
            DO:
              IF VALID-HANDLE(SOURCE-PROCEDURE) THEN 
              DO:
                RUN oppdVPIArtPris.p. 
                RUN oppdVPIBildeRegister IN SOURCE-PROCEDURE NO-ERROR.
                RUN oppdVPIBildeData     IN SOURCE-PROCEDURE NO-ERROR.
              END.
              RUN artbas_new.p 
                (STRING(VPIArtBas.EkstVPILevNr) + ';;' + VPIArtBas.VareNr , 
                 ?, 
                 '', 
                 OUTPUT ocReturn, 
                 OUTPUT obOk).   
              LEAVE STREKKODESJEKK. /* TN 7/12-09 */       
            END. 
          END. /* OPPRETTE_LOKAL_ARTIKKEL_PA_HK */
        END. /* STREKKODESJEKK */
      END. /* LOKALE_ARTIKLER_FRA_BUTIKK */
      */
  END. /* VPIKORRPOST */
  
  /* Artikkelinformasjonen skal oppdateres direkte */
  IF bHKDirekteOppd THEN
  KOPIER_ART_INFO:
  DO:
      /*
      FIND ArtBas EXCLUSIVE-LOCK WHERE
          ArtBas.ArtikkelNr = dec(VPIArtBas.VareNr) NO-ERROR.
      IF AVAILABLE ArtBas THEN
      DO:
          /* Kopierer over all artikkelinformasjon.             */
          /* Løpenummer settes til ? hvis varegruppe er byttet. */
          {oppdVPIArtbas.i "ArtBas" "VPIArtBas"}

          /* Legger ut nye strekkoder på størrelser som er i bruk */
          RUN vpistrekkode_til_strekkode.p (VPIArtBas.EkstVPILevNr, VPIArtBas.VareNr).

          /* Artikkelen har byttet varegruppe */
          IF ArtBas.Vg <> VPIArtBas.Vg THEN
          DO:
              ASSIGN
                  ArtBas.Vg    = VPIArtBas.Vg
                  ArtBas.LopNr = ?. 
          END.

          IF ArtBas.LopNr = ? AND bHK = FALSE THEN
          DO:
              /* Henter nytt løpenummer */
              iLopNr = 0.
              RUN SettLopNr.p (ArtBas.Vg,'F',OUTPUT iLopNr).
              IF iLopNr <> 0 THEN DO:
                  ASSIGN ArtBas.LopNr = iLopNr.
                  FOR EACH ArtLag EXCLUSIVE-LOCK WHERE
                      ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:
                      ASSIGN
                          ArtLag.Vg    = ArtBas.Vg
                          ArtLag.LopNr = ArtBas.LopNr
                          .
                  END.
              END.
          END.
      END.
      */ 
  END. /* KOPIER_ART_INFO */

  RELEASE VPIArtBas.
  DELETE TT_VPIArtBas.
END. /* NY-ENDRE */
END PROCEDURE.

PROCEDURE behandleSletteposter:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
SLETTEPOSTER:
FOR EACH TT_VPIArtBas WHERE
  TT_VPIArtBas.RecType = 3:
  FIND VPIArtBas EXCLUSIVE-LOCK WHERE
    VPIArtBas.EkstVPILevNr = TT_VPIArtBas.EkstVPILevNr AND
    VPIArtBas.VareNr       = TT_VPIArtBas.VareNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN
  DO:
    /* Tar også bort priser og strekkoder */
    FOR EACH VPIArtPris OF VPIArtBas:
      DELETE VPIArtPris.
    END.
    FOR EACH VPIStrekkode OF VPIArtBas:
      DELETE VPISTrekkode.
    END.
    FOR EACH VPIErstattningsvare WHERE
        VPIErstattningsVare.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIErstattningsVare.VareNr       = VPIArtBas.VareNr:
        DELETE VPIErstattningsVare.
    END.
    FOR EACH VPIPakkeLinje WHERE
        VPIPakkeLinje.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIPakkeLinje.VareNr       = VPIArtBas.VareNr:
        DELETE VPIPakkeLinje.
    END.
    FOR EACH VPIAltLevBas WHERE
        VPIAltLevBas.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIAltLevBas.VareNr       = VPIArtBas.VareNr:
        DELETE VPIAltLevBAs. 
    END.
    FOR EACH VPIArtBestPkt OF VPIArtBas:
        DELETE VPIArtBestPkt.
    END.
    FOR EACH VPIBildeRegister WHERE
        VPIBildeRegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIBildeRegister.VareNr       = VPIArtBas.VareNr:
        FOR EACH VPIBildeData WHERE
            VPIBildeData.EkstVPILevNr = VPIBildeRegister.EkstVPILevNr AND
            VPIBildeData.VareNr       = VPIBildeRegister.VareNr:
            DELETE VPIBildeData.
        END.
        DELETE VPIBildeRegister.
    END.
    DELETE VPIArtBas NO-ERROR.
    DELETE TT_VPIArtBas.
  END.
END. /* SLETTEPOSTER */


END PROCEDURE.

