&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN OppdaterMva.     /* Moms */

RUN OppdaterGruppe.  /* Avdeling, HuvGr, VarGr, VgKat */

RUN OppdaterLev.     /* Levbas, Produsent */

RUN OppdaterFylke.   /* Fylke, Kommune */

RUN OppdaterPost.    /* Post */

RUN OppdaterButiker. /* Butiker */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OppdaterButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterButiker Procedure 
PROCEDURE OppdaterButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH Infopos.Butikk NO-LOCK.
       FIND SkoTex.Butiker WHERE SkoTex.Butiker.butik = Infopos.butikk.butnr NO-LOCK NO-ERROR.
       FIND skotex.post WHERE skotex.post.postnr = STRING(Infopos.butikk.postnr) NO-LOCK NO-ERROR.
       IF NOT AVAIL SkoTex.Butiker THEN DO:
           CREATE SkoTex.Butiker.
           ASSIGN SkoTex.Butiker.Butik       = Infopos.butikk.butnr
                  SkoTex.Butiker.ProfilNr    = Infopos.butikk.profnr
                  SkoTex.Butiker.ApningsDato = DATE(01,01,2005)
                  SkoTex.Butiker.BuAdr       = Infopos.butikk.adresse
                  SkoTex.Butiker.BuKon       = Infopos.butikk.kontakt
                  SkoTex.Butiker.BuPadr      = IF AVAIL Skotex.Post THEN SkoTex.Post.Beskrivelse ELSE ""
                  SkoTex.Butiker.BuPonr      = STRING(Infopos.butikk.postnr)
                  SkoTex.Butiker.BuTel       = Infopos.butikk.telefon
                  SkoTex.Butiker.Butik       = Infopos.butikk.butnr
                  SkoTex.Butiker.ProfilNr    = Infopos.butikk.profnr
                  SkoTex.Butiker.ButNamn     = REPLACE(Infopos.butikk.navn,","," ")
                  SkoTex.Butiker.KortNavn    = STRING(Infopos.butikk.butnr) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               DELETE SkoTex.Butiker.
           ELSE
               RUN kopierKasse2Preem.p (Skotex.Butiker.Butik) NO-ERROR.
       END.
       ELSE IF SkoTex.Butiker.ProfilNr <> Infopos.butikk.profnr         OR
               SkoTex.Butiker.BuAdr    <> Infopos.butikk.adresse        OR
               SkoTex.Butiker.BuKon    <> Infopos.butikk.kontakt        OR
               SkoTex.Butiker.BuPonr   <> STRING(Infopos.butikk.postnr) OR
               SkoTex.Butiker.BuTel    <> Infopos.butikk.telefon        OR
               SkoTex.Butiker.Butik    <> Infopos.butikk.butnr          OR
               SkoTex.Butiker.ButNamn  <> REPLACE(Infopos.butikk.navn,","," ") OR
               SkoTex.Butiker.KortNavn <> STRING(Infopos.butikk.butnr)   THEN DO:
           FIND CURRENT SkoTex.Butiker EXCLUSIVE NO-WAIT.
           IF AVAIL SkoTex.Butiker THEN DO:
               ASSIGN SkoTex.Butiker.ProfilNr = Infopos.butikk.profnr
                      SkoTex.Butiker.BuAdr    = Infopos.butikk.adresse       
                      SkoTex.Butiker.BuKon    = Infopos.butikk.kontakt       
                      SkoTex.Butiker.BuPonr   = STRING(Infopos.butikk.postnr)
                      SkoTex.Butiker.BuPadr   = IF AVAIL Skotex.Post THEN SkoTex.Post.Beskrivelse ELSE ""
                      SkoTex.Butiker.BuTel    = Infopos.butikk.telefon       
                      SkoTex.Butiker.Butik    = Infopos.butikk.butnr         
                      SkoTex.Butiker.ButNamn  = REPLACE(Infopos.butikk.navn,","," ")
                      SkoTex.Butiker.KortNavn = STRING(Infopos.butikk.butnr) NO-ERROR.
           END.
       END.
       RELEASE SkoTex.Butiker.
   END.
/* SkoTex.Butiker.BankKonto               = */
/* SkoTex.Butiker.Butiker.ButLand         = */
/* SkoTex.Butiker.Butiker.ePostAdresse    = */
/* SkoTex.Butiker.Butiker.harButikksystem = */
/* SkoTex.Butiker.Butiker.LevAdresse1     = */
/* SkoTex.Butiker.Butiker.LevAdresse2     = */
/* SkoTex.Butiker.Butiker.LevKontakt      = */
/* SkoTex.Butiker.Butiker.LevMerknad      = */
/* SkoTex.Butiker.Butiker.LevPostBoks     = */
/* SkoTex.Butiker.Butiker.LevPostNr       = */
/* SkoTex.Butiker.Butiker.LevTelefon      = */
/* SkoTex.Butiker.Butiker.OrganisasjonsNr = */
/* SkoTex.Butiker.Butiker.Postgiro        = */
/* SkoTex.Butiker.Butiker.ProfilNr        = */
/* SkoTex.Butiker.Butiker.StdVeksel       = */
/* SkoTex.Butiker.Butiker.Telefaks        = */
/* SkoTex.Butiker.Butiker.URLAdresse      = */
/* SkoTex.Butiker.Butiker.VaarREf         = */
/* SkoTex.Butiker.Butiker.FaktTekstNr     = */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterFylke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterFylke Procedure 
PROCEDURE OppdaterFylke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(kommune WHERE kommune.kommnr = "") THEN DO:
        CREATE kommune.
        ASSIGN kommune.kommnr = "" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE kommune.
    END.
    IF NOT CAN-FIND(fylke WHERE fylke.fylkesnr = "") THEN DO:
        CREATE fylke.
        ASSIGN fylke.fylkesnr = "" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE fylke.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterGruppe Procedure 
PROCEDURE OppdaterGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER SE_Avdeling FOR skotex.Avdeling.
    FOR EACH infopos.Avdeling NO-LOCK:
        FIND skotex.Avdeling WHERE skotex.Avdeling.avdelingnr = infopos.avdeling.avd NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.Avdeling THEN DO:
            CREATE skotex.Avdeling.
            ASSIGN SkoTex.Avdeling.AvdelingNr   = infopos.avdeling.avd
                   SkoTex.Avdeling.AvdelingNavn = infopos.avdeling.avdtekst NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.Avdeling.
        END.
        ELSE IF SkoTex.Avdeling.AvdelingNavn <> infopos.avdeling.avdtekst THEN DO:
            FIND CURRENT SkoTex.Avdeling EXCLUSIVE NO-WAIT.
            IF AVAIL SkoTex.Avdeling THEN
                ASSIGN SkoTex.Avdeling.AvdelingNavn = infopos.avdeling.avdtekst.
        END.
        RELEASE skotex.Avdeling.
        FOR EACH grledomr OF infopos.avdeling NO-LOCK.
            FIND skotex.huvgr WHERE skotex.huvgr.hg = infopos.grledomr.glo NO-LOCK NO-ERROR.
            IF NOT AVAIL skotex.huvgr THEN DO:
                CREATE skotex.huvgr.
                ASSIGN SkoTex.HuvGr.AvdelingNr = infopos.avdeling.avd
                       SkoTex.HuvGr.Hg         = infopos.grledomr.glo
                       SkoTex.HuvGr.HgBeskr    = infopos.grledomr.glotekst NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE skotex.HuvGr.
            END.
            ELSE IF SkoTex.HuvGr.AvdelingNr <> infopos.avdeling.avd OR
                    SkoTex.HuvGr.HgBeskr    <> infopos.grledomr.glotekst THEN DO:
                FIND CURRENT SkoTex.HuvGr EXCLUSIVE NO-WAIT.
                ASSIGN SkoTex.HuvGr.AvdelingNr = infopos.avdeling.avd
                       SkoTex.HuvGr.HgBeskr    = infopos.grledomr.glotekst NO-ERROR.
            END.
            RELEASE Skotex.HuvGr.    
            FOR EACH hovedgr OF grledomr NO-LOCK.
                FIND skotex.VarGr WHERE skotex.Vargr.vg = Infopos.hovedgr.hgr NO-LOCK NO-ERROR.
                FIND infopos.hgrprof WHERE infopos.hgrprof.profnr = 1 AND 
                                           Infopos.hgrprof.hgr    = Infopos.hovedgr.hgr NO-LOCK NO-ERROR.
                IF NOT AVAIL skotex.VarGr THEN DO:
                    CREATE skotex.VarGr.
                    ASSIGN SkoTex.VarGr.Hg        = infopos.grledomr.glo
                           SkoTex.VarGr.Kost_Proc = IF AVAIL Infopos.hgrprof THEN 100 - Infopos.hgrprof.brutto% ELSE 0
                           SkoTex.VarGr.MomsKod   = Infopos.hovedgr.mvagr
                           SkoTex.VarGr.Vg        = Infopos.hovedgr.hgr     
                           SkoTex.VarGr.VgBeskr   = Infopos.hovedgr.hgrtekst NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN DO:
                        DELETE skotex.vargr.
                        NEXT.
                    END.
                    CREATE skotex.vgkat.
                    ASSIGN SkoTex.VgKat.KatNr = 1
                           SkoTex.VgKat.Vg    = Infopos.hovedgr.hgr
                           SkoTex.VgKat.VgKat = 1 NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        DELETE skotex.VgKat.
                END.
                ELSE IF SkoTex.VarGr.Hg        <> infopos.grledomr.glo OR
                        SkoTex.VarGr.Kost_Proc <> (IF AVAIL Infopos.hgrprof THEN 100 - Infopos.hgrprof.brutto% ELSE 0) OR
                        SkoTex.VarGr.MomsKod   <> Infopos.hovedgr.mvagr OR
                        SkoTex.VarGr.VgBeskr   <> Infopos.hovedgr.hgrtekst THEN DO:
                    FIND CURRENT SkoTex.VarGr EXCLUSIVE NO-WAIT.
                    ASSIGN SkoTex.VarGr.Hg        = infopos.grledomr.glo
                           SkoTex.VarGr.Kost_Proc = IF AVAIL Infopos.hgrprof THEN 100 - Infopos.hgrprof.brutto% ELSE 0
                           SkoTex.VarGr.MomsKod   = Infopos.hovedgr.mvagr
                           SkoTex.VarGr.VgBeskr   = Infopos.hovedgr.hgrtekst NO-ERROR.
                END.
                RELEASE SkoTex.VarGr.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterLev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterLev Procedure 
PROCEDURE OppdaterLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH infopos.lev NO-LOCK:
        FIND skotex.levbas WHERE skotex.levbas.levnr = Int(infopos.lev.levnr) NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.levbas THEN DO:
            CREATE skotex.levbas.
            ASSIGN SkoTex.LevBas.levnr        = infopos.lev.levnr
                   SkoTex.LevBas.levnamn      = infopos.lev.navn
                   SkoTex.LevBas.E_MailLev    = infopos.lev.emailadr
                   SkoTex.LevBas.levadr       = infopos.lev.adresse
                   SkoTex.LevBas.levkon       = infopos.lev.kontakt
                   SkoTex.LevBas.levpadr      = infopos.lev.poststed
                   SkoTex.LevBas.levponr      = STRING(infopos.lev.postnr)
                   SkoTex.LevBas.levtel       = infopos.lev.telefon
                   SkoTex.LevBas.telefax      = infopos.lev.faxnr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.levbas.
        END.
        ELSE IF SkoTex.LevBas.E_MailLev <> infopos.lev.emailadr       OR
                SkoTex.LevBas.levadr    <> infopos.lev.adresse        OR
                SkoTex.LevBas.levkon    <> infopos.lev.kontakt        OR
                SkoTex.LevBas.levnamn   <> infopos.lev.navn           OR
                SkoTex.LevBas.levpadr   <> infopos.lev.poststed       OR
                SkoTex.LevBas.levponr   <> STRING(infopos.lev.postnr) OR
                SkoTex.LevBas.levtel    <> infopos.lev.telefon        OR
                SkoTex.LevBas.telefax   <> infopos.lev.faxnr THEN DO:
            FIND CURRENT SkoTex.LevBas EXCLUSIVE NO-WAIT.
            IF AVAIL SkoTex.LevBas THEN DO:
                ASSIGN SkoTex.LevBas.E_MailLev = infopos.lev.emailadr
                       SkoTex.LevBas.levadr    = infopos.lev.adresse
                       SkoTex.LevBas.levkon    = infopos.lev.kontakt
                       SkoTex.LevBas.levnamn   = infopos.lev.navn
                       SkoTex.LevBas.levpadr   = infopos.lev.poststed
                       SkoTex.LevBas.levponr   = STRING(infopos.lev.postnr)
                       SkoTex.LevBas.levtel    = infopos.lev.telefon
                       SkoTex.LevBas.telefax   = infopos.lev.faxnr.
                RELEASE Skotex.LevBas.
            END.
        END.
        RELEASE SkoTex.LevBas.
    END.
    FOR EACH Infopos.produsent NO-LOCK.
        FIND skotex.produsent WHERE skotex.produsent.ProdNr = infopos.produsent.prodnr NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.produsent THEN DO:
            CREATE skotex.produsent.
            ASSIGN SkoTex.Produsent.ProdNr      = infopos.produsent.prodnr
                   SkoTex.Produsent.Beskrivelse = infopos.produsent.navn 
                   SkoTex.Produsent.Adresse1    = infopos.produsent.adresse
                   SkoTex.Produsent.Kontakt     = infopos.produsent.kontakt 
                   SkoTex.Produsent.PostNr      = STRING(infopos.produsent.postnr)
                   SkoTex.Produsent.Telefon     = infopos.produsent.telefon NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.produsent.
        END.
        ELSE IF SkoTex.Produsent.Beskrivelse <> infopos.produsent.navn           OR
                   SkoTex.Produsent.Adresse1 <> infopos.produsent.adresse        OR
                   SkoTex.Produsent.Kontakt  <> infopos.produsent.kontakt        OR
                   SkoTex.Produsent.PostNr   <> STRING(infopos.produsent.postnr) OR
                   SkoTex.Produsent.Telefon  <> infopos.produsent.telefon THEN DO:
            FIND CURRENT SkoTex.Produsent EXCLUSIVE NO-WAIT.
            IF AVAIL skotex.produsent THEN DO:
                ASSIGN SkoTex.Produsent.Beskrivelse = infopos.produsent.navn 
                       SkoTex.Produsent.Adresse1    = infopos.produsent.adresse
                       SkoTex.Produsent.Kontakt     = infopos.produsent.kontakt 
                       SkoTex.Produsent.PostNr      = STRING(infopos.produsent.postnr)
                       SkoTex.Produsent.Telefon     = infopos.produsent.telefon NO-ERROR.
                RELEASE skotex.produsent.
            END.
        END.
        RELEASE skotex.produsent.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterMva Procedure 
PROCEDURE OppdaterMva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Infopos.mva NO-LOCK.
        FIND skotex.moms WHERE skotex.moms.momskod = Infopos.mva.mvagr NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.moms THEN DO:
            CREATE skotex.moms.
            ASSIGN skotex.moms.MomsKod     = infopos.mva.mvagr
                   skotex.moms.Beskrivelse = infopos.mva.mvatekst
                   skotex.moms.MomsProc    = infopos.mva.mva NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.moms.
        END.
        ELSE IF skotex.moms.Beskrivelse <> infopos.mva.mvatekst OR
                skotex.moms.MomsProc    <> infopos.mva.mva THEN DO:
            FIND CURRENT skotex.moms EXCLUSIVE NO-WAIT.
            IF AVAIL skotex.moms THEN DO:
                ASSIGN skotex.moms.Beskrivelse = infopos.mva.mvatekst
                       skotex.moms.MomsProc    = infopos.mva.mva NO-ERROR.
                RELEASE skotex.moms.
            END.
        END.
        RELEASE skotex.moms.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPost Procedure 
PROCEDURE OppdaterPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH infopos.post NO-LOCK:
        FIND skotex.post WHERE skotex.pos.postnr = STRING(infopos.post.postnr) NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.post THEN DO:
            CREATE Skotex.post.
            ASSIGN Skotex.post.PostNr      = STRING(infopos.post.postnr)
                   Skotex.post.Beskrivelse = infopos.post.poststed NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE Skotex.post.
        END.
        ELSE IF SkoTex.Post.Beskrivelse <> infopos.post.poststed THEN DO:
            FIND CURRENT Skotex.post EXCLUSIVE NO-WAIT.
            IF AVAIL Skotex.post THEN
                ASSIGN Skotex.post.Beskrivelse = infopos.post.poststed NO-ERROR.
        END.
        RELEASE Skotex.post.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

