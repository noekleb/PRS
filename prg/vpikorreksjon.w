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

DEFINE TEMP-TABLE TT_Elogg NO-UNDO LIKE Elogg.

DEFINE VARIABLE cTabellnavn     AS CHARACTER INIT "VPIArtBas" NO-UNDO.
DEFINE VARIABLE cEksterntSystem AS CHARACTER INIT "KORRHK"    NO-UNDO.

DEFINE VARIABLE h_dvpiartbas AS HANDLE     NO-UNDO.

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
         HEIGHT             = 13.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

  IF NOT VALID-HANDLE(h_dvpiartbas) THEN DO:
      RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
      RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
  END.

  IF VALID-HANDLE(h_dvpiartbas) THEN
  DO:
      RUN KopierElogg.
      RUN BehandlaTTElogg.
      RUN SlettElogg.
      DELETE PROCEDURE h_dvpiartbas.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BehandlaTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandlaTTElogg Procedure 
PROCEDURE BehandlaTTElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iEkstVPILevNr LIKE vpiartbas.EkstVPILevNr NO-UNDO.
    DEFINE VARIABLE cVarenr       LIKE vpiartbas.varenr       NO-UNDO.
    DEFINE VARIABLE dArtikkelnr   LIKE Artbas.Artikkelnr      NO-UNDO.
    DEFINE VARIABLE dNyArtikkelnr  AS DECIMAL    NO-UNDO.

    FOR EACH TT_Elogg.
        IF NUM-ENTRIES(TT_Elogg.verdier,"|") = 3 THEN DO:
            /* HK har mottatt en ukjent vare via pakkseddelimport og har oppdatert                    */
            /* informasjonen på den. butikkens kopi av artikkelen skal oppdateres med ny informasjon. */
            IF ENTRY(2,TT_Elogg.Verdier,"|") = ENTRY(3,TT_Elogg.Verdier,"|") THEN 
            UKJENT_VARE:
            DO:
                /* - Er artikkelen slettet lokalt, gjør vi ingenting. */
                /* - Oppdatere artikkelinformasjon                    */
                /* - Oppdatere kalkyle, men ikke utpris.              */
                /* - Hente ut eventuelle nye strekkoder.              */
                /* TN 11/11-08 Dette gjøres i oppdVPIArtBas.p og bare på artikler som ligger i ArtBas
                RUN OppdaterArtBas.
                */
            END. /* UKJENT_VARE */

            /* Korr melding fra HK. Resultat av at butikken har sendt melding om ny */
            /* artikkel til hk, og at hk har koblet denne mot en hk artikkel.       */
            ELSE KORR_MELDING: DO:
                iEkstVPILevNr = INT(ENTRY(1,TT_Elogg.Verdier,"|")).
                cVarenr       = ENTRY(2,TT_Elogg.Verdier,"|").
                dArtikkelnr   = DECI(ENTRY(3,TT_Elogg.Verdier,"|")).
                /* Här har den lokala artikeln blivit borttagen */
                IF NOT CAN-FIND(Artbas WHERE Artbas.Artikkelnr = dArtikkelnr) THEN
                    NEXT.
                /* om vi mot förmodan inte hittar vpiposten */
                IF NOT CAN-FIND(Artbas WHERE Artbas.Artikkelnr = DECI(cVarenr)) AND
                   NOT CAN-FIND(VpiArtbas WHERE vpiartbas.EkstVPILevNr = iEkstVPILevNr AND 
                                                vpiartbas.varenr = cVarenr) THEN
                    NEXT.
                /* Vpiposten skall hämtas ned */
                IF NOT CAN-FIND(ArtBas WHERE Artbas.Artikkelnr = DECI(cVarenr)) THEN DO:
                    RUN HentVpiArtBas(iEkstVPILevNr,cVarenr,dArtikkelnr,OUTPUT dNyArtikkelnr). 
                    IF RETURN-VALUE = "AVBRYT" THEN
                        NEXT.
                END.
                ELSE
                    dNyArtikkelnr = DECI(cVarenr).
                /* Her flyttes strekkoden. */
                RUN FlyttStrekKode (dNyArtikkelnr,dArtikkelnr).

                /* Här uppdaterar gammal info för att vi skall kunna anropa separat */
                RUN vpikorrutfor.p (dNyArtikkelnr,dArtikkelnr).
            END. /* KORR_MELDING */
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FlyttStrekKode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttStrekKode Procedure 
PROCEDURE FlyttStrekKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dNyArtikkelnr AS DECIMAL    NO-UNDO.
    DEFINE INPUT  PARAMETER dArtikkelnr   AS DECIMAL    NO-UNDO.
    DO TRANSACTION:
        FOR EACH strekkode WHERE strekkode.artikkelnr = dArtikkelnr:
             strekkode.artikkelnr = dNyArtikkelnr.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentVpiArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentVpiArtBas Procedure 
PROCEDURE HentVpiArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  iEkstVPILevNr AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER  cVarenr       AS CHARACTER  NO-UNDO. /* Ny artikel i vpi */
    DEFINE INPUT PARAMETER  dArtikkelnr   AS DECIMAL    NO-UNDO. /* denna skall flyttas */
    DEFINE OUTPUT PARAMETER dNyArtikkelNr AS DECIMAL    NO-UNDO.

    DEFINE VARIABLE plArtikkelnr AS DECIMAL    NO-UNDO.
    DEFINE BUFFER bNyArtbas  FOR Artbas.
    DEFINE BUFFER bNyArtpris FOR Artpris.
    
    DO TRANSACTION:
        RUN OpprettNy IN h_dvpiartbas (INT(ENTRY(1,TT_Elogg.verdier,"|")),ENTRY(2,TT_Elogg.verdier,"|"), OUTPUT plArtikkelNr).
        dNyArtikkelnr = DECI(cVareNr).
        FIND bNyartbas WHERE bNyartbas.artikkelnr = dNyArtikkelnr NO-LOCK NO-ERROR.
        IF NOT AVAIL bNyArtbas THEN
            RETURN "AVBRYT".
        FOR EACH artpris WHERE artpris.artikkelnr = dNyArtikkelnr:
            DELETE artpris.
        END. 
        RUN OppdaterInfo IN h_dvpiartbas (INT(ENTRY(1,TT_Elogg.verdier,"|")), ENTRY(2,TT_Elogg.verdier,"|"), plArtikkelNr).
        /* old, vi har testat att den finns */
        FIND ArtBas WHERE Artbas.artikkelnr = dArtikkelnr.
        FOR EACH artpris WHERE artpris.artikkelnr = artbas.artikkelnr NO-LOCK:
            CREATE bNyartpris.
            BUFFER-COPY artpris EXCEPT artikkelnr TO bNyartpris
            ASSIGN bNyartpris.artikkelnr = bNyArtbas.artikkelnr.
        END.
        ASSIGN artbas.beskr     = "KORR: " + artbas.beskr
               artbas.bongtekst = "KORR: " + artbas.bongtekst
               artbas.IKasse    = FALSE
               artbas.Aktivert  = FALSE
               artbas.notat     = "KORR_til: " + STRING(bNyArtbas.artikkelnr) + " " + artbas.notat.
        ASSIGN bNyArtbas.notat  = "KORR_fra: " + STRING(Artbas.artikkelnr) + " " + bNyartbas.notat.
        FOR EACH prisko WHERE prisko.artikkelnr = artbas.artikkelnr:
            prisko.artikkelnr = bNyArtbas.Artikkelnr.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    DEFINE VARIABLE dOpprettet AS DECIMAL    NO-UNDO.
    
    dOpprettet = dec(STRING(YEAR(TODAY),"9999") +
                     string(MONTH(TODAY),"99")  + 
                     string(DAY(TODAY),"99")    +
                     string(TIME)).
                     
    FOR EACH Elogg WHERE Elogg.Tabellnavn     = cTabellnavn AND
                         ELogg.EksterntSystem = cEksterntSystem AND
                         ELogg.Behandlet      = FALSE AND
                         ELogg.EndringsType   = 1 NO-LOCK.

    
/*     MESSAGE                                                         */
/*     PROGRAM-NAME(1) SKIP                                            */
/*     PROGRAM-NAME(2) SKIP                                            */
/*     PROGRAM-NAME(3) PROGRAM-NAME(3) MATCHES '*Saner_fra_liste*' SKIP*/
/*     PROGRAM-NAME(4) SKIP                                            */
/*     PROGRAM-NAME(5) SKIP                                            */
/*     PROGRAM-NAME(6) SKIP(1)                                         */
/*     (PROGRAM-NAME(3) MATCHES '*w-vartkor.w*' OR                     */
/*           PROGRAM-NAME(4) MATCHES '*strekkode.w*' OR                */
/*           PROGRAM-NAME(3) MATCHES '*Saner_fra_liste*')              */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                              */

        /* Tidskontroll skal ikke gjøres når det kjøres fra artikkelkortet. */                 
/*        IF PROGRAM-NAME(3) MATCHES '*w-vartkor.w*' OR */
/*           PROGRAM-NAME(4) MATCHES '*strekkode.w*' OR */
/*           PROGRAM-NAME(3) MATCHES '*Saner_fra_liste*'*/
/*           THEN /* Gjør ingenting */.                 */
/*        ELSE TIDSKONTROLL: DO:                        */
/*            IF dOpprettet - Elogg.Opprettet < 5 THEN  */
/*                NEXT.                                 */
/*        END. /* TIDSKONTROLL */                       */
        
        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL Elogg THEN DO:
            CREATE TT_Elogg.
            BUFFER-COPY bElogg TO TT_Elogg NO-ERROR.
            bElogg.behandlet = TRUE.
            RELEASE bElogg.
            RELEASE TT_Elogg.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterArtBas Procedure 
PROCEDURE OppdaterArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   DEFINE VARIABLE iEkstVPILevNr LIKE vpiartbas.EkstVPILevNr NO-UNDO.                                        */
/*   DEFINE VARIABLE cVarenr       LIKE vpiartbas.varenr       NO-UNDO.                                        */
/*   DEFINE VARIABLE dArtikkelnr   LIKE Artbas.Artikkelnr      NO-UNDO.                                        */
/*   DEFINE VARIABLE dNyArtikkelnr  AS DECIMAL    NO-UNDO.                                                     */
/*                                                                                                             */
/*                                                                                                             */
/*   DEF BUFFER bvpiArtBas    FOR VPIArtBas.                                                                   */
/*   DEF BUFFER bvpiArtPris   FOR VPIArtPris.                                                                  */
/*   DEF BUFFER bvpiStrekkode FOR VPIStrekkode.                                                                */
/*   DEF BUFFER bArtBas       FOR ArtBas.                                                                      */
/*   DEF BUFFER bArtPris      FOR ArtPris.                                                                     */
/*   DEF BUFFER bStrekkode    FOR Strekkode.                                                                   */
/*                                                                                                             */
/*                                                                                                             */
/*   ASSIGN                                                                                                    */
/*       iEkstVPILevNr = INT(ENTRY(1,TT_Elogg.Verdier,"|"))                                                    */
/*       cVarenr       = ENTRY(2,TT_Elogg.Verdier,"|")                                                         */
/*       dNyArtikkelnr = dec(ENTRY(2,TT_Elogg.Verdier,"|"))                                                    */
/*       dArtikkelnr   = DECI(ENTRY(3,TT_Elogg.Verdier,"|")).                                                  */
/*                                                                                                             */
/*   DO TRANSACTION:                                                                                           */
/*       FIND bVPIArtBas EXCLUSIVE-LOCK WHERE                                                                  */
/*           bVPIArtBas.EkstVPILevNr = iEkstVPILevNr AND                                                       */
/*           bVPIArtBas.VareNr       = cVarenr NO-ERROR.                                                       */
/*       FIND bArtBas EXCLUSIVE-LOCK WHERE                                                                     */
/*           bArtBas.ArtikkelNr = dArtikkelnr NO-ERROR.                                                        */
/*       IF (AVAILABLE bVPIartBas AND                                                                          */
/*           AVAILABLE bArtBas) THEN DO:                                                                       */
/*           BUFFER-COPY bVPIArtBas EXCEPT Katalogpris ForhRab%                                                */
/*               TO bArtBas                                                                                    */
/*               ASSIGN                                                                                        */
/*               bArtBas.KatalogPris = bVPIArtBas.KatalogPris[1]                                               */
/*               bArtBas.ForhRab%    = bVPIArtBas.ForhRab%[1].                                                 */
/*                                                                                                             */
/*           RELEASE bVPIArtBas.                                                                               */
/*           RELEASE bArtBas.                                                                                  */
/*       END.                                                                                                  */
/*                                                                                                             */
/*       FIND FIRST bVPIArtPris EXCLUSIVE-LOCK WHERE                                                           */
/*           bVPIArtPris.EkstVPILevNr = iEkstVPILevNr AND                                                      */
/*           bVPIArtPris.VareNr       = cVarenr NO-ERROR.                                                      */
/*       FIND FIRST bArtPris EXCLUSIVE-LOCK WHERE                                                              */
/*           bArtPris.ArtikkelNr = dArtikkelnr NO-ERROR.                                                       */
/*       IF (AVAILABLE bVPIartPris AND                                                                         */
/*           AVAILABLE bArtPris) THEN DO:                                                                      */
/*           BUFFER-COPY bVPIArtPris EXCEPT Pris                                                               */
/*               TO bArtPris                                                                                   */
/*               ASSIGN                                                                                        */
/*               bArtPris.MvaKr[1] = bArtPris.Pris[1] * (bArtPris.Mva%[1] / (100 + bArtPris.Mva%[1]))          */
/*               bArtPris.DbKr[1]  = bArtPris.Pris[1] - bArtPris.MvaKr[1] - bArtPris.VareKost[1]               */
/*               bArtPRis.Db%[1]   = ROUND((bArtPris.DbKr[1] * 100)/ (bArtPris.Pris[1] - bArtPris.MvaKr[1]),2) */
/*               .                                                                                             */
/*                                                                                                             */
/*           RELEASE bVPIArtPris.                                                                              */
/*           RELEASE bArtPris.                                                                                 */
/*       END.                                                                                                  */
/*                                                                                                             */
/*   END. /* Transaction */                                                                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettElogg Procedure 
PROCEDURE SlettElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Elogg WHERE Elogg.Tabellnavn     = cTabellnavn AND
                         ELogg.EksterntSystem = cEksterntSystem AND
                         ELogg.Behandlet      = TRUE:
        DELETE Elogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

