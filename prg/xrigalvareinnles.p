&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xrigalvareinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn filen og splitter den i to filer.

    Author(s)   : Kennet Olausseon
    Created     : 17/7-05
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

                                                
DEFINE VARIABLE cOKVersion AS CHARACTER INIT "Rigal98,RIGAL02" NO-UNDO.

DEF VAR cRigalversion AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR cOrgNumFormat AS CHAR NO-UNDO.

DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR piLevNr       AS INT  NO-UNDO.
DEF VAR lEuKurs       AS DEC  NO-UNDO.
DEF VAR iProfilNr     AS INTE NO-UNDO.
DEF VAR cSalgsEnhListe AS CHAR NO-UNDO.
DEF VAR lN9kasse       AS LOG NO-UNDO. /* Om inläsningen finns i ett nucleus-miljö så skall vi exportera uppdaterade data */

DEF VAR cPFfil        AS CHAR NO-UNDO. /* för uppkoppling mot Infopos db, hårdkodat i 'Main' */

DEF STREAM InnFil.

/* DEFINE TEMP-TABLE TT_xxxx  NO-UNDO LIKE xxxx. */

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .

{windows.i}
{RigalVare.i}

DEFINE TEMP-TABLE tmpTT_RigalVare NO-UNDO LIKE TT_RigalVare.

DEFINE BUFFER BufTT_RigalVare FOR TT_RigalVare.
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufArtBas FOR ArtBas.
DEFINE TEMP-TABLE TT_Avdeling NO-UNDO LIKE Avdeling.
DEFINE TEMP-TABLE TT_HuvGr NO-UNDO LIKE HuvGr.
DEFINE TEMP-TABLE TT_VarGr NO-UNDO LIKE VarGr.
DEFINE TEMP-TABLE TT_ArtBas NO-UNDO LIKE ArtBas.
DEFINE TEMP-TABLE TT_ArtPris NO-UNDO LIKE ArtPris.
DEFINE TEMP-TABLE TT_PrisKo NO-UNDO LIKE PrisKo.
DEFINE TEMP-TABLE TT_StrekKode NO-UNDO LIKE StrekKode.
DEFINE TEMP-TABLE TT_LevBas NO-UNDO LIKE LevBas.
DEFINE TEMP-TABLE TT_Produsent NO-UNDO LIKE Produsent.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FinnArtNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinnArtNr Procedure 
FUNCTION FinnArtNr RETURNS DECIMAL
  ( INPUT dStrekkode AS DECIMAL,INPUT iTmpArtNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StreckkodFinns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StreckkodFinns Procedure 
FUNCTION StreckkodFinns RETURNS LOGICAL
  ( INPUT dNummer AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ToDate Procedure 
FUNCTION ToDate RETURNS DATE
  ( INPUT dAAAAMMDD AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 34.1
         WIDTH              = 112.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
PROCESS EVENTS.

/* MESSAGE PROGRAM-NAME(1) SKIP           */
/* "num-format" SKIP                      */
/* "convert-source"                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

ASSIGN cOrgNumFormat = SESSION:NUMERIC-FORMAT.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.
{syspara.i 2 1 1 lEuKurs DEC}
{syspara.i 2 4 10 cSalgsEnhListe}

IF lEuKurs = 0 THEN lEuKurs = 1.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = TRIM(VPIFilHode.Katalog,"~\") + "~\" + VPIFilHode.FilNavn
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    .
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILEvNr NO-ERROR.
ASSIGN
    piLevNr = EkstVPILev.LevNr.

/* Starter procedure bibliotek. */
IF NOT VALID-HANDLE(h_prisko) THEN
    RUN  prisko.p PERSISTENT SET h_prisko.

ASSIGN cPFfil = "db\InfoPosPro.pf".
IF SEARCH(cPFfil) <> ? THEN DO:
    RUN connectPro.p ("-pf " + cPFfil).
END.
IF CONNECTED("Infopos") THEN DO:
    RUN db2dbInfoposPro.p NO-ERROR.
    DISCONNECT Infopos NO-ERROR.
END.
/* Nucleus kasse */
IF CAN-FIND(FIRST kasse WHERE kasse.modellnr = 51 AND kasse.aktiv = TRUE) THEN
    ASSIGN lN9kasse = TRUE.

/* Här sätter vi profilnr = 1 tillsvidare */
ASSIGN iProfilnr = 1.

ASSIGN SESSION:NUMERIC-FORMAT = "American".

RUN LesInnFil.

ASSIGN SESSION:NUMERIC-FORMAT = cOrgNumFormat.

RUN BehandlaTT.

RUN OppdaterVarePris.

/* Här har vi eny funktion om vi finns i ett nucleus-miljö */
/* Alla varor skall hamna i ELOGG */
IF lN9kasse = TRUE AND CAN-FIND(FIRST Elogg WHERE ELogg.TabellNavn = "ArtBas" AND ELogg.EksterntSystem = "POS") THEN DO:
    RUN FixElogg.
    RUN batchExportNucleus.p.
END.

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

/* OUTPUT TO "c:\home\lindbak\ankommet\preem\TT_artbas.d".    */
/*     FOR EACH TT_ArtBas.                                    */
/*         EXPORT TT_ArtBas.                                  */
/*     END.                                                   */
/* OUTPUT CLOSE.                                              */
/* OUTPUT TO "c:\home\lindbak\ankommet\preem\TT_ArtPris.d".   */
/*     FOR EACH TT_ArtPris.                                   */
/*         EXPORT TT_ArtPris.                                 */
/*     END.                                                   */
/* OUTPUT CLOSE.                                              */
/* OUTPUT TO "c:\home\lindbak\ankommet\preem\TT_StrekKode.d". */
/*     FOR EACH TT_StrekKode.                                 */
/*         EXPORT TT_StrekKode.                               */
/*     END.                                                   */
/* OUTPUT CLOSE.                                              */


RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BehandlaTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandlaTT Procedure 
PROCEDURE BehandlaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR dArtikkelNr AS DECI NO-UNDO.
    FOR EACH TT_RigalVare:
        /* Test om Vargr och HuvGr finns annars lägg upp */
        RUN Behandla_Hg_Vg.
        /* Test om Leverantör, Produsent finns annars lägg upp */
        RUN Behandla_Lev_Prod.
    END.
    /* Här tar vi hand om varor som skall deaktiveras */
    FOR EACH bufTT_RigalVare WHERE bufTT_RigalVare.Flag = "U" OR bufTT_RigalVare.Flag = "S" OR bufTT_RigalVare.VType = "I" BREAK BY bufTT_RigalVare.nummer:
        IF FIRST-OF(bufTT_RigalVare.nummer) THEN
            RUN deAktiverArtBas(bufTT_RigalVare.nummer).
        DELETE bufTT_RigalVare.
    END.
    FOR EACH BufTT_RigalVare WHERE BufTT_RigalVare.Linknr <> 0:
        /* Vi måste se till att linkvarorna finns innan vi gör vanliga behandlingen */
        /* I Rigal är linknr EAN/Plu medan vi har Artikkelnr */
        ASSIGN dArtikkelNr = 0.
        /* Många varor kan ha samma linknr  */
        IF NOT CAN-FIND(TT_StrekKode WHERE TT_StrekKode.Kode = STRING(TT_Rigalvare.LinkNr)) THEN DO:
            FIND FIRST TT_RigalVare WHERE TT_Rigalvare.Salgskode3 = BufTT_RigalVare.Linknr NO-ERROR.
            IF AVAIL TT_RigalVare THEN
                ASSIGN dArtikkelNr = TT_Rigalvare.Nummer.
            ELSE DO:
                FIND FIRST TT_RigalVare WHERE TT_Rigalvare.Salgskode4 = BufTT_RigalVare.Linknr NO-ERROR.
                IF AVAIL TT_RigalVare THEN
                    ASSIGN dArtikkelNr = TT_Rigalvare.Nummer.
            END.
        END.
        /* Här skall vi skapa alla pantartiklar */
        IF dArtikkelNr > 0 THEN DO:
            FOR EACH TT_RigalVare WHERE TT_RigalVare.nummer = dArtikkelNr:
                RUN Behandla_ArtBas.
            END.
        END.
        /* Här behandlar vi artikel som har pant kopplad till sig */
        FOR EACH TT_RigalVare WHERE TT_RigalVare.nummer = BufTT_RigalVare.nummer:
            RUN Behandla_ArtBas.
        END.
    END.
    /* Här behandlar vi ArtBas, ArtPr5is och Strekkoder */
    FOR EACH bufTT_RigalVare WHERE bufTT_RigalVare.Linknr = 0:
        /*  */
        FOR EACH TT_RigalVare WHERE TT_RigalVare.nummer = BufTT_RigalVare.nummer:
            RUN Behandla_ArtBas.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Behandla_ArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Behandla_ArtBas Procedure 
PROCEDURE Behandla_ArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* TT_ArtBas    */
/* TT_ArtPris   */
/* TT_StrekKode */
   
   RELEASE TT_ArtBas.
   RELEASE TT_ArtPris.
   RELEASE TT_StrekKode.

   /* Här skapar vi en temp-table som skall ligga till grund för Ny eller uppdatering av existerande */
   /* vara oavsett vilken ändringstype vi har */
   IF NOT CAN-FIND(TT_ArtBas WHERE TT_ArtBas.ArtikkelNr = TT_RigalVare.nummer) THEN DO:
       RUN SkapaTT_ArtBas.
/*        IF TT_RigalVare.Flag = "M" THEN DO:                                                                        */
/*        /*  om varan inte finns bör vi lägga upp den men ingen hantering av MedlemsTilbud !!!! inte akturellt . */ */
/*        END.                                                                                                       */
/*        IF TT_RigalVare.Flag = "K" THEN DO: /* Kampanje */                                                 */
/*        END.                                                                                               */
/*        IF TT_RigalVare.Flag = "A" THEN DO: /* Slett ikke påbegynt kampanje, Avbryt pågående kampanje */ . */
/*        END.                                                                                               */
/*        IF TT_RigalVare.Flag = "N" THEN DO: /* Normal vare prisendring */ . */

       /* innan vi skall hantera kampanj måste vi hantera "K" som "N" */

       IF TT_RigalVare.Flag = "N" OR TT_RigalVare.Flag = "K" THEN ARTPRIS: DO: /* Normal vare prisendring */ .
           /* Om varan finns sedan tidigare och vi får kampanj så ändrar vi inte pris */
           IF TT_RigalVare.Flag = "K" AND CAN-FIND(artpris WHERE artpris.profilnr = iprofilnr AND artpris.artikkelnr = TT_RigalVare.Nummer) THEN
               LEAVE ARTPRIS.
           /* Skapa TT_ArtPris */
           RUN SkapaTT_Artpris.
       END.
   END.
   RUN SkapaTT_StrekKode.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Behandla_Hg_Vg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Behandla_Hg_Vg Procedure 
PROCEDURE Behandla_Hg_Vg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(HuvGr WHERE HuvGr.Hg = TT_RigalVare.Avdeling) THEN DO:
        CREATE HuvGr.
        ASSIGN HuvGr.AvdelingNr = 9
               HuvGr.Hg         = TT_RigalVare.Avdeling
               HuvGr.HgBeskr    = "Automatiskt fr. Rigal".
    END.
    FIND VarGr WHERE VarGr.Vg = TT_RigalVare.Hgr NO-LOCK NO-ERROR.
    IF NOT AVAIL VarGr THEN DO:
        FIND Moms WHERE Moms.MomsProc = TT_RigalVare.Mva% NO-LOCK NO-ERROR.
        IF AVAIL Moms THEN DO: /* Detta skall inte vara nödvändigt. Vi har hoppat över denna post om vi inte finner mva */
            CREATE VarGr.
            ASSIGN VarGr.Hg         = TT_RigalVare.Avdeling
                   VarGr.Vg         = TT_RigalVare.Hgr
                   VarGr.VgBeskr    = "Automatiskt fr. Rigal"
                   VarGr.Kost_Proc  = 65
                   VarGr.MomsKod    = Moms.MomsKod.
            CREATE vgkat.
            ASSIGN VgKat.Vg    = VarGr.vg
                   VgKat.KatNr = 1
                   VgKat.VgKat = 1 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE vgkat.
        END.
    END.
/*     Skall vi ta hänsyn till om vargr har bytt huvgr? */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Behandla_Lev_Prod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Behandla_Lev_Prod Procedure 
PROCEDURE Behandla_Lev_Prod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND LevBas WHERE Levbas.Levnr = TT_RigalVare.Leverandor NO-LOCK NO-ERROR.
    IF NOT AVAIL LevBas THEN DO:
        CREATE LevBas.
        ASSIGN LevBas.Levnr   = TT_RigalVare.Leverandor
               LevBas.levnamn = "Automatiskt fr. Rigal".
    END.
    FIND Produsent WHERE Produsent.ProdNr = TT_RigalVare.Produsent NO-LOCK NO-ERROR.
    IF NOT AVAIL Produsent THEN DO:
        CREATE Produsent.
        ASSIGN Produsent.Prodnr      = TT_RigalVare.Produsent
               Produsent.Beskrivelse = "Automatiskt fr. Rigal".
    END.
                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deAktiverArtbas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deAktiverArtbas Procedure 
PROCEDURE deAktiverArtbas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipArtikkelnr AS DECIMAL    NO-UNDO.
    FIND ArtBas WHERE ArtBas.ArtikkelNr = ipArtikkelNr EXCLUSIVE NO-ERROR.
    IF AVAIL ArtBas THEN DO:
       ASSIGN ArtBas.Aktivert = FALSE
              ArtBas.IKasse   = FALSE.
       RELEASE ArtBas.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndreArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreArtBas Procedure 
PROCEDURE EndreArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLopNr AS INTEGER    NO-UNDO.
    FIND ArtBas WHERE ArtBas.Artikkelnr = TT_ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
    IF TT_ArtBas.Alder           <> ArtBas.Alder           OR
       TT_ArtBas.AnbefaltPris    <> ArtBas.AnbefaltPris    OR
       TT_ArtBas.Aktivert        <> ArtBas.Aktivert        OR
       TT_ArtBas.AntIPakn        <> ArtBas.AntIPakn        OR
       TT_ArtBas.ArtSlag         <> ArtBas.ArtSlag         OR
       TT_ArtBas.Beskr           <> ArtBas.Beskr           OR
       TT_ArtBas.BongTekst       <> ArtBas.BongTekst       OR
       TT_ArtBas.Etikettekst1    <> ArtBas.Etikettekst1    OR
       TT_ArtBas.EtiLayout       <> ArtBas.EtiLayout       OR
       TT_ArtBas.Hg              <> ArtBas.Hg              OR
       TT_ArtBas.IKasse          <> ArtBas.IKasse          OR
       TT_ArtBas.KonvFaktEtikett <> ArtBas.KonvFaktEtikett OR
       TT_ArtBas.LevKod          <> ArtBas.LevKod          OR
/*        TT_ArtBas.LevNr           <> */
       TT_ArtBas.LinkVareNr      <> ArtBas.LinkVareNr      OR
       TT_ArtBas.Lokasjon        <> ArtBas.Lokasjon        OR
       TT_ArtBas.LokPris         <> ArtBas.LokPris         OR
       TT_ArtBas.ManRabIKas      <> ArtBas.ManRabIKas      OR
       TT_ArtBas.OPris           <> ArtBas.OPris           OR
       TT_ArtBas.Pant            <> ArtBas.Pant            OR
       TT_ArtBas.ProdNr          <> ArtBas.ProdNr          OR
       TT_ArtBas.SalgsEnhet      <> ArtBas.SalgsEnhet      OR
       TT_ArtBas.Vg              <> ArtBas.Vg              THEN DO:
        
        IF TT_ArtBas.Vg           <> ArtBas.Vg THEN DO:
            FIND LAST bufArtBas WHERE bufArtBas.Vg = TT_ArtBas.Vg AND bufartbas.lopnr <> ? USE-INDEX vglopnr NO-LOCK NO-ERROR.
            ASSIGN iLopNr = IF AVAIL bufArtBas THEN bufArtBas.Lopnr + 1 ELSE 1.
/*              MESSAGE "TT " TT_ArtBas.Vg  " " tt_ArtBas.lopnr SKIP */
/*                      "ART" Artbas.vg     " " artbas.lopnr    SKIP */
/*                       iLopnr                                      */
/*                  VIEW-AS ALERT-BOX INFO BUTTONS OK.               */
        END.
        FIND CURRENT ArtBas EXCLUSIVE NO-WAIT.
        IF AVAIL ArtBas THEN DO:
            ASSIGN ArtBas.Alder           = TT_ArtBas.Alder          
                   ArtBas.AnbefaltPris    = TT_ArtBas.AnbefaltPris   
                   ArtBas.Aktivert        = TT_ArtBas.Aktivert       
                   ArtBas.AntIPakn        = TT_ArtBas.AntIPakn       
                   ArtBas.ArtSlag         = TT_ArtBas.ArtSlag        
                   ArtBas.Beskr           = TT_ArtBas.Beskr          
                   ArtBas.BongTekst       = TT_ArtBas.BongTekst      
                   ArtBas.Etikettekst1    = TT_ArtBas.Etikettekst1   
                   ArtBas.EtiLayout       = TT_ArtBas.EtiLayout      
                   ArtBas.Hg              = TT_ArtBas.Hg             
                   ArtBas.IKasse          = TT_ArtBas.IKasse         
                   ArtBas.KonvFaktEtikett = TT_ArtBas.KonvFaktEtikett
                   ArtBas.LevKod          = TT_ArtBas.LevKod
           /*      ArtBas.LevNr           = TT_*/                 
                   ArtBas.LinkVareNr      = TT_ArtBas.LinkVareNr     
                   ArtBas.Lokasjon        = TT_ArtBas.Lokasjon       
                   ArtBas.LokPris         = TT_ArtBas.LokPris        
                   ArtBas.ManRabIKas      = TT_ArtBas.ManRabIKas     
                   ArtBas.OPris           = TT_ArtBas.OPris          
                   ArtBas.Pant            = TT_ArtBas.Pant           
                   ArtBas.ProdNr          = TT_ArtBas.ProdNr         
                   ArtBas.SalgsEnhet      = TT_ArtBas.SalgsEnhet     
                   ArtBas.Vg              = TT_ArtBas.Vg
                   ArtBas.LopNr           = IF iLopNr > 0 THEN iLopNr ELSE ArtBas.LopNr NO-ERROR.
        END.
        RELEASE ArtBas.
    END.
    FIND FIRST TT_ArtPris OF TT_ArtBas NO-ERROR.
    IF AVAIL TT_ArtPris THEN DO:
        FIND ArtPris WHERE ArtPris.ArtikkelNr = TT_ArtPris.Artikkelnr AND ArtPris.profilnr = TT_Artpris.profilnr EXCLUSIVE NO-WAIT NO-ERROR.
        IF NOT AVAIL ArtPris THEN DO:
            CREATE ArtPris.
            BUFFER-COPY TT_ArtPris TO ArtPris NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE ArtPris.
        END.
        ELSE DO:
            BUFFER-COPY TT_ArtPris TO ArtPris NO-ERROR.
        END.
        RELEASE ArtPris.
        FOR EACH TT_StrekKode OF TT_ArtBas:
            FIND strekkode WHERE strekkode.kode = TT_strekkode.kode EXCLUSIVE NO-ERROR.
            IF AVAIL strekkode AND strekkode.artikkelnr <> TT_strekkode.artikkelNr THEN
                DELETE strekkode.
            CREATE Strekkode.
            BUFFER-COPY TT_strekkode TO strekkode NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE strekkode.
        END.
        RELEASE Strekkode.
    END.

/*            TT_ArtBas.AnonseArtikkel */
/*            TT_ArtBas.anv-id */                           
/*            TT_ArtBas.BehKode */
/*            TT_ArtBas.BestForslag */
/*            TT_ArtBas.BrukerID */
/*            TT_ArtBas.Dato1gSendtHk */
/*            TT_ArtBas.DivInfo[1]       /* - TT_ArtBas.DivInfo[20] */ */
/*  ???          TT_ArtBas.Etikett */
/*            TT_ArtBas.Farg */
/*            TT_ArtBas.foder-id */
/*            TT_ArtBas.forhRab%     */
/*            TT_ArtBas.FrittTillegg */
/*            TT_ArtBas.GarantiKl    */
/*            TT_ArtBas.HKVareId */
/*            TT_ArtBas.HovedModellFarge */
/*            TT_ArtBas.IndividType */
/*            TT_ArtBas.inner-id    */
/*            TT_ArtBas.inn_dato */
/*  ???          TT_ArtBas.KatalogPris */
/*            TT_ArtBas.KjentPaHK */
/*            TT_ArtBas.Klack */
/*            TT_ArtBas.Kommentar */
/*  ???          TT_ArtBas.KundeRabatt */
/*            TT_ArtBas.LapTop */
/*            TT_ArtBas.last-id */
/*            TT_ArtBas.LevDato1 */
/*            TT_ArtBas.LevDato2 */
/*            TT_ArtBas.LevDato3 */
/*            TT_ArtBas.LevDato4 */
/*            TT_ArtBas.LevFargKod */
/*            TT_ArtBas.LevVareTekst     = */
/*            TT_ArtBas.LinjeMerknad */
/*            TT_ArtBas.MatKod */
/* ???           TT_ArtBas.Mengde           = TT_RigalVare. */
/*            TT_ArtBas.ModellFarge */
/*            TT_ArtBas.Notat       */
/*            TT_ArtBas.OLLager */
/*            TT_ArtBas.ov-id */
/*            TT_ArtBas.Pakke   */
/*            TT_ArtBas.Pakkenr */
/*            TT_ArtBas.PrisGrpNr */
/*            TT_ArtBas.ProvKod */
/*            TT_ArtBas.RabKod */
/*            TT_ArtBas.SaSong */
/*            TT_ArtBas.SattPaKampanje */
/*            TT_ArtBas.SentralBestilling */
/*            TT_ArtBas.SlaskArtikkelNr */
/*            TT_ArtBas.Slasket */
/*            TT_ArtBas.slit-id */
/*            TT_ArtBas.StrKode1 */
/*            TT_ArtBas.StrKode2 */
/*            TT_ArtBas.supRab% */
/*            TT_ArtBas.Tilv-Land */
/*            TT_ArtBas.valkod    */
/*            TT_ArtBas.VareFakta */
/*            TT_ArtBas.VgKat */
/*            TT_ArtBas.VMId */
/*            TT_ArtBas.VPIBildeKode */
/*            TT_ArtBas.VPIDato      */
               .
/*     FOR EACH TT_Artpris:   */
/*     END.                   */
/*     FOR EACH TT_StrekKode: */
/*     END.                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixElogg Procedure 
PROCEDURE FixElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_RigalVare BREAK BY TT_RigalVare.Nummer:
        IF FIRST-OF(TT_RigalVare.Nummer) THEN DO:
            IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                                    ELogg.EksterntSystem = "POS"    AND
                                    ELogg.Verdier        = STRING(TT_RigalVare.Nummer)) THEN
                NEXT.
            FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_RigalVare.Nummer NO-LOCK NO-ERROR.
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "ArtBas"
                   ELogg.EksterntSystem = "POS"   
                   ELogg.Verdier        = STRING(TT_RigalVare.Nummer)
                   ELogg.EndringsType   = IF AVAIL ArtBas AND ArtBas.iKasse = TRUE THEN 1 ELSE 3 
                   ELogg.Behandlet      = FALSE.
            RELEASE ELogg.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Procedure 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAMETER wStorl AS CHAR NO-UNDO.

  DEF VAR wDecimaler AS CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  ASSIGN
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 OR
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  IF index(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genEAN Procedure 
PROCEDURE genEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT PARAMETER cStorl       AS CHAR NO-UNDO.

  DEF VAR cKode AS CHAR NO-UNDO.

  FIND StrKonv WHERE StrKonv.Storl = cStorl USE-INDEX Storl NO-LOCK NO-ERROR.
  IF NOT AVAIL StrKonv THEN
      RETURN.
  /* Finnes det strekkode på størrrelsen fra før, skal vi ikke legge opp ny. */
  IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                              StrekKode.KodeType = 1 AND
                              StrekKode.StrKode  = StrKonv.StrKode
                          /*  AND StrekKode.Kode BEGINS "02" */
                              ) THEN RETURN.

  ASSIGN cKode = "02" + STRING(ArtBas.ArtikkelNr,"9999999") + STRING(StrKonv.StrKode,"999")
         cKode = FixChk(cKode).

  CREATE StrekKode.
  ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
         StrekKode.Kode       = cKode
         StrekKode.KodeType   = 1 /* använd inte iKodeType, vi kan ha 0 */
         StrekKode.StrKode    = StrKonv.StrKode 
         StrekKode.VareId     = ArtBas.ArtikkelNr
      NO-ERROR.
  /* TN Koden kan finnes fra før - 02 koder gav feilmelding. */
  IF ERROR-STATUS:ERROR THEN
  DO:
      IF AVAILABLE StrekKode THEN
          DELETE StrekKode.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEF VAR iStrKode  AS INT  NO-UNDO.
  DEF VAR pcStorl   AS CHAR NO-UNDO.
  DEF VAR dEngros   AS DECI NO-UNDO.
  DEF VAR dNetto    AS DECI NO-UNDO.
  DEF VAR dUtpris   AS DECI NO-UNDO.
  DEF VAR dVeil     AS DECI NO-UNDO.
  DEF VAR dMva%     AS DECI NO-UNDO.
  DEF VAR dUtprisUmva AS DECI NO-UNDO.
  DEF VAR cEAN_Plu1   AS CHAR NO-UNDO.
  DEF VAR cEAN_Plu2   AS CHAR NO-UNDO.
  DEF VAR dArtikkelNr AS DECI NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH TT_RigalVare:
      DELETE TT_RigalVare.
  END.

  RUN TellOppLinjer.
  IF RETURN-VALUE = "FEIL" THEN DO:
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = 1
        tt_Error.Tekst   = "** Feil på linje 1. IKKE gyldig Rigal-header".
        .
      RETURN.
  END.

  ASSIGN iAntLinjer = 0.
  INPUT STREAM InnFil FROM VALUE(cFilNavn) CONVERT SOURCE "IBM850" NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje. /* Läs bort första raden! Rigal Header, testet om Rigal i TellOppLinjer */
  LESERLINJER:
  REPEAT:
    /* Record buffer å lese inn filen i */
    CREATE tmpTT_RigalVare.
    /* Leser linje fra filen */
    IMPORT STREAM InnFil DELIMITER "," tmpTT_RigalVare NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN
    DO:
      ASSIGN iAntLinjer = iAntLinjer + 1.
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
      END.
      DELETE tmpTT_RigalVare.
      NEXT LESERLINJER.
    END.
    IF NOT tmpTT_RigalVare.Kode = "VAR" THEN DO:
        ASSIGN iAntLinjer = iAntLinjer + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Kode <> 'VAR' " + tmpTT_RigalVare.Kode.
        DELETE tmpTT_RigalVare.
        NEXT LESERLINJER.
    END.
    ELSE IF tmpTT_RigalVare.Nummer = 0 THEN DO:
        ASSIGN iAntLinjer = iAntLinjer + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Artikkelnr = '0'. " + STRING(tmpTT_RigalVare.Nummer).
        DELETE tmpTT_RigalVare.
        NEXT LESERLINJER.
    END.
    ELSE IF (tmpTT_RigalVare.Flag = "U" OR tmpTT_RigalVare.VType = "I" OR tmpTT_RigalVare.VType = "S") AND NOT CAN-FIND(Artbas WHERE ArtBas.Artikkelnr = tmpTT_RigalVare.Nummer) THEN DO:
        /* Här har vi fått en utmelding 'U' eller inaktivering "I" på en vara vi inte har sedan tidigare  */
        ASSIGN iAntLinjer = iAntLinjer + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Borttag inte funnen art. " + STRING(tmpTT_RigalVare.Nummer).
        DELETE tmpTT_RigalVare.
        NEXT LESERLINJER.
    END.
/* DISP iantlinjer WITH FRAME a 20 DOWN. */
/* PAUSE 0.                              */
/* DOWN WITH FRAME iantlinjer.           */
    KALKYLE:
    DO:
/*         IF tmpTT_RigalVare.utpris = 0 THEN DO:                                                                                  */
/*             ASSIGN iAntLinjer = iAntLinjer + 1.                                                                                 */
/*             CREATE tt_Error.                                                                                                    */
/*             ASSIGN                                                                                                              */
/*               tt_Error.LinjeNr = piAntFeil                                                                                      */
/*               tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Utpris = '0'. " + STRING(tmpTT_RigalVare.Nummer). */
/*             DELETE tmpTT_RigalVare.                                                                                             */
/*             NEXT LESERLINJER.                                                                                                   */
/*         END.                                                                                                                    */
        ASSIGN dUtprisUmva = ROUND(dUtpris / (1 + (tmpTT_RigalVare.Mva%)),2).

        IF tmpTT_RigalVare.Engros = 0 THEN 
            tmpTT_RigalVare.Engros = tmpTT_RigalVare.Netto.
        IF tmpTT_RigalVare.Engros = 0 THEN DO:
            /* räkna ut kalkyle m h a kostprocent på VarGr */
            FIND vargr WHERE vargr.vg = tmpTT_RigalVare.hgr NO-LOCK NO-ERROR.
            ASSIGN tmpTT_RigalVare.Engros = ROUND(dUtprisUmva * (IF AVAIL VarGr THEN VarGr.Kost_Proc / 100 ELSE 65 / 100),2)
                   tmpTT_RigalVare.Netto  = tmpTT_RigalVare.Engros.
        END.
        ASSIGN tmpTT_RigalVare.Netto  = IF tmpTT_RigalVare.Netto <= 0 OR tmpTT_RigalVare.Netto > tmpTT_RigalVare.Engros THEN 
                                           tmpTT_RigalVare.Engros ELSE tmpTT_RigalVare.Netto.
/*         IF dUtprisUmva < tmpTT_RigalVare.Netto THEN DO:                                                                            */
/*             ASSIGN iAntLinjer = iAntLinjer + 1.                                                                                    */
/*             CREATE tt_Error.                                                                                                       */
/*             ASSIGN                                                                                                                 */
/*               tt_Error.LinjeNr = piAntFeil                                                                                         */
/*               tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Inpris > utpris. " + STRING(tmpTT_RigalVare.Nummer). */
/*             DELETE tmpTT_RigalVare.                                                                                                */
/*             NEXT LESERLINJER.                                                                                                      */
/*         END.                                                                                                                       */
    END.
    /* Vi får artikelnr i .nummer */
    /* Test om Plu/EAN kommer in och finns sedan tidigare och har samma artikkelnr */
/*     DO:                                                                                                        */
/*         ASSIGN cEAN_Plu1   = STRING(TT_RigalVare.SalgsKode3)                                                   */
/*                cEAN_Plu2   = STRING(TT_RigalVare.SalgsKode4).                                                  */
/*         /* Eventuell konverteering av 21..., 7388... */                                                        */
/*         RELEASE StrekKode.                                                                                     */
/*         IF cEAN_Plu1 <> "0" THEN                                                                               */
/*             FIND Strekkode WHERE Strekkode.Kode = cEAN_Plu1 NO-LOCK NO-ERROR.                                  */
/*         IF AVAIL StrekKode AND StrekKode.ArtikkelNr <> TT_RigalVare.Nummer THEN DO:                            */
/*             /* Här har streckkode dykt upp på annan vara och inläsningen skall förkastas  */                   */
/*             ASSIGN iAntLinjer = iAntLinjer + 1.                                                                */
/*             CREATE tt_Error.                                                                                   */
/*             ASSIGN                                                                                             */
/*               tt_Error.LinjeNr = piAntFeil                                                                     */
/*               tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " EAN på annan art. " + cEAN_Plu1. */
/*             DELETE tmpTT_RigalVare.                                                                            */
/*             NEXT LESERLINJER.                                                                                  */
/*         END.                                                                                                   */
/*         IF cEAN_Plu2 <> "0" THEN                                                                               */
/*             FIND Strekkode WHERE Strekkode.Kode = cEAN_Plu2 NO-LOCK NO-ERROR.                                  */
/*         IF AVAIL StrekKode AND StrekKode.ArtikkelNr <> TT_RigalVare.Nummer THEN DO:                            */
/*             /* Här har streckkode dykt upp på annan vara och inläsningen skall förkastas  */                   */
/*             ASSIGN iAntLinjer = iAntLinjer + 1.                                                                */
/*             CREATE tt_Error.                                                                                   */
/*             ASSIGN                                                                                             */
/*               tt_Error.LinjeNr = piAntFeil                                                                     */
/*               tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " EAN på annan art. " + cEAN_Plu2. */
/*             DELETE tmpTT_RigalVare.                                                                            */
/*             NEXT LESERLINJER.                                                                                  */
/*         END.                                                                                                   */
/*     END.                                                                                                       */
    IF NOT CAN-FIND(Moms WHERE Moms.MomsProc = tmpTT_RigalVare.Mva%) THEN DO:
        ASSIGN iAntLinjer = iAntLinjer + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " Finner inte Momskod för " + STRING(tmpTT_RigalVare.Mva%).
        DELETE tmpTT_RigalVare.
        NEXT LESERLINJER.
    END.
    ASSIGN iAntLinjer = iAntLinjer + 1.
    RUN ValiderInput (INPUT-OUTPUT piAntFeil).
    IF RETURN-VALUE = "FEIL" THEN DO:
        NEXT LESERLINJER.
    END.

    CREATE TT_RigalVare.
    BUFFER-COPY tmpTT_RigalVare TO TT_RigalVare NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
            .
      END.
      DELETE tmpTT_RigalVare.
      DELETE TT_RigalVare.
      NEXT LESERLINJER.
    END.
    ELSE
        DELETE tmpTT_RigalVare.

/*     ASSIGN TT_RigalVare.ArtNr = FinnArtNr(TT_RigalVare.Nummer,iAntLinjer * -1). */
    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 3
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* PAKKSEDDELFILEN */
  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtBas Procedure 
PROCEDURE NyArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLopNr AS INTEGER    NO-UNDO.
    FIND LAST bufArtBas WHERE bufArtBas.Vg = TT_ArtBas.Vg AND bufArtBas.Lopnr <> ? USE-INDEX vglopnr NO-LOCK NO-ERROR.
    ASSIGN iLopNr = IF AVAIL bufArtBas THEN bufArtBas.Lopnr + 1 ELSE 1 NO-ERROR.
    CREATE ArtBas.
    BUFFER-COPY TT_ArtBas TO ArtBas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DELETE ArtBas.
        RETURN.
    END.
    ASSIGN ArtBas.LopNr = iLopnr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DELETE ArtBas.
        RETURN.
    END.
    RELEASE ArtBas.
    FIND FIRST TT_ArtPris OF TT_ArtBas NO-ERROR.
    IF AVAIL TT_ArtPris THEN DO:
        CREATE ArtPris.
        BUFFER-COPY TT_ArtPris TO ArtPris NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            DELETE ArtPris.
            RETURN.
        END.
        RELEASE ArtBas.
        FOR EACH TT_StrekKode OF TT_ArtBas:
            FIND strekkode WHERE strekkode.kode = TT_strekkode.kode NO-ERROR.
            IF AVAIL strekkode THEN
                DELETE strekkode.
            CREATE Strekkode.
            BUFFER-COPY TT_strekkode TO strekkode NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE strekkode.
        END.
        RELEASE Strekkode.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterVarePris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterVarePris Procedure 
PROCEDURE OppdaterVarePris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ArtBas:
        IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtBas.Artikkelnr) THEN
            RUN EndreArtBas.
        ELSE
            RUN NyArtBas.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettArtBas Procedure 
PROCEDURE OpprettArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT_ArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_ArtBas Procedure 
PROCEDURE SkapaTT_ArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSalgsEnhet AS CHARACTER    NO-UNDO.
    IF NOT CAN-FIND(TT_ArtBas WHERE TT_ArtBas.ArtikkelNr = TT_RigalVare.Nummer) THEN DO:
        RELEASE TT_StrekKode.
        RELEASE StrekKode.
        FIND VarGr WHERE VarGr.Vg = TT_RigalVare.Hgr NO-LOCK NO-ERROR.
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_RigalVare.Nummer NO-LOCK NO-ERROR.
        IF TT_RigalVare.LinkNr <> 0 THEN DO:
            FIND FIRST TT_Strekkode WHERE TT_StrekKode.artikkelnr = TT_RigalVare.Nummer NO-ERROR.
            IF NOT AVAIL TT_Strekkode THEN
                FIND FIRST TT_Strekkode WHERE TT_StrekKode.artikkelnr = TT_RigalVare.Nummer NO-ERROR.
        END.
/*         MESSAGE TT_RigalVare.Nummer            */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        ASSIGN cSalgsEnhet = IF TT_RigalVare.Pakn > 0 AND CAN-DO(cSalgsEnhListe,STRING(TT_RigalVare.Pakn)) THEN 
                  ENTRY(TT_RigalVare.Pakn,cSalgsEnhListe) ELSE ENTRY(1,cSalgsEnhListe).
        CREATE TT_ArtBas.
        ASSIGN TT_ArtBas.AktivAv    = userid("skotex")   
               TT_ArtBas.AktivDato  = TODAY
               TT_ArtBas.Aktivert   = TT_RigalVare.VType <> "I"
               TT_ArtBas.Alder      = TT_RigalVare.Idkrav /* 0=Ingen,1=Alkohol,2=Tobak */
               TT_ArtBas.AnbefaltPris = TT_RigalVare.Utpris
    /*            TT_ArtBas.AnonseArtikkel */
               TT_ArtBas.AntIPakn       = TT_RigalVare.Antpkn
    /*            TT_ArtBas.anv-id */
               TT_ArtBas.ArtikkelNr     = TT_RigalVare.Nummer
               TT_ArtBas.ArtSlag        = IF TT_RigalVare.Vtype = "O" THEN 4 ELSE 3  /* 3=Ikke Lager u/str,8=Pantevare */
    /*            TT_ArtBas.BehKode */
               TT_ArtBas.Beskr          = SUBSTR(TT_RigalVare.Varetek,1,30)
    /*            TT_ArtBas.BestForslag */
               TT_ArtBas.BildeIKasse = FALSE
               TT_ArtBas.BildNr      = 0
               TT_ArtBas.BongTekst   = SUBSTR(TT_RigalVare.Bongtek,1,20)
    /*            TT_ArtBas.BrukerID */
    /*            TT_ArtBas.Dato1gSendtHk */
    /*            TT_ArtBas.DivInfo[1]       /* - TT_ArtBas.DivInfo[20] */ */
    /*  ???          TT_ArtBas.Etikett */
               TT_ArtBas.Etikettekst1     = TT_RigalVare.Etitekst1
               TT_ArtBas.EtiLayout        = TT_RigalVare.Etikett
    /*            TT_ArtBas.Farg */
    /*            TT_ArtBas.foder-id */
    /*            TT_ArtBas.forhRab%     */
    /*            TT_ArtBas.FrittTillegg */
    /*            TT_ArtBas.GarantiKl    */
               TT_ArtBas.Hg               = IF AVAIL VarGr THEN VarGr.hg ELSE 1
               TT_ArtBas.HKArtikkelNr     = TT_RigalVare.Nummer
               TT_ArtBas.HkStyrt          = TRUE
    /*            TT_ArtBas.HKVareId */
    /*            TT_ArtBas.HovedModellFarge */
               TT_ArtBas.IKasse           = TT_RigalVare.VType <> "I"
    /*            TT_ArtBas.IndividType */
    /*            TT_ArtBas.inner-id    */
    /*            TT_ArtBas.inn_dato */
    /*  ???          TT_ArtBas.KatalogPris */
               TT_ArtBas.KjedeVare        = TT_RigalVare.Laggros = "J"
    /*            TT_ArtBas.KjentPaHK */
    /*            TT_ArtBas.Klack */
    /*            TT_ArtBas.Kommentar */
               TT_ArtBas.KonvFaktEtikett  = TT_Rigalvare.Konvfak
    /*  ???          TT_ArtBas.KundeRabatt */
               TT_ArtBas.lager            = FALSE
    /*            TT_ArtBas.LapTop */
    /*            TT_ArtBas.last-id */
    /*            TT_ArtBas.LevDato1 */
    /*            TT_ArtBas.LevDato2 */
    /*            TT_ArtBas.LevDato3 */
    /*            TT_ArtBas.LevDato4 */
    /*            TT_ArtBas.LevFargKod */
               TT_ArtBas.LevKod           = STRING(TT_RigalVare.BestNr)
               TT_ArtBas.LevNr            = TT_RigalVare.Grossist
    /*            TT_ArtBas.LevVareTekst     = */
    /*            TT_ArtBas.LinjeMerknad */
               TT_ArtBas.LinkVareNr       = IF AVAIL TT_Strekkode THEN TT_StrekKode.artikkelnr ELSE IF AVAIL Strekkode THEN StrekKode.artikkelnr ELSE 0 /* EAN/Plu på linkvara */
               TT_ArtBas.Lokasjon         = TT_RigalVare.Lok_i_butikk
               TT_ArtBas.LokPris          = FALSE
               TT_ArtBas.LopNr            = IF AVAIL ArtBas AND ArtBas.Vg = TT_RigalVare.Hgr THEN ArtBas.LopNr ELSE ? /* TT_ArtBas.LopNr */
               TT_ArtBas.ManRabIKas       = FALSE
    /*            TT_ArtBas.MatKod */
    /* ???           TT_ArtBas.Mengde           = TT_RigalVare. */
    /*            TT_ArtBas.ModellFarge */
    /*            TT_ArtBas.Notat       */
               TT_ArtBas.ny_dato          = IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = TT_RigalVare.Nummer) THEN TODAY ELSE ?
    /*            TT_ArtBas.OLLager */
               TT_ArtBas.OPris            = TT_RigalVare.Vtype = "O"
    /*            TT_ArtBas.ov-id */
    /*            TT_ArtBas.Pakke   */
    /*            TT_ArtBas.Pakkenr */
               TT_ArtBas.Pant             = TT_ArtBas.LinkVareNr > 0
    /*            TT_ArtBas.PrisGrpNr */
               TT_ArtBas.ProdNr           = TT_RigalVare.Produsent
    /*            TT_ArtBas.ProvKod */
    /*            TT_ArtBas.RabKod */
                  TT_ArtBas.SalgsEnhet    = cSalgsEnhet
    /*            TT_ArtBas.SaSong */
    /*            TT_ArtBas.SattPaKampanje */
    /*            TT_ArtBas.SentralBestilling */
    /*            TT_ArtBas.SlaskArtikkelNr */
    /*            TT_ArtBas.Slasket */
    /*            TT_ArtBas.slit-id */
               TT_ArtBas.Storrelser        = FALSE /* NOT TT_ArtBas.OPris */
    /*            TT_ArtBas.StrKode1 */
    /*            TT_ArtBas.StrKode2 */
               TT_ArtBas.StrTypeID         = 2
    /*            TT_ArtBas.supRab% */
    /*            TT_ArtBas.Tilv-Land */
    /*            TT_ArtBas.valkod    */
    /*            TT_ArtBas.VareFakta */
               TT_ArtBas.Vg                = TT_RigalVare.Hgr
    /*            TT_ArtBas.VgKat */
    /*            TT_ArtBas.VMId */
    /*            TT_ArtBas.VPIBildeKode */
    /*            TT_ArtBas.VPIDato      */
                   .
/* IF TT_RigalVare.Nummer = 900101 OR (TT_ArtBas.Vg = 298 AND TT_ArtBas.Lopnr = 1) THEN */
/*     MESSAGE TT_Rigalvare.nummer SUBSTR(TT_RigalVare.Varetek,1,30)                    */
/*     SKIP TT_Artbas.Vg TT_ArtBas.Lopnr TT_ArtBas.artikkelnr TT_Artbas.beskr           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */
/*                    NO-ERROR.                                       */
/* IF ERROR-STATUS:ERROR THEN DO:                                     */
/*     MESSAGE TT_artBas.Artikkelnr TT_Artbas.vg TT_Artbas.lopnr SKIP */
/*     tt_rigalvare.nummer                                            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                         */
/*         DELETE TT_artbas.                                          */
/*     END.                                                           */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT_ArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_ArtPris Procedure 
PROCEDURE SkapaTT_ArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE INPUT  PARAMETER iType       AS INTEGER NO-UNDO. */
    DEFINE        VARIABLE  dUtprisUmva AS DECI    NO-UNDO.
    DEFINE VARIABLE iPrisType AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dEngros AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dNetto  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dDbKr   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dDb%    AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRabKr  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRab%   AS DECIMAL    NO-UNDO.
    /* Här skall vi ta hänsyn till kampanje och göra priskö i framtiden */
    ASSIGN iPrisType = 1.
    ASSIGN dUtprisUmva = ROUND(TT_RigalVare.Utpris / (1 + (TT_RigalVare.Mva% / 100)),2)
           dRabKr      = TT_RigalVare.Engros - TT_RigalVare.Netto
           dRab%       = IF dRabKr > 0 THEN ROUND(dRabKr / TT_RigalVare.Engros * 100,2) ELSE 0
           dDbKr       = dUtprisUmva - TT_RigalVare.Netto
           dDb%        = IF dDbKr <> 0 THEN ROUND(dDbKr / dUtprisUmva * 100,2) ELSE 0.

    FIND Moms WHERE Moms.MomsProc = TT_RigalVare.Mva% NO-LOCK NO-ERROR.
    CREATE TT_Artpris.
    ASSIGN TT_ArtPris.AktivFraDato            = TODAY
           TT_ArtPris.AktivFraTid             = 1
           TT_ArtPris.ArtikkelNr              = TT_RigalVare.nummer
           TT_ArtPris.DB%[iPrisType]          = dDb%
           TT_ArtPris.DBKr[iPrisType]         = dDbKr
/*            TT_ArtPris.EuroManuel              = */
/*            TT_ArtPris.EuroPris[iPrisType]     = */
           TT_ArtPris.InnkjopsPris[iPrisType] = TT_RigalVare.Engros
           TT_ArtPris.MomsKod[iPrisType]      = IF AVAIL Moms THEN Moms.MomsKod ELSE 0
           TT_ArtPris.Mva%[iPrisType]         = TT_RigalVare.Mva%
           TT_ArtPris.MvaKr[iPrisType]        = TT_RigalVare.Utpris - dUtprisUmva
           TT_ArtPris.Pris[iPrisType]         = TT_RigalVare.Utpris
           TT_ArtPris.ProfilNr                = iProfilNr
           TT_ArtPris.Rab1%[iPrisType]        = dRab%
           TT_ArtPris.Rab1Kr[iPrisType]       = dRabKr
/*            TT_ArtPris.Rab2%[iPrisType]        = */
/*            TT_ArtPris.Rab2Kr[iPrisType]       = */
/*            TT_ArtPris.Rab3%[iPrisType]        = */
/*            TT_ArtPris.Rab3Kr[iPrisType]       = */
           TT_ArtPris.Tilbud                  = FALSE
/*            TT_ArtPris.TilbudFraDato           = */
/*            TT_ArtPris.TilbudFraTid            = */
/*            TT_ArtPris.TilbudTilDato           = */
/*            TT_ArtPris.TilbudTilTid            = */
/*            TT_ArtPris.TilbudTimeStyrt         = */
           TT_ArtPris.ValPris[iPrisType]      = TT_RigalVare.Engros
           TT_ArtPris.VareKost[iPrisType]     = TT_RigalVare.Netto
           NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE TT_artpris.artikkelnr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    DELETE TT_artpris.
END.
/*     CREATE TT_PrisKo.                                                            */
/*     ASSIGN TT_PrisKo.AktiveresDato = ToDate(TT_RigalVare.Fradato)                */
/*            TT_PrisKo.AktiveresTid  = 1                                           */
/*            TT_PrisKo.Aktivert      =                                             */
/*            TT_PrisKo.ArtikkelNr    = TT_RigalVare.Nummer                         */
/*            TT_PrisKo.DB%           =                                             */
/*            TT_PrisKo.DBKr          =                                             */
/*            TT_PrisKo.DivKost%      =                                             */
/*            TT_PrisKo.DivKostKr     =                                             */
/*            TT_PrisKo.EndringsType  =                                             */
/*            TT_PrisKo.ETid          =                                             */
/*            TT_PrisKo.EuroManuel    =                                             */
/*            TT_PrisKo.EuroPris      =                                             */
/*            TT_PrisKo.Frakt         =                                             */
/*            TT_PrisKo.Frakt%        =                                             */
/*            TT_PrisKo.GyldigTilDato =                                             */
/*            TT_PrisKo.GyldigTilTid  =                                             */
/*            TT_PrisKo.InnkjopsPris  =                                             */
/*            TT_PrisKo.KoNummer      =                                             */
/*            TT_PrisKo.LevNr         =                                             */
/*            TT_PrisKo.MomsKod       =                                             */
/*            TT_PrisKo.Mva%          =                                             */
/*            TT_PrisKo.MvaKr         =                                             */
/*            TT_PrisKo.Pris          = TT_RigalVare.Utpris                         */
/*            TT_PrisKo.ProfilNr      = 1                                           */
/*            TT_PrisKo.Rab1%         =                                             */
/*            TT_PrisKo.Rab1Kr        =                                             */
/*            TT_PrisKo.Rab2%         =                                             */
/*            TT_PrisKo.Rab2Kr        =                                             */
/*            TT_PrisKo.Rab3%         =                                             */
/*            TT_PrisKo.Rab3Kr        =                                             */
/*            TT_PrisKo.Tilbud        =                                             */
/*            TT_PrisKo.Timestyrt     =                                             */
/*            TT_PrisKo.TYPE          = iType /* 1 = Normal, 2 = Tilbud-kampanje */ */
/*            TT_PrisKo.ValPris       =                                             */
/*            TT_PrisKo.VareKost      =.                                            */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT_Strekkode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_Strekkode Procedure 
PROCEDURE SkapaTT_Strekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Vi skall skapa TT även om strekkode finns i Strekkode. Vi gör en test vid uppdatering om Strekkode finns och  */
/* om den bytt artikkelnr så skall vi ta bort den från den gamla artikkeln */
/*        NOT CAN-FIND(StrekKode WHERE StrekKode.Kode = STRING(TT_RigalVare.SalgsKode3)) AND */
    IF TT_RigalVare.Nummer <> 0 AND NOT CAN-FIND(TT_StrekKode WHERE TT_StrekKode.Kode = STRING(TT_RigalVare.Nummer)) THEN DO:
        CREATE TT_StrekKode.
        ASSIGN TT_StrekKode.ArtikkelNr     = TT_RigalVare.Nummer
               TT_StrekKode.IKasse         = TRUE
               TT_StrekKode.Kode           = STRING(TT_RigalVare.Nummer)
               TT_StrekKode.KodeType       = IF LENGTH(TT_Strekkode.Kode) < 8 THEN 0 ELSE 1
               TT_StrekKode.StrKode        = IF LENGTH(TT_Strekkode.Kode) < 8 THEN 0 ELSE 1 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_StrekKode.
    END.
    IF TT_RigalVare.SalgsKode3 <> 0 AND NOT CAN-FIND(TT_StrekKode WHERE TT_StrekKode.Kode = STRING(TT_RigalVare.SalgsKode3)) THEN DO:
        CREATE TT_StrekKode.
        ASSIGN TT_StrekKode.ArtikkelNr     = TT_RigalVare.Nummer
               TT_StrekKode.IKasse         = TRUE
               TT_StrekKode.Kode           = STRING(TT_RigalVare.Salgskode3)
               TT_StrekKode.KodeType       = IF LENGTH(TT_Strekkode.Kode) < 8 THEN 0 ELSE 1
               TT_StrekKode.StrKode        = IF LENGTH(TT_Strekkode.Kode) < 8 THEN 0 ELSE 1 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_StrekKode.
    END.
    IF 
/*        NOT CAN-FIND(StrekKode WHERE StrekKode.Kode = STRING(TT_RigalVare.SalgsKode4)) AND */
       TT_RigalVare.SalgsKode4 <> 0 AND NOT CAN-FIND(TT_StrekKode WHERE TT_StrekKode.Kode = STRING(TT_RigalVare.SalgsKode4)) THEN DO:
        CREATE TT_StrekKode.
        ASSIGN TT_StrekKode.ArtikkelNr     = TT_RigalVare.Nummer
               TT_StrekKode.IKasse         = TRUE
               TT_StrekKode.Kode           = STRING(TT_RigalVare.Salgskode4)
               TT_StrekKode.KodeType       = IF LENGTH(TT_Strekkode.Kode) < 8 THEN 0 ELSE 1
               TT_StrekKode.StrKode        = IF LENGTH(TT_Strekkode.Kode) < 8 THEN 0 ELSE 1 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          DELETE TT_StrekKode.
    END.
/*     IF LENGTH(STRING(TT_RigalVare.Nummer)) > 7 AND LENGTH(STRING(TT_RigalVare.Nummer)) < 13 AND                                                        */
/*               NOT CAN-FIND(StrekKode WHERE StrekKode.Kode = FILL("0",13 - LENGTH(STRING(TT_RigalVare.Nummer))) + STRING(TT_RigalVare.Nummer)) THEN DO: */
/*         CREATE TT_StrekKode.                                                                                                                           */
/*         ASSIGN TT_StrekKode.ArtikkelNr     = TT_RigalVare.ArtNr                                                                                        */
/*                TT_StrekKode.IKasse         = TRUE                                                                                                      */
/*                TT_StrekKode.Kode           = FILL("0",13 - LENGTH(STRING(TT_RigalVare.Nummer))) + STRING(TT_RigalVare.Nummer)                          */
/*                TT_StrekKode.KodeType       = 1                                                                                                         */
/*                TT_StrekKode.StrKode        = 1.                                                                                                        */
/*     END.                                                                                                                                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = -1 /* Första linjen är en header */
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje.
  IF NOT CAN-DO(cOKVersion,ENTRY(1,cLinje)) THEN DO:
      INPUT STREAM InnFil CLOSE.
      RETURN "FEIL".
  END.
  ASSIGN cRigalversion = ENTRY(1,cLinje).
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderInput) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderInput Procedure 
PROCEDURE ValiderInput :
/* /*------------------------------------------------------------------------------                                                                                              */
/*   Purpose:                                                                                                                                                                    */
/*   Parameters:  <none>                                                                                                                                                         */
/*   Notes:                                                                                                                                                                      */
/* ------------------------------------------------------------------------------*/                                                                                              */
  DEFINE INPUT-OUTPUT PARAMETER piAntFeil AS INTEGER  NO-UNDO.
/*   DEFINE VARIABLE               iInputAnt AS INTEGER    NO-UNDO.                                                                                                              */
/*   ASSIGN iInputAnt = piAntFeil.                                                                                                                                               */
/*   IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = ttOrdre.ArtikkelNr) THEN DO:                                                                                               */
/*       CREATE tt_Error.                                                                                                                                                        */
/*       ASSIGN                                                                                                                                                                  */
/*         piAntFeil = piAntFeil + 1                                                                                                                                             */
/*         tt_Error.LinjeNr = piAntFeil                                                                                                                                          */
/*         tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + "Ukjent artikkel på ordre (OrdreNr/artikkelNr): " + string(ttOrdre.OrdreNr) + ").".               */
/*         .                                                                                                                                                                     */
/*   END.                                                                                                                                                                        */
/*   IF ttOrdre.Ordrenr > 0 THEN DO:                                                                                                                                             */
/*       IF NOT CAN-FIND(Ordre WHERE Ordre.OrdreNr = ttOrdre.Ordrenr) THEN DO:                                                                                                   */
/*           CREATE tt_Error.                                                                                                                                                    */
/*           ASSIGN                                                                                                                                                              */
/*             piAntFeil = piAntFeil + 1                                                                                                                                         */
/*             tt_Error.LinjeNr = piAntFeil                                                                                                                                      */
/*             tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + "Finner ikke ordre for (Ordrebekreftelse: " + string(ttOrdre.OrdreNr) + ").".                 */
/*             .                                                                                                                                                                 */
/*       END.                                                                                                                                                                    */
/*       ELSE IF CAN-FIND(Ordre WHERE Ordre.OrdreNr = ttOrdre.Ordrenr AND Ordre.OrdreStatus <> 2) THEN DO:                                                                       */
/*           FIND Ordre WHERE Ordre.OrdreNr = ttOrdre.Ordrenr NO-LOCK.                                                                                                           */
/*           CREATE tt_Error.                                                                                                                                                    */
/*           ASSIGN                                                                                                                                                              */
/*             piAntFeil = piAntFeil + 1                                                                                                                                         */
/*             tt_Error.LinjeNr = piAntFeil                                                                                                                                      */
/*             tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " +                                                                                               */
/*                                   "Ordre har feil status (Bekreftelse er allerede mottatt) -> " + STRING(Ordre.OrdreStatus) + " (OrdreNr: " + string(ttOrdre.OrdreNr) + ").". */
/*             .                                                                                                                                                                 */
/*       END.                                                                                                                                                                    */
/*       IF NOT CAN-FIND(BestHode WHERE BestHode.BestNr = ttOrdre.BestNr) THEN DO:                                                                                               */
/*           CREATE tt_Error.                                                                                                                                                    */
/*           ASSIGN                                                                                                                                                              */
/*             piAntFeil = piAntFeil + 1                                                                                                                                         */
/*             tt_Error.LinjeNr = piAntFeil                                                                                                                                      */
/*             tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + "Finner ikke bestilling for ordre (OrdreNr: " + string(ttOrdre.OrdreNr) + ").".               */
/*             .                                                                                                                                                                 */
/*       END.                                                                                                                                                                    */
/*   END.                                                                                                                                                                        */
/*                                                                                                                                                                               */
/*   RETURN STRING(iInputAnt = piAntFeil,"/FEIL").                                                                                                                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FinnArtNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinnArtNr Procedure 
FUNCTION FinnArtNr RETURNS DECIMAL
  ( INPUT dStrekkode AS DECIMAL,INPUT iTmpArtNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStreckkod AS CHARACTER  NO-UNDO.
    ASSIGN cStreckKod = STRING(TT_RigalVare.Nummer).
    IF LENGTH(cStreckKod) < 8 THEN
        FIND StrekKode WHERE StrekKode.kode = cStreckKod NO-LOCK NO-ERROR.
    ELSE DO:
        FIND StrekKode WHERE StrekKode.kode = cStreckKod NO-LOCK NO-ERROR.
        IF NOT AVAIL Strekkode THEN
            FIND StrekKode WHERE StrekKode.kode = FILL("0",13 - LENGTH(cStreckKod)) + cStreckKod NO-LOCK NO-ERROR.
    END.

    RETURN IF AVAIL StrekKode THEN Strekkode.Artikkelnr ELSE DECI(iTmpArtNr).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.

      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StreckkodFinns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StreckkodFinns Procedure 
FUNCTION StreckkodFinns RETURNS LOGICAL
  ( INPUT dNummer AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStreckkod AS CHARACTER  NO-UNDO.
    ASSIGN cStreckKod = STRING(TT_RigalVare.Nummer).
    IF LENGTH(cStreckKod) < 8 THEN
        RETURN CAN-FIND(StrekKode WHERE StrekKode.kode = cStreckKod).
    ELSE
        RETURN (CAN-FIND(StrekKode WHERE StrekKode.kode = cStreckKod) OR 
               CAN-FIND(StrekKode WHERE StrekKode.kode = FILL("0",13 - LENGTH(cStreckKod)) + cStreckKod)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ToDate Procedure 
FUNCTION ToDate RETURNS DATE
  ( INPUT dAAAAMMDD AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDato AS DATE       NO-UNDO.
  DEFINE VARIABLE cAAAAMMDD AS CHARACTER  NO-UNDO.
  ASSIGN cAAAAMMDD = STRING(dAAAAMMDD).
  ASSIGN dDato = DATE(INT(SUBSTR(cAAAAMMDD,5,2)),INT(SUBSTR(cAAAAMMDD,7,2)),INT(SUBSTR(cAAAAMMDD,1,4))) NO-ERROR.
  RETURN IF dDato = ? THEN TODAY ELSE dDato.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

