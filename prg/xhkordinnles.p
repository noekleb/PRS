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
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE ipFileName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCL        AS INT        NO-UNDO.

DEFINE VARIABLE cFilRad       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAktTabell    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFile      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wForslagLopNr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
DEFINE VARIABLE bBrukLokalUtpris      AS LOG        NO-UNDO.
DEFINE VARIABLE bBrukLokalInnpris     AS LOG        NO-UNDO.

DEFINE VARIABLE cFieldList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk        AS LOG       NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE icSessionId AS CHARACTER NO-UNDO.  

DEFINE TEMP-TABLE TT_VareBehHode      NO-UNDO LIKE VareBehHode.
DEFINE TEMP-TABLE TT_Ordre            NO-UNDO LIKE Ordre.
DEFINE TEMP-TABLE TT_LevBas           NO-UNDO LIKE LevBas.
DEFINE TEMP-TABLE TT_LevSort          NO-UNDO LIKE LevSort.
DEFINE TEMP-TABLE TT_Farg             NO-UNDO LIKE Farg.
DEFINE TEMP-TABLE TT_Sasong           NO-UNDO LIKE Sasong.
DEFINE TEMP-TABLE TT_StrType          NO-UNDO LIKE StrType.
DEFINE TEMP-TABLE TT_StrTstr          NO-UNDO LIKE StrTstr.
DEFINE TEMP-TABLE TT_StrKonv          NO-UNDO LIKE StrKonv.
DEFINE TEMP-TABLE TT_Produsent        NO-UNDO LIKE Produsent.
DEFINE TEMP-TABLE TT_Varemerke        NO-UNDO LIKE Varemerke.
DEFINE TEMP-TABLE TT_ArtBas           NO-UNDO LIKE ArtBas.
DEFINE TEMP-TABLE TT_ArtPris          NO-UNDO LIKE ArtPris.
DEFINE TEMP-TABLE TT_StrekKode        NO-UNDO LIKE StrekKode.
DEFINE TEMP-TABLE TT_VareBehBestHode  NO-UNDO LIKE VareBehBestHode.
DEFINE TEMP-TABLE TT_VareBehBestLinje NO-UNDO LIKE VareBehBestLinje.
DEFINE TEMP-TABLE TT_VareBehLinje     NO-UNDO LIKE VareBehLinje.
DEFINE TEMP-TABLE TT_BestHode         NO-UNDO LIKE BestHode.
DEFINE TEMP-TABLE TT_BestLinje        NO-UNDO LIKE BestLinje.
DEFINE TEMP-TABLE TT_BestPris         NO-UNDO LIKE BestPris.
DEFINE TEMP-TABLE TT_BestSort         NO-UNDO LIKE BestSort.
DEFINE TEMP-TABLE TT_FriButik         NO-UNDO LIKE FriButik.
DEFINE TEMP-TABLE TT_BestKasse        NO-UNDO LIKE BestKasse.
DEFINE TEMP-TABLE TT_BestStr          NO-UNDO LIKE BestStr.
DEFINE TEMP-TABLE TT_AltLevBas        NO-UNDO LIKE AltLevBas.
DEFINE TEMP-TABLE TT_LevSAnt          NO-UNDO LIKE LevSAnt.
DEFINE TEMP-TABLE TT_PkSdlHode        NO-UNDO LIKE PkSdlHode.
DEFINE TEMP-TABLE TT_PkSdlLinje       NO-UNDO LIKE PkSdlLinje.
DEFINE TEMP-TABLE TT_PkSdlPris        NO-UNDO LIKE PkSdlPris.
DEFINE TEMP-TABLE TT_Bilderegister    NO-UNDO LIKE Bilderegister.
DEFINE TEMP-TABLE TT_BildeData        NO-UNDO LIKE BildeData.
DEFINE TEMP-TABLE TT_Pakkelinje       NO-UNDO LIKE PakkeLinje.

DEFINE STREAM tmpfil.

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

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
         HEIGHT             = 42.95
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{syspara.i 5 1 1 iCL INT}
{syspara.i 2 4 1 wForslagLopNr}
IF NOT CAN-DO("F,N,",wForslagLopNr) THEN
  wForslagLopNr = "F".
  
{syspara.i 22 1 1 cTekst}
IF CAN-DO("1,Ja,True,Yes",cTekst) THEN
    bBrukLokalUtpris = TRUE.
ELSE
    bBrukLokalUtpris = FALSE.

{syspara.i 22 1 2 cTekst}
IF CAN-DO("1,Ja,True,Yes",cTekst) THEN
    bBrukLokalInnpris = TRUE.
ELSE
    bBrukLokalInnpris = FALSE.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    ipFileName = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

IF SEARCH(ipFileName) = ? THEN
    RETURN "ERROR: Finner ikke fil" .
ASSIGN cTmpFile = SESSION:TEMP-DIRECTORY + "tmpLinje.txt".

/* Henter sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCL NO-ERROR.

RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: MainBlock Start: ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).

INPUT FROM VALUE(ipFileName).
REPEAT:
    IMPORT UNFORMATTED cFilRad NO-ERROR.
    IF TRIM(cFilRad) <> "." 
        THEN iAntLinjer = iAntLinjer + 1. 
    IF TRIM(cFilRad) = "." OR ENTRY(1,cFilRad," ") = '"H"' THEN DO:

        /* slut på import av tabell */
        IF cAktTabell <> "" THEN DO:
            OUTPUT STREAM tmpfil CLOSE.
            CASE cAktTabell:
                WHEN "VareBehHode" THEN
                    RUN ToTT_VareBehHode.
                WHEN "Ordre" THEN
                    RUN ToTT_Ordre.
                WHEN "LevBas" THEN
                    RUN ToTT_LevBas.
                WHEN "LevSort" THEN
                    RUN ToTT_LevSort.
                WHEN "Farg" THEN
                    RUN ToTT_Farg.
                WHEN "Sasong" THEN
                    RUN ToTT_Sasong.
                WHEN "StrType" THEN
                    RUN ToTT_StrType.
                WHEN "StrTstr" THEN
                    RUN ToTT_StrTstr.
                WHEN "StrKonv" THEN
                    RUN ToTT_StrKonv.
                WHEN "Produsent" THEN
                    RUN ToTT_Produsent.
                WHEN "Varemerke" THEN
                    RUN ToTT_Varemerke.
                WHEN "ArtBas" THEN
                    RUN ToTT_ArtBas.
                WHEN "ArtPris" THEN
                    RUN ToTT_ArtPris.
                WHEN "StrekKode" THEN
                    RUN ToTT_StrekKode.
                WHEN "Pakkelinje" THEN
                    RUN ToTT_Pakkelinje.
                WHEN "VareBehBestHode" THEN
                    RUN ToTT_VareBehBestHode.
                WHEN "VareBehBestLinje" THEN
                    RUN ToTT_VareBehBestLinje.
                WHEN "VareBehLinje" THEN
                    RUN ToTT_VareBehLinje.
                WHEN "BestHode" THEN
                    RUN ToTT_BestHode.
                WHEN "BestLinje" THEN
                    RUN ToTT_BestLinje.
                WHEN "BestPris" THEN
                    RUN ToTT_BestPris.
                WHEN "BestSort" THEN
                    RUN ToTT_BestSort.
                WHEN "FriButik" THEN
                    RUN ToTT_FriButik.
                WHEN "BestKasse" THEN
                    RUN ToTT_BestKasse.
                WHEN "BestStr" THEN
                    RUN ToTT_BestStr.
                WHEN "AltLevBas" THEN
                    RUN ToTT_AltLevBas.
                WHEN "LevSAnt" THEN
                    RUN ToTT_LevSAnt.
                WHEN "PkSdlHode" THEN
                    RUN ToTT_PkSdlHode.
                WHEN "PkSdlLinje" THEN
                    RUN ToTT_PkSdlLinje.
                WHEN "PkSdlPris" THEN
                    RUN ToTT_PkSdlPris.
                WHEN "Bilderegister" THEN
                    RUN ToTT_Bilderegister.
                WHEN "BildeData" THEN
                    RUN ToTT_BildeData.
            END CASE.
        END.

        IF TRIM(cFilRad) = "." THEN
            LEAVE.
        ASSIGN cAktTabell = TRIM(ENTRY(2,cFilRad," "),'"').
        OUTPUT STREAM tmpfil TO VALUE(cTmpFile).
        NEXT.
    END.
    PUT STREAM tmpfil UNFORMATTED cFilRad SKIP.
END.
INPUT CLOSE.

RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Innlesning ferdig: ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).

/* MESSAGE PROGRAM-NAME(1) SKIP           */
/* "TESTDUMP"                             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* RUN Dumpa. */
RUN Grundregister. /* Farg,Sasong,Varemerke,Produsent */
RUN Storrelser.    /* StrKonv,StrType,StrTStr         */
RUN LevInfo.       /* LevBas,LevSort                  */

RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Start Artiklar ' + ' ' + string(TIME,"HH:MM:SS")).
RUN Artiklar.      /* ArtBas,Artpris,StrekKode,AltLevBas */
RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Slutt Artiklar ' + ' ' + string(TIME,"HH:MM:SS")).

RUN VareBeh.       /* VareBehHode,VareBehLinje,VareBehBestHode,VareBehBestLinje */
RUN Order.         /* Ordre,BestHode,BestPris,BestLinje,BestKasse,BestSort,BestStr,FriButik */
RUN PkSdl.         /* Pakkseddel                      */
RUN Bilder.        /* Bilder på artikler.             */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
/*       IF pbOk = FALSE THEN                             */
/*       DO:                                              */
/*         ASSIGN                                         */
/*           cTekst = "** Feil ved oppdatering av fil."   */
/*           .                                            */
/*         PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40"). */
/*       END.                                             */
      ASSIGN
        cTekst = "Fil oppdatert (Antall linjer: " + STRING(iAntLinjer) + ").". 
        .
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: MainBlock Ferdig: ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Artiklar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artiklar Procedure 
PROCEDURE Artiklar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLopNr     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iStrTypeId AS INTEGER NO-UNDO.
 
    DEFINE BUFFER bArtBas  FOR ArtBas.
    DEFINE BUFFER b2ArtBas FOR ArtBas.
       
    RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-1 ' + string(TIME,"HH:MM:SS")).

    FOR EACH TT_ArtBas:
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-2 ' + string(TIME,"HH:MM:SS")).
        FIND ArtBas EXCLUSIVE-LOCK WHERE ArtBas.ArtikkelNr = TT_ArtBas.ArtikkelNr NO-ERROR.
        /* Sjekker VPI register */
        IF NOT AVAILABLE ArtBas AND TT_ArtBas.ArtikkelNr > 0 THEN 
        DO:
            FIND FIRST VPIArtBas NO-LOCK WHERE
              VPIArtBas.ArtikkelNr = TT_ArtBas.ArtikkelNr NO-ERROR.
            IF AVAILABLE VPIArtBas THEN 
            DO:
              cFieldList = {tbchooseAll.i}.
              RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + STRING(TT_ArtBas.ArtikkelNr), 
                                ihBuffer, 
                                icSessionid, 
                                OUTPUT ocReturn, 
                                OUTPUT obOk).
            END.     
            FIND ArtBas EXCLUSIVE-LOCK WHERE ArtBas.ArtikkelNr = TT_ArtBas.ArtikkelNr NO-ERROR.
        END.
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-2 ' + string(TIME,"HH:MM:SS")).

        IF NOT AVAIL ArtBas THEN DO:
            CREATE ArtBas.
            BUFFER-COPY TT_ArtBas EXCEPT Lopnr TO ArtBas
                ASSIGN 
                    ArtBas.ManRabIKas = TRUE
                    ArtBas.LopNr      = ?  
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE ArtBas.
                NEXT.
            END.
            ASSIGN ArtBas.Notat     = REPLACE(REPLACE(ArtBas.Notat,CHR(1),CHR(10)),CHR(2),CHR(13))
                   ArtBas.Varefakta = REPLACE(REPLACE(ArtBas.Varefakta,CHR(1),CHR(10)),CHR(2),CHR(13))
                   ArtBas.Lager     = (IF ArtBas.OPris = FALSE THEN TRUE ELSE FALSE) 
                   ArtBas.ArtSlag   = 0 /* Intill videre */
                   NO-ERROR.
            FIND LAST bArtBas WHERE bArtBas.Vg = TT_ArtBas.Vg USE-INDEX vglopnr NO-LOCK NO-ERROR.
            RUN SettLopNr.p (TT_ArtBas.Vg,wForslagLopNr,OUTPUT iLopNr).
            ASSIGN ArtBas.Lopnr = iLopNr
                   ArtBas.IKasse = FALSE.
            FOR EACH TT_ArtPris OF TT_ArtBas:
                CREATE ArtPris.
                BUFFER-COPY TT_ArtPris TO ArtPris NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE ArtPris.
            END.
            RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-3-A ' + string(TIME,"HH:MM:SS")).
        END.
    
        ELSE DO:
            IF Artbas.vg <> TT_Artbas.vg THEN
                ASSIGN Artbas.lopnr = ?.
            BUFFER-COPY TT_ArtBas EXCEPT Lopnr IKasse WebButikkArtikkel TO ArtBas.
            ASSIGN ArtBas.Lager   = (IF ArtBas.OPris = FALSE THEN TRUE ELSE FALSE)
                   ArtBas.ArtSlag = 0 /* Intill videre */ 
                   NO-ERROR.
            IF Artbas.lopnr = ? THEN DO:
                RUN SettLopNr.p (TT_ArtBas.Vg,wForslagLopNr,OUTPUT iLopNr).
                ASSIGN ArtBas.Lopnr = iLopNr.
                FOR EACH ArtLag WHERE
                    ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:
                    ASSIGN
                        ArtLag.Vg    = ArtBas.Vg
                        ArtLag.LopNr = ArtBas.LopNr.
                END.
            END.
            RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-3-B ' + string(TIME,"HH:MM:SS")).
        END.
    
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-4 ' + string(TIME,"HH:MM:SS")).
        
        FOR EACH TT_StrekKode OF TT_ArtBas.
            FIND Strekkode WHERE StrekKode.Kode = TT_StrekKode.Kode NO-ERROR.
            /* Tar bort strekkoden hvis den ligger på feil størrelse eller artikkel. */
            IF AVAILABLE Strekkode THEN DO:
                /* Flytter eventuelt lager fra gammel størrelse til ny størrelse på samme artikkel. */
                IF (Strekkode.StrKode <> TT_Strekkode.StrKode AND 
                    Strekkode.ArtikkelNr = TT_STrekkode.ArtikkelNr) THEN
                DO:
                    RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Start bibl_flytt_lager_str.p ' + string(TIME,"HH:MM:SS")).
                    RUN bibl_flytt_lager_str.p (Strekkode.Kode, TT_STrekkode.StrKode).
                    RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Ferdig bibl_flytt_lager_str.p ' + string(TIME,"HH:MM:SS")).
                END.

                /* Sanerer gammel artikkel til ny artikkel. */
                /* 10/10-11 TN Dette skal ikke gjøres automatisk. Bruk dialog i pakkseddel ved regsitrering av strekkode.
                            NB: Testet koden under, og saneringen fungerer utmerket.
                IF (Strekkode.StrKode <> TT_Strekkode.StrKode AND 
                    Strekkode.ArtikkelNr <> TT_STrekkode.ArtikkelNr) THEN
                SANER_ART:
                DO:
                  CREATE Elogg.
                  ASSIGN ELogg.EksterntSystem  = "KORRHK"
                         ELogg.TabellNavn      = "VPIArtBas"
                         ELogg.Verdier         = "0|" + STRING(TT_STrekkode.ArtikkelNr) + "|"
                                                      + STRING(Strekkode.ArtikkelNr)
                         ELogg.EndringsType    = 1 
                         ELogg.Behandlet       = FALSE.
                  FIND CURRENT elogg NO-LOCK.
                  RELEASE elogg.
                  RUN vpikorreksjon.w.
                END. /* SANER_ART */
                */
                
                /* Tar bort gammelstrekkode på samme artikkel. Den blir erstattet av ny */
                IF (Strekkode.StrKode <> TT_Strekkode.StrKode AND 
                    Strekkode.ArtikkelNr = TT_STrekkode.ArtikkelNr) THEN
                DO:
                    RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Start Slett strekkode ' + string(TIME,"HH:MM:SS")).
                    DELETE Strekkode.
                    RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Ferdig Slett STrekkode ' + string(TIME,"HH:MM:SS")).
                END.
                /* Koden under gjør at strekkoden flyttes også fra en annen artikkel.
                   Koden over lar strekkoden ligge igjen, slik at bruker kan få opp en dialog og velge å sanere artikkelen.    
                IF (Strekkode.StrKode <> TT_Strekkode.StrKode OR 
                    Strekkode.ArtikkelNr <> TT_STrekkode.ArtikkelNr) THEN
                    DELETE Strekkode.
                */
            END.
            IF NOT AVAIL StrekKode THEN DO:
                RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Start Create STrekkode ' + string(TIME,"HH:MM:SS")).
                CREATE StrekKode.
                BUFFER-COPY TT_StrekKode TO StrekKode NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE StrekKode.
                RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Slutt Create STrekkode ' + string(TIME,"HH:MM:SS")).
            END.
            ELSE IF AVAIL StrekKode AND StrekKode.ArtikkelNr = TT_Strekkode.ArtikkelNr THEN
            DO:
                RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Start BUFFER-COPY STrekkode ' + string(TIME,"HH:MM:SS")).
                BUFFER-COPY TT_StrekKode TO StrekKode NO-ERROR.
                RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Slutt BUFFER-COPY STrekkode ' + string(TIME,"HH:MM:SS")).
            END.
        END.
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-5 ' + string(TIME,"HH:MM:SS")).

        IF (TT_ArtBas.StrTypeId = 0 OR 
            NOT CAN-FIND(StrType WHERE StrType.StrTypeId = TT_ArtBas.StrTypeId)) THEN
        DO:
          iStrTypeId = 0.
          RUN bibl_opprettStrtypeForModell.p (ArtBas.ArtikkelNr, OUTPUT iStrTypeID).
          ASSIGN 
            ArtBas.StrTypeId    = iStrTypeId
            TT_ArtBas.StrTypeId = iStrTypeId.
        END.
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-6 ' + string(TIME,"HH:MM:SS")).
        
        FOR EACH TT_AltLevBas WHERE TT_AltLevBas.ArtikkelNr = TT_ArtBas.ArtikkelNr:
            FIND AltLevBas WHERE AltLevBas.Artikkelnr = TT_AltLevBas.ArtikkelNr AND
                                 AltLevBas.LevNr      = TT_AltLevBas.LevNr NO-LOCK NO-ERROR.
            IF NOT AVAIL AltLevBas THEN DO:
                CREATE AltLevBas.
                BUFFER-COPY TT_AltLevBas TO AltLevBas NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE AltLevBas.
            END.
        END.
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-7 ' + string(TIME,"HH:MM:SS")).
        
        FOR EACH TT_Pakkelinje OF TT_ArtBas.
            FIND Pakkelinje WHERE Pakkelinje.ArtikkelNr   = TT_Pakkelinje.ArtikkelNr AND
                                  PakkeLinje.PkArtikkelNr = TT_PakkeLinje.PkArtikkelNr AND
                                  Pakkelinje.StrKode      = TT_Pakkelinje.StrKode NO-ERROR.
            /* Tar bort strekkoden hvis den ligger på feil størrelse eller artikkel. */
            IF AVAILABLE Pakkelinje THEN DO:
                    DELETE Pakkelinje.
            END.
            IF NOT AVAIL Pakkelinje THEN DO:
                CREATE Pakkelinje.
                BUFFER-COPY TT_Pakkelinje TO Pakkelinje NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE Pakkelinje.
            END.
        END.
        RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-8 ' + string(TIME,"HH:MM:SS")).
    END.
/*         TT_AltLevBas.        */

    RUN bibl_logg.p ('PkSdlImport', 'xhkordinnles.p: Subrutine Artiklar Test-9 ' + string(TIME,"HH:MM:SS")).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Bilder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bilder Procedure 
PROCEDURE Bilder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ON CREATE OF Bilderegister OVERRIDE DO: END.
    ON WRITE  OF Bilderegister OVERRIDE DO: END.
    ON CREATE OF BildeData     OVERRIDE DO: END.
    ON WRITE  OF BildeData     OVERRIDE DO: END.
    
    FOR EACH TT_Bilderegister:
        FIND Bilderegister OF TT_Bilderegister NO-LOCK NO-ERROR.
        IF NOT AVAIL Bilderegister THEN DO:
            CREATE Bilderegister.
            BUFFER-COPY TT_Bilderegister TO Bilderegister NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE Bilderegister.
                NEXT.
            END.
        END.
        FOR EACH TT_BildeData OF TT_Bilderegister:
            FIND BildeData OF TT_BildeData NO-LOCK NO-ERROR.
            IF NOT AVAIL BildeData THEN DO:
                CREATE BildeData.
                BUFFER-COPY TT_BildeData TO BildeData NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE BildeData.
                    NEXT.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Dumpa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dumpa Procedure 
PROCEDURE Dumpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT TO "C:\home\Lindbak\ankommet\ArtBas_TT.txt".
    FOR EACH TT_ArtBas.
        EXPORT TT_ArtBas.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\ArtPris_TT.txt".
    FOR EACH TT_ArtPris.
        EXPORT TT_ArtPris.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\AltLevBas_TT.txt".
    FOR EACH TT_AltLevBas.
        EXPORT TT_AltLevBas.
    END.
    OUTPUT TO "C:\home\Lindbak\ankommet\BestHode_TT.txt".
    FOR EACH TT_BestHode.
        EXPORT TT_BestHode.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\BestKasse_TT.txt".
    FOR EACH TT_BestKasse.
        EXPORT TT_BestKasse.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\BestLinje_TT.txt".
    FOR EACH TT_BestLinje.
        EXPORT TT_BestLinje.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\BestPris_TT.txt".
    FOR EACH TT_BestPris.
        EXPORT TT_BestPris.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\BestSort_TT.txt".
    FOR EACH TT_BestSort.
        EXPORT TT_BestSort.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\BestStr_TT.txt".
    FOR EACH TT_BestStr.
        EXPORT TT_BestStr.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\Farg_TT.txt".
    FOR EACH TT_Farg.
        EXPORT TT_Farg.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\FriButik_TT.txt".
    FOR EACH TT_FriButik.
        EXPORT TT_FriButik.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\LevBas_TT.txt".
    FOR EACH TT_LevBas.
        EXPORT TT_LevBas.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\LevSort_TT.txt".
    FOR EACH TT_LevSort.
        EXPORT TT_LevSort.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\Ordre_TT.txt".
    FOR EACH TT_Ordre.
        EXPORT TT_Ordre.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\Produsent_TT.txt".
    FOR EACH TT_Produsent.
        EXPORT TT_Produsent.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\Sasong_TT.txt".
    FOR EACH TT_Sasong.
        EXPORT TT_Sasong.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\StrekKode_TT.txt".
    FOR EACH TT_StrekKode.
        EXPORT TT_StrekKode.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\StrKonv_TT.txt".
    FOR EACH TT_StrKonv.
        EXPORT TT_StrKonv.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\StrTStr_TT.txt".
    FOR EACH TT_StrTStr.
        EXPORT TT_StrTStr.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\StrType_TT.txt".
    FOR EACH TT_StrType.
        EXPORT TT_StrType.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\VareBehBestHode_TT.txt".
    FOR EACH TT_VareBehBestHode.
        EXPORT TT_VareBehBestHode.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\VareBehBestLinje_TT.txt".
    FOR EACH TT_VareBehBestLinje.
        EXPORT TT_VareBehBestLinje.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\VareBehHode_TT.txt".
    FOR EACH TT_VareBehHode.
        EXPORT TT_VareBehHode.
    END.
    OUTPUT CLOSE.
    OUTPUT TO "C:\home\Lindbak\ankommet\Varemerke_TT.txt".
    FOR EACH TT_Varemerke.
        EXPORT TT_Varemerke.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Grundregister) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grundregister Procedure 
PROCEDURE Grundregister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        FOR EACH TT_Farg:
            FIND Farg WHERE Farg.Farg = TT_Farg.Farg NO-ERROR.
            IF NOT AVAIL Farg THEN DO:
                CREATE Farg.
                BUFFER-COPY TT_Farg TO Farg NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE Farg.
            END.
            ELSE
                BUFFER-COPY TT_Farg TO Farg NO-ERROR.
            RELEASE Farg.
        END.
        
        FOR EACH TT_Sasong:
            FIND Sasong WHERE Sasong.Sasong = TT_Sasong.Sasong NO-ERROR.
            IF NOT AVAIL Sasong THEN DO:
                CREATE Sasong.
                BUFFER-COPY TT_Sasong TO Sasong NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE SaSong.
            END.
            ELSE
                BUFFER-COPY TT_Sasong TO Sasong NO-ERROR.
            RELEASE Sasong.
        END.

        FOR EACH TT_Varemerke:
            FIND Varemerke WHERE Varemerke.VMId = TT_Varemerke.VMId NO-ERROR.
            IF NOT AVAIL Varemerke THEN DO:
                CREATE Varemerke.
                BUFFER-COPY TT_Varemerke TO Varemerke NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE Varemerke.
            END.
            ELSE
                BUFFER-COPY TT_Varemerke TO Varemerke NO-ERROR.
            RELEASE Varemerke.
        END.
        
        FOR EACH TT_Produsent:
            FIND Produsent WHERE Produsent.Prodnr = TT_Produsent.Prodnr NO-ERROR.
            IF NOT AVAIL Produsent THEN DO:
                CREATE Produsent.
                BUFFER-COPY TT_Produsent TO Produsent NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE Produsent.
            END.
            ELSE
                BUFFER-COPY TT_Produsent TO Produsent NO-ERROR.
            ASSIGN Produsent.Notat = REPLACE(REPLACE(Produsent.Notat,CHR(1),CHR(10)),CHR(2),CHR(13)) NO-ERROR.
            RELEASE Produsent.
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LevInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevInfo Procedure 
PROCEDURE LevInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ON CREATE OF LevBas OVERRIDE DO: END.
    ON WRITE OF LevBas OVERRIDE DO: END.
    ON CREATE OF LevSort OVERRIDE DO: END.
    ON WRITE OF LevSort OVERRIDE DO: END.
    ON CREATE OF LevSAnt OVERRIDE DO: END.
    ON WRITE OF LevSAnt OVERRIDE DO: END.

    FOR EACH TT_LevBas:
        FIND LevBas OF TT_LevBas NO-ERROR.
        IF NOT AVAIL LevBas THEN DO:
            CREATE LevBas.
            BUFFER-COPY TT_LevBas TO LevBas NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE LevBas.
        END.
        ELSE
            BUFFER-COPY TT_LevBas TO LevBas NO-ERROR.
        ASSIGN LevBas.Notat = REPLACE(REPLACE(LevBas.Notat,CHR(1),CHR(10)),CHR(2),CHR(13)) NO-ERROR.
        RELEASE LevBas.
    END.
    FOR EACH TT_LevSort:
        FIND LevSort OF TT_LevSort NO-ERROR.
        IF NOT AVAIL LevSort THEN DO:
            CREATE LevSort.
            BUFFER-COPY TT_LevSort TO LevSort NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE LevSort.
        END.
        ELSE
            BUFFER-COPY TT_LevSort TO LevSort NO-ERROR.
        FOR EACH LevSAnt OF LevSort:
            DELETE LevSAnt.
        END.
        FOR EACH TT_LevSAnt OF TT_LevSort:
            CREATE LevSAnt.
            BUFFER-COPY TT_LevSAnt TO LevSAnt NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE LevSAnt.
        END.
        RELEASE LevSort.
        RELEASE LevSAnt.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Order) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Order Procedure 
PROCEDURE Order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ON CREATE OF Ordre OVERRIDE DO: END.
    ON WRITE OF Ordre OVERRIDE DO: END.
    ON CREATE OF BestHode OVERRIDE DO: END.
    ON WRITE OF BestHode OVERRIDE DO: END.
    FOR EACH TT_Ordre:
        FIND Ordre OF TT_Ordre NO-ERROR.
        IF NOT AVAIL Ordre THEN DO:
            CREATE Ordre.
            BUFFER-COPY TT_Ordre TO Ordre NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE Ordre.
                NEXT.
            END.
            ASSIGN Ordre.Notat = REPLACE(REPLACE(Ordre.Notat,CHR(1),CHR(10)),CHR(2),CHR(13)) NO-ERROR.
        END.

        FOR EACH TT_BestHode OF TT_Ordre:
            FIND BestHode WHERE BestHode.BestNr = TT_BestHode.BestNr NO-LOCK NO-ERROR.
            IF AVAIL BestHode AND BestHode.BestStat <> TT_BestHode.BestStat THEN
                NEXT.
            ELSE IF AVAIL BestHode THEN DO:
                RELEASE BestHode.
                RUN SlettBest (TT_BestHode.BestNr).
            END.
            CREATE BestHode.
            BUFFER-COPY TT_BestHode TO BestHode NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE BestHode.
                NEXT.
            END.
            ASSIGN BestHode.Merknad = REPLACE(REPLACE(BestHode.Merknad,CHR(1),CHR(10)),CHR(2),CHR(13)) NO-ERROR.
            FOR EACH TT_BestPris OF TT_BestHode:
                CREATE BestPris.
                BUFFER-COPY TT_BestPris TO BestPris NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE BestPris.
                    NEXT.
                END.
            END.
            FOR EACH TT_BestLinje OF TT_BestHode.
                CREATE BestLinje.
                BUFFER-COPY TT_BestLinje TO BestLinje NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE BestLinje.
                    NEXT.
                END.
            END.
            FOR EACH TT_BestKasse OF TT_BestHode:
                CREATE BestKasse.
                BUFFER-COPY TT_BestKasse TO BestKasse NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE BestKasse.
                    NEXT.
                END.
            END.
            FOR EACH TT_BestSort OF TT_BestHode:
                CREATE BestSort.
                BUFFER-COPY TT_BestSort TO BestSort NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE BestSort.
                    NEXT.
                END.
            END.
            FOR EACH TT_BestStr OF TT_BestHode:
                CREATE BestStr.
                BUFFER-COPY TT_BestStr TO BestStr NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE BestStr.
                    NEXT.
                END.
            END.
            FOR EACH TT_FriButik OF TT_BestHode.
                CREATE FriButik.
                BUFFER-COPY TT_FriButik TO FriButik NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE FriButik.
                    NEXT.
                END.
            END.
        END.
    END.

        
/*                 */
/*                 */
/*                 */
/*                 */
/*                 */
/*                 */
/*         EXPORT .         */
/*                                     */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PkSdl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PkSdl Procedure 
PROCEDURE PkSdl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER lokArtPris FOR ArtPris.

  DEF VAR fAntBest   AS DEC NO-UNDO.
  DEF VAR fAntLevert AS DEC NO-UNDO.
  DEF VAR fMvaKr     AS DEC NO-UNDO.
  DEF VAR fDbKr      AS DEC NO-UNDO.

  ON CREATE OF PkSdlHode  OVERRIDE DO: END.
  ON WRITE  OF PkSdlHode  OVERRIDE DO: END.
  ON CREATE OF PkSdlLinje OVERRIDE DO: END.
  ON WRITE  OF PkSdlLinje OVERRIDE DO: END.
  ON CREATE OF PkSdlPris  OVERRIDE DO: END.
  ON WRITE  OF PkSdlPris  OVERRIDE DO: END.
  
  FOR EACH TT_PkSdlHode:
      FIND PkSdlHode OF TT_PkSdlHode NO-LOCK NO-ERROR.
      IF NOT AVAIL PkSdlHode THEN DO:
          CREATE PkSdlHode.
          BUFFER-COPY TT_PkSdlHode TO PkSdlHode 
              ASSIGN PkSdlHode.PkSdlStatus = 10 NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
              DELETE PkSdlHode.
              NEXT.
          END.
      END.
      FOR EACH TT_PkSdlLinje OF TT_PkSdlHode:
          FIND PkSdlLinje OF TT_PkSdlLinje NO-LOCK NO-ERROR.
          IF NOT AVAIL PkSdlLinje THEN DO:
              CREATE PkSdlLinje.
              BUFFER-COPY TT_PkSdlLinje TO PkSdlLinje NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  DELETE PkSdlLinje.
                  NEXT.
              END.
              
              /* Sjekker VPI register hvis artikkel ikke er aktivert. */
              IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr) THEN 
              DO:
                FIND FIRST VPIArtBas NO-LOCK WHERE
                  VPIArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                IF AVAILABLE VPIArtBas THEN 
                DO:
                  cFieldList = {tbchooseAll.i}.
                  RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + STRING(PkSdlLinje.ArtikkelNr), 
                                    ihBuffer, 
                                    icSessionid, 
                                    OUTPUT ocReturn, 
                                    OUTPUT obOk).
                END.     
              END.

              /* TN - Retter opp rest antall. Nødvendig pga usikkerhet om hva som har skjedd med ordre lokalt. */
              /* TN 20/10-2011 Det kan ikke gjøres delelveranser på pakkseddel. Dette er ikke nødvendig.
              IF AVAIL PkSdlLinje THEN 
              RESTBEREGNIN:
              DO:
                ASSIGN
                    fAntLevert = 0
                    fAntBest   = 0.
                /* Henter antall opprinnelig bestillt. */
                FIND FIRST StrKonv NO-LOCK
                     WHERE StrKonv.StrKode = PkSdlLinje.StrKode
                     NO-ERROR.
                IF AVAIL StrKonv THEN
                  FOR EACH BestStr NO-LOCK
                      WHERE BestStr.BestNr = PkSdlLinje.BestNr
                        AND BestStr.Butik  = PkSdlLinje.ButikkNr
                        AND TRIM(BestStr.Storl) = TRIM(StrKonv.Storl)
                      BY BestStr.BestStat DESC
                      :
                    fAntBest = BestStr.Bestilt.
                    LEAVE.
                  END.

                /* Henter antall tidligere levert. */
                FIND FIRST StrKonv NO-LOCK
                     WHERE StrKonv.StrKode = PkSdlLinje.StrKode
                     NO-ERROR.
                IF AVAIL StrKonv THEN
                  FOR EACH BestLevert NO-LOCK
                      WHERE BestLevert.BestNr = PkSdlLinje.BestNr
                        AND BestLevert.Butik  = PkSdlLinje.ButikkNr
                        AND TRIM(BestLevert.Storl) = TRIM(StrKonv.Storl):
                    ASSIGN
                        fAntLevert   = fAntLevert + BestLevert.Levert.                        
                  END.
                /* Beregner rest. */
                ASSIGN
                  PkSdlLinje.AntRest = fAntBest - fAntLevert - PkSdlLinje.AntLevert.
              END. /* RESTBEREGNING */
              */
          END.
      END.
      FOR EACH TT_PkSdlPris OF TT_PkSdlHode:
          FIND PkSdlPris OF TT_PkSdlPris NO-LOCK NO-ERROR.
          IF NOT AVAIL PkSdlPris THEN DO:
              CREATE PkSdlPris.
              BUFFER-COPY TT_PkSdlPris TO PkSdlPris NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  DELETE PkSdlPris.
                  NEXT.
              END.

              /* Overstyrer pakkseddel inn/utpris med lokal pris.   */
              /* Systemparameter 22 1 1 (Utpris og 22 1 2 Innpris). */
              IF bBrukLokalUtpris OR bBrukLokalInnPris THEN
              LOKALPRIS:
              DO:
                  IF AVAILABLE Butiker THEN
                      FIND lokArtPris WHERE
                        lokArtPris.ArtikkelNr = TT_PkSdlPris.ArtikkelNr AND
                        lokArtPris.ProfilNr   = Butiker.ProfilNr NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE lokArtPris THEN
                      FIND FIRST lokArtPris WHERE 
                        lokArtPris.ArtikkelNr = TT_PkSdlPris.ArtikkelNr NO-LOCK NO-ERROR.

                  /* Retter opp feil som lå i import på HK . */
                  ASSIGN
                      /* Innkjopspris Rab1% Frakt Varekost Pris */
                      PkSdlPris.Varekost = PkSdlPris.InnkjopsPris - ((PkSdlPris.Innkjopspris * PkSdlPris.Rab1%) / 100)
                      /*fMvaKr          = PkSdlPris.Pris - (PkSdlPris.Pris / (1 + (lokArtPris.Mva%[1] / 100)))*/
                      fMvaKr          = PkSdlPris.Pris * (lokArtPris.Mva%[1] / (100 + lokArtPris.Mva%[1]))
                      fDbKr           = PkSdlPris.Pris - fMvaKr - PkSdlPris.Varekost
                      PkSdlPris.DB%   = ROUND((fDbKr * 100) / (PkSdlPris.Varekost + fDbKr),2)
                      PkSdlPris.DB%   = IF PkSdlPris.DB% = ? THEN 0 ELSE PkSdlPris.DB%
                      .

                  IF AVAILABLE lokArtPris THEN
                  DO:
                      IF bBrukLokalUtpris AND (lokArtPris.Pris[1] <> PkSdlPris.NyPris) THEN
                        /* Setter lokal utpris. */
                        ASSIGN
                          PkSdlPris.OverstyrPris = TRUE /* Sikrer at den lokale prisen aktiveres med mottak. */
                          PkSdlPris.NyPris       = lokArtPris.Pris[1]
                          /* --- Omregning inn her ----- */
                          fMvaKr                 = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (lokArtPris.Mva%[1] / 100)))
                          fDbKr                  = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
                          PkSdlPris.NyDB%        = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
                          PkSdlPris.NyDB%        = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                          .
                      IF bBrukLokalInnpris AND (lokArtPris.InnkjopsPris[1] <> PkSdlPris.NyInnkjopsPris) THEN
                        /* Setter lokal Innpris. */
                        ASSIGN
                          PkSdlPris.OverstyrPris   = TRUE /* Sikrer at den lokale prisen aktiveres med mottak. */
                          PkSdlPris.NyInnkjopsPris = lokArtPris.InnkjopsPris[1]
                          PkSdlPris.NyRab1%        = lokArtPris.Rab1%[1]
                          PkSdlPris.NyFrakt        = lokArtPris.Frakt[1]
                          PkSdlPris.NyVareKost     = lokArtPris.VareKost[1]                          
                          /* --- Omregning inn her ----- */
                          fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (lokArtPris.Mva%[1] / 100)))
                          fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
                          PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
                          PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                          .
                  END.
              END. /* LOKALPRIS */
          END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettBest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBest Procedure 
PROCEDURE SlettBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iBestNr LIKE BestHode.BestNr    NO-UNDO.
    FIND BestHode WHERE BestHode.Bestnr = iBestNr EXCLUSIVE.
    IF NOT AVAIL BestHode THEN
        RETURN.
    FOR EACH BestKasse OF BestHode: DELETE BestKasse. END.
    FOR EACH BestLinje OF BestHode: DELETE BestLinje. END.
    FOR EACH BestPris  OF BestHode: DELETE BestPris. END.
    FOR EACH BestSort  OF BestHode: DELETE BestSort. END.
    FOR EACH BestStr   OF BestHode: DELETE BestStr. END.
    FOR EACH Fributik  OF BestHode: DELETE Fributik. END.
    DELETE BestHode.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Storrelser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Storrelser Procedure 
PROCEDURE Storrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*         EXPORT TT_StrKonv.          */
    FOR EACH TT_StrKonv:
        FIND StrKonv WHERE StrKonv.StrKode = TT_StrKonv.StrKode NO-LOCK NO-ERROR.
        IF NOT AVAIL StrKonv THEN DO:
            CREATE StrKonv.
            BUFFER-COPY TT_StrKonv TO StrKonv NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE StrKonv.
            IF AVAILABLE StrKonv THEN RELEASE StrKonv.
        END.
    END.
    FOR EACH TT_StrType:
        FIND StrType OF TT_StrType EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE StrType THEN DO:
          IF StrType.Alfafordeling = TT_StrType.Alfafordeling THEN
            NEXT.
        END.
        IF NOT AVAIL StrType THEN DO:
            CREATE StrType.
            BUFFER-COPY TT_StrType TO StrType NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE StrType.
        END.
        ELSE
            BUFFER-COPY TT_StrType TO StrType.
        /* Rydder bort det gamle for å sikre at det ikke ligger igjen størrelser som ikke skal være der. */
        IF CAN-FIND(FIRST TT_StrTStr OF TT_StrType) THEN
          FOR EACH StrTStr OF StrType:
            DELETE StrTStr.
          END.
        RELEASE StrType.
        FOR EACH TT_StrTStr OF TT_StrType.
            CREATE StrTStr.
            BUFFER-COPY TT_StrTStr TO StrTStr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE StrTStr.
            IF AVAILABLE StrTStr THEN RELEASE StrTStr.
        END.
    END.

/*         EXPORT TT_StrTStr.          */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_AltLevBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_AltLevBas Procedure 
PROCEDURE ToTT_AltLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_AltLevBas.
        IMPORT STREAM tmpfil TT_AltLevBas.
    END.
    DELETE TT_AltLevBas.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_ArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_ArtBas Procedure 
PROCEDURE ToTT_ArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_ArtBas.
        IMPORT STREAM tmpfil TT_ArtBas NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_ArtBas.
    END.
    IF AVAIL TT_ArtBas THEN
        DELETE TT_ArtBas.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_ArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_ArtPris Procedure 
PROCEDURE ToTT_ArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_ArtPris.
        IMPORT STREAM tmpfil TT_ArtPris NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_ArtPris.
    END.
    IF AVAIL TT_ArtPris THEN
        DELETE TT_ArtPris.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BestHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BestHode Procedure 
PROCEDURE ToTT_BestHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_BestHode.
        IMPORT STREAM tmpfil TT_BestHode NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_BestHode.
    END.
    IF AVAIL TT_BestHode THEN
        DELETE TT_BestHode.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BestKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BestKasse Procedure 
PROCEDURE ToTT_BestKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_BestKasse.
        IMPORT STREAM tmpfil TT_BestKasse NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_BestKasse.
    END.
    IF AVAIL TT_BestKasse THEN
        DELETE TT_BestKasse.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BestLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BestLinje Procedure 
PROCEDURE ToTT_BestLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_BestLinje.
        IMPORT STREAM tmpfil TT_BestLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_BestLinje.
    END.
    IF AVAIL TT_BestLinje THEN
        DELETE TT_BestLinje.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BestPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BestPris Procedure 
PROCEDURE ToTT_BestPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_BestPris.
        IMPORT STREAM tmpfil TT_BestPris NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_BestPris.
    END.
    IF AVAIL TT_BestPris THEN
        DELETE TT_BestPris.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BestSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BestSort Procedure 
PROCEDURE ToTT_BestSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_BestSort.
        IMPORT STREAM tmpfil TT_BestSort NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_BestSort.
    END.
    IF AVAIL TT_BestSort THEN
        DELETE TT_BestSort.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BestStr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BestStr Procedure 
PROCEDURE ToTT_BestStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_BestStr.
        IMPORT STREAM tmpfil TT_BestStr NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_BestStr.
    END.
    IF AVAIL TT_BestStr THEN
        DELETE TT_BestStr.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_BildeData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_BildeData Procedure 
PROCEDURE ToTT_BildeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Bildedata.
        IMPORT STREAM tmpfil TT_Bildedata NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Bildedata.
    END.
    IF AVAIL TT_Bildedata THEN
        DELETE TT_Bildedata.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Bilderegister) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Bilderegister Procedure 
PROCEDURE ToTT_Bilderegister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Bilderegister.
        IMPORT STREAM tmpfil TT_Bilderegister NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Bilderegister.
    END.
    IF AVAIL TT_Bilderegister THEN
        DELETE TT_Bilderegister.
    INPUT STREAM tmpfil CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Farg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Farg Procedure 
PROCEDURE ToTT_Farg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Farg.
        IMPORT STREAM tmpfil TT_Farg NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Farg.
    END.
    IF AVAIL TT_Farg THEN
        DELETE TT_Farg.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_FriButik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_FriButik Procedure 
PROCEDURE ToTT_FriButik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_FriButik.
        IMPORT STREAM tmpfil TT_FriButik NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_FriButik.
    END.
    IF AVAIL TT_FriButik THEN
        DELETE TT_FriButik.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_LevBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_LevBas Procedure 
PROCEDURE ToTT_LevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_LevBas.
        IMPORT STREAM tmpfil TT_LevBas NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_LevBas.
    END.
    IF AVAIL TT_LevBas THEN
        DELETE TT_LevBas.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_LevSAnt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_LevSAnt Procedure 
PROCEDURE ToTT_LevSAnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_LevSAnt.
        IMPORT STREAM tmpfil TT_LevSAnt NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_LevSAnt.
    END.
    IF AVAIL TT_LevSAnt THEN
        DELETE TT_LevSAnt.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_LevSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_LevSort Procedure 
PROCEDURE ToTT_LevSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_LevSort.
        IMPORT STREAM tmpfil TT_LevSort NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_LevSort.
    END.
    IF AVAIL TT_LevSort THEN
        DELETE TT_LevSort.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Ordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Ordre Procedure 
PROCEDURE ToTT_Ordre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Ordre.
        IMPORT STREAM tmpfil TT_Ordre NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Ordre.
    END.
    IF AVAIL TT_Ordre THEN
       DELETE TT_Ordre.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Pakkelinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Pakkelinje Procedure 
PROCEDURE ToTT_Pakkelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Pakkelinje.
        IMPORT STREAM tmpfil TT_Pakkelinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Pakkelinje.
    END.
    IF AVAIL TT_Pakkelinje THEN
        DELETE TT_Pakkelinje.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_PkSdlHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_PkSdlHode Procedure 
PROCEDURE ToTT_PkSdlHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_PkSdlHode.
        IMPORT STREAM tmpfil TT_PkSdlHode NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_PkSdlHode.
    END.
    IF AVAIL TT_PkSdlHode THEN
        DELETE TT_PkSdlHode.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_PkSdlLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_PkSdlLinje Procedure 
PROCEDURE ToTT_PkSdlLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_PkSdlLinje.
        IMPORT STREAM tmpfil TT_PkSdlLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_PkSdlLinje.
    END.
    IF AVAIL TT_PkSdlLinje THEN
        DELETE TT_PkSdlLinje.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_PkSdlPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_PkSdlPris Procedure 
PROCEDURE ToTT_PkSdlPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_PkSdlPris.
        IMPORT STREAM tmpfil TT_PkSdlPris NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_PkSdlPris.
    END.
    IF AVAIL TT_PkSdlPris THEN
        DELETE TT_PkSdlPris.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Produsent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Produsent Procedure 
PROCEDURE ToTT_Produsent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Produsent.
        IMPORT STREAM tmpfil TT_Produsent NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Produsent.
    END.
    IF AVAIL TT_Produsent THEN
        DELETE TT_Produsent.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Sasong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Sasong Procedure 
PROCEDURE ToTT_Sasong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Sasong.
        IMPORT STREAM tmpfil TT_Sasong NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Sasong.
    END.
    IF AVAIL TT_Sasong THEN
        DELETE TT_Sasong.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_StrekKode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_StrekKode Procedure 
PROCEDURE ToTT_StrekKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_StrekKode.
        IMPORT STREAM tmpfil TT_StrekKode NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_StrekKode.
    END.
    IF AVAIL TT_StrekKode THEN
        DELETE TT_StrekKode.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_StrKonv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_StrKonv Procedure 
PROCEDURE ToTT_StrKonv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_StrKonv.
        IMPORT STREAM tmpfil TT_StrKonv NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_StrKonv.
    END.
    IF AVAIL TT_StrKonv THEN
        DELETE TT_StrKonv.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_StrTstr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_StrTstr Procedure 
PROCEDURE ToTT_StrTstr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_StrTStr.
        IMPORT STREAM tmpfil TT_StrTStr NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_StrTStr.
    END.
    IF AVAIL TT_StrTStr THEN
        DELETE TT_StrTStr.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_StrType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_StrType Procedure 
PROCEDURE ToTT_StrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_StrType.
        IMPORT STREAM tmpfil TT_StrType NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_StrType.
    END.
    IF AVAIL TT_StrType THEN
        DELETE TT_StrType.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_VareBehBestHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_VareBehBestHode Procedure 
PROCEDURE ToTT_VareBehBestHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_VareBehBestHode.
        IMPORT STREAM tmpfil TT_VareBehBestHode NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_VareBehBestHode.
    END.
    IF AVAIL TT_VareBehBestHode THEN
        DELETE TT_VareBehBestHode.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_VareBehBestLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_VareBehBestLinje Procedure 
PROCEDURE ToTT_VareBehBestLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_VareBehBestLinje.
        IMPORT STREAM tmpfil TT_VareBehBestLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_VareBehBestLinje.
    END.
    IF AVAIL TT_VareBehBestLinje THEN
        DELETE TT_VareBehBestLinje.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_VareBehHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_VareBehHode Procedure 
PROCEDURE ToTT_VareBehHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_VareBehHode.
        IMPORT STREAM tmpfil TT_VareBehHode NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_VareBehHode.
    END.
    IF AVAIL TT_VareBehHode THEN
        DELETE TT_VareBehHode.
    INPUT STREAM tmpfil CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_VareBehLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_VareBehLinje Procedure 
PROCEDURE ToTT_VareBehLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_VareBehLinje.
        IMPORT STREAM tmpfil TT_VareBehLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_VareBehLinje.
    END.
    IF AVAIL TT_VareBehLinje THEN
        DELETE TT_VareBehLinje.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToTT_Varemerke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToTT_Varemerke Procedure 
PROCEDURE ToTT_Varemerke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT STREAM tmpfil FROM VALUE(cTmpFile).
    REPEAT:
        CREATE TT_Varemerke.
        IMPORT STREAM tmpfil TT_Varemerke NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Varemerke.
    END.
    IF AVAIL TT_Varemerke THEN
        DELETE TT_Varemerke.
    INPUT STREAM tmpfil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VareBeh) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VareBeh Procedure 
PROCEDURE VareBeh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ON CREATE OF VareBehHode OVERRIDE DO: END.
    ON WRITE OF VareBehHode OVERRIDE DO: END.
    ON CREATE OF VareBehLinje OVERRIDE DO: END.
    ON WRITE OF VareBehLinje OVERRIDE DO: END.
    ON CREATE OF VareBehBestHode OVERRIDE DO: END.
    ON WRITE OF VareBehBestHode OVERRIDE DO: END.
    ON CREATE OF VareBehBestLinje OVERRIDE DO: END.
    ON WRITE OF VareBehBestLinje OVERRIDE DO: END.
    
    FOR EACH TT_VareBehHode:
        FIND VareBehHode OF TT_VareBehHode NO-LOCK NO-ERROR.
        IF NOT AVAIL VareBehHode THEN DO:
            CREATE VareBehHode.
            BUFFER-COPY TT_VareBehHode TO VareBehHode NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE VareBehHode.
                NEXT.
            END.
            ASSIGN VareBehHode.VareBehNotat = REPLACE(REPLACE(VareBehHode.VareBehNotat,CHR(1),CHR(10)),CHR(2),CHR(13)) NO-ERROR.
        END.
        FOR EACH TT_VareBehLinje OF TT_VareBehHode:
            FIND VareBehLinje OF TT_VareBehLinje NO-LOCK NO-ERROR.
            IF NOT AVAIL VareBehLinje THEN DO:
                CREATE VareBehLinje.
                BUFFER-COPY TT_VareBehLinje TO VareBehLinje NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE VareBehLinje.
                    NEXT.
                END.
                ASSIGN VareBehLinje.LinjeMerknad = REPLACE(REPLACE(VareBehLinje.LinjeMerknad,CHR(1),CHR(10)),CHR(2),CHR(13)) NO-ERROR.
            END.
        END.
        FOR EACH TT_VareBehBestHode OF TT_VareBehHode:
            FIND VareBehBestHode OF TT_VareBehBestHode NO-LOCK NO-ERROR.
            IF NOT AVAIL VareBehBestHode THEN DO:
                CREATE VareBehBestHode.
                BUFFER-COPY TT_VareBehBestHode TO VareBehBestHode NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    DELETE VareBehBestHode.
                    NEXT.
                END.
            END.
            FOR EACH TT_VareBehBestLinje OF TT_VareBehBestHode:
                FIND VareBehBestLinje OF TT_VareBehBestLinje NO-LOCK NO-ERROR.
                IF NOT AVAIL VareBehBestLinje THEN DO:
                    CREATE VareBehBestLinje.
                    BUFFER-COPY TT_VareBehBestLinje TO VareBehBestLinje NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN DO:
                        DELETE VareBehBestLinje.
                        NEXT.
                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

