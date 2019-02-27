&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnhseljournalpos.p.p
    Purpose     :  Innlesning av kvitteringsfil fra Hugin/Sveda S9500 kassene.

    Syntax      :
                    RUN VALUE(pcInnKvittering + ".p") 
                        (INPUT  piButikkNr,
                         INPUT  piGruppeNr,
                         INPUT  piKasseNr,
                         INPUT  Filer.Katalog + "~\" + Filer.FilNavn,
                         INPUT  plFilId,
                         OUTPUT piAntLinjer
                        ).

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  14/02-02
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cFilError       AS CHAR  NO-UNDO.
DEF VAR cError          AS CHAR  NO-UNDO.
DEF VAR piLoop1         AS INT   NO-UNDO.
DEF VAR cLinje          AS CHAR  NO-UNDO.
DEF VAR cFilNavn        AS CHAR  NO-UNDO.
DEF VAR cOFilNavn       AS CHAR  NO-UNDO.
DEF VAR cFilId          AS CHAR  NO-UNDO.
DEF VAR rKvittoRadRecid AS RECID NO-UNDO.
DEF VAR iSlKort         AS INT   NO-UNDO.
DEF VAR bOkStatus       AS LOG   NO-UNDO.
DEF VAR iAntISett       AS INT   NO-UNDO.

DEF VAR iButikkNr       AS INT   NO-UNDO.
DEF VAR iGruppeNr       AS INT   NO-UNDO.
DEF VAR iKasseNr        AS INT   NO-UNDO.
DEF VAR cInnKvittering  AS CHAR  NO-UNDO.
DEF VAR iTotAntLinjer   AS INT   NO-UNDO.
DEF VAR iAntBonger      AS INT   NO-UNDO.
DEF VAR cDatoListe      AS CHAR  NO-UNDO.
DEF VAR iRadNr          AS INT   NO-UNDO.
DEF VAR ikvitto_seq     AS INT   NO-UNDO.
DEF VAR cBkuFil         AS CHAR  NO-UNDO.
DEF VAR cPOSKoder       AS CHAR  NO-UNDO.
DEF VAR cTTIdKoder      AS CHAR  NO-UNDO.
DEF VAR cArtikkelNr     AS CHAR  NO-UNDO.
DEF VAR iLopeNr         AS INT   NO-UNDO.
DEF VAR cTekst          AS CHAR  NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.
DEF VAR cUtbetal        AS CHAR   NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* MottaksKontroll av vilken data vi skall testa mot */

DEF VAR bProfitBase     AS LOG   INITIAL FALSE NO-UNDO.

{s9500.i}

DEF BUFFER bttKvittoRad FOR ttKvittoRad.

DEF STREAM InnFil.

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
         HEIGHT             = 23.62
         WIDTH              = 65.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Varegrupper med utbetalinger */
{syspara.i 210 2 1 cUtbetal}
IF cUtbetal = "" THEN
    cUtbetal = "1976,1995".

/* integrasjon ProfitBase */
SUBSCRIBE TO 'PBR' ANYWHERE.
{syspara.i 50 200 1 cTekst}
IF TRIM(cTekst) = "1" THEN
  bProfitBase = TRUE.
ELSE 
  bProfitBase = FALSE.
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

ASSIGN
  cPOSKoder      = "004,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,081,xxx,091,005,006,007,xxx,092,094,151,165,161,031,xxx,xxx,xxx," + 
                   "xxx,xxx,xxx,xxx,xxx,xxx,xxx,028,xxx,008,xxx,xxx,xxx,xxx,xxx,xxx," + 
                   "022,xxx,xxx,xxx,xxx,xxx,xxx,002,003,xxx,xxx," +
                   "021,050,020,009,011,027,029,080,180,082,083,087,089,095,101,190,013,193,019" +
                   "001,002,003,023,192"

  cTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,022,023,050,051,052,053," + 
                   "054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069," + 
                   "070,071,072,073,080,081,082,096,097,098,099," +
                   "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143" +
                   "200,201,202,203,098"
  .


IF NOT VALID-HANDLE(h_Prisko) THEN
    RUN Prisko.p PERSISTENT SET h_Prisko.

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinnhseljournalpos.p startet.").

FIND Filer NO-LOCK WHERE
    Filer.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE Filer THEN
DO:
    RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " -  Ukjent post 'filer' (" + STRING(lFilId) + ")." + 
             CHR(1) + "1") NO-ERROR.

    RETURN " ** Ukjent Filer post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilId    = STRING(Filer.FilId)
    cFilNavn  = Filer.Katalog + "~\" + Filer.FilNavn
    cOFilNavn = cFilNavn
    cBkuFil   = Filer.Katalog + "~\" + "bku" + "~\" + Filer.FilNavn
    .
   
/* Fil konverteres */
KONVERTER:
DO:
    OVERLAY(cFilNavn , length(cFilNavn) - 2) = "TXT".
    RUN Telleverk IN h_Telleverk
        ("Konverterer fil " + cFilNavn + " til " + cOFilNavn + " .") NO-ERROR.
    OS-COMMAND SILENT VALUE("ldparse " +
                           cOFilNavn +
                           "> " + cFilNavn).
    RUN Telleverk IN h_Telleverk
        (" ") NO-ERROR.
    IF search(cFilNavn) <> ? THEN
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " +
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") +
                   " - Konvertert fil : " + cOFilNavn + "." +
                   CHR(1) + "0").
    ELSE DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " +
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") +
                    " - Klarte ikke å konvertere fil :" + cOFilNavn + "." +
                   CHR(1) + "3").
      RETURN "- Klarte ikke å telle opp linjene i filen " + cOFilNavn + ".".
    END.
END. /* KONVERTER */

/* Tømmer temp-tabellene. */
EMPTY TEMP-TABLE ldpost.
EMPTY TEMP-TABLE ttKvitto.
EMPTY TEMP-TABLE ttKvittoNr.

/* Leser første linjen i filen. */
/*
INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
INPUT STREAM InnFil CLOSE.
*/

RUN koblekasse.p (INPUT lFilId,
                  INPUT h_Parent,
                  INPUT 0,
                  INPUT Filer.FilType,
                  INPUT cLinje,
                  OUTPUT iButikkNr,
                  OUTPUT iGruppeNr,
                  OUTPUT iKasseNr
                 ).
IF (iButikkNr = 0 AND iGruppeNr = 0 AND iKasseNr = 0) THEN
    RETURN "** Kobling av kasse misslykkes.".

/* Konverterer butikknummer */
FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
     ImpKonv.Tabell     = "Butiker" AND
     ImpKonv.EksterntID = TRIM(string(iButikkNr)) NO-LOCK NO-ERROR.
IF AVAILABLE ImpKonv THEN
    iButikkNr = int(ImpKonv.InterntId).

/* Teller opp antall linjer i filen */
RUN Telleverk IN h_Telleverk 
    ("Teller opp linjer i filen. ") NO-ERROR.
RUN TellOppLinjer.
IF bOkStatus = TRUE THEN
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Antall linjer i filen: " + STRING(iTotAntLinjer) + "." + 
               CHR(1) + "0").
ELSE DO:
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Klarte ikk å telle opp linjene i filen." + 
               CHR(1) + "3").
  RETURN "- Klarte ikk å telle opp linjene i filen.".
END.

/* Lerser inn filen i temp-table. */
RUN InnLesFil.
IF RETURN-VALUE = "AVBRYT" THEN DO:
    OS-RENAME value(cOFilNavn) VALUE(REPLACE(cOFilNavn,"DBF","DBFERR")).
    IF SEARCH(cFilNavn) <> ? THEN
        OS-DELETE VALUE(cFilNavn).
    RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                  STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                 " - Klarte ikke å lese inn fil i temp-table: " + cOFilNavn + 
                 CHR(1) + "3").
    RETURN "- Klarte ikke å lese inn fil i temp-table: " + cOFilNavn.
END.

IF bOkStatus = TRUE THEN
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Lest inn fil i temp-table: " + cOFilNavn + " (" + STRING(iTotAntLinjer) + ")." + 
               CHR(1) + "0").
ELSE DO:
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Klarte ikke å lese inn fil i temp-table: " + cOFilNavn + 
               CHR(1) + "3").
  OS-RENAME value(cFilNavn) VALUE(REPLACE(cFilNavn,"TXT","ERR")).
  RETURN "- Klarte ikke å lese inn fil i temp-table: " + cOFilNavn.
END.

/* Lagrer bongene i dtabasen */
RUN LagreBong.
IF bOkStatus = TRUE THEN
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Bongene lagret i databasen: " + STRING(iAntISett) + " bonger." + 
               CHR(1) + "0").
ELSE DO:
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Klarte ikke å lagre bongene i databasen. " + 
               CHR(1) + "3").
  RETURN "- Klarte ikke å lagre bongene i databasen.".
END.

/* /* Oppdaterer akkumulatorer for profitbase.               */ */
/* /* Programmet logger selv i loggtabellen via PBR rutinen. */ */
/* IF bProfitBase THEN                                          */
/* PROFITBASE:                                                  */
/* DO:                                                          */
/*   RUN pfxoppdatstat.p.                                       */
/* END. /* PROFITBASE */                                        */

RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
RUN Telleverk IN h_Parent (" ") NO-ERROR.

/* Renser bort temporær fil */
IF SEARCH(cFilNavn) <> ? THEN
DO:
  /*
  RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " - Filen " + cOFilNavn + " kopiert til backup katalog: " + Filer.Katalog + "~\bku" + 
               CHR(1) + "0").
  OS-CREATE-DIR value(Filer.Katalog + "~\bku").
  OS-COPY value(cOFilNavn) value(cBkuFil).
  IF SEARCH(cBkuFil) <> ? THEN
      OS-DELETE VALUE(cOFilNavn).
  */
    OS-DELETE VALUE(cFilNavn).
END.

/* Stempler filen som oppdatert */
IF SEARCH(cFilNavn) = ? THEN
DO:
  /*RUN SettFilOppdatert IN h_Parent (INPUT string(cFilId)) NO-ERROR.*/
  DO TRANSACTION:
    FIND CURRENT filer EXCLUSIVE-LOCK.
    ASSIGN
      /* Dette er gjort sammtidig. */
      Filer.Innlest       = TRUE
      Filer.InnlestDato   = TODAY
      Filer.InnlestKl     = TIME
      Filer.InnlestAv     = USERID("SkoTex")
      /* Ferdig så langt */
      Filer.Oppdatert     = TRUE
      Filer.OppdatertDato = TODAY
      Filer.OppdatertKl   = TIME
      Filer.OppdatertAv   = USERID("SkoTex")
      .
  END. /* TRANSACTION */
  FIND CURRENT filer NO-LOCK.
END.

IF VALID-HANDLE(h_Prisko) THEN
    DELETE PROCEDURE h_prisko.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BehandleMixMatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandleMixMatch Procedure 
PROCEDURE BehandleMixMatch :
/*------------------------------------------------------------------------------
  Purpose:     Fordeling av Mva på de varelinjer som skal ha Mva.
               Eventuell avrudning/resthåndtering legges på den rad som 
               har størst salgsverdi.
               
               Rutinen kjøres fra doLogId50. Dvs ved slutten av hver bong.
               
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufttKvittoRad  FOR ttKvittoRad.
  DEF BUFFER buf2ttKvittoRad FOR ttKvittoRad.

  DEF VAR pRadRecid    AS RECID NO-UNDO.

  /* Leser MM poster som skal benyttes for makulering. */
  LES-MM-POSTER:
  FOR EACH bufttKvittoRad WHERE
      bufttKvittoRad.kvitto_seq = ttKvitto.Kvitto_seq AND
      bufttKvittoRad.LogId = "4" AND
      bufttKvittoRad.Antall < 0 AND
      bufttKvittoRad.Makulert = FALSE:

      /* Leser alle logid 4 - varesalgsposter for den aktuelle mvakoden. */
      FIND FIRST buf2ttKvittoRad WHERE
          buf2ttKvittoRad.kvitto_seq = ttKvitto.Kvitto_Seq AND
          buf2ttKvittoRad.LogId      = "4" AND
          buf2ttKvittoRad.StrekKode  = bufttKvittoRad.StrekKode AND
          buf2ttKvittoRad.Antall     > 0 AND
          buf2ttKvittoRad.Makulert   = FALSE NO-ERROR.
      IF AVAILABLE buf2ttKvittoRad THEN
          ASSIGN
          buf2ttKvittoRad.Makulert = TRUE.

      /* Ferdigbehandler denne posten */
      ASSIGN
          bufttKvittoRad.Makulert = TRUE
          .
  END. /* LES-MM-POSTER */
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doKvittoSlutt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doKvittoSlutt Procedure 
PROCEDURE doKvittoSlutt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
  CREATE ttKvitto.
  ASSIGN
      ttKvitto.kvitto_seq = ttKvittoNr.kvitto_seq
      ttKvitto.ButikkNr   = iButikkNr
      ttKvitto.KasseNr    = int(ldPost.tno)
      ttKvitto.Dato       = DATE( /* "20010909" */
                                 int(substring(ldPost.transDate,5,2)),
                                 int(substring(ldPost.transDate,7,2)),
                                 int(substring(ldPost.transDate,1,4))
                                 )
      ttKvitto.Tid        = int(substring(ldPost.transTime,1,2)) * 3600 + 
                            int(substring(ldPost.transTime,3,2)) * 60 +
                            int(substring(ldPost.transTime,5,2))
      ttKvitto.BongNr     = int(ldpost.receipt)
      ttKvitto.KassererNr = int(ldPost.oprno)
      ttKvitto.BongStatus = 1 /* Under klargjøring */
      ttKvitto.OpdKvit    = FALSE
      ttKvitto.GruppeNr   = 1
      ttKvitto.KortType   = 0
      .
  ASSIGN
      ttKvitto.subTotal = ldpost.amount
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doKvittoTotal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doKvittoTotal Procedure 
PROCEDURE doKvittoTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/
  DEF BUFFER bldPost FOR ldPost.
        
  CREATE ttKvitto.
  ASSIGN
      ttKvitto.kvitto_seq = ttKvittoNr.kvitto_seq
      ttKvitto.ButikkNr   = iButikkNr
      ttKvitto.KasseNr    = int(ldPost.tno)
      ttKvitto.Dato       = DATE( /* "20010909" */
                                 int(substring(ldPost.transDate,5,2)),
                                 int(substring(ldPost.transDate,7,2)),
                                 int(substring(ldPost.transDate,1,4))
                                 )
      ttKvitto.Tid        = int(substring(ldPost.transTime,1,2)) * 3600 + 
                            int(substring(ldPost.transTime,3,2)) * 60 +
                            int(substring(ldPost.transTime,5,2))
      ttKvitto.BongNr     = int(ldpost.receipt)
      ttKvitto.KassererNr = int(ldPost.oprno)
      ttKvitto.Belop      = ldpost.amount
      ttKvitto.BongStatus = 1 /* Under klargjøring */
      ttKvitto.OpdKvit    = FALSE
      ttKvitto.GruppeNr   = 1
      ttKvitto.KortType   = 0
      .
  /* Settes i doKvittoSlutt 
  IF ldPost.n2 = "0" THEN
  DO:
      FIND bldPost WHERE 
          (bldpost.tno = ttKvittonr.tno) AND
          (bldpost.receipt = ttKvittonr.knr) AND 
          (bldpost.logid = "21") AND
          (bldPost.n2 = "1") NO-ERROR.
      IF AVAILABLE bldPost THEN DO: 
          ttKvitto.subTotal = bldpost.amount.
      END.
      ELSE 
          ttKvitto.subTotal = ldpost.amount.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId101) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId101 Procedure 
PROCEDURE doLogId101 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   IF ldPost.N2 = "2" THEN
   DO:
       assign
           ldPost.logid = "4".
       RUN fixEan.
       assign
           ldPost.logid = "101".
   END.

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittoRad.Antall     = ldPost.quantity
       ttKvittoRad.LinjeSum   = abs(ldPost.amount)
       ttKvittoRad.BongPris   = abs(ldPost.amount)
       ttKvittoRad.Makulert   = TRUE
       .

    /* Flagger den opprinnelige raden som makkulsert. */
   FIND bttKvittoRad WHERE
       RECID(bttKvittoRad) = rKvittoRadRecid NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 101 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 31 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId11) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId11 Procedure 
PROCEDURE doLogId11 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.StrekKode  = ldPost.N1
       ttKvittoRad.TEXT_1     = ldPost.T1 /* Card name */
       ttKvittoRad.BongPris   = DEC(ldPost.amount) 
       ttKvittoRad.LinjeSum   = DEC(ldPost.amount) 
       ttKvittoRad.Antall     = DEC(ldPost.quantity) / 100
       ttKvittoRad.TEXT_2     = ldPost.T2 /* MM Id nr */
       .

   /* Fikser opp i radene på bongen. */
   RUN BehandleMixMatch.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId13) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId13 Procedure 
PROCEDURE doLogId13 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid151) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid151 Procedure 
PROCEDURE doLogid151 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "4".
   RUN fixEan.
   assign
       ldPost.logid = "151".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%       = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort      = iSlKort
       iSlKort                = 0
       .
   /*
   /* Om det är ett negativt tal skall skall kpXkvan inte räknas upp */
   /* såvida det inte är tomglas (cupong?)..                         */
   IF ((ldPost.amount < 0) AND (substring(ldPost.n2, 3, 1) <> "6")) OR
      (SUBSTRING(ldPost.n2, 1, 1)) = "2"  THEN.
   ELSE 
      ttKvittoRad.LinjeSum   = ttKvittoRad.LinjeSum + ldPost.amount.
   */
   ASSIGN
       ttKvittoRad.Antall   = ldPost.quantity 
       ttKvittoRad.BongPris = abs(ldPost.amount)
       ttKvittoRad.LinjeSum = abs(ldPost.amount)
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND bttKvittoRad WHERE
       RECID(bttKvittoRad) = rKvittoRadRecid NO-ERROR.
   /*
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.kvitto_seq = ttKvittoRad.kvitto_seq AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "81" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   */
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 91 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 81 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId152) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId152 Procedure 
PROCEDURE doLogId152 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId153) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId153 Procedure 
PROCEDURE doLogId153 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId161) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId161 Procedure 
PROCEDURE doLogId161 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "4".
   RUN fixEan.
   assign
       ldPost.logid = "161".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = 2
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%       = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort      = iSlKort
       iSlKort                = 0
       .
   
   ASSIGN
       ttKvittoRad.Antall     = ldPost.quantity 
       ttKvittoRad.BongPris   = abs(ldPost.amount)
       ttKvittoRad.LinjeSum   = abs(ldPost.amount).
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr    = ttKvittoRad.KasseNr AND
       bttKvittoRad.BongNr     = ttKvittoRad.BongNr AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "4" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 161 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 4 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId162) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId162 Procedure 
PROCEDURE doLogId162 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId163) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId163 Procedure 
PROCEDURE doLogId163 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId164) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId164 Procedure 
PROCEDURE doLogId164 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "4".
   RUN fixEan.
   assign
       ldPost.logid = "164".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%       = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       .
   
   ASSIGN
       ttKvittoRad.Antall     = ldPost.quantity 
       ttKvittoRad.BongPris   = abs(ldPost.amount)
       ttKvittoRad.LinjeSum   = abs(ldPost.amount).
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr    = ttKvittoRad.KasseNr AND
       bttKvittoRad.BongNr     = ttKvittoRad.BongNr AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "7" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 164 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 7 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId165) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId165 Procedure 
PROCEDURE doLogId165 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "4".
   RUN fixEan.
   assign
       ldPost.logid = "165".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%   = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort      = iSlKort
       iSlKort                = 0
       .
   
   ASSIGN
       ttKvittoRad.Antall     = ldPost.quantity 
       ttKvittoRad.BongPris   = abs(ldPost.amount)
       ttKvittoRad.LinjeSum   = abs(ldPost.amount).
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr    = ttKvittoRad.KasseNr AND
       bttKvittoRad.BongNr     = ttKvittoRad.BongNr AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "81" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 165 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 81 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId166) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId166 Procedure 
PROCEDURE doLogId166 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId169) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId169 Procedure 
PROCEDURE doLogId169 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.StrekKode  = ldPost.N1
       ttKvittoRad.TEXT_1     = ldPost.T1 /* Card name */
       ttKvittoRad.BongPris   = DEC(ldPost.amount)
       ttKvittoRad.LinjeSum   = DEC(ldPost.amount)
       ttKvittoRad.Antall     = DEC(ldPost.quantity) / 100
       ttKvittoRad.Makulert   = TRUE
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr    = ttKvittoRad.KasseNr AND
       bttKvittoRad.BongNr     = ttKvittoRad.BongNr AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "29" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 169 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 29 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId180) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId180 Procedure 
PROCEDURE doLogId180 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* TN 4/3-02 Vi legger ikke opp denne.
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1 /* Card name */
       ttKvittoRad.BongPris   = DEC(ldPost.amount)
       ttKvittoRad.LinjeSum   = DEC(ldPost.amount)
       .
   */
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId19) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId19 Procedure 
PROCEDURE doLogId19 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId190) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId190 Procedure 
PROCEDURE doLogId190 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId192) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId192 Procedure 
PROCEDURE doLogId192 :
/*------------------------------------------------------------------------------
  Purpose:     Spesifikasjon av værdebevis 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = ldPost.TBId

       ttKvittorad.LogId        = ldPost.LogId
       ttKvittoRad.StrekKode    = ldPost.N1
       ttKvittoRad.TEXT_1       = ldPost.T1 /* Card name */
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId193) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId193 Procedure 
PROCEDURE doLogId193 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId2 Procedure 
PROCEDURE doLogId2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.LinjeSum   = ldPost.amount
       ttKvittoRad.BongPris   = ldPost.amount
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId20) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId20 Procedure 
PROCEDURE doLogId20 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = 1
       ttKvittorad.LogId        = ldPost.LogId
       ttKvittoRad.TEXT_1       = ldPost.T1 
       ttKvittoRad.TEXT_2       = ldPost.T2 
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId21) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId21 Procedure 
PROCEDURE doLogId21 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = (IF int(ldPost.N2) = 0
                                   THEN 1
                                   ELSE 2)
       ttKvittorad.LogId        = ldPost.LogId
       ttKvittoRad.TEXT_1       = ldPost.T1 /* Total name */
       ttKvittoRad.Nb2          = int(ldPost.N2)            
       ttKvittoRad.BongPris     = DEC(ldPost.amount)
       ttKvittoRad.LinjeSum     = DEC(ldPost.amount)
       ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
       ttKvittoRad.Avrunding    = dec(ldPost.N3) / 100 
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId22) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId22 Procedure 
PROCEDURE doLogId22 :
/*------------------------------------------------------------------------------
  Purpose:     VEKSEL
  Parameters:  
  Notes:       Fortegn skal alltid snus.
------------------------------------------------------------------------------*/

                                                         
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = 1
       ttKvittorad.LogId        = ldPost.LogId
       ttKvittoRad.TEXT_1       = ldPost.T1 /* VAT name */
       ttKvittoRad.Nb2          = int(ldPost.N2) /* Veksel type */           
       ttKvittoRad.BongPris     = DEC(ldPost.amount2)  * -1 
       ttKvittoRad.LinjeSum     = DEC(ldPost.amount)  * -1  
       ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId23) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId23 Procedure 
PROCEDURE doLogId23 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId27) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId27 Procedure 
PROCEDURE doLogId27 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittorad.Nb2        = int(ldPost.N2)
       ttKvittorad.Nb3        = int(ldPost.N3)
       ttKvittoRad.LinjeSum   = ldPost.amount
       ttKvittoRad.BongPris   = ldPost.amount2
       ttKvittoRad.Antall     = ldPost.Quantity

       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId28) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId28 Procedure 
PROCEDURE doLogId28 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId29) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId29 Procedure 
PROCEDURE doLogId29 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = ldPost.TBId

       ttKvittorad.LogId        = ldPost.LogId
       ttKvittoRad.StrekKode    = ldPost.N1
       ttKvittoRad.TEXT_1       = ldPost.T1 /* Card name */
       ttKvittoRad.BongPris     = DEC(ldPost.amount)
       ttKvittoRad.LinjeSum     = DEC(ldPost.amount)
       /*ttKvittoRad.kpsxKvant  = DEC(ldPost.amount2) / 100*/
       ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
       /* Kvittohode. */
       ttKvitto.ExpDate         = INT(ldPost.N3)
       ttKvitto.flBetalingskort = IF int(ldPost.N2) <= 3
                                    THEN TRUE
                                    ELSE FALSE
       ttKvitto.flBankKort      = IF int(ldPost.N2) <= 2
                                    THEN TRUE
                                    ELSE FALSE
       ttKvitto.flKreditkort    = IF (int(ldPost.N2) >= 3 AND
                                      INT(ldPost.N2) <= 4)
                                    THEN TRUE
                                    ELSE FALSE
       .
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId3 Procedure 
PROCEDURE doLogId3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId31) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId31 Procedure 
PROCEDURE doLogId31 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Betalingsmiddel.
  Notes:       
------------------------------------------------------------------------------*/
   
   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   IF ldPost.N2 = "2" THEN
   DO:
       assign
           ldPost.logid = "4".
       RUN fixEan.
       assign
           ldPost.logid = "31".
   END.

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittoRad.Antall       = ldPost.quantity
       ttKvittoRad.LinjeSum     = ldPost.amount
       ttKvittoRad.BongPris     = ldPost.amount
       ttKvitto.flRekvisisasjon = IF int(ldPost.N2) = 4
                                    THEN TRUE
                                    ELSE FALSE
       ttKvitto.flKupong1       = IF int(ldPost.N2) = 2
                                    THEN TRUE
                                    ELSE FALSE

       ttKvitto.flGavekort      = IF int(ldPost.N2) = 6
                                    THEN TRUE
                                    ELSE FALSE
       .
    /* Tildeling av transaksjonstype */
    CASE int(ldPost.N2):
        WHEN 0 THEN ttKvittoRad.TTId = 50. /* Kontant   */
        WHEN 1 THEN ttKvittoRad.TTId = 54. /* Sjekk     */
        WHEN 2 THEN ttKvittoRad.TTId = 56. /* Kupong    */
        WHEN 3 THEN ttKvittoRad.TTId = 58. /* Bank      */
        WHEN 4 THEN ttKvittoRad.TTId = 65. /* Konto     */
        WHEN 6 THEN ttKvittoRad.TTId = 53. /* Gavekort. */
        OTHERWISE .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId4 Procedure 
PROCEDURE doLogId4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       Varesalg.
               Salg på hovedgruppe 1905 skal håndteres som utbetalinger.
------------------------------------------------------------------------------*/
   /*
      Hänsyn skall tas till logid 91, 7 och 94
      Har alltid EAN nummer
      Alla + poster (utom sold under mixmatch) skall tilldela kolumn kp och kp-r och antal fältet.
      minus - poster skall korigera kp-r och antal
      om SOLD UNDER MIX & MATCH SÅ Skall beloppet tilldelas kp-r och mixMatchSu
      antal skall tilldelas antal och mixmatchKvant.
   
   */

   DEF VAR plMva%  AS DEC NO-UNDO.
   DEF VAR pl%     AS DEC NO-UNDO.

   RUN fixEan.

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.SUMixMatch = IF (SUBSTRING(ldPost.n2, 1, 1)) = "2"  
                                  THEN TRUE /* Flagger at denne er "Solgt under Mix&Match */
                                  ELSE FALSE
       ttKvittoRad.VareGr     = int(LdPost.N3)
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort      = iSlKort
       iSlKort                = 0
       pl%                    = DEC(ldPost.t2) / 100
       .   
   /* Lagrer originalverdier før konvertering */
   IF ttKvittoRad.ForKonvertering = "" THEN
       ASSIGN
       ttKvittoRad.ForKonvertering = FILL(CHR(1),20)
       .
   ASSIGN
       entry(1,ttKvittoRad.ForKonvertering,CHR(1)) = "VarGr=" + STRING(ttKvittoRad.VareGr)
       .

    /* Konverterer varegruppen TN 8/10-03 */
    FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
                       ImpKonv.Tabell     = "VarGr"    AND
                       ImpKonv.EksterntID = TRIM(string(ttKvittoRad.VareGr)) NO-LOCK NO-ERROR.
    IF AVAILABLE ImpKonv THEN
        ttKvittoRad.VareGr = int(ImpKonv.InterntId).
    
    /* Dekoder Mva grupper */
    CASE DEC(ldPost.t2):
        WHEN    0.0 THEN ttKvittoRad.Mva% =  0.0.
        WHEN 2000.0 THEN ttKvittoRad.Mva% = 25.0.
        WHEN 1071.0 THEN ttKvittoRad.Mva% = 12.0.
        WHEN  565.0 THEN ttKvittoRad.Mva% =  6.0.
    END CASE.

    /* Varegruppetekst */
    FIND VarGr NO-LOCK WHERE 
        VarGr.Vg = ttKvittoRad.VareGr NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
        ASSIGN
            ttKvittoRad.VareGruppeNavn = VarGr.VgBeskr
            .
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        IF AVAILABLE HuvGr THEN
            ASSIGN
            ttKvittoRad.HovedGr            = HuvGr.Hg
            ttKvittoRad.HovedGrBeskrivelse = HuvGr.HgBeskr
            .
        ELSE
            ttKvittoRad.HovedGrBeskrivelse = "** Ukjent hovedgruppe *".
    END.
    ELSE
        ttKvittoRad.VareGruppeNavn = "** Ukjent varegruppe **".

    /* Spesiell håndtering av utbetalinger av premier hos Pressbyrån. */
    IF can-do(cUtbetal,string(ttKvittoRad.VareGr)) THEN
        ASSIGN
        ttKvittoRad.Antall   = 0 
        ttKvittoRad.LinjeSum = ldPost.amount * -1
        ttKvittoRad.BongPris = ldPost.amount * -1
        ttKVittoRad.TTId     = 62
        .
    ELSE
        ASSIGN
        ttKvittoRad.Antall   = ldPost.quantity 
        ttKvittoRad.LinjeSum = abs(ldPost.amount)
        ttKvittoRad.BongPris = abs(ldPost.amount)
        ttKvittoRad.MvaKr    = (IF pl% <> 0
                                  THEN round(ttKvittoRad.LinjeSum * (pl% / 100),2)
                                  ELSE 0)
        .

   /* Oppdaterer Mva gruppenavn */
   FIND FIRST Moms NO-LOCK WHERE
       Moms.MomsProc = ttKvittoRad.Mva% NO-ERROR.
   IF AVAILABLE Moms THEN
       ASSIGN
       ttKvittoRad.MvaKod        = Moms.MomsKod
       ttKvittoRad.MvaGruppeNavn = Moms.Beskrivelse
       .
   ELSE
       ttKvittoRad.MvaGruppeNavn = "** Ukjent mva gruppe **".

   /* O såld under mixmatch så skall värdena även in under MixMatch fälten */
   IF SUBSTRING(ldPost.n2, 1, 1) = "2"  THEN DO:
       ASSIGN
           ttKvittorad.mixMatchSA    = ttKvittorad.mixMatchSA + ldPost.amount
           ttKvittorad.mixMatchKvant = ttKvittorad.mixMatchKvant + ldPost.quantity
           .
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid5) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid5 Procedure 
PROCEDURE doLogid5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Ikke for Pressbyrån
   
   RUN fixEan.

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Antall  = ldPost.quantity
       ttKvittoRad.LinjeSum   = ldPost.amount
       ttKvittoRad.BongPris = ldPost.amount2
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.OriginalData = ldPost.OriginalData
       .
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId50) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId50 Procedure 
PROCEDURE doLogId50 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Legger inn avrunding hvis det er gjort avrunding på bongen. */
  IF DEC(ldPost.N1) <> 0 THEN
  AVRUNDING:
  DO:
      CREATE ttKvittoRad.
      ASSIGN
          ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
          ttKvittoRad.ButikkNr     = iButikkNr
          ttKvittoRad.KasseNr      = int(ldPost.tno)
          ttKvittoRad.TransDato    = DATE(ldPost.transDate)
          ttKvittorad.tid          = ldPost.transTime
          ttKvittoRad.BongNr       = int(ldpost.receipt)
          ttKvittoRad.LinjeNr      = iRadNr
          ttKvittoRad.OriginalData = ldPost.OriginalData
          ttKvittoRad.TTId         = 78
          ttKvittoRad.TBId         = 1
          ttKvittorad.LogId        = ldPost.LogId
          ttKvittoRad.TEXT_1       = "AVRUNDING"
          ttKvittoRad.BongPris     = DEC(ldPost.Amount)
          ttKvittoRad.LinjeSum     = (DEC(ldPost.N1) / 100) * -1
          .

  END. /* AVRUNDING */

  CREATE ttKvittorad.
  ASSIGN
      iRadNr                   = iRadNr + 1
      ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
      ttKvittoRad.ButikkNr     = iButikkNr
      ttKvittoRad.KasseNr      = int(ldPost.tno)
      ttKvittoRad.TransDato    = DATE(ldPost.transDate)
      ttKvittorad.tid          = ldPost.transTime
      ttKvittoRad.BongNr       = int(ldpost.receipt)
      ttKvittoRad.LinjeNr      = iRadNr
      ttKvittoRad.OriginalData = ldPost.OriginalData
      ttKvittoRad.TTId         = ldPost.TTId
      ttKvittoRad.TBId         = (IF int(ldPost.N2) = 0
                                  THEN 1
                                  ELSE 2)
      ttKvittorad.LogId        = ldPost.LogId
      ttKvittoRad.TEXT_1       = ldPost.T1 /* Total name */
      ttKvittoRad.Nb2          = int(ldPost.N2)            
      ttKvittoRad.BongPris     = DEC(ldPost.amount)
      ttKvittoRad.LinjeSum     = DEC(ldPost.amount)
      ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
      ttKvittoRad.Avrunding    = dec(ldPost.N1) / 100
      .

  /* Fordeler MVA på radene. */
  RUN FordelMva.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId6) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId6 Procedure 
PROCEDURE doLogId6 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid7) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid7 Procedure 
PROCEDURE doLogid7 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   RUN fixEan.
   
   FIND FIRST ttKvittoRad WHERE
       ttKvittoRad.ButikkNr   = iButikkNr AND
       ttKvittoRad.KasseNr    = int(ldPost.tno) AND
       ttKvittoRad.BongNr     = int(ldpost.receipt) AND 
       ttKvittoRad.StrekKode  = ldpost.n1 NO-ERROR.

   IF NOT AVAILABLE ttkvittoRad THEN DO:
       /* Ukjente logid logges som gravenrede feil. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 7 med EAN: " + ldPost.n1 + 
                      " savner logid 4 post. Butikk/Dato/BongNr/Linje: " + 
                      string(iButikkNr) + "/" +
                      ldPost.TransDat + "/" +
                      ldPost.receipt + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE DO:
       ASSIGN
           ttKvittoRad.SubTotalRab = ttKvittoRad.SubTotalRab + ABS(ldPost.amount)
           .
       /* Om "sold under MixMatch" så minska mixMatchSA oxå */
       IF SUBSTRING(ldPost.n2, 1, 1) = "2"  THEN 
           ttKvittorad.mixMatchSA    = ttKvittorad.mixMatchSA + ldPost.amount.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid8) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid8 Procedure 
PROCEDURE doLogid8 PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = int(ldPost.N2) + 1
       ttKvittorad.LogId        = ldPost.LogId

       ttKvittoRad.TEXT_1       = ldPost.T1 
       ttKvittoRad.Nb2          = int(ldPost.N2)            
       ttKvittoRad.MvaKode      = INT(ldPost.N3) 
       ttKvittoRad.Mva%         = DEC(ldPost.T2) 
       ttKvittoRad.BongPris     = DEC(ldPost.amount) * -1
       ttKvittoRad.LinjeSum     = DEC(ldPost.amount) * -1
       ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid80) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid80 Procedure 
PROCEDURE doLogid80 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF NOT AVAILABLE ttkvitto THEN DO:
       /* Ukjente logid logges som gravenrede feil. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 80 kan ikke oppdatere kvitteringshode " + 
                      " Butikk/Dato/BongNr/Linje: " + 
                      string(iButikkNr) + "/" +
                      ldPost.TransDat + "/" +
                      ldPost.receipt + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE DO:
      ASSIGN
        ttKvitto.aterkop = ldPost.n1
        .
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid81) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid81 Procedure 
PROCEDURE doLogid81 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
  /* Fikser opp EAN koden ved hjelp av identitetbytte */
  ASSIGN
      ldPost.logid = "4"
      .
  RUN fixEan.
  ASSIGN
      ldPost.logid = "81"
      .

  /* Oppretter returtransaksjonen */
  CREATE ttKvittorad.
  ASSIGN
      ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
      ttKvittoRad.ButikkNr   = iButikkNr
      ttKvittoRad.KasseNr    = int(ldPost.tno)
      ttKvittoRad.TransDato  = DATE(ldPost.transDate)
      ttKvittorad.tid        = ldPost.transTime
      ttKvittoRad.BongNr     = int(ldpost.receipt)
      ttKvittoRad.ArtikkelNr = cArtikkelNr
      ttKvittoRad.LopeNr     = iLopeNr
      ttKvittoRad.LinjeNr    = iRadNr
      ttKvittorad.LogId      = ldPost.LogId
      ttKvittoRad.TTId       = ldPost.TTId
      ttKvittoRad.TBId       = ldPost.TBId
      ttKvittoRad.TEXT_1     = ldPost.T1
      ttKvittoRad.StrekKode  = ldpost.n1
      ttKvittoRad.Mva%   = DEC(ldPost.t2) / 100
      ttKvittoRad.LinjeSum   = abs(ldPost.amount)
      ttKvittoRad.Antall     = ldPost.quantity 
      ttKvittoRad.BongPris   = abs(ldPost.amount) 
      ttKvittoRad.OriginalData = ldPost.OriginalData
      /* Setter flagg i bonghodet. */
      ttKvitto.flSlKort      = iSlKort
      iSlKort                = 0
      .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId82) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId82 Procedure 
PROCEDURE doLogId82 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Ikke for Pressbyrån
   /* Fikser opp EAN koden ved hjelp av identitetbytte */
   ASSIGN
       ldPost.logid = "5"
       .
   RUN fixEan.
   ASSIGN
       ldPost.logid = "82"
       .

   /* Oppretter retur på gruppesalg transaksjonen */
   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%   = DEC(ldPost.t2) / 100
       ttKvittoRad.LinjeSum   = ldPost.amount
       ttKvittoRad.Antall  = ldPost.quantity 
       ttKvittoRad.BongPris = ldPost.amount 
       ttKvittoRad.OriginalData = ldPost.OriginalData
      .

  */                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId83) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId83 Procedure 
PROCEDURE doLogId83 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId87) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId87 Procedure 
PROCEDURE doLogId87 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittorad.Nb2        = int(ldPost.N2)
       ttKvittorad.Nb3        = int(ldPost.N3)
       ttKvittoRad.LinjeSum   = ldPost.amount
       ttKvittoRad.BongPris   = ldPost.amount2
       ttKvittoRad.Antall     = ldPost.Quantity
       ttKvittoRad.Makulert   = TRUE
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr    = ttKvittoRad.KasseNr AND
       bttKvittoRad.BongNr     = ttKvittoRad.BongNr AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "27" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 87 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 27 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId89) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId89 Procedure 
PROCEDURE doLogId89 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId

       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.StrekKode  = ldPost.N1
       ttKvittoRad.TEXT_1     = ldPost.T1 
       ttKvittoRad.BongPris   = DEC(ldPost.amount)
       ttKvittoRad.LinjeSum   = DEC(ldPost.amount)
       ttKvittoRad.Antall     = DEC(ldPost.quantity) / 100
       .

    /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr    = ttKvittoRad.KasseNr AND
       bttKvittoRad.BongNr     = ttKvittoRad.BongNr  AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "29" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 89 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 29 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid9) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid9 Procedure 
PROCEDURE doLogid9 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Vi legger kun opp Mva spesifikasjon, når det er noe å spesifisere. */
IF ldPost.Amount <> 0 THEN
DO:
    CREATE ttKvittorad.
    ASSIGN
        ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
        ttKvittoRad.ButikkNr     = iButikkNr
        ttKvittoRad.KasseNr      = int(ldPost.tno)
        ttKvittoRad.TransDato    = DATE(ldPost.transDate)
        ttKvittorad.tid          = ldPost.transTime
        ttKvittoRad.BongNr       = int(ldpost.receipt)
        ttKvittoRad.LinjeNr      = iRadNr
        ttKvittoRad.OriginalData = ldPost.OriginalData
        ttKvittoRad.TTId         = ldPost.TTId
        ttKvittoRad.TBId         = (IF int(ldPost.N2) = 0
                                    THEN 1
                                    ELSE 2)
        ttKvittorad.LogId        = ldPost.LogId
        ttKvittoRad.TEXT_1       = ldPost.T1 /* VAT name */
        ttKvittoRad.Nb2          = int(ldPost.N2) /* VAT type */           
        ttKvittoRad.MvaKode      = INT(ldPost.N3) /* Mva kode */
        ttKvittoRad.Mva%         = DEC(ldPost.T2) / 100
        ttKvittoRad.BongPris     = abs(DEC(ldPost.amount2)) /* Totalt beløp for denne mva koden */
        ttKvittoRad.LinjeSum     = abs(DEC(ldPost.amount))  /* Totalt mva beløp for denne mva koden. */
        ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
        .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid91) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid91 Procedure 
PROCEDURE doLogid91 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "4".
   RUN fixEan.
   assign
       ldPost.logid = "91".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%       = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort      = iSlKort
       iSlKort                = 0
       .
   /*
   /* Om det är ett negativt tal skall skall kpXkvan inte räknas upp */
   /* såvida det inte är tomglas (cupong?)..                         */
   IF ((ldPost.amount < 0) AND (substring(ldPost.n2, 3, 1) <> "6")) OR
      (SUBSTRING(ldPost.n2, 1, 1)) = "2"  THEN.
   ELSE 
      ttKvittoRad.LinjeSum   = ttKvittoRad.LinjeSum + ldPost.amount.
   */
   ASSIGN
       ttKvittoRad.Antall     = ldPost.quantity 
       ttKvittoRad.BongPris   = abs(ldPost.amount)
       ttKvittoRad.LinjeSum   = abs(ldPost.amount).
       .

   /* O såld under mixmatch så skall värdena även in under MixMatch fälten */
   IF SUBSTRING(ldPost.n2, 1, 1) = "2"  THEN DO:
       ASSIGN
           ttKvittorad.mixMatchSA    = ttKvittorad.mixMatchSA + ldPost.amount
           ttKvittorad.mixMatchKvant = ttKvittorad.mixMatchKvant + ldPost.quantity
           .
   END.

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND bttKvittoRad WHERE
       RECID(bttKvittoRad) = rKvittoRadRecid NO-ERROR.
   /*
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.ButikkNr   = ttKvittoRad.ButikkNr AND
       bttKvittoRad.KasseNr = ttKvittoRad.KasseNr AND
       bttKvittoRad.StrekKode        = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "4" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   */
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 91 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 4 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid92) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid92 Procedure 
PROCEDURE doLogid92 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Ikke for pressbyrån
   
   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "5".
   RUN fixEan.
   assign
       ldPost.logid = "92".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = ldPost.TBId
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%   = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.kvitto_seq = ttKvittoRad.kvitto_seq AND
       bttKvittoRad.StrekKode        = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "5" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 92 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 5 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId93) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId93 Procedure 
PROCEDURE doLogId93 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogid94) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogid94 Procedure 
PROCEDURE doLogid94 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Liten fuling för att fixean skall hantera posten exakt som en logid-4 */
   assign
       ldPost.logid = "7".
   RUN fixEan.
   assign
       ldPost.logid = "94".

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr   = iButikkNr
       ttKvittoRad.KasseNr    = int(ldPost.tno)
       ttKvittoRad.TransDato  = DATE(ldPost.transDate)
       ttKvittorad.tid        = ldPost.transTime
       ttKvittoRad.BongNr     = int(ldpost.receipt)
       ttKvittoRad.ArtikkelNr = cArtikkelNr
       ttKvittoRad.LopeNr     = iLopeNr
       ttKvittoRad.LinjeNr    = iRadNr
       ttKvittorad.LogId      = ldPost.LogId
       ttKvittoRad.TTId       = ldPost.TTId
       ttKvittoRad.TBId       = 1
       ttKvittoRad.TEXT_1     = ldPost.T1
       ttKvittoRad.StrekKode  = ldpost.n1
       ttKvittoRad.Mva%   = DEC(ldPost.t2) / 100
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.Makulert   = TRUE
       /* Setter flagg i bonghodet. */
       ttKvitto.flSlKort      = iSlKort
       iSlKort                = 0
       .
   ASSIGN
       ttKvittoRad.Antall     = ldPost.quantity 
       ttKvittoRad.BongPris   = abs(ldPost.amount)
       ttKvittoRad.LinjeSum   = abs(ldPost.amount).
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND bttKvittoRad WHERE
       RECID(bttKvittoRad) = rKvittoRadRecid NO-ERROR.
   /*
   FIND FIRST bttKvittoRad WHERE
       bttKvittoRad.kvitto_seq = ttKvittoRad.kvitto_seq AND
       bttKvittoRad.StrekKode  = ttKvittoRad.StrekKode AND
       bttKvittoRad.LogId      = "7" AND
       bttKvittoRad.Makulert   = FALSE NO-ERROR.
   */
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 94 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 7 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLogId95) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLogId95 Procedure 
PROCEDURE doLogId95 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CREATE ttKvittorad.
   ASSIGN
       ttKvittoRad.kvitto_seq   = ttKvitto.kvitto_seq
       ttKvittoRad.ButikkNr     = iButikkNr
       ttKvittoRad.KasseNr      = int(ldPost.tno)
       ttKvittoRad.TransDato    = DATE(ldPost.transDate)
       ttKvittorad.tid          = ldPost.transTime
       ttKvittoRad.BongNr       = int(ldpost.receipt)
       ttKvittoRad.LinjeNr      = iRadNr
       ttKvittoRad.OriginalData = ldPost.OriginalData
       ttKvittoRad.TTId         = ldPost.TTId
       ttKvittoRad.TBId         = int(ldPost.N2) + 1
       ttKvittorad.LogId        = ldPost.LogId

       ttKvittoRad.TEXT_1       = ldPost.T1 
       ttKvittoRad.Nb2          = int(ldPost.N2)            
       ttKvittoRad.MvaKode      = INT(ldPost.N3) 
       ttKvittoRad.Mva%         = DEC(ldPost.T2) 
       ttKvittoRad.BongPris     = DEC(ldPost.amount)
       ttKvittoRad.LinjeSum     = DEC(ldPost.amount) 
       ttKvittoRad.Antall       = DEC(ldPost.quantity) / 100
       ttKvittoRad.Makulert     = TRUE
       .

   /* Flagger den opprinnelige raden som makkulsert. */
   FIND bttKvittoRad WHERE
       RECID(bttKvittoRad) = rKvittoRadRecid NO-ERROR.
   IF NOT AVAILABLE bttKvittoRad THEN
   DO:
       /* Finner ikke rad som skal makuleres. */
       RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Logid post 95 med EAN: " + ttKvittoRad.StrekKode + 
                      " savner logid 8 post. Butikk/Dato/BongNr/Linje: " + 
                      string(ttKvittoRad.KasseNr) + "/" +
                      string(ttKvittoRad.TransDato) + "/" +
                      string(ttKvittoRad.BongNr) + "/" + 
                      STRING(iAntLinjer + 1) + 
                      CHR(1) + "3").
   END.
   ELSE
       ASSIGN
           bttKvittoRad.Makulert = TRUE
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixEan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixEan Procedure 
PROCEDURE fixEan :
/*------------------------------------------------------------------------------
  Purpose: Fixa till Ean numret.    
  Parameters:  <none>
  Notes: Om numret som ligger i number_1 i lf posten är längre än X så är det ett
         ean nummer som skall vara som det är.
         Om det är mindre så kan det vara något av följande:
         PLU - nummer, fritt valt av butiken eller tilldelat centralt
         av konsum värmland.
         Om logid = 5 så är varan såld under sin grupptillhörighet, alltså
         finns det vare sig ean eller PLU man använder i stället varugruppen.
         
------------------------------------------------------------------------------*/
DEF VAR i            AS INT  NO-UNDO.
DEF VAR plArtikkelNr AS DEC  NO-UNDO.
DEF VAR pdDato       AS DATE NO-UNDO.
DEF VAR cKode        AS CHAR NO-UNDO.
/*
** en liten extra uppgift som inte har med ean numret att göra men som 
** görs bäst här.
*/
ASSIGN
    ldpost.n2   = FILL("0", 3 - LENGTH(ldPost.n2)) + ldpost.n2
    ldPost.n1   = TRIM(ldPost.n1)
    cArtikkelNr = ""
    pdDato      = DATE( /* "20010909" */
                       int(substring(ldPost.transDate,5,2)),
                       int(substring(ldPost.transDate,7,2)),
                       INT(substring(ldPost.transDate,1,4))
                      )
    .
/* Sjekker strekkode som den kommer fra kassen. */
FIND Strekkode NO-LOCK WHERE
    Strekkode.Kode = ldPost.N1 NO-ERROR.
IF NOT AVAILABLE Strekkod THEN
DO:
    /* Er det en EAN 8? Er det det, sjekkes med og uten nullutfylling. */
    IF LENGTH(STRING(DEC(ldPost.n1))) = 8 THEN
    DO:
        /* Tetster med 8 siffer. */
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = STRING(DEC(ldPost.n1)) NO-ERROR.
        IF AVAILABLE Strekkode THEN
            ldPost.N1 = Strekkode.Kode.
        /* Tester med 13 siffer */
        ELSE DO:
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = "00000" + STRING(DEC(ldPost.n1)) NO-ERROR.
            IF AVAILABLE Strekkode THEN
                ldPost.N1 = Strekkode.Kode.
        END.
    END.
    /* Sjekker uten nullutfylling */
    ELSE DO:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = LEFT-TRIM(ldPost.n1,"0") NO-ERROR.
        IF AVAILABLE STrekkode THEN
            ldPost.n1 = Strekkode.Kode.
    END.
END.

/* Lagrer originalverdier før konvertering */
IF AVAIL ttKvittorad THEN DO:
    IF ttKvittoRad.ForKonvertering = "" THEN
        ASSIGN
        ttKvittoRad.ForKonvertering = FILL(CHR(1),20)
        .
    ASSIGN
        entry(2,ttKvittoRad.ForKonvertering,CHR(1)) = "Strekkode=" + STRING(ttKvittoRad.Strekkode)
        .
END.
/* Konverterer strekkoden TN 8/10-03 */
FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk"  AND
                   ImpKonv.Tabell     = "Strekkode" AND
                   ImpKonv.EksterntID = TRIM(ldPost.n1) NO-LOCK NO-ERROR.
IF AVAILABLE ImpKonv THEN
    ldPost.n1 = ImpKonv.InterntId.

CASE ldPost.logId:
    WHEN "4" THEN DO: /* Article sales */
       IF LENGTH(ldPost.n1) < 8 THEN DO:
          i = INT(ldPost.n1).
          /*
          /* Konsum värmlands fria intervall som butiekerna själva förfogar över */
          IF (i > 999) AND (i < 2000) THEN
             ldPost.n1 = "F" + butik + ldPost.n1.
          ELSE
             ldPost.n1 = "P" + ldPost.n1.
          */
       END.
    END.
    WHEN "5" THEN DO: /* Group Sales */
       ldPost.n1 = "V" + ldPost.n3.
    END.
    WHEN "7" THEN DO: /* Adjusted item */
       IF LENGTH(ldPost.n1) < 8 THEN DO:
          IF ldpost.n1 = ""  THEN
              ldPost.n1 = "V" + ldPost.n3.
          ELSE DO:
              i = INT(ldPost.n1).
              /*
              /* Konsum värmlands fria intervall som butiekerna själva förfogar över */
              IF (i > 999) AND (i < 2000) THEN
                ldPost.n1 = "F" + butik + ldPost.n1.
              ELSE
                ldPost.n1 = "P" + ldPost.n1.
              */
          END.
       END.
    END.
END CASE.

/* Setter postens artikkelNr */
IF AVAILABLE Strekkode THEN
DO:
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    ASSIGN
        cArtikkelNr = string(Strekkode.ArtikkelNr)
        iLopeNr     = IF AVAILABLE ArtBas
                        THEN ArtBas.LopNr
                        ELSE 0
        .
END.
ELSE DO:
  ASSIGN
      plArtikkelNr = DEC(ldPost.N1) NO-ERROR.
  IF plArtikkelNr <> 0 THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      ASSIGN
      cArtikkelNr = string(ArtBas.ArtikkelNr)
      iLopeNr     = ArtBas.LopNr
      .
END.

/* Sjekker om det er en artikkel som skal følges opp. */
ASSIGN
    iSlKort = 0
    .
/*
IF CAN-FIND(SlKort WHERE
            SlKort.KortId = ldPost.N1) THEN
*/
IF CAN-FIND(FIRST AnalyseArtikkel WHERE
            AnalyseArtikkel.ArtikkelNr  = DEC(ldPost.N1) AND
            AnalyseArtikkel.Aktiv       = TRUE AND
            AnalyseArtikkel.StartDato  <= pdDato AND
            AnalyseArtikkel.SluttDato  >= pdDato) THEN
    ASSIGN
    iSlKort = 1
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FordelMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FordelMva Procedure 
PROCEDURE FordelMva :
/*------------------------------------------------------------------------------
  Purpose:     Fordeling av Mva på de varelinjer som skal ha Mva.
               Eventuell avrudning/resthåndtering legges på den rad som 
               har størst salgsverdi.
               
               Rutinen kjøres fra doLogId50. Dvs ved slutten av hver bong.
               
  Parameters:  <none>
  Notes:       TN 3/5-03 Mva legges på ved LogId 4. Der legges også korrekt 
               MvaGruppe inn.
------------------------------------------------------------------------------*/
/*   DEF BUFFER bufttKvittoRad  FOR ttKvittoRad.                                                    */
/*   DEF BUFFER buf2ttKvittoRad FOR ttKvittoRad.                                                    */
/*                                                                                                  */
/*   DEF VAR pRadRecid    AS RECID NO-UNDO.                                                         */
/*   DEF VAR plMaksBelop  AS DEC   NO-UNDO.                                                         */
/*   DEF VAR plTildeltMva AS DEC   NO-UNDO.                                                         */
/*   DEF VAR plAndel%     AS DEC   NO-UNDO.                                                         */
/*                                                                                                  */
/*   /* Leser Mva spesifikasjonspostene. */                                                         */
/*   LES-MVA-POSTER:                                                                                */
/*   FOR EACH bufttKvittoRad WHERE                                                                  */
/*       bufttKvittoRad.kvitto_seq = ttKvitto.Kvitto_seq AND                                        */
/*       bufttKvittoRad.LogId = "9" AND                                                             */
/*       bufttKvittoRad.LinjeSum <> 0 AND                                                           */
/*       bufttKvittoRad.Makulert = FALSE:                                                           */
/*                                                                                                  */
/*       ASSIGN                                                                                     */
/*           plTildeltMva = 0                                                                       */
/*           .                                                                                      */
/*       /* Leser alle logid 4 - varesalgsposter for den aktuelle mvakoden. */                      */
/*       KVITTORADEN:                                                                               */
/*       FOR EACH buf2ttKvittoRad WHERE                                                             */
/*           buf2ttKvittoRad.kvitto_seq = ttKvitto.Kvitto_Seq AND                                   */
/*           buf2ttKvittoRad.LogId      = "4" AND                                                   */
/*           buf2ttKvittoRad.Mva%       = bufttKvittoRad.Mva% AND                                   */
/*           buf2ttKvittoRad.Makulert   = FALSE                                                     */
/*           BREAK BY buf2ttKvittoRad.LinjeSum:                                                     */
/*                                                                                                  */
/*           ASSIGN                                                                                 */
/*               /* Beregner % andel og posterer andel av Mva. */                                   */
/*               plAndel%              = (buf2ttKvittoRad.LinjeSum / bufttKvittoRad.BongPris) * 100 */
/*               /* Posterer andel av Mva beløp. */                                                 */
/*               buf2ttKvittoRad.MvaKr   = ROUND((bufttKvittoRad.LinjeSum * plAndel%) / 100,2)      */
/*               buf2ttKvittoRad.MvaKode = bufttKvittoRad.MvaKode                                   */
/*               plTildeltMva            = plTildeltMva + buf2ttKvittoRad.MvaKr                     */
/*               .                                                                                  */
/*           /* Håndterer eventuel avrundingsfeil. */                                               */
/*           IF LAST(buf2ttKvittoRad.LinjeSum) THEN                                                 */
/*           DO:                                                                                    */
/*               ASSIGN                                                                             */
/*                   buf2ttKvittoRad.MvaKr = buf2ttKvittoRad.MvaKr +                                */
/*                                           (bufttKvittoRad.LinjeSum - plTildeltMva)               */
/*                   plTildeltMva          = 0                                                      */
/*                   .                                                                              */
/*           END.                                                                                   */
/*                                                                                                  */
/*       END. /* KVITTORADEN */                                                                     */
/*   END. /* LES-MVA-POSTER */                                                                      */
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentVareLinjeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentVareLinjeInfo Procedure 
PROCEDURE HentVareLinjeInfo :
/*------------------------------------------------------------------------------
  Purpose:     Hent varelinjeinfo
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  /* Henter mva koden.                                      */
  /* Forutsetter at det ikke ligger to koder med samme mva% */
  FIND FIRST Moms NO-LOCK WHERE
      Moms.MomsProc = BongLinje.Mva% NO-ERROR.
  IF AVAILABLE Moms THEN
      ASSIGN
      BongLinje.MvaGr         = Moms.MomsKod
      BongLinje.MvaGruppeNavn = Moms.Beskrivelse
      .
  ELSE DO:
      ASSIGN
      BongLinje.MvaGr         = 0
      BongLinje.MvaGruppeNavn = "** Ukjent momskode **"
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent MvaKode på transaksjonen " + string(BongLinje.MvaGr) + 
               "."
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent MvaKode på transaksjonen " + string(BongLinje.MvaGr) + 
               "." + CHR(1) + "2"
      .
  END.

  /* Henter feilkodeteksten */
  FIND FIRST FeilKode NO-LOCK WHERE
      FeilKode.FeilKode = BongLinje.FeilKode NO-ERROR.
  IF AVAILABLE FeilKode THEN
      BongLinje.FeilKodeTekst = FeilKode.Beskrivelse.
  ELSE DO:
      ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent feilkode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent feilkode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "." + CHR(1) + "2"
          BongLinje.FeilKodeTekst = "** Ukjent feilkode **".
  END.

  /* Henter tiltakskode */
  FIND FIRST KravKode NO-LOCK WHERE
      KravKode.KravKode = BongLinje.NotatKode NO-ERROR.
  IF AVAILABLE KravKode THEN
      BongLinje.NotatKodeTekst = KravKode.Beskrivelse.
  ELSE DO:
      ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent tiltakskode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent tiltakskode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "." + CHR(1) + "2"
          BongLinje.NotatKodeTekst = "** Ukjent tiltakskode **"
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnLesFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnLesFil Procedure 
PROCEDURE InnLesFil :
DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR pcLinje      AS CHAR  NO-UNDO.
  DEF VAR pcOLinje     AS CHAR  NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR pcDato       AS CHAR  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pcSokMaske   AS CHAR  NO-UNDO.
  DEF VAR pcOSokMaske  AS CHAR  NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.
  DEF VAR piEntry      AS INT   NO-UNDO.
  DEF VAR pcPOS        AS CHAR  NO-UNDO.
  DEF VAR iTst         AS INTE  NO-UNDO.

  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = ""
      pcOSokMaske = ""
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      iAntBonger  = 0
      iKvitto_seq = 0
      bOkStatus   = FALSE
      .
  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  /*
  /* TEST TEST TEST */
  INPUT STREAM InnFil FROM VALUE("C:\Home\pressbyran\ankommet\05113\LD040102-TEST.TXT") NO-ECHO.
  */
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO. 
  /* INPUT STREAM InnFil THROUGH VALUE("./ldparse " + cOFilNavn) NO-ECHO. */
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    CREATE ldPost.
    IMPORT STREAM InnFil ldpost NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DELETE ldPost.
        NEXT.
    END.
    IF TRIM(ldPost.logid) = "4" THEN DO:
        ASSIGN 
            iTst      = INT(ldPost.t2) 
            ldPost.N1 = TRIM(ldPost.N1)
            NO-ERROR. /* momsfält ibland fel */
        IF ERROR-STATUS:ERROR THEN DO:
            /* om det är en vara, ie fält logid = 4 hämtar jag momsen från vargrtabellen */
            /* och lägger in i t2, om inte vargr/momsen finns lägger jag in "0001071" (12%) */
            /* finn vargr från artbas genom strekkode */
            /* Sjekker strekkode med og uten nullutfylling. */
/*             FIND Strekkode NO-LOCK WHERE                                       */
/*                 Strekkode.Kode = trim(ldPost.N1) NO-ERROR.                     */
/*             IF NOT AVAILABLE Strekkod THEN                                     */
/*                 FIND Strekkode NO-LOCK WHERE                                   */
/*                     Strekkode.Kode = LEFT-TRIM(trim(ldPost.n1), "0") NO-ERROR. */
            /* Sjekker strekkode som den kommer fra kassen. */
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = ldPost.N1 NO-ERROR.
            IF NOT AVAILABLE Strekkod THEN
            DO:
                /* Er det en EAN 8? Er det det, sjekkes med og uten nullutfylling. */
                IF LENGTH(STRING(DEC(ldPost.n1))) = 8 THEN
                DO:
                    /* Tetster med 8 siffer. */
                    FIND Strekkode NO-LOCK WHERE
                        Strekkode.Kode = STRING(DEC(ldPost.n1)) NO-ERROR.
                    IF AVAILABLE Strekkode THEN
                        ldPost.N1 = Strekkode.Kode.
                    /* Tester med 13 siffer */
                    ELSE DO:
                        FIND Strekkode NO-LOCK WHERE
                            Strekkode.Kode = "00000" + STRING(DEC(ldPost.n1)) NO-ERROR.
                        IF AVAILABLE Strekkode THEN
                            ldPost.N1 = Strekkode.Kode.
                    END.
                END.
                /* Sjekker uten nullutfylling */
                ELSE DO:
                    FIND Strekkode NO-LOCK WHERE
                        Strekkode.Kode = LEFT-TRIM(ldPost.n1,"0") NO-ERROR.
                    IF AVAILABLE STrekkode THEN
                        ldPost.n1 = Strekkode.Kode.
                END.
            END.

            IF AVAIL strekkode THEN
                FIND artbas OF strekkode NO-LOCK NO-ERROR.
            IF AVAIL Artbas THEN
                FIND VarGr OF artbas.
            /* om inte funnen VarGr: Finn konverterade varegruppen */
            IF NOT AVAIL VarGr THEN FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
                                         ImpKonv.Tabell     = "VarGr"    AND
                                         ImpKonv.EksterntID = TRIM(string(ldPost.n3)) NO-LOCK NO-ERROR.
            IF AVAILABLE ImpKonv THEN
                FIND VarGr WHERE VarGr.Vg = int(ImpKonv.InterntId) NO-LOCK NO-ERROR.
            IF AVAIL VarGr THEN DO:
                FIND Moms OF VarGr NO-LOCK NO-ERROR.
                RELEASE VarGr.
                IF AVAIL Moms THEN DO:
                    ASSIGN ldpost.t2 = IF Moms.momsproc = 6.0 THEN "0000565" ELSE 
                        string(round(Moms.Momsproc / (Moms.Momsproc + 100),4) * 10000,"9999999").
                END.
                ELSE 
                    ASSIGN ldpost.t2 = "0001071".
            END.
            ELSE 
                ASSIGN ldpost.t2 = "0001071".
        END.
    END.
    ASSIGN iTst = INT(ldPost.transTime) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        ASSIGN ldPost.transTime = "000000".
    ASSIGN
        iantLinjer  = iantLinjer + 1
        iKvitto_seq = iKvitto_seq + 1
        .
    /* Setter originaldata */
    ASSIGN
        ldPost.OriginalData = ldPost.recno + CHR(1) + 
                              ldPost.logid + CHR(1) +      
                              ldPost.tno + CHR(1) +        
                              ldPost.oprno + CHR(1) +      
                              ldPost.receipt + CHR(1) +    
                              ldPost.transDate + CHR(1) +  
                              ldPost.transTime + CHR(1) +  
                              string(ldPost.n1) + CHR(1) +         
                              ldPost.t1 + CHR(1) +         
                              string(ldPost.n2) + CHR(1) +         
                              string(ldPost.n3) + CHR(1) +         
                              string(ldPost.amount) + CHR(1) +     
                              string(ldPost.amount2) + CHR(1) +    
                              string(ldPost.quantity) + CHR(1) +   
                              ldPost.t2 + CHR(1) +         
                              ldPost.tag + CHR(1) +        
                              string(ldPost.TTId) + CHR(1) +       
                              string(ldPost.TBId) + CHR(1) +       
                              string(ldPost.Tid) + CHR(1) + 
                              STRING(iAntLinjer).       

    /* Skaper peker til bonger i TEMP-FILE.                                  */
    /* Flagger også om bongen inneholder totaler eller om det kun er en info */
    /* kvittering.                                                           */
    IF can-do("21,50",ldPost.logid) /* TOTAL=21, KVITTO SLUT=50 */ THEN 
    KVITTERING:
    DO:
      IF NOT CAN-FIND(ttKvittoNr WHERE 
          (ttKvittoNr.tno = ldPost.tno) AND
          (ttKvittoNr.knr = ldPost.receipt)) THEN 
      OPPSTANDELSEN:
      DO:
        CREATE ttKvittoNr.
        ASSIGN
            ttKvittoNr.kvitto_seq = iKvitto_seq
            ttKvittoNr.tno        = ldPost.tno
            ttKvittoNr.knr        = ldPost.receipt
            iAntBonger            = iAntBonger + 1
            .
        /* Oppretter kvitteringshode */
        IF ldpost.logid = "21" /*AND
           ldPost.n2 = "0"*/ THEN
          RUN doKvittoTotal. /* Total and subtotal. */
        ELSE IF ldPost.LogId = "50" THEN
            RUN doKvittoSlutt. /* Receipt completed */

      END. /* OPPSTANDELSEN */
      /* Oppdaterer beløp i kvittohode. */
      ELSE IF can-do("50",ldPost.logid) THEN DO:
          FIND ttKvittoNr WHERE 
              (ttKvittoNr.tno = ldPost.tno) AND
              (ttKvittoNr.knr = ldPost.receipt) NO-ERROR.
          IF AVAILABLE ttKvittoNr THEN
              FIND ttKvitto WHERE
                   ttKvitto.kvitto_seq = ttKvittoNr.kvitto_seq NO-ERROR.
          IF AVAILABLE ttKvitto THEN
              ASSIGN
              ttKvitto.Belop = ldpost.amount
              .
      END.
    END. /* KVITTERING */

    IF iAntLinjer MODULO 100 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + Filer.Katalog + "\" + Filer.FilNavn + 
           " Leser record " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  RUN SletterAvbrutteKvitteringer.

  ASSIGN
      piLoop = 0
      .

  /* Går igjennom alle poster, kvittering for kvittering. */
  /* LD-Postene er sortert på LogId og TerminalNr/KvittoNr      */
  KVITTO:
  FOR EACH ttKvittonr:
      ASSIGN
          piLoop = piLoop + 1
          .
      IF piLoop MODULO 25 = 0 THEN
      DO:
        RUN Telleverk IN h_Telleverk 
            ("Fil: " + string(Filer.FilId) + " " + 
             Filer.FilNavn + " Oppretter bong " + STRING(piLoop) +
             " av " + STRING(iAntBonger) + ".") NO-ERROR.
      END.
      /* Henter kvitteringshode */
      FIND FIRST ttKvitto where
          ttKvitto.kvitto_seq   = ttKvittoNr.kvitto_seq NO-ERROR.

      ASSIGN
          iRadNr   = 0
          rKvittoRadRecid = ?
          .
      KVITTOLINJER:
      FOR EACH ldPost WHERE 
          (ldpost.tno     = ttKvittonr.tno) AND
          (ldpost.receipt = ttKvittonr.knr)
          BY ldPost.Tno
          BY ldPost.receipt 
          BY int(ldPost.recno):

          /* Konverterer transaksjonskoden */
          KONVTRANSKODE:
          DO:
              ASSIGN
                pcPOS        = string(int(ldPost.LogId),"999")
                piEntry      = LOOKUP(pcPOS,cPOSKoder)
                ldPost.TTId  = 0
                ldPost.TbId  = 1
                .
              IF piEntry = 0 THEN
                  ASSIGN
                  ldPost.TTId = 0
                  ldPost.TbId = 1
                  .
              ELSE
                  ASSIGN
                    ldPost.TTId = int(ENTRY(piEntry,cTTIdKoder))
                    ldPost.TbId = 1
                  NO-ERROR.
          END. /* KONVTRANSKODE */

          ASSIGN
              iRadNr = iRadNr + 1
              .
          /* Behandler linjene */
          CASE ldPost.LogId:
              WHEN   "2" THEN RUN doLogid2.   /* Sign-on */
              WHEN   "3" THEN RUN doLogid3.   /* Sign-of */
              WHEN   "4" THEN RUN doLogid4.   /* Article sales */
              WHEN   "5" THEN RUN doLogid5.   /* IKKE IMPLEMENTERT Group sales */
              WHEN   "6" THEN RUN doLogId6.   /* IKKE IMPLEMENTERT Department Sales */
              WHEN   "7" THEN RUN doLogid7.   /* Adjustet item */
              WHEN   "8" THEN RUN doLogid8.   /* Adjusted total - Rabatter på kvittonivå */
              WHEN   "9" THEN RUN doLogid9.   /* Olika Moms grejor */
              WHEN  "11" THEN RUN doLogid11.  /* Mix and Match */
              WHEN  "13" THEN RUN doLogid13.  /* IKKE IMPLEMENTERT Id Nr */
              WHEN  "19" THEN RUN doLogid19.  /* IKKE IMPLEMENTERT customer park */
              WHEN  "20" THEN RUN doLogId20.  /* Bongkopi utskrevet. */                  
              WHEN  "21" THEN RUN doLogId21.  /* Total and subtotal. Håndteres ved load. */                  
              WHEN  "22" THEN RUN doLogId22.  /* Change. */                  
              WHEN  "23" THEN RUN doLogid23.  /* No sale */
              WHEN  "27" THEN RUN doLogid27.  /* Invoice */
              WHEN  "28" THEN RUN doLogid28.  /* IKKE IMPLEMENTERT Paid in & Paid out */
              WHEN  "29" THEN RUN doLogid29.  /* Credit/Charge card */
              WHEN  "50" THEN RUN doLogId50.  /* Recit completed. Håndteres ved load. */    
              WHEN  "31" THEN RUN doLogid31.  /* Payment type */
              WHEN  "80" THEN RUN doLogid80.  /* Return number */
              WHEN  "81" THEN RUN doLogid81.  /* Return of article sales sales */
              WHEN  "82" THEN RUN doLogid82.  /* IKKE IMPLEMENTERT Return group sales */
              WHEN  "83" THEN RUN doLogid83.  /* IKKE IMPLEMENTERT Return department sales */
              WHEN  "87" THEN RUN doLogid87.  /* Return invoice */
              WHEN  "89" THEN RUN doLogid89.  /* Return credit/charge cark */
              WHEN  "91" THEN RUN doLogid91.  /* Line void article sales(91) */
              WHEN  "92" THEN RUN doLogid92.  /* IKKE IMPLEMENTERT Line void, Goup sales */
              WHEN  "93" THEN RUN doLogid93.  /* IKKE IMPLEMENTERT Line void department sales */
              /* Skal ikke forekomme hos Pressbyrån
              WHEN  "94" THEN RUN doLogid94.  /* Line void adjusted item */
              WHEN  "95" THEN RUN doLogid95.  /* Line void adjusted total */
              */
              WHEN "101" THEN RUN doLogid101. /* Line void, payment type */
              WHEN "151" THEN RUN doLogid151. /* Line void, return of article sales */
              WHEN "152" THEN RUN doLogid152. /* IKKE IMPLEMENTERT Line void return group sales */
              WHEN "153" THEN RUN doLogid153. /* IKKE IMPLEMENTERT Line void return department sales */
              WHEN "161" THEN RUN doLogid161. /* Past void article sales */
              WHEN "162" THEN RUN doLogid162. /* IKKE IMPLEMENTERT Past void goup sales */
              WHEN "163" THEN RUN doLogid163. /* IKKE IMPLEMENTERT Past void department sales */
              /* Skal ikke forekomme hos Pressbyrån
              WHEN "164" THEN RUN doLogid164. /* Past void, adjusted items */
              */
              WHEN "165" THEN RUN doLogId165. /* Past void, return of artikle sale. */
              WHEN "166" THEN RUN doLogid166. /* IKKE IMPLEMENTERT Past void return group sales */
              WHEN "169" THEN RUN doLogid169. /* Past void, credit charge card */
              WHEN "180" THEN RUN doLogid180. /* Cashdrawer change amount */
              WHEN "190" THEN RUN doLogid190. /* IKKE IMPLEMENTERT More */
              WHEN "193" THEN RUN doLogid193. /* IKKE IMPLEMENTERT Item not found */
              OTHERWISE DO:
                /* Ukjente logid logges som gravenrede feil. */
                RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                               " - Ukjent LogId på linje " + (IF num-entries(ldPost.OriginalData,CHR(1)) >= 20 
                                                                THEN entry(20,ldPost.OriginalData,CHR(1))
                                                                ELSE "0") + 
                               ". (" + ldPost.LogId + ")." + 
                               CHR(1) + "3").
              END.
          END CASE.

          /* "Husker" siste behandlede post. */
          ASSIGN
              rKvittoRadRecid = RECID(ttKvittoRad)
              .
      END. /* KVITTOLINJER */

      DELETE ttKvittonr.
  END. /* KVITTO */

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
          DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
      FIND CURRENT Filer EXCLUSIVE-LOCK.
  END.

  /* Flagger ok på innlesning */
  IF CAN-FIND(FIRST ttKvitto) THEN
      ASSIGN
      bOkStatus     = TRUE
      .
  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

  RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
  RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBong Procedure 
PROCEDURE LagreBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR prRowId      AS ROWID NO-UNDO.
DEF VAR plDataSettId AS DEC   NO-UNDO.
DEF VAR piSettNr     AS INT   NO-UNDO.
DEF VAR piAntBonger  AS INT   NO-UNDO.
DEF VAR d31DecFgAr AS DATE NO-UNDO.
ASSIGN
    iAntISett   = 0
    prRowId     = ?
    piAntBonger = 0
    bOkStatus   = FALSE
    .

/* Leser alle bonger i temp-file og lagrer dem i databasen. */
LAGRE-KVITTO:
FOR EACH ttKvitto 
    BREAK BY ButikkNr 
          BY GruppeNr 
          BY KasseNr 
          BY Dato 
          BY BongNr TRANSACTION:

    ASSIGN
        cError    = ""
        cFilError = ""
        .

    /* Oppretter datasett og BongHode. */
    IF FIRST-OF(ttKvitto.Dato) THEN
    DATASETT:
    DO:
      /* Ferdigstempler den vi hold på med. */
      IF prRowId <> ? THEN
      DO:
          FIND DataSett EXCLUSIVE-LOCK WHERE
              ROWID(Datasett) = prRowid.
          ASSIGN
            DataSett.AntallLinjer = DataSett.AntallLinjer + iAntISett
            DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                      THEN 3 /* Ekstra  */
                                      ELSE 2 /* Mottatt */)
            DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                      THEN DataSett.SettStatus
                                      ELSE 9  /* Ikke koblet */)
            DataSett.Behandlet    = 3 /* Behandlet */
            iAntISett             = 0
            prRowId               = ?
            bOkStatus             = TRUE
            .
          /* Åpningsskjemahantering */
          IF cKontrolltabell = "1" THEN DO:
              FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                                   ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.  
              IF AVAIL ApnSkjema THEN DO:
                  ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                         ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
              END.
          END.
          /* Åpnings.... SLUTT      */
          RELEASE DataSett.
      END.

      /* Finner neste DataSettId */
      FIND LAST DataSett NO-LOCK
          USE-INDEX DataSettId NO-ERROR.
      IF AVAILABLE DataSett THEN 
          plDataSettId = DataSett.DataSettId + 1.
      ELSE
          plDataSettId = 1.

      /* Finner neste SettNr */
      FIND LAST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = ttKvitto.ButikkNr AND
          Datasett.GruppeNr = ttKvitto.GruppeNr AND
          Datasett.KasseNr  = ttKvitto.KasseNr  AND
          Datasett.Dato     = ttKvitto.Dato     AND
          DataSett.FilType  = 1 /* EL-Journal */
          USE-INDEX DataSett NO-ERROR.
      IF AVAILABLE DataSett THEN
          piSettNr = DataSett.SettNr + 1.
      ELSE 
          piSettNr = 1.

      /* Alle kvitteringer som kommer inn på samme dag skal kobles  */
      /* til det samme datasettet. Forutsetning er at settet ikke   */
      /* har behandlingsstatus > 1.                                 */
      IF AVAILABLE DataSett THEN
      DO:
        IF DataSett.SettNr <= 2 AND DataSett.SettStatus <= 2 THEN
            ASSIGN
              plDataSettId = DataSett.DataSettId
              piSettNr     = DataSett.SettNr
              .
        ELSE RELEASE DataSett. /* Ny post skal skapes. */
      END.

      IF NOT AVAILABLE DataSett THEN
      EVIG-LOOP:
      DO WHILE TRUE:
        CREATE DataSett.
        ASSIGN
            /*DataSett.DataSettId = plDataSettId - settes av trigger */
            DataSett.SettStatus = 8 /* Innlesning avbrutt */
            plDataSettId        = DataSett.DataSettId
            NO-ERROR.
        /* Ekstra sjekk på om datasett kan skapes. */
        IF ERROR-STATUS:ERROR THEN
        DO:
            /* Finner neste DataSettId */
            FIND LAST DataSett NO-LOCK
                USE-INDEX DataSettId NO-ERROR.
            IF AVAILABLE DataSett THEN 
                ASSIGN
                plDataSettId = DataSett.DataSettId + 1
                piSettNr     = 1
                .
            ELSE
                ASSIGN
                    plDataSEttId = 1
                    piSettNr     = 1.
            NEXT EVIG-LOOP.
        END.

        IF NOT CAN-DO(cDatoListe,STRING(ttKvitto.Dato)) THEN
            ASSIGN
              cDatoListe = cDatoListe + 
                           (IF cDatoListe = ""
                              THEN ""
                              ELSE ",") +
                            STRING(ttKvitto.Dato)
                            .
        /* Forlater evigheten */
        LEAVE EVIG-LOOP.
      END. /* EVIG-LOOP */
      ELSE  /* Bruker det vi fant. */
          FIND CURRENT DataSett EXCLUSIVE-LOCK.

      ASSIGN
        prRowId             = ROWID(DataSett)
        DataSett.ButikkNr   = ttKvitto.ButikkNr 
        DataSett.GruppeNr   = ttKvitto.GruppeNr
        DataSett.KasseNr    = ttKvitto.KasseNr
        DataSett.Dato       = ttKvitto.Dato
        DataSett.SettNr     = piSettNr
        DataSett.Tid        = ttKvitto.tid
        DataSett.FilId      = lFilId
        DataSett.FilType    = 1 /* EL-Journal */
        .
    END. /* DATASETT */

    /* Oppretter bongHode. */
    IF NOT CAN-FIND(BongHode WHERE
                    BongHode.ButikkNr = ttKvitto.ButikkNr AND
                    BongHode.GruppeNr = ttKvitto.GruppeNr AND
                    BongHode.KasseNr  = ttKvitto.KasseNr  AND
                    BongHode.Dato     = ttKvitto.Dato     AND
                    BongHode.BongNr   = ttKvitto.BongNr) THEN
    OPPRETTBONG:
    DO:
        CREATE BongHode.
        BUFFER-COPY ttKvitto 
             EXCEPT Utskriftskopi 
                 TO BongHode
             ASSIGN BongHode.DataSettId = DataSett.DataSettId
                    BongHode.BongStatus = 5
             .
    END. /* OPPRETTBONG */
    ELSE 
        RELEASE BongHode.
    /* Legger opp radene */
    IF AVAILABLE BongHode THEN
    DO:
        FOR EACH ttKvittoRad where
            ttKvittoRad.kvitto_seq = ttKvitto.kvitto_seq:
            CREATE BongLinje.
            BUFFER-COPY ttKvittoRad 
                     TO BongLinje
                 ASSIGN 
                     BongLinje.B_Id      = BongHode.B_Id
                     BongLinje.GruppeNr  = BongHode.GruppeNr
                     BongLinje.Dato      = BongHode.Dato
                     BongLinje.TransDato = BongHode.Dato
                     BongLinje.TransTid  = BongHode.Tid
                     BongLinje.BongTekst = ttKvittoRad.Text_1
                     BongLinje.TTId      = ttKvittoRad.TTId
                     BongLinje.TBId      = ttKvittoRad.TBId
                     BongLinje.MvaGr     = ttKvittoRad.MvaKode
                     .
            /* Hent varelinjeinfo */
            IF CAN-DO("001,002,003,004,005,006,007,008,009,010,011,012",STRING(BongLinje.TTId,"999")) THEN
            DO:
                RUN ValiderArtikkel.
                RUN HentVareLinjeInfo.
                RUN SettVareKost.
            END.

        END.
    END.

    /* Teller antall bonger. */
    ASSIGN
        iAntISett   = iAntISett   + 1
        piantBonger = piAntBonger + 1
        .
    IF iAntISett MODULO 50 = 0 OR 
       LAST(ttKvitto.ButikkNr) THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + string(Filer.FilId) + " " + 
           Filer.FilNavn + " Lagrer kvittering " + STRING(piAntBonger) +
           " av " + STRING(iAntBonger) + ".") NO-ERROR.
    END.

END. /* LAGRE-KVITTO */

/* Ferdigstempler den vi hold på med.        */
/* Ekstra her for å ta det siste datasettet. */
IF prRowId <> ? THEN
DO TRANSACTION:
    FIND DataSett EXCLUSIVE-LOCK WHERE
        ROWID(Datasett) = prRowid.
    ASSIGN
      DataSett.AntallLinjer = DataSett.AntallLinjer + iAntISett
      DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                THEN 3 /* Ekstra  */
                                ELSE 2 /* Mottatt */)
      DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                THEN DataSett.SettStatus
                                ELSE 9  /* Ikke koblet */)
      DataSett.Behandlet    = 3 /* Behandlet */
      iAntISett             = 0
      prRowId               = ?
      bOkStatus             = TRUE
      .
   /* Åpningsskjemahantering */
   IF cKontrolltabell = "1" THEN DO:
       FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                            ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.  
       IF AVAIL ApnSkjema THEN DO:
           ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                  ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
       END.
   END.
/* Åpnings.... SLUTT      */
    RELEASE DataSett.
END. /* TRANSACTION */
  
RUN Telleverk IN h_Parent (" ") NO-ERROR.

ASSIGN
    iAntISett = piAntBonger
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PBR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PBR Procedure 
PROCEDURE PBR :
/*------------------------------------------------------------------------------
  Purpose:     Videreformidler meldinger fra rutine som håndterer
               integrasjonen mot profitbase.
               
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcMsg     AS CHAR NO-UNDO.

  RUN Telleverk IN h_Telleverk (pcMsg) NO-ERROR.
  RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
           pcMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettVareKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettVareKost Procedure 
PROCEDURE SettVareKost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pdVVAreKost AS DEC NO-UNDO.
      
/*   IF BongLinje.ArtikkelNr = "" THEN */
/*   DO:                               */
/*       ASSIGN                        */
/*           pdVVAreKost = 0           */
/*           .                         */
/*       RETURN.                       */
/*   END.                              */
  
/*   /* Henter varekost i butikken det overføres fra. */                                   */
/*   /* Dette er pris eExMva.                         */                                   */
/*   IF BongLinje.ArtikkelNr <> "" THEN                                                    */
/*       FIND Lager NO-LOCK WHERE                                                          */
/*           Lager.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND                              */
/*           Lager.Butik      = BongLinje.Butik NO-ERROR.                                  */
/*   IF AVAILABLE Lager THEN                                                               */
/*       pdVVarekost = Lager.VVareKost.                                                    */
/*   ELSE                                                                                  */
/*       pdVVareKost = 0.                                                                  */
/*                                                                                         */
/*   /* Sjekker om varekost er satt.                                       */              */
/*   /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */              */
/*   if pdVVareKost = 0 then /* or wBrutto% *** Skal også utføres for brutto% artikkler */ */
/*     DO:                                                                                 */
/*       ASSIGN                                                                            */
/*           /* Omsetning eks. mva */                                                      */
/*           pdVVareKost = (IF BongLinje.Antall >= 0                                       */
/*                           THEN (BongLinje.LinjeSum -                                    */
/*                                 (BongLinje.LinjeRab + BongLinje.SubtotalRab) -          */
/*                                 BongLinje.MvaKr)                                        */
/*                           ELSE (BongLinje.LinjeSum -                                    */
/*                                 (BongLinje.LinjeRab + BongLinje.SubtotalRab) -          */
/*                                 BongLinje.MvaKr) * -1)                                  */
/*           pdVVareKost = pdVVareKost / ABS(BongLinje.Antall)                             */
/*           .                                                                             */
/*                                                                                         */
/*       /* Hvis ukjent artikkel, benyttes varegruppens kostnadsprosent. */                */
/*       /* Varekost settes inn pr. enhet.                               */                */
/*       IF NOT CAN-FIND(ArtBas WHERE                                                      */
/*                       ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr)) THEN               */
/*       DO:                                                                               */
/*           FIND VarGr NO-LOCK WHERE                                                      */
/*               VarGr.Vg = BongLinje.VareGr NO-ERROR.                                     */
/*           IF AVAILABLE VarGr THEN                                                       */
/*           DO:                                                                           */
/*               IF VarGr.Kost_Proc > 0 THEN                                               */
/*                   ASSIGN                                                                */
/*                   pdVVareKost = abs(ROUND((pdVVareKost * VarGr.Kost_Proc) / 100,2))     */
/*                   .                                                                     */
/*           END.                                                                          */
/*       END.                                                                              */
/*       ELSE if VALID-HANDLE(h_PrisKo) then                                               */
/*           RUN HentVareKost in h_PrisKo (INPUT  BongLinje.ArtikkelNr,                    */
/*                                         input  BongLinje.Butik,                         */
/*                                         INPUT  pdVVareKost,                             */
/*                                         output pdVVareKost).                            */
/*                                                                                         */
/*     END.                                                                                */
  
  ASSIGN
      /* Omsetning eks. mva */
      pdVVareKost = (IF BongLinje.Antall >= 0
                      THEN (BongLinje.LinjeSum -
                            (BongLinje.LinjeRab + BongLinje.SubtotalRab) -
                            BongLinje.MvaKr)
                      ELSE (BongLinje.LinjeSum -
                            (BongLinje.LinjeRab + BongLinje.SubtotalRab) -
                            BongLinje.MvaKr) * -1)
      pdVVareKost = pdVVareKost / ABS(BongLinje.Antall)
          .
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = BongLinje.VareGr NO-ERROR.
  IF AVAILABLE VarGr THEN
  DO:
      IF VarGr.Kost_Proc > 0 THEN
          ASSIGN
          pdVVareKost = abs(ROUND((pdVVareKost * VarGr.Kost_Proc) / 100,2))
          .
  END.
  
  IF pdVVareKost = ? THEN
      pdVVareKost = 0.

  assign
      BongLinje.VVareKost = ABS(pdVVareKost)
      . 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SletterAvbrutteKvitteringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SletterAvbrutteKvitteringer Procedure 
PROCEDURE SletterAvbrutteKvitteringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bLdPost FOR LdPost.

FOR EACH ldPost WHERE 
    (ldPost.logid = "191") AND /* Brak/Reset */  
    (ldPost.n2 = "2"):         /* Kun RESET  */

    /* Sletter alle linjer på den resatte kvitteringen */
    FOR EACH bldPost WHERE 
        (bldPost.tno     = ldPost.tno) AND
        (bldPost.receipt = ldPost.receipt):
        DELETE BldPost.
    END.
END.

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
      iTotAntLinjer = 0
      bOkStatus     = FALSE 
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  repeat:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
    IF bOkStatus = FALSE THEN
        bOkStatus = TRUE.
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderArtikkel Procedure 
PROCEDURE ValiderArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plDec AS DEC NO-UNDO.

  /* Det forekommer at her ligger alfanumeriske tegn */
  ASSIGN
      plDec = DEC(BongLinje.Strekkode)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      RETURN.
                   
  /* Konverterer PLU koder. */
  IF DEC(BongLinje.Strekkode) <= 99999 THEN
      BongLinje.Strekkode = LEFT-TRIM(BongLinje.Strekkode,"0").

  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = BongLinje.Strekkode NO-ERROR.

  /* Blank strekkode eller ukjent strekkode/Artikkel */
  IF BongLinje.Strekkode = "" OR NOT AVAILABLE Strekkode OR BongLinje.ArtikkelNr = "" THEN
  STREK-PLU:
  DO:
      /* Pressbyrån spesial for innlesning av LD data. */
      IF BongLinje.ArtikkelNr = "" THEN
      PLU-SJEKK:
      DO:
          /* Henter varegruppens PLU nummer */
          FIND FIRST ArtBas NO-LOCK where
                     ArtBas.Vg    = BongLinje.VareGr and
                     ArtBas.OPris = TRUE USE-INDEX PLU NO-ERROR.
          /* Henter diverse varegruppe */
          IF NOT AVAILABLE ArtBas THEN
              FIND FIRST ArtBas NO-LOCK where
                         ArtBas.Vg    = 1499 and
                         ArtBas.OPris = TRUE USE-INDEX PLU NO-ERROR.
          IF AVAILABLE ArtBas THEN
          DO:
              ASSIGN
                  BongLinje.ArtikkelNr = trim(STRING(ArtBas.ArtikkelNr))
                  cError = cError + 
                           (IF cError = ""
                              THEN ""
                              ELSE chr(10)) + 
                           "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                           " Blank/ukjent artikkel strekkode/PLU nummer. Salget er postert på varegruppe " +
                           STRING(BongLinje.VareGr) +
                           "."
                  cFilError = cFilError + 
                           (IF cFilError = ""
                              THEN ""
                              ELSE "|") + 
                           " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                           "  Blank/ukjent artikkel strekkode/PLU nummer. Salget er postert på varegruppe . " +
                           "." + CHR(1) + "2"
                  BongHode.Gradering = 0
                  BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
                  .
              LEAVE STREK-PLU.
          END.
      END. /* PLU-SJEKK */

      ASSIGN
          BongLinje.ArtikkelNr = ""
          BongLinje.LopeNr     = 0
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE chr(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent strekkode/PLU nummer. " +
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent strekkode/PLU nummer. " +
                   "." + CHR(1) + "2"
          BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
          BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
          .
      RETURN.
  END. /* STREK-PLU */
  
  IF AVAILABLE Strekkode THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
  ELSE FIND ArtBas WHERE
      ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.

  /* Kontrollerer varegruppen */
  IF AVAILABLE ArtBas THEN
  DO:
      IF BongLinje.VareGr <> ArtBas.Vg THEN
      DO:
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ") " +
                     " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ") " +
                     " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 1 THEN 1 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            .
      END.
  END.
  /* Ukjent artikkel */
  ELSE 
  UKJENT-ARTIKKEL:
  DO:
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ")" + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(BongLinje.ArtikkelNr) + ")" + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
        .
  END. /* UKJENT-ARTIKKEL */

  ASSIGN
      BongLinje.ArtikkelNr = (IF AVAILABLE ArtBas
                                THEN trim(string(ArtBas.ArtikkelNr,">>>>>>>>>>>>9"))
                                ELSE trim(BongLinje.ArtikkelNr))
      BongLinje.VareGr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.Vg
                               ELSE BongLinje.VareGr)
      BongLinje.LopeNr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.LopNr
                               ELSE ?)
      .

  /* Kontrollerer gyldig varegruppe */
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = BongLinje.VareGr NO-ERROR.

  IF AVAILABLE VarGr THEN
      BongLinje.VareGruppeNavn = VarGr.VgBeskr.
  ELSE DO:
      ASSIGN  
        BongLinje.VareGruppeNavn = "** Ukjent varegruppe **"
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent varegruppe på transaksjonen " + STRING(BongLinje.VareGr) + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent varegruppe på transaksjonen " + STRING(BongLinje.VareGr) + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
        .
  END.

  IF AVAILABLE VarGr THEN
  DO:
      FIND HuvGr NO-LOCK WHERE
          HuvGr.Hg = VarGr.Hg NO-ERROR.
      IF NOT AVAILABLE HuvGr THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent hovedgruppe på den varegruppe som er på varelinjen: " + string(BongLinje.VareGr) + "/" + STRING(VarGr.Hg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     "  Ukjent hovedgruppe på den varegruppe som er på varelinjen: : " + string(BongLinje.VareGr) + "/" + STRING(VarGr.Hg) +  
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            .
      ELSE
          ASSIGN
          BongLinje.HovedGr            = VarGr.Hg
          BongLinje.HovedGrBeskrivelse = HuvGr.HgBeskr
          .
  END.

  /* Henter størrelsen hvis det er en størrelseskode <> 0 */
  IF AVAILABLE Strekkode THEN
  IF Strekkode.StrKode <> 0 THEN
  DO:
      FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = StrekKode.StrKode NO-ERROR.
      IF AVAILABLE StrKonv THEN
          BongLinje.Storrelse = StrKonv.Storl.
      ELSE DO:
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
              " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            BongLinje.Storrelse = ""

            .
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

