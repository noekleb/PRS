&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xgantpkinnles.p
    Purpose     :

    Syntax      :

    Description : Leser inn filen og splitter den i to filer.

    Author(s)   : Tom Nøkleby
    Created     : 17/7-05
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER cInFilNavn      AS CHAR    NO-UNDO.

DEFINE VARIABLE cOKVersion AS CHARACTER INIT "Rigal98,RIGAL02" NO-UNDO.

DEF VAR cRigalversion AS CHAR INIT "RIGAL02,8.0" NO-UNDO.
DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEFINE VARIABLE iAntlinjer AS INTEGER    NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR cOrgNumFormat AS CHAR NO-UNDO.

DEF VAR piLevNr       AS CHAR INIT "5000" NO-UNDO.
DEF VAR cSalgsEnhListe AS CHAR NO-UNDO.

DEF VAR cFlyttDir       AS CHAR  NO-UNDO.
DEF VAR cFlyttKommando  AS CHAR  NO-UNDO.
DEF VAR cFinansProDir   AS CHAR  NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR ctmpTxt AS CHAR NO-UNDO.
DEF VAR lFinansPro      AS LOGICAL    NO-UNDO.


DEF VAR iNumEntries  AS INTE INIT 15 NO-UNDO.

DEF STREAM InnFil.

/* DEFINE TEMP-TABLE TT_xxxx  NO-UNDO LIKE xxxx. */

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .

{windows.i}
{RigalVare.i}

DEFINE TEMP-TABLE TT_Input NO-UNDO
    FIELD vklass    AS CHAR  
    FIELD hgr       AS CHAR  
    FIELD avd       AS CHAR  
    FIELD bestnr    AS CHAR  
    FIELD ean       AS CHAR  
    FIELD beskr     AS CHAR  
    FIELD levartnr  AS CHAR  
    FIELD antfrp    AS CHAR  
    FIELD inpris    AS CHAR  
    FIELD utpris    AS CHAR  
    FIELD moms%     AS CHAR  
    FIELD enhet     AS CHAR  
    FIELD jmffaktor AS CHAR  
    FIELD jmfenh    AS CHAR  
    FIELD levnr     AS CHAR.


DEFINE TEMP-TABLE tmpTT_Input NO-UNDO LIKE TT_Input.

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

/* ASSIGN cOrgNumFormat = SESSION:NUMERIC-FORMAT. */

{syspara.i 2 4 10 cSalgsEnhListe}
/* Programmet förutsätter att vi lägger ut till finanspro */
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    .

RUN LesInnFil.

RUN ExportTT.

/* ASSIGN SESSION:NUMERIC-FORMAT = cOrgNumFormat. */




RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
      "Feil i fil: " + cInFilNavn skip
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

&IF DEFINED(EXCLUDE-ExportTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportTT Procedure 
PROCEDURE ExportTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRigalEntries AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRigalstr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cUtfil        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFilnamn      AS CHARACTER  NO-UNDO.

    ASSIGN cFilnamn = "v" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME,"99999") + "1.001".
           cUtfil = cInFilnavn.
           ENTRY(NUM-ENTRIES(cUtfil,"\"),cUtfil,"\") = cFilnamn.
     
/*     MESSAGE cFinansProDir SKIP cUtfil      */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    ASSIGN cRigalEntries = FILL(",",109)
           ENTRY(1,cRigalEntries) = "VAR"
           ENTRY(2,cRigalEntries) = "E".
           ENTRY(5,cRigalEntries) = "N".
    OUTPUT TO VALUE(cUtfil).
    PUT UNFORMATTED cRigalversion SKIP.
    FOR EACH TT_Input:
        ASSIGN cRigalstr = cRigalEntries
               ENTRY(3,cRigalstr)   = TT_Input.Ean 
               ENTRY(6,cRigalstr)   = TT_Input.Levnr
               ENTRY(7,cRigalstr)   = TT_Input.bestnr
               ENTRY(10,cRigalstr)  = '"' + TRIM(SUBSTR(TT_Input.beskr,1,30)) + '"'
               ENTRY(11,cRigalstr)  = '"' + TRIM(SUBSTR(TT_Input.beskr,1,20)) + '"'
               ENTRY(12,cRigalstr)  = '"' + TRIM(SUBSTR(TT_Input.beskr,1,30)) + '"'
               ENTRY(14,cRigalstr)  = IF TT_Input.enhet = "KG" THEN "2" ELSE "1" /* IF TT_Input.enhet = "KG" THEN "2" ELSE if TT_Innput.enhet = "L" then "3" else "1" */
               ENTRY(18,cRigalstr)  = TT_Input.hgr
               ENTRY(19,cRigalstr)  = "1"
               ENTRY(20,cRigalstr)  = REPLACE(TT_Input.Inpris,",",".")
               ENTRY(22,cRigalstr)  = REPLACE(TT_Input.Utpris,",",".")
               ENTRY(45,cRigalstr)  = REPLACE(TT_Input.Moms%,",",".")
               ENTRY(48,cRigalstr)  = "N" /* if TT_input.enhet = "kg" then "K" else "N" */
               ENTRY(79,cRigalstr)  = REPLACE(TT_Input.jmffaktor,",",".") /* "1" */
               ENTRY(92,cRigalstr)  = IF TT_Input.jmfenh = "KG" THEN "2" ELSE IF TT_Input.jmfenh = "L" THEN "3" ELSE "1"             /* "0" */
               ENTRY(110,cRigalstr) = TT_Input.Levartnr.
        PUT UNFORMATTED cRigalStr SKIP.
    END.
    OUTPUT CLOSE.
/*     IF cFlyttkommando <> "" THEN                     */
/*         RUN flyttprofiler.p ("","",cUtfil) NO-ERROR. */
/*                                                                                                                                                                                                                                                                                                                                */
/* /*   1 */ TT_RigalVare.Kode       = "VAR"                                                                                                                                                                                                                                                                                      */
/* /*   2 */ TT_RigalVare.AID_kode   = IF LENGTH(cNummer) < 6 THEN "P" ELSE "E"     /* "E" eller "P" Neste felt EAN eller PLU */                                                                                                                                                                                                  */
/* /*   3 */ TT_RigalVare.Nummer     = DECI(cNummer) /* EAN/PLU-nummer  I(13) */                                                                                                                                                                                                                                                  */
/* /*   4 */ TT_RigalVare.Artnr      = ArtBas.ArtikkelNr /* ??? Internt artikkelnr */                                                                                                                                                                                                                                             */
/* /*   5 */ TT_RigalVare.Flag       = /* IF NOT ArtBas.Aktivert OR NOT Artbas.IKasse THEN "U" ELSE */ "N" /* Funksjonskode "N" = "Normal", dvs.  vare/prisendring"K" = Kampanje"M" =    Medlemstilbud "U" = Utmelding (tolkes som sletting)"A" =  Slett ikke påbegynt kampanje, avslutt påbegynt kamåpanje */ */
/* /*   6 */ TT_RigalVare.Grossist   = ArtBas.LevNr         /* ?? */                                                                                                                                                                                                                                                              */
/* /*   7 */ TT_RigalVare.Bestnr     = 0                    /* Leverandørens artikkelnummer I(8) Bestillingsnummer */                                                                                                                                                                                                       */
/* /*  10 */ TT_RigalVare.Varetek    = SUBSTR(ArtBas.Bongtekst,1,30) /* Varetekst C(30) */                                                                                                                                                                                                                                        */
/* /*  11 */ TT_RigalVare.Bongtek    = SUBSTR(ArtBas.Bongtekst,1,20) /* Bongtekst C(20) */                                                                                                                                                                                                                                        */
/* /*  12 */ TT_RigalVare.Etitekst1  = SUBSTR(ArtBas.Bongtekst,1,30) /* Etikettekst 1 C(30) Må i fylles ut med kunderettede tekster */                                                                                                                                                                                            */
/* /*  14 */ TT_RigalVare.Pakn       = IF ArtBas.SalgsEnhet = "Kg" THEN 2 ELSE 1 /* Salgsenhet I     1=stk, 2=kg, 3=liter, 4=meter osv.                           */                                                                                                                                         */
/* /*  18 */ TT_RigalVare.Hgr        = ArtBas.Vg            /* HovedgruppeI      */                                                                                                                                                                                                                                               */
/* /*  19 */ TT_RigalVare.Ugr        = 1                    /* 0/1 Undergruppe       */                                                                                                                                                                                                                                           */
/* /*  20 */ TT_RigalVare.Engros     = ArtPris.VareKost[iPrisEntry] /* ? */  /*   47 EngrosprisD  */                                                                                                                                                                                                                              */
/* /*  22 */ TT_RigalVare.Utpris     = ArtPris.Pris[iPrisEntry]     /* ? */  /*  102 UtsalgsprisD */                                                                                                                                                                                                                              */
/* /*  45 */ TT_RigalVare.Mva%       = Moms.MomsProc        /* Mva-prosent D */                                                                                                                                                                                                                                                   */
/* /*  48 */ TT_RigalVare.Vtype      = IF ArtBas.OPris THEN "O" ELSE IF ArtBas.Salgsenhet = "Kg" THEN "K" ELSE "N" /* IF NOT ArtBas.Aktivert THEN "I" ELSE "N" = vanlig vare"K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen */                                                                          */
/* /*  61  */ FIELD AID_kode3    AS CHARACTER /* "E" eller "P"                           Angir om neste felt er EAN eller PLU                         */ */
/* /*  62  */ FIELD Salgskode3   AS DECIMAL   /* EAN/PLU-nummer                  I(13)   Alternativt varenummer (tandem)                              */ */
/* /*  79 */ TT_RigalVare.Konvfak      = IF ArtBas.Mengde = 0 THEN 1 ELSE ArtBas.Mengde      /* Konverteringsfaktor             D       Brukes ved beregning av enhetspris      */                                                                                                                                                */
/* /*  92 */ TT_RigalVare.Konvenh       = 0                 /* Konverteringsenhet              I       Angir enhet som brukes ifm. enhetspris. Samme koder som i  felt 7.1.14                                     */                                                                                                              */
/* /* 110 */ TT_RigalVare.Artnr2         = ""               /* Alfanumerisk internt artikkelnr C(20) */                                                                                                                                                                                                                           */
/*                                                                                                                                                                                                                                                                                                                                */


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
  DEF VAR dTst        AS DEC NO-UNDO.
  DEF VAR iTst        AS INT NO-UNDO.

  RUN TellOppLinjer.
  IF RETURN-VALUE = "FEIL" THEN DO:
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = 1
        tt_Error.Tekst   = "** Feil på linje 1. Fel antal entries".
        .
      RETURN.
  END.

  ASSIGN iAntLinjer = 0.
  INPUT STREAM InnFil FROM VALUE(cInFilNavn) NO-ECHO.
/*   IMPORT STREAM InnFil UNFORMATTED cLinje. /* Läs bort första raden! Rigal Header, testet om Rigal i TellOppLinjer */ */
  LESERLINJER:
  REPEAT:
    /* Record buffer å lese inn filen i */
    CREATE tmpTT_Input.
    /* Leser linje fra filen */
    IMPORT STREAM InnFil DELIMITER ";" tmpTT_Input NO-ERROR.
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
      DELETE tmpTT_Input.
      NEXT LESERLINJER.
    END.
    ASSIGN tmpTT_Input.Inpris = TRIM(tmpTT_Input.Inpris).

    ASSIGN tmpTT_Input.beskr = REPLACE(tmpTT_Input.Beskr,","," ").
    ASSIGN dTst = ROUND(DECI(tmpTT_Input.Utpris),1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN iAntLinjer = iAntLinjer + 1.
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Fel Utpris " + tmpTT_Input.Utpris + " Linje: " + STRING(iAntLinjer).
        DELETE tmpTT_Input.
        NEXT LESERLINJER.
    END.
    tmpTT_Input.Utpris = STRING(dTst).
    IF tmpTT_Input.EAN BEGINS "20" OR tmpTT_Input.EAN BEGINS "23" AND LENGTH(tmpTT_Input.EAN) = 13 THEN
        ASSIGN tmpTT_Input.EAN = SUBSTR(tmpTT_Input.EAN,1,12) + "0".
    tmpTT_Input.levnr = piLevNr.
    tmpTT_Input.jmfenh = IF tmpTT_Input.jmfenh = "k" THEN "KG" ELSE IF tmpTT_Input.jmfenh = "0" THEN "L" ELSE tmpTT_Input.jmfenh.
/*     ASSIGN dTst = ROUND(DECI(tmpTT_Input.AAFpris) / INT(tmpTT_Input.Frp),2) NO-ERROR.                                               */
/*     IF ERROR-STATUS:ERROR OR dTst = 0 THEN DO:                                                                                      */
/*         ASSIGN iAntLinjer = iAntLinjer + 1.                                                                                         */
/*         CREATE tt_Error.                                                                                                            */
/*         ASSIGN                                                                                                                      */
/*           tt_Error.LinjeNr = piAntFeil                                                                                              */
/*           tt_Error.Tekst   = "** Fel kalk inpris " + tmpTT_Input.AAFpris + "/" + tmpTT_Input.Frp + " Linje: " + STRING(iAntLinjer). */
/*         DELETE tmpTT_Input.                                                                                                         */
/*         NEXT LESERLINJER.                                                                                                           */
/*     END.                                                                                                                            */
/*     ASSIGN tmpTT_Input.Inpris = REPLACE(STRING(dTst),",","."). */
/*     FIND VarGr WHERE VarGr.Vg = INT(tmpTT_Input.Varuklasskod) NO-LOCK. */
/*     FIND Moms WHERE Moms.MomsKod = VarGr.MomsKod NO-LOCK NO-ERROR.                                                 */
/*     IF NOT AVAIL Moms THEN DO:                                                                                     */
/*         ASSIGN iAntLinjer = iAntLinjer + 1.                                                                        */
/*         CREATE tt_Error.                                                                                           */
/*         ASSIGN                                                                                                     */
/*           tt_Error.LinjeNr = piAntFeil                                                                             */
/*           tt_Error.Tekst   = "** Moms saknas vargr " + tmpTT_Input.Varuklasskod + " Linje: " + STRING(iAntLinjer). */
/*         DELETE tmpTT_Input.                                                                                        */
/*         NEXT LESERLINJER.                                                                                          */
/*     END.                                                                                                           */
/*     ASSIGN tmpTT_Input.Moms% = REPLACE(STRING(Moms.MomsProc),",",".")                                              */
/*            iAntLinjer = iAntLinjer + 1.                                                                            */

    CREATE TT_Input.
    BUFFER-COPY tmpTT_Input TO TT_Input NO-ERROR.
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
      DELETE tmpTT_Input.
      DELETE TT_RigalVare.
      NEXT LESERLINJER.
    END.
    ELSE
        DELETE tmpTT_Input.

/*     ASSIGN TT_RigalVare.ArtNr = FinnArtNr(TT_RigalVare.Nummer,iAntLinjer * -1). */
    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

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
/*   ASSIGN                                                  */
/*       iTotAntLinjer = -1 /* Första linjen är en header */ */
/*       .                                                   */
/*                                                           */
/*   INPUT STREAM InnFil FROM VALUE(cInFilNavn) NO-ECHO.     */
/*   IMPORT STREAM InnFil UNFORMATTED cLinje.                */
/*   IF NUM-ENTRIES(cLinje,";") <> iNumentries THEN DO:      */
/*       INPUT STREAM InnFil CLOSE.                          */
/*       RETURN "FEIL".                                      */
/*   END.                                                    */
/*   repeat:                                                 */
/*     IMPORT STREAM InnFil UNFORMATTED cLinje.              */
/*     ASSIGN                                                */
/*         iTotAntLinjer = iTotAntLinjer + 1                 */
/*         .                                                 */
/*   END.                                                    */
/*   INPUT STREAM InnFil CLOSE.                              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

