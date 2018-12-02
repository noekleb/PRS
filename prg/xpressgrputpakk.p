&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xpressgrputpakk.p
    Purpose     : Utpakking av vareinndeling/grupperinger fra MEgaDisk.

    Syntax      :

    Description : Fra MegaDisk leveres en fil som inneholder:
                    1. Avdelinger
                    2. Hovedgrupper
                    3. Varegrupper
                  Disse pakkes opp og oppdateres direkte inn i motsvarende
                  tabeller i SkoTex databasen.

    Author(s)   : Tom Nøkleby
    Created     : 16/2-03
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEF VAR iTotAntLinjer AS INT NO-UNDO.
DEF VAR iCL           AS INT NO-UNDO.
DEF VAR lLapTop       AS LOG NO-UNDO.
DEF VAR iProfilNr     AS INT NO-UNDO.
DEF VAR iAntNya       AS INT NO-UNDO.
DEF VAR iAntUppdat    AS INT NO-UNDO.
DEF VAR iAntNyaAvd    AS INT NO-UNDO.
DEF VAR iAntNyaHg     AS INT NO-UNDO.
DEF VAR iAntFel       AS INT NO-UNDO.

DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.

DEF VAR cEDB-System        LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell         LIKE ImpKonv.Tabell     NO-UNDO.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
{windows.i}

DEF VAR piAntFeil   AS INT  NO-UNDO.
DEF VAR bProfitBase AS LOG  NO-UNDO.

DEFINE TEMP-TABLE TT_ProdGr
/* PRODGRP.PRODGR_ID  */  FIELD Produktgrupp     AS CHAR LABEL "VarGr"
/* PRODGRP.DESCRIPT   */  FIELD Benamning        AS CHAR LABEL "VarGrBen"
/* PRODGRP.GRP_FAM_ID */  FIELD Overproduktgrupp AS CHAR LABEL "Huvudgrupp" 
/* GRP_FAM.DESCRIPT   */  FIELD OprodBenamning   AS CHAR LABEL "HuvGrBen"   
/* PRODGRP.GRP_SUM_ID */  FIELD Bransch          AS CHAR LABEL "Avdelning"  
/* GRP_SUM.DESCRIPT   */  FIELD BranchBen        AS CHAR LABEL "AvdelBen"
/* PRODGRP.MARGIN_P   */  FIELD Marginal%        AS CHAR LABEL "Marginal %"
/* VAT_CODE.VAT_P     */  FIELD Moms%            AS CHAR LABEL "Moms %"
/* PRODGRP.VAT_CODE   */  FIELD Momskod          AS CHAR LABEL "Momskod"
/* PRODGRP.ACCOUNTKEY */  FIELD AccountKey       AS CHAR LABEL "AccountKey"
/* PRODGRP.IS_COMMISS */  FIELD KommissionTF     AS CHAR LABEL "Kommission (T/F)"
                          FIELD OrsakDel         AS CHAR LABEL "Orsak ej inläst"
                          FIELD Radnr            AS INTE LABEL "Ordningsföljd inläsning"
                                INDEX Radnr IS PRIMARY Radnr
                                INDEX Pg Produktgrupp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

FOR EACH tt_Error:
  DELETE tt_Error.
END.

SUBSCRIBE TO 'PBR' ANYWHERE.
{syspara.i 50 200 1 cTekst}
IF TRIM(cTekst) = "1" THEN
  bProfitBase = TRUE.
ELSE 
  bProfitBase = FALSE.

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Sjekker om det kjøres på en LapTop */
if valid-handle(h_dproclib) then
  run SjekkLapTop in h_dproclib (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
IF NOT AVAILABLE Butiker THEN
DO:
    MESSAGE "Sentrallager " + STRING(iCL) + " er ikke lagt inn i butikkregister."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    MESSAGE "Ingen VPIFilHode tilgjengelig"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ASSIGN
    cEDB-System = EkstVPILev.KortNavn
    .

/* Datasett statuspost. */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
                                                
/* Er datasettet ferdig oppdatert? */
IF AVAILABLE VPIDatasett THEN
DO:
    IF VPIDatasett.DatasettStatus >= 3 AND
       VPIDatasett.DatasettStatus <= 4 THEN
    DO:
        MESSAGE "Forrige datasett er ikke oppdatert." SKIP
                "Det må være ferdig oppdatert før ny import av VPI kan gjøres."                
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.

/* Sjekker at default verdier er lagt inn i databasen. */
RUN SkapaDiverse.

/* Utpakking av Gruppeinndelinger.                       */
/* Her kommer også avdleinger, hovedgrupper og mvakoder. */
RUN UtpakkGruppe.

/* /* Kjører oppdatering av faste registre til ProfitBase. */ */
/* IF bProfitBase THEN                                        */
/* PROFITBASE:                                                */
/* DO:                                                        */
/*   RUN pfxoppdatfastereg.p.                                 */
/* END. /* PROFITBASE */                                      */

ON CLOSE OF THIS-PROCEDURE 
DO:
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

IF CAN-FIND(FIRST tt_Error) THEN
  RUN ErrorLogg.

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
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip
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

&IF DEFINED(EXCLUDE-Kalkulasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Procedure 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  def INPUT        PARAMETER pcFraFelt  as char no-undo.
  DEF INPUT-OUTPUT PARAMETER pcSkjerm   AS CHAR NO-UNDO.
  
  def var pcFeltListe as char no-undo.
  def var piFeltNr    as int  no-undo.
  DEF VAR lTilbud     AS LOG  NO-UNDO.
  DEF VAR lDirekte    AS LOG  NO-UNDO.

  DEF BUFFER bArtBas FOR VPIArtBas.
  DEF BUFFER bVarGr  FOR VarGr.
  DEF BUFFER bMoms   FOR Moms.
  DEF BUFFER bValuta FOR Valuta.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  assign
    lTilbud     = FALSE
    lDirekte    = FALSE
    pcFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                  "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                  "FI-Mva,FI-Mva%,Pris,EU-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  assign
    /*pcFraFelt = substring(pcFraFelt,4)*/
    piFeltNr  = lookup(pcFraFelt,pcFeltListe)
    .

  /* Ukjent felt. */  
  if piFeltNr = 0 then
    do:
      message "Ukjent felt!" view-as alert-box title "Kalkylefeil".
      return no-apply.  
    end.

  /* Henter nødvendige buffere */
  FIND bArtBas NO-LOCK WHERE
      RECID(bArtBas) = rArtBasRecid NO-ERROR.
  IF NOT AVAILABLE bArtBas THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent artikkel."
      .
    RETURN "AVBRYT".
  END.
  FIND bVarGr OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bVarGr THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent varegruppe."
      .
    RETURN "AVBRYT".
  END.
  FIND bMoms  OF bVarGr  NO-ERROR.
  IF NOT AVAILABLE bMoms THEN
  DO:
    MESSAGE "Ukjent bMoms"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bValuta OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bValuta THEN
  DO:
    CREATE tt_Error.
    ASSIGN
      piAntFeil = piAntFeil + 1
      tt_Error.LinjeNr = piAntFeil
      tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent valuta."
      .
    RETURN "AVBRYT".
  END.

  /* Starter omkalkulering.                         */
  run Omregning in h_PrisKo
       (input rArtBasRecid, 
        input iProfilNr,
        input-output pcSkjerm,
        input bMoms.MomsProc,
        input bValuta.ValKurs, 
        input piFeltNr,
        INPUT lTilbud).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PBR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PBR Procedure 
PROCEDURE PBR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaDiverse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaDiverse Procedure 
PROCEDURE SkapaDiverse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMomsKod AS CHARACTER INIT "1,4,6,7" NO-UNDO.
    DEFINE VARIABLE cMoms%   AS CHARACTER INIT "25,0,12,6" NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
    /* Detta finns i PGR-fil */
/*     DO iCount = 1 TO NUM-ENTRIES(cMomsKod):                                  */
/*         FIND Moms WHERE Moms.MomsKod = INT(ENTRY(iCount,cMomsKod)) NO-ERROR. */
/*         IF NOT AVAIL Moms THEN DO:                                           */
/*             CREATE Moms.                                                     */
/*             ASSIGN Moms.MomsKod = INT(ENTRY(iCount,cMomsKod)).               */
/*         END.                                                                 */
/*         ASSIGN Moms.MomsProc    = DECI(ENTRY(iCount,cMoms%))                 */
/*                Moms.Beskrivelse = IF ENTRY(iCount,cMoms%) = "0" THEN         */
/*                                   "Ingen moms" ELSE                          */
/*                                   ENTRY(iCount,cMoms%) + "% moms" .          */
/*     END.                                                                     */
    IF NOT CAN-FIND(StrType WHERE StrType.StrTypeId = 1) THEN DO:
        CREATE StrType.
        ASSIGN StrType.StrTypeId   = 1
               StrType.KortNavn    = "*DEF*"
               StrType.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Rabatt WHERE Rabatt.RabKod = 0) THEN DO:
        CREATE Rabatt.
        ASSIGN Rabatt.RabKod   = 0
               Rabatt.RabProc  = 0
               Rabatt.RabBeskr = "Default".
    END.
    IF NOT CAN-FIND(Prov WHERE Prov.ProvKod = 0) THEN DO:
        CREATE Prov.
        ASSIGN Prov.ProvKod   = 0
               Prov.ProvProc  = 0
               Prov.ProvBeskr = "Default".
    END.
    IF NOT CAN-FIND(Kategori WHERE Kategori.KatNr = 1) THEN DO:
        CREATE Kategori.
        ASSIGN Kategori.KatNr       = 1
               Kategori.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Prisprofil WHERE Prisprofil.ProfilNr = 1) THEN DO:
        CREATE Prisprofil.
        ASSIGN Prisprofil.ProfilNr    = 1
               Prisprofil.KortNavn    = "*DEF*"
               Prisprofil.Beskrivelse = "Default"
               Prisprofil.Merknad     = "Skapad import".                
    END.
    IF NOT CAN-FIND(Fylke WHERE Fylke.FylkesNr = "1") THEN DO:
        CREATE Fylke.
        ASSIGN Fylke.FylkesNr = "1"
               Fylke.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Kommune WHERE Kommune.FylkesNr = "1" AND 
                                  Kommune.KommNr   = "1") THEN DO:
        CREATE Kommune.
        ASSIGN Kommune.FylkesNr    = "1"
               Kommune.KommNr      = "1"
               Kommune.Beskrivelse = "Default".
    END.
    IF NOT CAN-FIND(Farg WHERE Farg.Farg = 0) THEN DO:
        CREATE Farg.
        ASSIGN Farg.Farg     = 0
               Farg.KFarge   = "*DEF*"
               Farg.FarBeskr = "Default".
    END.
    IF NOT CAN-FIND(Sasong WHERE SaSong.Sasong = 0) THEN DO:
        CREATE SaSong.
        ASSIGN SaSong.Sasong   = 0
               SaSong.SasBeskr = "Default".
    END.
    IF NOT CAN-FIND(Valuta WHERE Valuta.ValKod = "SEK") THEN DO:
        CREATE Valuta.
        ASSIGN Valuta.ValKod   = "SEK"
               Valuta.ValDatum = TODAY
               Valuta.ValKurs  = 1
               Valuta.ValLand  = "Default".
    END.
    IF NOT CAN-FIND(Material WHERE Material.Matkod = 0) THEN DO:
        CREATE Material.
        ASSIGN Material.MatKod   = 0
               Material.MatBeskr = "Default".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtPakkGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtPakkGruppe Procedure 
PROCEDURE UtPakkGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcHg          AS CHAR NO-UNDO.
DEF VAR pcVg          AS CHAR NO-UNDO.
DEF VAR pcUgr         AS CHAR NO-UNDO.
DEF VAR pcMva%        AS CHAR NO-UNDO.
DEF VAR piTid         AS INT  NO-UNDO.

DEF VAR piHg          AS INT  NO-UNDO.
DEF VAR piVg          AS INT  NO-UNDO.
DEF VAR piAvdelingNr  AS INT  NO-UNDO.
DEF VAR piMomsKod     AS INT  NO-UNDO.
DEF VAR piHovedGr     AS INT  NO-UNDO.
DEF VAR pbStatus      AS LOG  NO-UNDO.
DEF VAR piAntKoblet   AS INT  NO-UNDO.
DEF VAR piAntOppdat   AS INT  NO-UNDO.
DEF VAR iTstNr        AS INT  NO-UNDO.
DEF VAR dTst          AS DECI NO-UNDO.
DEF VAR dTmpKost_Proc AS DECI NO-UNDO.

/*
DEF VAR piFarg        AS INT  NO-UNDO.
DEF VAR pcVareNr      AS CHAR NO-UNDO.
DEF VAR piTilbud      AS INT  NO-UNDO.
DEF VAR piAntArtikler AS INT  NO-UNDO.
*/
DEF VAR piLoop1       AS INT  NO-UNDO.

DEF BUFFER bMoms FOR Moms.

ASSIGN
    piTid  = TIME
    cTekst = "Starter utpakking av VPI.".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

/* Aldri tilbud */
ASSIGN
    pbStatus = FALSE
    .

/* Standard Mva */
FIND FIRST Moms NO-LOCK WHERE 
    Moms.Momsproc > 0 NO-ERROR.
IF AVAILABLE Moms THEN
    piMomsKod = Moms.MomsKod.

/* Standard hovedgruppe */
FIND FIRST HuvGr NO-LOCK WHERE
    HuvGr.Hg > 0 NO-ERROR.
IF AVAILABLE HuvGr THEN
    piHovedGr = HuvGr.Hg.

/* Oppretter VPIDatasett hvis det ikke finnes. */
IF NOT AVAILABLE VPIDatasett THEN
DO TRANSACTION:
    CREATE VPIDatasett.
    ASSIGN
        VPIDatasett.EkstVPILevNr   = VPIFilHode.EkstVPILevNr
        VPIDatasett.DatasettStatus = 1 /* Opprettet. */
        .
    FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

/* Tømmer temp-table */
FOR EACH TT_ProdGr:
  DELETE TT_ProdGr.
END.

/* Setter datasettets status. */
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
      VPIDatasett.DatasettStatus = 2 /* VPI importeres */
      VPIDatasett.ImportDato     = TODAY
      VPIDatasett.ImportKl       = TIME
      VPIDatasett.FilId          = VPIFilHode.FilId
      .
  FIND CURRENT VPIDatasett NO-LOCK.
END. /* TRANSACTION */

ASSIGN
    iAntNya     = 0
    iAntUppdat  = 0
    iAntNyaHg   = 0
    iAntNyaAvd  = 0
    iAntFel     = 0
/*     cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel" */
    .

/* Behandler fillinjene */
VPIFILLINJE:
FOR EACH VPIFilLinje OF VPIFilHode TRANSACTION:
    CREATE TT_ProdGr.
    ASSIGN
        cEndelse                   = ""
        piLoop1                    = piLoop1 + 1
        iTotAntLinjer              = iTotAntLinjer + 1
        TT_ProdGr.Produktgrupp     = trim(ENTRY( 1,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.Benamning        = trim(ENTRY( 2,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.Overproduktgrupp = trim(ENTRY( 3,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.OprodBenamning   = trim(ENTRY( 4,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.Bransch          = trim(ENTRY( 5,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.BranchBen        = trim(ENTRY( 6,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.Marginal%        = trim(ENTRY( 7,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.Moms%            = trim(ENTRY( 8,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.Momskod          = trim(ENTRY( 9,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.AccountKey       = trim(ENTRY(10,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.KommissionTF     = trim(ENTRY(11,VPIFilLinje.StorTekst,";"),'"')
        TT_ProdGr.OrsakDel         = IF CAN-DO(",0",TT_ProdGr.Overproduktgrupp) THEN "Fel överproduktgrupp"
                                      ELSE IF CAN-DO(",0",TT_ProdGr.Produktgrupp) THEN "Fel produktgrupp"
                                      ELSE IF CAN-DO(",0",TT_ProdGr.Bransch) THEN "Fel bransch"
                                      ELSE IF CAN-DO(",0",TT_ProdGr.Momskod) THEN "Fel momskod" ELSE "".
        TT_ProdGr.Radnr            = piLoop1 
        .
        IF NOT CAN-FIND(Moms WHERE Moms.MomsKod = INT(TT_ProdGr.Momskod)) THEN DO:
            ASSIGN dTst   = DECI(TT_ProdGr.Moms%)
                   iTstNr = INT(TT_ProdGr.Momskod) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN DO:
                CREATE Moms.
                ASSIGN Moms.MomsKod     = iTstNr
                       Moms.MomsProc    = dTst / 100
                       Moms.Beskrivelse = IF Moms.MomsProc = 0 THEN "Ingen moms" ELSE
                           STRING(dTst) + "% moms".
            END.
        END.
    /* Melding til brukeren. */
    IF piLoop1 MODULO 50 = 0 THEN
        STATUS DEFAULT "Behandler linje " + STRING(iTotAntLinjer) + ".".

    /* Konverterer verdier. */
    ASSIGN
        piVg         = int(TT_ProdGr.Produktgrupp)
        /* HG overstyres nedenfor hvis varegruppen finnes. */
        piHg         = int(TT_ProdGr.Overproduktgrupp)
        piAvdelingNr = int(TT_ProdGr.Bransch)
        NO-ERROR.
/* MESSAGE piVg piHg piAvdelingNr SKIP    */
/*     TT_ProdGr.Produktgrupp             */
/*     TT_ProdGr.Benamning                */
/*     TT_ProdGr.Overproduktgrupp         */
/*     TT_ProdGr.OprodBenamning           */
/*     TT_ProdGr.Bransch                  */
/*     TT_ProdGr.BranchBen                */
/*     TT_ProdGr.Marginal%                */
/*     TT_ProdGr.Moms%                    */
/*     TT_ProdGr.Momskod                  */
/*     TT_ProdGr.AccountKey               */
/*     TT_ProdGr.KommissionTF             */
/*     TT_ProdGr.OrsakDel                 */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    
    /* Håndterer feil i importerte data. */
    IF ERROR-STATUS:ERROR THEN
        NEXT VPIFILLINJE.
    /* Test om något av id-fälten inte är numeriska */
    IF TT_ProdGr.OrsakDel = "" THEN 
    TESTBLOCK: 
    DO:
        ASSIGN iTstNr = INT(TT_ProdGr.Bransch) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN TT_ProdGr.OrsakDel = "Bransch inte numerisk".
            LEAVE TESTBLOCK.
        END.
        ASSIGN iTstNr = INT(TT_ProdGr.Overproduktgrupp) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN TT_ProdGr.OrsakDel = "Överproduktgrupp inte numerisk".
            LEAVE TESTBLOCK.
        END.
        ASSIGN iTstNr = INT(TT_ProdGr.Produktgrupp) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN TT_ProdGr.OrsakDel = "Produktgrupp inte numerisk".
            LEAVE TESTBLOCK.
        END.
        ASSIGN iTstNr = INT(TT_ProdGr.Momskod) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN TT_ProdGr.OrsakDel = "Momskod inte numerisk".
            LEAVE TESTBLOCK.
        END.
        IF TT_ProdGr.OrsakDel = "" AND 
               NOT CAN-FIND(Moms WHERE Moms.Momskod = INT(TT_ProdGr.Momskod)) THEN
            ASSIGN TT_ProdGr.OrsakDel = "Fel momskod".
    END. /* TESTBLOCK */   
    /* Poster med feil skal ikke behandles. */
    IF TT_ProdGr.OrsakDel <> "" THEN
        NEXT VPIFILLINJE.

    KONVERTER:
    DO:
        /* ----- Konverteringstabell - Varegruppe ----- */
        {getvpitabell.i &LevNr = VPIFilHode.EkstVPILevNr
                        &Nr = 1503
                        &Felt = cImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE
            ImpKonv.EDB-System = cEDB-System AND
            ImpKonv.Tabell     = cImpTabell AND
            ImpKonv.EksterntId = string(piVg) NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert varegruppe " + string(piVg) + " til " + ImpKonv.InterntId + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
            ASSIGN
              piVg = int(ImpKonv.InterntId)
              .
        END.
        /* Kontroll av varegruppe */
        IF NOT CAN-FIND(VarGr WHERE
                        VarGr.Vg = piVg) THEN
        DO:
            ASSIGN
              piAntFeil = piAntFeil + 1
              cTekst   =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny varegruppe opprettet " + string(piVg) + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny varegruppe opprettet " + string(piVg) + "."
              .
        END.
        /* ---- Konverteringstabell - Hovedgrupper ----- */
        {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                        &Nr = 1502
                        &Felt = cImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE
            ImpKonv.EDB-System = cEDB-System AND
            ImpKonv.Tabell     = cImpTabell AND
            ImpKonv.EksterntId = string(piHg) NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert hovedgruppe " + string(piHg) + " til " + ImpKonv.InterntId + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
            ASSIGN
              piHg = int(ImpKonv.InterntId)
              .
        END.
        /* Kontroll av hovedgruppe */
        IF NOT CAN-FIND(HuvGr WHERE
                        HuvGr.Hg = piHg) THEN
        DO:
            ASSIGN
              piAntFeil = piAntFeil + 1
              cTekst   =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny hovedgruppe opprettet " + string(piHg) + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny hovedgruppe opprettet " + string(piHg) + "."
              .
        END.
        /* ---- Konverteringstabell - Avdeling ----- */
        {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                        &Nr = 1011
                        &Felt = cImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE
            ImpKonv.EDB-System = cEDB-System AND
            ImpKonv.Tabell     = cImpTabell AND
            ImpKonv.EksterntId = string(piAvdelingNr) NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert avdeling " + string(piAvdelingNr) + " til " + ImpKonv.InterntId + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
            ASSIGN
              piHg = int(ImpKonv.InterntId)
              .
        END.
        /* Kontroll av avdeling */
        IF NOT CAN-FIND(Avdeling WHERE
                        Avdeling.AvdelingNr = piAvdelingNr) THEN
        DO:
            ASSIGN
              piAntFeil = piAntFeil + 1
              cTekst   =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny avdeling opprettet " + string(piAvdelingNr) + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ny avdeling opprettet " + string(piAvdelingNr) + "."
              .
        END.
        /* ----- Konverteringstabell Mva ----- */
        {getvpitabell.i &LevNr =VPIFilHode.EkstVPILevNr
                        &Nr = 1013
                        &Felt = cImpTabell}
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell     = cImpTabell AND 
            ImpKonv.EksterntId = pcMva% NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": Konvertert mva% til mvakode " + string(pcMva%) + " til " + ImpKonv.InterntId + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "20").
            ASSIGN
              piMomsKod = int(ImpKonv.InterntId)
              .
        END.
        ELSE FIND FIRST Moms NO-LOCK WHERE
            Moms.MomsProc = DEC(pcMva%) NO-ERROR.
        IF AVAILABLE Moms THEN
            ASSIGN
            piMomsKod = Moms.MomsKod
            .
        ELSE DO:
            ASSIGN
                cTekst =  "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent mva% " + pcMva% + ".".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "30").
            ASSIGN
              piAntFeil = piAntFeil + 1
              piMomsKod = 0
              .
            CREATE tt_Error.
            ASSIGN
              tt_Error.LinjeNr = piAntFeil
              tt_Error.Tekst   = "Linje " + STRING(iTotAntLinjer,"zzzzzz9") + ": * Ukjent mva% " + pcMva% + "."
              .
        END.
    END. /* KONVERTER */

    /* Oppretter/oppdaterer poster i registrene */
    REGISTEROPPDAT:
    DO:
        /* Avdelning har kommit i tillägg */
        /* Om Vi får en ny produktgrupp skall det testas om överproduktgr.(Huvgr) */
        /* finns. Annars skapas den. Om produktgruppen finns registrerad sedan    */
        /* tidigare kan HuvGr-kopplingen var utbytt och därför görs testen här */
        FIND Avdeling WHERE Avdeling.AvdelingNr = piAvdelingNr NO-ERROR.
        
        IF NOT AVAIL Avdeling THEN DO:
            CREATE Avdeling.
            ASSIGN Avdeling.AvdelingNr   = piAvdelingNr
                   Avdeling.AvdelingNavn = TRIM(TT_ProdGr.BranchBen)
                   iAntNyaAvd            = iAntNyaAvd + 1.
        END.
        ELSE IF Avdeling.AvdelingNavn <> TRIM(TT_ProdGr.BranchBen) THEN
            ASSIGN Avdeling.AvdelingNavn = TRIM(TT_ProdGr.BranchBen).

        FIND HuvGr WHERE HuvGr.Hg = piHg NO-ERROR.
        IF NOT AVAIL HuvGr THEN DO:
            CREATE HuvGr.
            ASSIGN HuvGr.Hg    = piHg
                   HuvGr.AvdelingNr = piAvdelingNr
                   HuvGr.HgBeskr    = TRIM(TT_ProdGr.OprodBenamning)
                   iAntNyaHg   = iAntNyaHg + 1.
        END.
        ELSE IF HuvGr.AvdelingNr <> piAvdelingNr OR HuvGr.HgBeskr <> TRIM(TT_ProdGr.OprodBenamning) THEN
            ASSIGN HuvGr.AvdelingNr = piAvdelingNr
                   HuvGr.HgBeskr    = TRIM(TT_ProdGr.OprodBenamning).

        FIND VarGr WHERE VarGr.Vg = piVg NO-ERROR.
        ASSIGN dTmpKost_Proc = (IF ROUND((dec(TT_ProdGr.Marginal%) / 100),1) > 0
                                 then 100 - (dec(TT_ProdGr.Marginal%) / 100)
                                 ELSE 65.0)
               dTmpKost_Proc = IF dTmpKost_Proc = 100 THEN
                                 65
                                 ELSE IF dTmpKost_Proc = 0 THEN
                                 65
                                 ELSE dTmpKost_Proc.
        IF NOT AVAIL VarGr THEN DO:
            CREATE VarGr.
            ASSIGN VarGr.Vg        = piVg
                   VarGr.Hg        = piHg
                   VarGr.VgBeskr   = TRIM(TT_ProdGr.Benamning)
                   VarGr.MomsKod   = INT(TT_ProdGr.Momskod)
                   VarGr.Kost_Proc = dTmpKost_Proc
                   iAntNya     = iAntNya + 1.
            IF NOT CAN-FIND(VgKat WHERE VgKat.Vg = VarGr.Vg AND
                            VgKat.VgKat = 1 AND VgKat.KatNr = 1) THEN
            CREATE VgKat.
            ASSIGN 
                VgKat.Vg = VarGr.Vg
                VgKat.VgKat = 1
                VgKat.KatNr = 1
                .
        END.
        ELSE DO:
            IF VarGr.Hg      <> piHg                   OR VarGr.VgBeskr   <> TRIM(TT_ProdGr.Benamning) OR
               VarGr.MomsKod <> INT(TT_ProdGr.Momskod) OR VarGr.Kost_Proc <> dTmpKost_Proc THEN
            ASSIGN 
                VarGr.Hg        = piHg
                VarGr.VgBeskr   = TRIM(TT_ProdGr.Benamning)
                VarGr.MomsKod   = INT(TT_ProdGr.Momskod)
                VarGr.Kost_Proc = dTmpKost_Proc.
        END.
        IF AVAILABLE VarGr    THEN RELEASE VarGr.
        IF AVAILABLE HuvGr    THEN RELEASE HuvGr.
        IF AVAILABLE Avdeling THEN RELEASE Avdeling.
        IF AVAILABLE Moms     THEN RELEASE Moms.
    END. /* REGISTEROPPDAT */

    ASSIGN
        pbStatus = TRUE
        .
END. /* TRANSACTION - VPIFILLINJE */

IF pbStatus = TRUE THEN
DO TRANSACTION:
  FIND CURRENT VPIDatasett EXCLUSIVE-LOCK.
  ASSIGN
/*       VPIDatasett.DatasettStatus = 3 /* Gruppeinfo mottatt og oppdatert lokalt */ */
      VPIDatasett.DatasettStatus = 5 /* Her MÅ vi ferdigstille med en gang     */
      .
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
  ASSIGN
      VPIFilHode.VPIFilStatus = 5
      .
  FIND CURRENT VPIDatasett NO-LOCK.
  FIND CURRENT VPIFilHode  NO-LOCK.

END. /* TRANSACTION */

STATUS DEFAULT "".

ASSIGN
    cTekst = "Behandlet " + STRING(iTotAntLinjer) + " VPI poster. " + 
             "Antall linjer med ukjente koder " + STRING(piAntFeil) + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
ASSIGN
    cTekst = "Utpakking av VPI ferdig.Tidsbruk " + STRING(TIME - piTid,"HH:MM:SS") + ".".
PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var pcTekst   as char no-undo.
  def var pdAktDato as date no-undo.
  DEF VAR plTilbud AS LOG  NO-UNDO.
  DEF VAR plManuel AS LOG  NO-UNDO.
  
  assign
    plTilbud = false
    plManuel = FALSE
    pdAktDato =  TODAY - 1
    pcTekst   =   
      /*string(input FI-ValPris) */ string(dcValPris) + ";" +
      /*string(input FI-InnPris) */ string(dcInnPris) + ";" +
      /*string(input FI-Rab1)    */ string(dcRabatt)  + ";" +
      /*string(input FI-Rab1%)   */ "0" + ";" +
      /*string(input FI-Rab2)    */ "0" + ";" +
      /*string(input FI-Rab2%)   */ "0" + ";" +
      /*string(input FI-Frakt)   */ "0" + ";" +
      /*string(input FI-Frakt%)  */ "0" + ";" +
      /*string(input FI-DivKost) */ "0" + ";" +
      /*string(input FI-DivKost%)*/ "0" + ";" +
      /*string(input FI-Rab3)    */ "0" + ";" +
      /*string(input FI-Rab3%)   */ "0" + ";" +
      /*string(input FI-VareKost)*/ "0" + ";" +
      /*string(input FI-Mva)     */ "0" + ";" +
      /*string(input FI-Mva%)    */ "0" + ";" +
      /*string(input FI-DB)      */ "0" + ";" +
      /*string(input FI-DB%)     */ "0" + ";" +
      /*string(input FI-Pris)    */ STRING(dcUtpris) + ";" +
      /*string(input FI-EUPris)  */ "0" + ";" +
      /*plManuel                 */ "no" + ";"
       .
  /* Normal aktiveringsdag/tid */                 
  ASSIGN  
    cEndelse = cEndelse +              
             (if pdAktDato <> ?
                then string(pdAktDato)
                else "") + ";" +
              "0;"
    cEndelse = cEndelse + 
             ";0;;0;no"
    pcTekst = pcTekst + cEndelse
    .
  
  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

