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


DEF VAR cFilNavn AS CHAR FORMAT "x(30)"  NO-UNDO.
DEF VAR cLoggFil AS CHAR FORMAT "x(30)"  NO-UNDO.
DEF VAR cLinje   AS CHAR FORMAT "x(100)" NO-UNDO.   
DEF VAR pcSkjerm      AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR iSasong       AS INT    NO-UNDO.

DEF VAR iProfilNr     AS INT NO-UNDO.
DEF VAR iCl AS INT NO-UNDO.

DEF VAR lKorriger AS LOG INITIAL FALSE NO-UNDO.

DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEF STREAM InnFil.
DEF STREAM LoggFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fixChkEan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEan Procedure 
FUNCTION fixChkEan RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCl INT}
    
/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .


ASSIGN
    cFilNavn = "C:\Appdir\Sport1HK\Sommerpricat modellnr flyttet 090707.csv"
    cLoggFil = "C:\Appdir\Sport1HK\Sommerpricat modellnr flyttet 090707.log"
    .

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

RUN LesInnFil.

IF VALID-HANDLE(h_PrisKo) THEN
  DELETE PROCEDURE h_Prisko.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  assign
    lTilbud     = FALSE
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
  FIND VarGr OF ArtBas NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
  DO:
    RETURN "AVBRYT".
  END.
  FIND Moms  OF VarGr  NO-ERROR.
  IF NOT AVAILABLE Moms THEN
  DO:
    RETURN "AVBRYT".
  END.

  /* Starter omkalkulering.                         */
  run Omregning in h_PrisKo
       (input rArtBasRecid, 
        input iProfilNr,
        input-output pcSkjerm,
        input Moms.MomsProc,
        input Valuta.ValKurs, 
        input piFeltNr,
        INPUT lTilbud).
  
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
DEF VAR iStrKode AS INT  FORMAT "999"   NO-UNDO.
DEF VAR iStorl   AS CHAR FORMAT "x(4)"  NO-UNDO.
DEF VAR cBilde   AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR cKode    AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR cBeskr   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR lAnonse  AS LOG                 NO-UNDO.
DEF VAR iAntIPakn AS INT NO-UNDO.
DEF VAR lKatalogpris AS DEC NO-UNDO.
DEF VAR lForHRab%    AS DEC NO-UNDO.
DEF VAR lSuppRab%    AS DEC NO-UNDO.
DEF VAR cLevDato1 AS CHAR NO-UNDO.
DEF VAR cLevDato2 AS CHAR NO-UNDO.
DEF VAR cLevDato3 AS CHAR NO-UNDO.
DEF VAR cLevDato4 AS CHAR NO-UNDO.
DEF VAR iLevNr    AS INT  NO-UNDO.
DEF VAR cLevFarg  AS CHAR NO-UNDO.
DEF VAR cModellNr AS CHAR NO-UNDO.
DEF VAR cBestillingsnr AS CHAR NO-UNDO.
DEF VAR iVareGruppe    AS INT  NO-UNDO.
DEF VAR iStrType       AS INT  NO-UNDO.
DEF VAR iDec AS dec NO-UNDO.
DEF VAR iLopNr AS INT NO-UNDO.
DEF VAR iLevKod AS CHAR NO-UNDO.
DEF VAR lVareBehNr AS DEC NO-UNDO.

DEF VAR piTilbud AS INT NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

CURRENT-WINDOW:WIDTH = 200.

INPUT  STREAM InnFil  FROM VALUE(cFilNavn).
OUTPUT STREAM LoggFil TO VALUE(cLoggFil).

/* Blank lije mellom artikklene */
PUT STREAM LoggFil SKIP(1).
PUT STREAM LoggFil 
    "Innlesning av fil " cFilNavn " startet. " TODAY " " STRING(TIME,"HH:MM:SS")
    SKIP.

ASSIGN
    /*
    lKorriger = FALSE /* Kun logging */
    */
    lKorriger  = TRUE  /* Utfører korreksjonene */
    piTilbud   = 1
    lVareBehNr = 90000026 /* Suppleringsbok */
    .

MAINLOOP:
REPEAT:
    IMPORT STREAM InnFil  UNFORMATTED cLinje.

    ASSIGN
        iLevNr         = IF length(trim(ENTRY(2,cLinje,";"))) = 5
                           THEN INT( substring(trim(ENTRY(2,cLinje,";")),3,5))
                           ELSE INT(trim(ENTRY(2,cLinje,";")))
        cKode          = trim(ENTRY(4,cLinje,";"))
        cBeskr         = trim(ENTRY(5,cLinje,";"))
        iAntIPakn      = int(ENTRY(12,cLinje,";"))
        iSasong        = int(ENTRY(34,cLinje,";"))
        cModellNr      = trim(ENTRY(3,cLinje,";"))
        cBestillingsNr = trim(ENTRY(25,cLinje,";"))
        cLevFarg       = trim(ENTRY(7,cLinje,";"))
        iVareGruppe    = int(ENTRY(23,cLinje,";"))
        lAnonse        = TRUE /*IF trim(ENTRY(33,cLinje,";")) = "1" THEN TRUE ELSE FALSE*/
        lKatalogpris   = dec(replace(trim(trim(ENTRY(13,cLinje,";"),'"'),"%"),' ',''))
        lForhRab%      = DEC(trim(trim(trim(ENTRY(15,cLinje,";"),'"'),"%"),''))
        lSuppRab%      = DEC(trim(trim(trim(ENTRY(16,cLinje,";"),'"'),"%"),''))
        cLevDato1      = trim(ENTRY(19,cLinje,";"),'"')
        cLevDato2      = trim(ENTRY(20,cLinje,";"),'"')
        cLevDato3      = trim(ENTRY(21,cLinje,";"),'"')
        cLevDato4      = trim(ENTRY(22,cLinje,";"),'"')
        /*
            ttPriKat.VeilPris       = trim(ENTRY(17,VPIFilLinje.StorTekst,";"),'"')
            ttPriKat.VeilPris       = trim(REPLACE(ttPriKat.VeilPris,' ',''),"%")
            ttPriKat.PAKstru        = trim(ENTRY(18,VPIFilLinje.StorTekst,";"),'"')
            ttPriKat.Enh            = trim(ENTRY(11,VPIFilLinje.StorTekst,";"),'"')
            ttPriKat.Enh            = (IF ttPriKat.Enh = "" THEN "Stk" ELSE ttPriKat.Enh)
        */
        cBilde      = trim(ENTRY(35,cLinje,";"))
        lArtikkelNr = DEC(
                          substring(
                                    ENTRY(37,cLinje,";"),1,LENGTH(ENTRY(37,cLinje,";")) - 3
                                    )
                             )
        iStrKode    = int(
                             substring(
                                       ENTRY(37,cLinje,";"),LENGTH(ENTRY(37,cLinje,";")) - 2,LENGTH(ENTRY(37,cLinje,";"))
                                       )
                             )
        .
    /* Hvis blank strekkode, skal den ikke behandles. */
    IF cKode = "" OR cKode BEGINS "00000000" THEN 
    DO:
        ASSIGN
            cKode = ""
            .
        PUT STREAM LoggFil 
            lArtikkelNr " " cBeskr "  *Strekkode med 0 ikke behandlet" cKode " LOGGET"
            SKIP
        .
    END.

    /* Bildekode skal ha 2 entries og ha endelse '.jpg' */
    IF NUM-ENTRIES(cBilde,".") <> 2 THEN
        cBilde = cBilde + '.jpg'.

    /* Størrelseskode "OS" (264) konverteres til "1" (001). */
    IF iStrKode = 264 THEN
        ASSIGN
        iStrKode = 1
        iStrType = 2.
    ELSE iStrType = 0.

    /* Koden skal være 13 tegn lang */
    IF LENGTH(cKode) <> 13 THEN
        cKode = FILL("0",13 - LENGTH(cKode)) + cKode.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.

    /* Ukjent artikkelnummer */
    IF NOT AVAILABLE ArtBas THEN
    DO:
        /* Blank lije mellom artikklene */
        PUT STREAM LoggFil SKIP(1).
        PUT STREAM LoggFil 
            lArtikkelNr " " cBeskr "  *Ukjent artikkelNr med strekkode " cKode
            SKIP
        .
        /* Hvis ikke artikkelen finnes, skal det ikke gjøres noe mer med denne linjen */
        NEXT MAINLOOP.
    END.
    ASSIGN 
        rArtBasRecid = RECID(ArtBas)
        .

    IF LKorriger AND AVAILABLE ArtBas THEN
    KORR:
    DO TRANSACTION:
        FIND CURRENT ArtBas EXCLUSIVE-LOCK.

        /* Logger varer som ikke er merket som kjedeleverte */
        PUT STREAM LoggFil 
            ArtBas.ArtikkelNr " " cBeskr " " "  *Artikkel ikke merket som kjedevare " 
            " Logget"
            SKIP
        .

        /* Oppdaterer VPI info */
        ASSIGN
             ArtBas.LevKod         = cModellNr             
            .

    END. /* KORR */

    FIND CURRENT ArtBas NO-LOCK.

END. /* MAINLOOP */

OUTPUT STREAM LoggFil CLOSE.
INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fixChkEan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEan Procedure 
FUNCTION fixChkEan RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
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

