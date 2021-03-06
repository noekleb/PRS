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

DEF INPUT PARAMETER dFraDato  AS DATE   NO-UNDO.
DEF INPUT PARAMETER dTilDato  AS DATE   NO-UNDO.
/* DEF INPUT PARAMETER iButikkNr AS INT    NO-UNDO. */

/* DEFINE VARIABLE iBrGrpNr AS INTEGER    NO-UNDO. */

DEFINE VARIABLE cUke       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAarWeek   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cWeeknum   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilprefix AS CHARACTER  NO-UNDO.
DEF VAR cFilNavn         AS CHAR   NO-UNDO.
DEF VAR cKopi            AS CHAR   NO-UNDO.
DEF VAR cKatalog         AS CHAR   NO-UNDO.
DEF VAR cDato            AS CHAR   NO-UNDO.
DEF VAR cTekst           AS CHAR   NO-UNDO.
DEF VAR cOutFilnavn       AS CHAR   NO-UNDO.

DEF STREAM Ut.
DEF STREAM Kopi.

DEFINE TEMP-TABLE TT_AC NO-UNDO
    FIELD butik      AS INTE
    FIELD vg         AS INTE
    FIELD ean        AS DECI
    FIELD artikkelnr AS DECI
    FIELD antall     AS DECI
    FIELD fsgsum      AS DECI
    FIELD beskr      AS CHAR
    FIELD frp        AS CHAR
         INDEX bha butik vg ean.

/* 
 
Vecka  BSnr  Pgr  Artikelnr     EANKOD        Antal           Belopp          
------ ----- ---- ------------- ------------- --------------- --------------- 
V35-07 12080 003  0000000000011             0            1.00           25.00
V35-07 12080 003  0000000030076 7391860825654            1.00          155.00
V35-07 12080 003  0000000030094 7391860826798            1.00           87.00
V35-07 12080 003  0000000826842 7391860826842            1.00          128.00
 
 
 */

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
         WIDTH              = 42.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR iLoop AS INT NO-UNDO.

/* {syspara.i 1 8 6 iButikkNr INT} */
/* IF iButikkNr = 0 THEN           */
/*     iButikkNr = 1.              */

{syspara.i 210 253 1 cKatalog}
IF cKatalog = "" THEN
    cKatalog = "c:\home\lindbak\sendes".

{syspara.i 210 253 6 cFilprefix}
IF cFilprefix = "" THEN cFilprefix = "Time_ACN_".
cFilNavn = cTekst.

cFilNavn = cFilNavn + STRING(iLoop,"9999999") + ".". 

RUN weeknum.p (dTilDato, OUTPUT iAarWeek).
cWeekNum = STRING(iAarWeek).

/* om det �r vecka 1 kan fradato var f�rra �ret d�rf�r kommer �ret fr�n tildato */
cUke = SUBSTR(cWeeknum,3).
/* cUke = "V" + SUBSTR(cWeeknum,5) + "-" + SUBSTR(cWeeknum,3,2). */
/* cUke = "V" + cWeeknum + "-" + IF cWeeknum = "01" THEN               */
/*                               SUBSTR(STRING(YEAR(dTilDato)),3) ELSE */
/*                               SUBSTR(STRING(YEAR(dFraDato)),3).     */

/* FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK. */
/* ASSIGN iBrGrpNr = bruker.BrGrpNr.                             */

cFilnavn = cFilprefix + cWeeknum.

RUN EksportFsg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksportFsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportFsg Procedure 
PROCEDURE EksportFsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
------------------------------------------------------------------------------*/
  
  DEF VAR plVVarekost AS DEC NO-UNDO.
  DEF VAR plOrdPris AS DEC NO-UNDO.
  DEF VAR pcKundeNr   AS CHAR NO-UNDO.
  DEF VAR pcLevKod1   AS CHAR NO-UNDO.
  DEF VAR pcLevKod2   AS CHAR NO-UNDO.
  DEF VAR pcPOS       AS CHAR NO-UNDO.
  DEFINE VARIABLE cButik AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cEAN   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAntal AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPris  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cBeskr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFRP   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dDato AS DATE       NO-UNDO.
  DEFINE VARIABLE iKoeff AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dArtikkelnr AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE lBeskrUt AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE dLinjesumTMP AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dStyckPris AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cSaveFilnavn AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dMengde      AS DECIMAL     NO-UNDO.
/*   FOR EACH butiker WHERE butiker.BrGrpNr = iBrGrpNr NO-LOCK. */
  cOutFilnavn = cKatalog + "\" + cFilNavn + ".tmp".
  cSaveFilnavn = cKatalog + "\" + cFilNavn + ".txt".
  
  IF SEARCH(cSaveFilnavn) <> ? THEN
      OS-DELETE VALUE(cSaveFilnavn).
  FOR EACH Butiker NO-LOCK.
      FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK.
          DO dDato = dFradato TO dTilDato: 
              FOR EACH bonghode WHERE bonghode.butikkNr = butiker.butik AND
                                      bonghode.gruppenr = 1 AND
                                      bonghode.kassenr  = kasse.kassenr AND
                                      bonghode.dato     = dDato NO-LOCK.
                  IF BongHode.Makulert = 2 THEN
                      NEXT.
                  FOR EACH BongLinje WHERE Bonglinje.b_id = Bonghode.b_id NO-LOCK.
                      IF bonglinje.makulert THEN
                          NEXT.
                      IF Bonglinje.TTId = 1 OR BongLinje.TTId = 10 THEN DO:
                          dArtikkelnr = DECI(BongLinje.Artikkelnr) NO-ERROR.
                          IF ERROR-STATUS:ERROR OR dArtikkelnr < 1 THEN /* 0 eller negativt ? :) */
                              NEXT.
                          FIND TT_AC WHERE TT_AC.Butik = butiker.butik AND
                                           TT_AC.vg    = bonglinje.varegr AND
                                           TT_AC.ean   = DECI(bonglinje.strekkode) NO-ERROR.
                          IF NOT AVAIL TT_AC THEN DO:
                              FIND ArtBas WHERE ArtBas.Artikkelnr = dArtikkelnr NO-LOCK NO-ERROR.
                              IF NOT AVAIL artbas THEN
                                  NEXT.
                              IF artbas.non_sale = TRUE OR artbas.pant = TRUE THEN
                                  NEXT.
                              IF artbas.opris = TRUE AND artbas.Hg <> 12 THEN NEXT.
                          END.
                          IF NOT AVAIL TT_AC THEN DO:
                              CREATE TT_AC.
                              ASSIGN TT_AC.Butik      = butiker.butik
                                     TT_AC.vg         = bonglinje.varegr
                                     TT_AC.ean        = DECI(bonglinje.strekkode)
                                     TT_AC.artikkelnr = dArtikkelnr
                                     TT_AC.Beskr      = SUBSTR(bonglinje.bongtekst,1,20).
                              IF artbas.mengde > 0 AND (artbas.jamforenhet = "kg" OR artbas.jamforenhet = "l") THEN DO:
                                  IF artbas.jamforenhet = "kg" THEN DO:
                                      dMengde = artbas.mengde.
                                      ASSIGN TT_AC.frp = STRING(TRUNC(dMengde * 1000,0)) + "G".
                                  END.
                                  ELSE DO:
                                      dMengde = artbas.mengde.
                                      ASSIGN TT_AC.frp = STRING(TRUNC(dMengde * 100,0)) + "CL".
                                  END.
                              END.
                              RELEASE Artbas.
                          END.
                          IF bonglinje.antall <> 0 THEN DO:
                              ASSIGN iKoeff = IF bonglinje.antall > 0 THEN 1 ELSE -1.
                              ASSIGN TT_AC.Antall  = TT_AC.Antall + bonglinje.antall.
                                      TT_AC.fsgsum = TT_AC.fsgsum + (iKoeff * (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)).
                          END.
                      END.
                  END.
              END.
          END.
      END.
  END.
/*   OUTPUT TO VALUE(SESSION:TEMP-DIR + "AC.txt"). */
/*   FOR EACH TT_AC:                               */
/*       EXPORT TT_AC.                             */
/*   END.                                          */
  OUTPUT TO VALUE(cOutFilnavn).
  FOR EACH TT_AC:
      ASSIGN dStyckPris = ROUND(TT_AC.fsgsum / TT_AC.Antall,2) NO-ERROR.
      IF dStyckPris = ? THEN
          NEXT.
      ASSIGN cButik = STRING(TT_AC.butik)
             cButik = cButik + FILL(" ",5 - LENGTH(cButik))
             cEAN   = STRING(TT_AC.ean)
             cEAN   = cEAN + FILL(" ",13 - LENGTH(cEAN))
             cAntal = STRING(TT_AC.Antall)
             cAntal = cAntal + FILL(" ",9 - LENGTH(cAntal))
             cPris  = TRIM(STRING(dStyckPris,">>>9.99"))
             cPris  = REPLACE(REPLACE(cPris,".",""),",","")
             cPris  = cPris + FILL(" ",7 - LENGTH(cPris))
             cBeskr = TRIM(SUBSTR(TT_AC.beskr,1,20))
             cBeskr = cBeskr + FILL(" ", 20 - LENGTH(cBeskr))
             cFRP   = TT_AC.frp
             cFRP   = cFRP + FILL(" ",7 - LENGTH(cFRP)).
      PUT UNFORMATTED cUke " "   
          cButik " " 
          cEAN   " "
          cAntal " "
          cPris  " "
          cBeskr " "
          cFRP   " "
          STRING(TT_AC.vg) SKIP.
  END.

/* 
 
Vecka  BSnr  Pgr  Artikelnr     EANKOD        Antal           Belopp          
------ ----- ---- ------------- ------------- --------------- --------------- 
V35-07 12080 003  0000000000011             0            1.00           25.00
V45-07 11016 003 0020008881102 7391860916703            1.00           90.00
V35-07 12080 003  0000000030076 7391860825654            1.00          155.00
V35-07 12080 003  0000000030094 7391860826798            1.00           87.00
V35-07 12080 003  0000000826842 7391860826842            1.00          128.00
 */
  
 /* OUTPUT STREAM Kopi CLOSE.*/
/*   OUTPUT STREAM Ut CLOSE. */
  OUTPUT CLOSE.
  OS-RENAME VALUE(cOutFilnavn) VALUE(cSaveFilnavn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

