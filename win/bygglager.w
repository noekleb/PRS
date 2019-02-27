&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE tmpLager NO-UNDO LIKE tmpLager.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wArtBasRecid AS RECID NO-UNDO.
  DEF VAR wStrListe    AS CHAR  NO-UNDO.
  DEF VAR wVisModus    AS INT   NO-UNDO.
  FIND FIRST Lager NO-LOCK.
  FIND ArtBas OF Lager NO-LOCK NO-ERROR.
  IF AVAILABLE Lager THEN
    wArtBasRecid = RECID(ArtBas).
&ELSE
  DEF INPUT  PARAMETER wArtBasRecid AS RECID NO-UNDO.
  DEF INPUT  PARAMETER wVisModus    AS INT   NO-UNDO.
  DEF OUTPUT PARAMETER wStrListe    AS CHAR  NO-UNDO.
  DEF OUTPUT PARAMETER cBrukteStr   AS CHAR  NO-UNDO.
&ENDIF

DEFINE TEMP-TABLE ttStrKonv NO-UNDO LIKE StrKonv.

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
   Temp-Tables and Buffers:
      TABLE: tmpLager T "SHARED" NO-UNDO Temp-DB tmpLager
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 11
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND ArtBas NO-LOCK WHERE
  RECID(ArtBas) = wArtBasRecid NO-ERROR.
IF NOT AVAILABLE ArtBas THEN
  DO:
    MESSAGE "Ukjent ArtBas!" VIEW-AS ALERT-BOX TITLE "Feil".
    RETURN NO-APPLY "AVBRYT".
  END.
RUN ByggLager.

/*
IF NUM-ENTRIES(cBrukteStr) = 201 THEN 
  cBrukteStr = 'J,,'.
*/
/* OUTPUT close. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggLager Procedure 
PROCEDURE ByggLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wSumAntall  AS DEC            NO-UNDO.
DEF VAR wSumVerdi   AS DEC            NO-UNDO.    
DEF VAR wSumLagAntall LIKE Lager.LagAnt NO-UNDO.
DEF VAR wSumLagVerdi  LIKE Lager.VerdiSolgt NO-UNDO.    
DEF VAR wVVK        AS DEC            NO-UNDO.
DEF VAR wDivAntall  AS DEC            NO-UNDO.
DEF VAR wAntall     AS CHAR EXTENT 99 NO-UNDO.
DEF VAR wLoop       AS INT            NO-UNDO.
DEF VAR wCaseAnt    AS INT            NO-UNDO.
DEFINE VARIABLE cBrukteTmp AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iLookup AS INTEGER    NO-UNDO.
DEF VAR wEntry    AS INT  NO-UNDO.
DEF VAR bFlagg    AS LOG NO-UNDO.
DEFINE VARIABLE wStrListe2 AS CHARACTER   NO-UNDO.

/* Nullstiller lagerposten */
FOR EACH tmpLager:
  DELETE tmpLager.
END.   
FOR EACH ttSTrKonv:
  DELETE ttStrKonv.
END. 
ASSIGN wStrListe  = "".
       cBrukteStr = "" /*FILL(",",200)*/.
{sww.i}

/* Bygger streng med størrelser */
/*
IF ArtBas.StrTypeId > 1 THEN
  FOR EACH StrTStr NO-LOCK WHERE
    StrTStr.StrTypeId = ArtBas.StrTypeId
    BY StrTStr.SeqNr:
    ASSIGN
      wStrListe = wStrListe +
                  (IF wStrListe = ""
                     THEN ""
                     ELSE ";") + 
                  StrTStr.SoStorl.  
  END.
*/
/* Lager en liste med størrelsene som det ligger strekkoder på. */
IF ArtBas.StrTypeId > 1 THEN
DO:
  /* Størrelser fra strekkodene */
  FOR EACH Strekkode OF ArtBas NO-LOCK:
    FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN NEXT.
    IF NOT CAN-FIND(FIRST ttStrKonv WHERE ttStrKonv.StrKode = StrKonv.StrKode) THEN 
    DO:
        CREATE ttSTrKonv.
        BUFFER-COPY StrKonv TO ttStrKonv.            
    END.
  END.
  /* Størrelser fra størrelsestypen */
  FOR EACH StrTStr NO-LOCK WHERE
    StrTStr.StrTypeId = ArtBas.StrTypeId:
    FIND FIRST StrKonv NO-LOCK WHERE StrKonv.Storl = StrTStr.SoStorl NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN NEXT.
    IF NOT CAN-FIND(FIRST ttStrKonv WHERE ttStrKonv.StrKode = StrKonv.StrKode) THEN 
    DO:
        CREATE ttStrKonv.
        BUFFER-COPY StrKonv TO ttStrKonv.            
    END.
  END.
  /* Størrelser fra størrelsestypen */
  FOR EACH ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr = ArtBas.ArtikkelNr
      BREAK BY ArtLag.ArtikkelNr
            BY ArtLag.Storl:
    IF LAST-OF(ArtLag.Storl) THEN
    BLOKKEN: 
    DO:
      FIND FIRST StrKonv NO-LOCK WHERE StrKonv.Storl = ArtLag.Storl NO-ERROR.
      IF NOT AVAILABLE StrKonv THEN LEAVE BLOKKEN.
      IF NOT CAN-FIND(FIRST ttStrKonv WHERE ttStrKonv.StrKode = StrKonv.StrKode) THEN 
      DO:
          CREATE ttStrKonv.
          BUFFER-COPY StrKonv TO ttStrKonv.            
      END.
    END. /* BLOKKEN */
  END.
  /* Sorterte størrelser */
  bFlagg = FALSE.
  FOR EACH ttStrKonv NO-LOCK WHERE
    ttStrKonv.SeqNr > 0
    BY ttStrKonv.SeqNr:
    IF LOOKUP(ttStrKonv.Storl,wStrListe,';') = 0 THEN 
      ASSIGN
      bFlagg    = TRUE
      wStrListe  = wStrListe  + (IF wStrListe = "" THEN "" ELSE ";") + ttStrKonv.Storl
/*       wStrListe  = wStrListe  + (IF wStrListe = "" THEN "" ELSE ";") + ttStrKonv.Storl */
      cBrukteStr = cBrukteStr + (IF wStrListe = "" THEN "" ELSE ",")
      .  
  END.
  IF bFlagg = FALSE THEN
  DO:
      IF ArtBas.StrTypeId > 1 THEN
        FOR EACH StrTStr NO-LOCK WHERE
          StrTStr.StrTypeId = ArtBas.StrTypeId
          BY StrTStr.SeqNr:
          IF LOOKUP(StrTStr.SoStorl,wStrListe,';') = 0 THEN 
            ASSIGN
              wStrListe   = wStrListe  + (IF wStrListe = "" THEN "" ELSE ";") + StrTStr.SoStorl
              wStrListe2  = wStrListe2  + (IF wStrListe2 = "" THEN "" ELSE ";") + StrTStr.SoStorl + (IF TRIM(StrTStr.EUStorl) <> "" AND
                                                                                                        TRIM(StrTStr.EUStorl) <> TRIM(StrTStr.SoStorl)
                                                                                                     THEN  "/" + TRIM(StrTStr.EUStorl) ELSE "")

            cBrukteStr = cBrukteStr + (IF wStrListe = "" THEN "" ELSE ",")
            .  
        END.
  END.
  /* Legger på usorterte størrelser bakerst */
  FOR EACH ttStrKonv NO-LOCK WHERE
    ttStrKonv.SeqNr = 0
    BY ttStrKonv.SeqNr:
    IF LOOKUP(ttStrKonv.Storl,wStrListe,';') = 0 THEN       
      ASSIGN
      wStrListe  = wStrListe  + (IF wStrListe = "" THEN "" ELSE ";") + ttStrKonv.Storl  
      wStrListe2  = wStrListe2  + (IF wStrListe2 = "" THEN "" ELSE ";") + ttStrKonv.Storl  
      cBrukteStr = cBrukteStr + (IF wStrListe = "" THEN "" ELSE ",")
      .
  END.
END.

/* Flagger de størrelsene det ligger strekkode på */
IF Artbas.strtype > 1 THEN 
DO:
    FIND strtype OF artbas NO-LOCK NO-ERROR.
    IF NOT AVAIL strtype THEN
        LEAVE.
    FOR EACH strekkode OF artbas NO-LOCK.
        FIND strkonv OF strekkode NO-LOCK NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        iLookup = LOOKUP(Strkonv.Storl,wStrListe,";").
        IF iLookup > 0 AND iLookup < 99 THEN
            ENTRY(iLookup,cBrukteStr) = "J".
    END.
END.

/* Påser at det finnes en lagerpost for artikkelen for alle butikker. */
LAGERMORGEN:
FOR EACH Butiker NO-LOCK WHERE
    Butiker.Butik > 0 AND
    Butiker.NedlagtDato = ? AND
    NOT CAN-FIND(Lager OF ArtBas WHERE
                 Lager.Butik = Butiker.Butik):
    CREATE Lager.
    ASSIGN
        Lager.ArtikkelNr = ArtBas.ArtikkelNr
        Lager.Butik      = Butiker.Butik
        .
END. /* LAGERMORGEN */

ASSIGN
    wSumLagVerdi  = 0
    wSumLagAntall = 0
    .

/* En post pr. butikk */
BUTIKK:
FOR EACH Butiker WHERE Butiker.NedlagtDato = ? NO-LOCK:
  FOR EACH Lager OF ArtBas NO-LOCK WHERE
    Lager.Butik = Butiker.Butik:

    FIND tmpLager WHERE
      tmpLager.ArtikkelNr = Lager.ArtikkelNr AND
      tmpLager.Butik      = string(Butiker.Butik,"zzzzz9") NO-ERROR.
    IF NOT AVAILABLE tmpLager THEN
      DO:
        CREATE tmpLager.
        ASSIGN 
          tmpLager.ArtikkelNr = Lager.ArtikkelNr
          tmpLager.Butik      = STRING(Butiker.Butik,"zzzzz9")
          tmpLager.VVAreKost  = Lager.VVareKost
          .
      END.
    
    ASSIGN
        wSumLagVerdi  = wSumLagVerdi  + DEC(Lager.Lagant * Lager.VVareKost)
        wSumLagAntall = wSumLagAntall + Lager.LagAnt
        .
    CASE wVisModus:  
      WHEN 1 THEN /* Lager */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.AntSolgt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.Lagant * Lager.VVareKost),'->>,>>>,>>9.99').
      WHEN 2 THEN /* Solgt */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.AntSolgt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.VerdiSolgt),'->>,>>>,>>9.99').
      WHEN 3 THEN /* Varekjøp */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.KjopAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi) + DEC(Lager.KjopVerdi),'->>,>>>,>>9.99').
      WHEN 4 THEN /* Brekkasje */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.BrekkAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.BrekkVerdi),'->>,>>>,>>9.99').
      WHEN 5 THEN /* Internt forbruk */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.IntAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.IntVerdi),'->>,>>>,>>9.99').
      WHEN 6 THEN /* Reklamert */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.ReklAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.ReklVerdi),'->>,>>>,>>9.99').
      WHEN 7 THEN /* Gjennkjøp */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.GjenkjopAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.GjenkjopVerdi),'->>,>>>,>>9.99').
      WHEN 8 THEN /* LagerReklamasjon */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.ReklLAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.ReklLVerdi),'->>,>>>,>>9.99').
      WHEN 9 THEN /* OVerført */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.OvAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + (Lager.OvVerdi),'->>,>>>,>>9.99').
      WHEN 10 THEN /*  */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.JustAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.JustVerdi),'->>,>>>,>>9.99').
      WHEN 11 THEN /*  */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.SvinnAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.SvinnVerdi),'->>,>>>,>>9.99').
      WHEN 12 THEN /*  */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.NedAnt,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.NedVerdi),'->>,>>>,>>9.99').
      WHEN 13 THEN /*  */
        ASSIGN
          tmpLager.DivAntall  = STRING(DEC(tmpLager.DivAntall) + Lager.LagAnt,'->>,>>>,>>9')
          tmpLager.SumAntall  = STRING(DEC(tmpLager.SumAntall) + Lager.AntRab,'->>,>>>,>>9')
          tmpLager.SumVerdi   = STRING(DEC(tmpLager.SumVerdi)  + DEC(Lager.VerdiRab),'->>,>>>,>>9.99').
    END CASE.

    ASSIGN
      tmpLager.DivAntall  = TRIM(tmpLager.DivAntall)
      tmpLager.SumAntall  = TRIM(tmpLager.SumAntall)
      tmpLager.SumVerdi   = TRIM(tmpLager.SumVerdi)      
      tmpLager.DivAntall  = IF DEC(tmpLager.DivAntall) = 0 THEN "" ELSE FILL(" ",10 - length(tmpLager.DivAntall)) + tmpLager.DivAntall
      tmpLager.SumAntall  = IF DEC(tmpLager.SumAntall) = 0 THEN "" ELSE FILL(" ",10 - length(tmpLager.SumAntall)) + tmpLager.SumAntall
      tmpLager.SumVerdi   = IF DEC(tmpLager.SumVerdi)  = 0 THEN "" ELSE FILL(" ",10 - length(tmpLager.SumVerdi))  + tmpLager.SumVerdi.
      
    FOR EACH ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
      ArtLag.Butik = Lager.Butik 
      BREAK
      BY ArtLag.Butik
      BY ArtLAg.ArtikkelNr
      /* TN 28/1-08
      by A r t l a g. V g
      by A r t L a g.L o p N r
      */
      BY Artlag.Storl:

      /* Legger alle ukjente størrelser inn i listen.       */
      /* Dette er størrelser som ikke er definert i StrTStr */
      /*
      IF NUM-ENTRIES(wStrListe,";") >= 99 
      OR (LOOKUP("(" + ArtLag.Storl + ")",wStrListe,";") <> 0 
      OR LOOKUP(Artlag.Storl,wStrListe,";") <> 0) THEN. /* Gjør ingenting */
      ELSE
        ASSIGN 
        wStrListe = wStrListe  + (IF wStrListe = "" THEN "" ELSE ";") + "(" + ArtLag.Storl + ")"
       cBrukteStr = cBrukteStr + (IF wStrListe = "" THEN "" ELSE ",")
                    .
      */
      ASSIGN
        wEntry = IF LOOKUP(ArtLag.Storl,wStrListe,";") <> 0
                   THEN LOOKUP(ArtLag.Storl,wStrListe,";") 
                 ELSE IF LOOKUP("(" + ArtLag.Storl + ")",wStrListe,";") <> 0
                   THEN LOOKUP("(" + ArtLag.Storl + ")",wStrListe,";")
                 ELSE NUM-ENTRIES(wStrListe,";") + 1
        wEntry = IF wEntry > 99 
                   THEN 99
                   ELSE wEntry.
                   
      CASE wVisModus:
        WHEN  1 THEN wCaseAnt = ArtLag.LagAnt.
        WHEN  2 THEN wCaseAnt = ArtLag.AntSolgt.
        WHEN  3 THEN wCaseAnt = ArtLag.KjopAnt.
        WHEN  4 THEN wCaseAnt = ArtLag.BrekkAnt.
        WHEN  5 THEN wCaseAnt = ArtLag.IntAnt.
        WHEN  6 THEN wCaseAnt = ArtLag.ReklAnt.
        WHEN  7 THEN wCaseAnt = ArtLag.gjenkjopAnt.
        WHEN  8 THEN wCaseAnt = ArtLag.ReklLAnt.
        WHEN  9 THEN wCaseAnt = ArtLag.OvAnt.
        WHEN 10 THEN wCaseAnt = ArtLag.JustAnt.
        WHEN 11 THEN wCaseAnt = ArtLag.SvinnAnt.
        WHEN 12 THEN wCaseAnt = ArtLag.NedAnt.
        WHEN 13 THEN wCaseAnt = ArtLag.AntRab.
      END CASE.
                   
      ASSIGN            
        tmpLager.Antall[wEntry] = STRING(DEC(tmpLager.Antall[wEntry]) + wCaseAnt)
        tmpLager.Antall[wEntry] = IF DEC(tmpLager.Antall[wEntry]) = 0
                                    THEN ""
/*             ELSE string(tmpLager.Antall[wEntry]). */
                                    ELSE FILL(" ",6 - length(STRING(tmpLager.Antall[wEntry]))) + string(tmpLager.Antall[wEntry]).
      ASSIGN ENTRY(wEntry,cBrukteStr) = "J".

    END.  
  END.
END. /* BUTIKK */

/* Sumpost */
ASSIGN
  wSumAntall    = 0
  wSumVerdi     = 0
  wDivAntall    = 0.

/* Summerer opp antall på hver enkelt størrelse. */
FOR EACH tmpLager NO-LOCK:

  /* Totalsummer */
  ASSIGN
    wSumAntall   = wSumAntall + DEC(tmpLager.SumAntall)
    wSumVerdi    = wSumVerdi  + DEC(tmpLager.SumVerdi)
    wDivAntall   = wDivAntall + DEC(tmpLager.DivAntall). 
    
  /* Sum pr. størrelse */
  DO wLoop = 1 TO 99:
    ASSIGN
      wAntall[wLoop] = STRING(
                         DEC(wAntall[wLoop]) + DEC(tmpLager.Antall[wLoop])
                             )
      wAntall[wLoop] = IF DEC(wAntall[wLoop]) = 0
                         THEN ""
                         ELSE wAntall[wLoop]
      wAntall[wLoop] = STRING(DEC(wAntall[wLoop])).
  END.     
END.
/* Beregner Vektet varekost i snitt. */
ASSIGN
    wVVk = wSumLagVerdi / wSumLagAntall
    wVVK = IF wVVk = ? THEN 0 ELSE wVVK
    .

/* Oppretter sumpost. */
FIND tmpLager WHERE
    tmpLager.ArtikkelNr = ArtBas.ArtikkelNr AND
    tmpLager.Butik      = "Total" NO-ERROR.
IF NOT AVAILABLE tmpLager THEN
  DO:
    CREATE tmpLager.
    ASSIGN 
      tmpLager.ArtikkelNr = ArtBas.ArtikkelNr
      tmpLager.Butik      = "Total"
      tmpLager.VVareKost  = wVVK
      .
  END.

/* Justerer totalsummene. */
/*   ASSIGN                                                                                         */
/*     tmpLager.SumAntall = IF wSumAntall = 0 THEN "" ELSE TRIM(STRING(wSumAntall,"->>,>>>,>>9"))   */
/*     tmpLager.SumVerdi  = IF wSumVerdi  = 0 THEN "" ELSE TRIM(STRING(wSumVerdi,"->>,>>>,>>9.99")) */
/*     tmpLager.DivAntall = IF wDivAntall = 0 THEN "" ELSE TRIM(STRING(wDivAntall,"->>,>>>,>>9"))   */
/*     .                                                                                            */
ASSIGN
  tmpLager.SumAntall = IF wSumAntall = 0 THEN "" ELSE FILL(" ",10 - length(TRIM(STRING(wSumAntall,"->>,>>>,>>9")))) + TRIM(STRING(wSumAntall,"->>,>>>,>>9"))
  tmpLager.SumVerdi  = IF wSumVerdi  = 0 THEN "" ELSE FILL(" ",10 - length(TRIM(STRING(wSumVerdi,"->>,>>>,>>9.99"))))   + TRIM(STRING(wSumVerdi,"->>,>>>,>>9.99"))
  tmpLager.DivAntall = IF wDivAntall = 0 THEN "" ELSE FILL(" ",10 - length(TRIM(STRING(wDivAntall,"->>,>>>,>>9")))) + TRIM(STRING(wDivAntall,"->>,>>>,>>9"))
  .
  
/* Legger på plass antall pr. størrelse */
DO wLoop = 1 TO 99:
  ASSIGN
    tmpLager.Antall[wLoop] = STRING(
                                    DEC(tmpLager.Antall[wLoop]) + 
                                    DEC(wAntall[wLoop])
                                   )
    tmpLager.Antall[wLoop] = IF DEC(tmpLager.Antall[wLoop]) = 0
                               THEN ""
/*         ELSE string(tmpLager.Antall[wLoop]). */
                               ELSE FILL(" ",6 - length(STRING(tmpLager.Antall[wLoop]))) + string(tmpLager.Antall[wLoop]).
END.     

IF NUM-ENTRIES(wStrListe,";") <> 0 THEN
  DO:
    IF NUM-ENTRIES(wStrListe,";") = 99 THEN DO:
        ASSIGN ENTRY(99,wStrListe,";") = " Rest".
        ASSIGN ENTRY(99,wStrListe2,";") = " Rest" NO-ERROR.
    END.
    ELSE
      ASSIGN wStrListe = wStrListe + ";Rest"
             wStrListe2 = wStrListe2 + ";Rest".
  END.
IF NOT CAN-DO(cBrukteStr,"J") THEN DO:
    cBrukteStr = FILL("J,",NUM-ENTRIES(wStrListe,";") - 1) + "J".
END.
IF NUM-ENTRIES(wStrListe,";") <> 99 THEN DO:
    cBrukteTmp = FILL(",",NUM-ENTRIES(wStrListe,";") - 1).
    DO wLoop = 1 TO NUM-ENTRIES(cBrukteTmp):
        ENTRY(wLoop,cBrukteTmp) = ENTRY(wLoop,cBrukteStr).
    END.
    cBrukteStr = cBrukteTmp.
    wStrListe = wStrListe2.
END.

{swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

