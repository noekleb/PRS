 /* Registrer Eksporter sieeksport record
   Parameter:  
   Opprettet: 5.4.2011           
-----------------------------------------------------------------------------------*/
CURRENT-WINDOW:WIDTH = 203.

DEF INPUT  PARAM icParam       AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer      AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK          AS LOG NO-UNDO.

DEFINE VARIABLE cFilNavn       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilSie        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExtent        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTab           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFnutt         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLbracket      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRbracket      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlank         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSIEBel        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHVgr          AS CHARACTER NO-UNDO.
DEFINE VARIABLE dBelop         AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dDiffBel       AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 NO-UNDO.
DEFINE VARIABLE cDiffBel       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFnamn         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPgmNamn       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirsta        AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE dSalgsdate LIKE SIEEksport.SalgsDato.
DEFINE VARIABLE iDebKred       AS INTEGER NO-UNDO.
DEFINE VARIABLE piSIEdiffKonto AS INTEGER NO-UNDO.
DEFINE VARIABLE piVgrSie       AS INTEGER NO-UNDO.
DEFINE VARIABLE lVgrSie        AS LOGICAL NO-UNDO.
DEFINE VARIABLE iButikkNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntLas        AS INTEGER NO-UNDO.
DEFINE VARIABLE iKontoNr       AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr         AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntal         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKortBel       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cButKonton     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lVissKst       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iKstButik      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cVerSerie      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dKontant1      AS DECIMAL NO-UNDO.
DEFINE VARIABLE dKontant2      AS DECIMAL NO-UNDO.

DEFINE VARIABLE cRegAv         AS CHARACTER NO-UNDO FORMAT "X(30)".
DEFINE VARIABLE ilRegAv        AS INTEGER NO-UNDO.
DEFINE VARIABLE piSIEStdKonto  AS INTEGER NO-UNDO.
DEFINE VARIABLE lTransTyp150   AS LOGICAL NO-UNDO.
DEFINE VARIABLE piTestTyp150   AS INTEGER NO-UNDO.
DEFINE VARIABLE lTestTyp150    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lTransTyp92    AS LOGICAL NO-UNDO.
DEFINE VARIABLE piTestTyp92    AS INTEGER NO-UNDO.
DEFINE VARIABLE lTestTyp92     AS LOGICAL NO-UNDO.
DEFINE VARIABLE iKonto3020     AS INTEGER     NO-UNDO.
DEFINE VARIABLE lKonto3020     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cWebButik      AS CHARACTER   NO-UNDO.   /* ghg 20130423*/
DEFINE VARIABLE cSIEOrgnr      AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE bTGTimeStamp LIKE TGTimeStamp.

DEFINE TEMP-TABLE ttKonto NO-UNDO
    FIELD tKontoNr AS INTEGER
    FIELD tKasseNr AS INTEGER
    FIELD tBelopp  AS DECIMAL
    INDEX ii tKontoNr tKasseNr.

/*DEFINE STREAM Ut.*/  
DEFINE STREAM UtSie.
/*DEFINE STREAM Utlogg.*/

DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN cLbracket = CHR(123)
       cRbracket = CHR(125)
       cFnutt = CHR(34)
       cTab = CHR(9).
/* Katalog där SIE-filerna skall skapas. */
{syspara.i 50 20 7 cKatalog}
IF cKatalog = '' THEN
  cKatalog = 'c:\home\lindbak\sendes'.
ELSE
  cKatalog = RIGHT-TRIM(cKatalog,'\').
                       
{syspara.i 1 1 100 cFnamn}
  cFnamn = RIGHT-TRIM(cFnamn).
  
{syspara.i 1 1 102 cPgmNamn}
  cPgmNamn = RIGHT-TRIM(cPgmNamn).

/* Setter SIE standardkonto */
{syspara.i 50 20 2 piSIEStdKonto INT}

IF piSIEStdKonto = 0 THEN piSIEStdKonto = 9999.

/* Sätt SIE-differans konto */
{syspara.i 50 20 3 piSIEdiffKonto INT}

IF piSIEdiffKonto = 0 THEN piSIEdiffKonto = 6993.

/* Kolla om per varugrupp eller ej */
{syspara.i 50 20 4 piVgrSie INT}

 IF piVgrSie = 0 THEN 
   ASSIGN lVgrSie = TRUE.
 ELSE
   ASSIGN lVgrSie = FALSE.

/* Kolla om 3020-konto ska skapas */
{syspara.i 50 20 8 iKonto3020 INT}

 IF iKonto3020 = 0 THEN 
   ASSIGN lKonto3020 = FALSE.
 ELSE
   ASSIGN lKonto3020 = TRUE.

/* Kolla om test mot kassarapport ska ske */
{syspara.i 50 20 5 piTestTyp150 INT}
 IF piTestTyp150 = 0 THEN
   lTestTyp150 = FALSE.
 ELSE
   lTestTyp150 = TRUE.

{syspara.i 50 20 11 cWebButik}        /* ghg 20130423*/

/* Kolla om test mot EOD ska ske */
{syspara.i 50 20 6 piTestTyp92 INT}
 IF piTestTyp92 = 0 THEN
   lTestTyp92 = FALSE.
 ELSE
   lTestTyp92 = TRUE.

/* Koll om bara vissa konton ska ha kst */
   {syspara.i 50 20 9 cButKonton}
     ASSIGN cButKonton = TRIM(cButKonton).
     IF cButKonton = "" THEN
       ASSIGN lVissKst = FALSE.
     ELSE
       ASSIGN lVissKst = TRUE.

   {syspara.i 50 20 13 cVerSerie}

/*   OUTPUT STREAM Utlogg TO VALUE(cKatalog + '\' + "ghlogg.xls") NO-ECHO.*/
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.
    
    
  DO TRANSACTION:
    FIND FIRST SIEEksport WHERE SIEEksport.SIEEksportNr = DEC(ihBuffer:BUFFER-FIELD('SIEEksportNr'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL SIEEksport THEN
    DO:
      FIND butiker WHERE butiker.butik = SIEEksport.ButikkNr NO-LOCK NO-ERROR.
      IF AVAIL butiker THEN DO:
          cSIEOrgnr = TRIM(butiker.organisasjonsnr).
          IF NUM-ENTRIES(cSIEOrgnr,"-") <> 2 OR LENGTH(cSIEOrgnr) <> 11 THEN
              cSIEOrgnr = "".
      END.
      ELSE
          cSIEOrgnr = "".
      IF lFirsta = TRUE THEN
      DO:
        ASSIGN
          cFilSie = 'SIE' + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
                          + STRING(MONTH(SIEEksport.SalgsDato),'99')  
                          + STRING(DAY(SIEEksport.SalgsDato),'99')
                          + "_" + STRING(SIEEksport.ButikkNr) + '.SI'.

/*        ASSIGN cRegAv = SIEEksport.RegistrertAv.
        ilRegAv = LENGTH(cRegAV).
        MESSAGE "ilRegAv-1 " STRING(ilRegAv) SKIP VIEW-AS ALERT-BOX.

        cRegAv = RIGHT-TRIM(cRegAv).
        ilRegAv = LENGTH(cRegAV).
        MESSAGE "ilRegAv-2 " STRING(ilRegAv) SKIP VIEW-AS ALERT-BOX.*/

        OUTPUT STREAM UtSie TO VALUE(cKatalog + '\' + cFilSie) NO-ECHO.
          PUT STREAM UtSie UNFORMATTED
          "#FLAGGA 0"  
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#PROGRAM " + STRING(cFnutt) + cPgmNamn + STRING(cFnutt)
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#FORMAT PC8"
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#GEN " + STRING(YEAR(TODAY),'9999')
           + STRING(MONTH(TODAY),'99')
           + STRING(DAY(TODAY),'99')
           + " "
           + SIEEksport.RegistrertAv
           SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#SIETYP 4"
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#FNAMN " + STRING(cFnutt) + cFnamn + STRING(cFnutt)
          SKIP.
          IF cSIEOrgnr <> "" THEN
              PUT STREAM UtSie UNFORMATTED
                "#ORGNR " + cSIEOrgnr 
              SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#DIM 1 " + STRING(cFnutt) + "Kostnadsställe" + STRING(cFnutt)
          SKIP.
          IF lVgrSie = TRUE THEN
            PUT STREAM UtSie UNFORMATTED
            "#DIM 20 " + STRING(cFnutt) + "Varugrupp" + STRING(cFnutt)
            SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#KPTYP EUBAS97"
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#VER " + STRING(cFnutt) + cVerSerie + STRING(cFnutt) + " " + STRING(cFnutt)
          + STRING(cFnutt) + " "
          + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
          + STRING(MONTH(SIEEksport.SalgsDato),'99')  
          + STRING(DAY(SIEEksport.SalgsDato),'99')
          + " "
          + STRING(cFnutt)
          + STRING(SIEEksport.ButikkNr)
          + "-"
          + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
          + STRING(MONTH(SIEEksport.SalgsDato),'99')  
          + STRING(DAY(SIEEksport.SalgsDato),'99')
          + STRING(cFnutt)
          SKIP.
        ASSIGN lFirsta = FALSE.
        ASSIGN dSalgsdate = SIEEksport.SalgsDato.
        ASSIGN iButikkNr = SIEEksport.ButikkNr.
        ASSIGN iKstButik = iButikkNr.
        IF iButikkNr = 10 THEN
          ASSIGN iKstButik = 7.
        ELSE IF  iButikkNr = 12 THEN
          ASSIGN iKstButik = 2.
        ASSIGN iAntLas = 0.
      END.    
      ELSE IF dSalgsdate <> SIEEksport.SalgsDato OR
              iButikkNr <> SIEEksport.ButikkNr THEN
      DO:
        OUTPUT STREAM UtSie CLOSE.

        IF iAntLas = 0 THEN
          OS-DELETE VALUE(cKatalog + '\' + cFilSie).

        ASSIGN
          cFilSie = 'SIE' + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
                          + STRING(MONTH(SIEEksport.SalgsDato),'99')  
                          + STRING(DAY(SIEEksport.SalgsDato),'99')
                          + "_" + STRING(SIEEksport.ButikkNr) + '.SI'.

        OUTPUT STREAM UtSie TO VALUE(cKatalog + '\' + cFilSie) NO-ECHO.
          PUT STREAM UtSie UNFORMATTED
          "#FLAGGA 0"  
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#PROGRAM " + STRING(cFnutt) + cPgmNamn + STRING(cFnutt)
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#FORMAT PC8"
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#GEN " + STRING(YEAR(TODAY),'9999')
           + STRING(MONTH(TODAY),'99')
           + STRING(DAY(TODAY),'99')
           + " "
           + SIEEksport.RegistrertAv
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#SIETYP 4"
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#FNAMN " + STRING(cFnutt) + cFnamn + STRING(cFnutt)
          SKIP.
          IF cSIEOrgnr <> "" THEN
              PUT STREAM UtSie UNFORMATTED
                "#ORGNR " + cSIEOrgnr 
              SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#DIM 1 " + STRING(cFnutt) + "Kostnadsställe" + STRING(cFnutt)
          SKIP.
          IF lVgrSie = TRUE THEN
            PUT STREAM UtSie UNFORMATTED
            "#DIM 20 " + STRING(cFnutt) + "Varugrupp" + STRING(cFnutt)
            SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#KPTYP EUBAS97"
          SKIP.
          PUT STREAM UtSie UNFORMATTED
          "#VER " + STRING(cFnutt) + cVerSerie + STRING(cFnutt) + " " + STRING(cFnutt)
          + STRING(cFnutt) + " "
          + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
          + STRING(MONTH(SIEEksport.SalgsDato),'99')  
          + STRING(DAY(SIEEksport.SalgsDato),'99')
          + " "
          + STRING(cFnutt)
          + STRING(SIEEksport.ButikkNr)
          + "-"
          + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
          + STRING(MONTH(SIEEksport.SalgsDato),'99')  
          + STRING(DAY(SIEEksport.SalgsDato),'99')
          + STRING(cFnutt)
          SKIP.
        ASSIGN lFirsta = FALSE.
        ASSIGN dSalgsdate = SIEEksport.SalgsDato.
        ASSIGN iButikkNr = SIEEksport.ButikkNr.
        ASSIGN iKstButik = iButikkNr.
        IF iButikkNr = 10 THEN
          ASSIGN iKstButik = 7.
        ELSE IF  iButikkNr = 12 THEN
          ASSIGN iKstButik = 2.
        ASSIGN iAntLas = 0.
      END.    
      
      IF lVgrSie = TRUE THEN
        RUN eksportSIETrans.
      ELSE
        RUN eksportSIETrans2.

      ASSIGN
        SIEEksport.EkspDato = TODAY 
        SIEEksport.EkspTid = TIME 
        SIEEksport.Notat       = SIEEksport.Notat 
                                + (IF SIEEksport.Notat <> '' THEN CHR(10) ELSE '')
                                + 'Eksportert ' + STRING(TODAY)
                                + ' ' + STRING(TIME,"HH:MM:SS")
                                + ' ' + USERID('SkoTex').

/*      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.*/
    END.
  END. /* TRANSACTION */
  IF AVAIL SIEEksport THEN RELEASE SIEEksport.
  hQuery:GET-NEXT().
END.



/* **********************  Internal Procedures  *********************** */

PROCEDURE eksportSIETrans:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
ASSIGN
  cExtent  = STRING(SIEEksport.ButikkNr)
  cFilNavn = '_SIETrans' + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
                         + STRING(MONTH(SIEEksport.SalgsDato),'99')  
                         + STRING(DAY(SIEEksport.SalgsDato),'99')
                         + '_' 
                         + REPLACE(STRING(SIEEksport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.
  
  PUT STREAM UtSie UNFORMATTED
  STRING(cLbracket)
  SKIP.

/*OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.
  PUT STREAM Ut UNFORMATTED
    "TransId" ';'
    "KontoNr" ';'     
    "TTId" ';'  
    "ButikkNr" ';'  
    "KasseNr" ';'    
    "Dato" ';'
    "AvdHgVg" ';'         
    "Belop" ';'
    "SIEEkspNr" ';'
    "EDato" ';'
    "ETid" ';'
    "BrukerID" ';'
    "RegDato" ';'
    "RegTid" ';'
    "RegAv"
    SKIP. */

  ASSIGN dDiffBel = 0.  
  ASSIGN dKontant1 = 0.
  ASSIGN dKontant2 = 0.
    
SIE_LOOP:
FOR EACH SIETrans OF SIEEksport NO-LOCK:
/*  PUT STREAM Ut UNFORMATTED
    SIETrans.SIETransId ';'
    SIETrans.KontoNr ';'     
    SIETrans.TTId ';'  
    SIETrans.ButikkNr ';'  
    SIETrans.KasseNr ';'    
    SIETrans.Dato ';'
    SIETrans.AvdHgVg ';'         
    SIETrans.Belop ';'
    SIETrans.SIEEksportNr ';'
    SIETrans.EDato ';'
    SIETrans.ETid ';'
    SIETrans.BrukerID ';'
    SIETrans.RegistrertDato ';'
    SIETrans.RegistrertTid ';'
    SIETrans.RegistrertAv
    SKIP. */
    
    IF SIETrans.TTId = 50 OR
       SIETrans.TTId = 67 OR
       SIETrans.TTId = 70
/*          OR SIETrans.TTId = 78 */
        THEN
      ASSIGN dKontant1 = dKontant1 + SIETrans.Belop.
    ELSE IF SIETrans.TTId = 150 THEN
      ASSIGN dKontant2 = dKontant2 + SIETrans.Belop.

  IF SIETrans.KontoNr >= 4000 THEN
    NEXT SIE_LOOP.
    
    ASSIGN dBelop = SIETrans.Belop.

    FIND FIRST KontoTabell NO-LOCK WHERE
      SIETrans.KontoNr = KontoTabell.KontoNr NO-ERROR.
    IF NOT AVAILABLE KontoTabell THEN
      ASSIGN iDebKred = 1.
    ELSE IF KontoTabell.DebKred = 0 THEN
      ASSIGN iDebKred = 1.
    ELSE
      ASSIGN iDebKred = KontoTabell.DebKred.

    IF iDebKred = 2 THEN
      DO:
/*         MESSAGE "Kredit" STRING(SIETrans.KontoNr) STRING(iDebKred) SKIP VIEW-AS ALERT-BOX.*/
         dBelop = (-1 * dBelop).
      END.
    
    IF SIETrans.KontoNr < 4000 THEN
      dDiffBel = dDiffBel + dBelop.     
    
    IF dBelop = 0 THEN
      NEXT SIE_LOOP.

    ASSIGN cSIEBel = TRIM(REPLACE(STRING(dBelop,"->>>>>>9.99"),",","."))
           cSIEBel = FILL(" ",11 - LENGTH(cSIEBel)) + cSIEBel.
           
    IF SIETrans.AvdHgVg > 0 THEN
      DO:
        ASSIGN cHVgr = TRIM(STRING(SIETrans.AvdHgVg,">>>9"))
               cHVgr = FILL(" ",4 - LENGTH(cHVgr)) + cHVgr.
    
        PUT STREAM UtSie UNFORMATTED
          "    #TRANS "
          + STRING(SIETrans.KontoNr)
          + " "
          + cLbracket
          + cFnutt + "1" + cFnutt + " "
          + cFnutt
          + STRING(SIETrans.ButikkNr)
          + cFnutt + " "
          + cFnutt + "20" + cFnutt + " "
          + cFnutt
          + cHVgr
          + cFnutt
          + cRbracket
          + cSIEBel
          SKIP.
        iAntLas = iAntLas + 1.
      END.  
    ELSE    
      PUT STREAM UtSie UNFORMATTED
        "    #TRANS "
        + STRING(SIETrans.KontoNr)
        + " "
        + cLbracket
        + cFnutt + "1" + cFnutt + " "
        + cFnutt
        + STRING(SIETrans.ButikkNr)
        + cFnutt 
        + cRbracket
        + "            "
        + cSIEBel
        SKIP.
      iAntLas = iAntLas + 1.
END.
  IF dDiffBel <> 0 THEN
    DO:
      ASSIGN cSIEBel = TRIM(REPLACE(STRING(dDiffBel,"->>>>>>9.99"),",","."))
             cSIEBel = FILL(" ",11 - LENGTH(cSIEBel)) + cSIEBel.
    
      ASSIGN iKontoNr = piSIEdiffKonto.
          
      PUT STREAM UtSie UNFORMATTED
        "    #TRANS "
        + STRING(iKontoNr)
        + " "
        + cLbracket
        + cFnutt + "1" + cFnutt + " "
        + cFnutt
        + STRING(SIEEksport.ButikkNr)
        + cFnutt
        + cRbracket
        + "            "
        + cSIEBel
        SKIP.
      iAntLas = iAntLas + 1.
    END.    
  IF NOT CAN-DO(cWebButik,STRING(iButikkNr)) THEN
    IF lTestTyp150 = TRUE THEN
    DO:
      IF dKontant1 <> dkontant2 THEN
        DO:
          ASSIGN dDiffBel = dKontant1 - dKontant2.
          ASSIGN cSIEBel = TRIM(REPLACE(STRING(dDiffBel,"->>>>>>9.99"),",","."))
                 cSIEBel = FILL(" ",11 - LENGTH(cSIEBel)) + cSIEBel.

          ASSIGN iKontoNr = piSIEdiffKonto.

          PUT STREAM UtSie UNFORMATTED
            "    #TRANS "
            + STRING(iKontoNr)
            + " "
            + cLbracket
            + cFnutt + "1" + cFnutt + " "
            + cFnutt
            + STRING(SIEEksport.ButikkNr)
            + cFnutt
            + cRbracket
            + "            "
            + cSIEBel
            SKIP.
          iAntLas = iAntLas + 1.
          ASSIGN dDiffBel = -1 * dDiffBel.
          ASSIGN cSIEBel = TRIM(REPLACE(STRING(dDiffBel,"->>>>>>9.99"),",","."))
                 cSIEBel = FILL(" ",11 - LENGTH(cSIEBel)) + cSIEBel.

/* Hämta kontant-konto för denna butik */
          RUN HentKontoNr (SIEEksport.ButikkNr, 50, OUTPUT iKontoNr).  

          PUT STREAM UtSie UNFORMATTED
            "    #TRANS "
            + STRING(iKontoNr)
            + " "
            + cLbracket
            + cFnutt + "1" + cFnutt + " "
            + cFnutt
            + STRING(SIEEksport.ButikkNr)
            + cFnutt
            + cRbracket
            + "            "
            + cSIEBel
            SKIP.
          iAntLas = iAntLas + 1.
      END. /* dKontant1 <> dkontant2 */
    END. /* lTestTyp150 = TRUE */
    
  PUT STREAM UtSie UNFORMATTED
  STRING(cRbracket)
  SKIP.

/*  OUTPUT STREAM Ut CLOSE.  
  IF iAntLas = 0 THEN
    OS-DELETE VALUE(cKatalog + '\' + cFilNavn).*/

/*OUTPUT STREAM UtSie CLOSE.
          OS-DELETE VALUE(cFilSie).*/
/*        IF iAntLas = 0 THEN
          OS-DELETE VALUE(cKatalog + '\' + cFilSie).*/

END PROCEDURE.

PROCEDURE eksportSIETrans2:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE lFirsta2 AS LOGICAL INITIAL TRUE NO-UNDO.

ASSIGN
  cExtent  = STRING(SIEEksport.ButikkNr)
  cFilNavn = '_SIETrans' + STRING(YEAR(SIEEksport.SalgsDato),'9999') 
                         + STRING(MONTH(SIEEksport.SalgsDato),'99')  
                         + STRING(DAY(SIEEksport.SalgsDato),'99')
                         + '_' 
                         + REPLACE(STRING(SIEEksport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.
  PUT STREAM UtSie UNFORMATTED
  STRING(cLbracket)
  SKIP.

  ASSIGN dBelop = 0.
  ASSIGN dDiffBel = 0.  
  ASSIGN dKontant1 = 0.
  ASSIGN dKontant2 = 0.

SIE_LOOP:
FOR EACH SIETrans OF SIEEksport NO-LOCK:
  IF lFirsta2 = TRUE THEN
    DO:
/*      MESSAGE "Firsta: " SIETrans.KontoNr SIETrans.Belop
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      ASSIGN lFirsta2 = FALSE.
      CREATE ttKonto.
      ASSIGN ttKonto.tKontoNr = SIETrans.KontoNr
             ttKonto.tKasseNr = SIETrans.KasseNr
             ttKonto.tBelopp = SIETrans.Belop.
      ASSIGN dBelop  = SIETrans.Belop
             iKontoNr = SIETrans.KontoNr
            iButNr = SIETrans.ButikkNr
            iKstButik = SIETrans.ButikkNr.
        IF iButNr = 10 THEN
            ASSIGN iKstButik = 7.
        ELSE IF iButNr = 12 THEN
            ASSIGN iKstButik = 2.
        IF SIETrans.TTId = 50 OR
           SIETrans.TTId = 67 OR
           SIETrans.TTId = 70 
/*             OR SIETrans.TTId = 78 */
             THEN
          ASSIGN dKontant1 = dKontant1 + SIETrans.Belop.
        ELSE IF SIETrans.TTId = 150 THEN
          ASSIGN dKontant2 = dKontant2 + SIETrans.Belop.
        NEXT SIE_LOOP.       
    END.
/*  MESSAGE "Next: " SIETrans.KontoNr SIETrans.Belop
  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  FIND ttKonto WHERE ttKonto.tKontoNr = SIETrans.KontoNr AND
                     ttKonto.tKasseNr = SIETrans.KasseNr NO-ERROR .
/*  IF SIETrans.KontoNr = iKontoNr AND
     SIETrans.ButikkNr = iButNr THEN  */
  IF AVAILABLE ttKonto THEN
    DO:
      ttKonto.tBelopp = ttKonto.tBelopp + SIETrans.Belop.
      dBelop = dBelop + SIETrans.Belop.
        IF SIETrans.TTId = 50 OR
           SIETrans.TTId = 67 OR
           SIETrans.TTId = 70 
/*             OR SIETrans.TTId = 78 */
            THEN
          ASSIGN dKontant1 = dKontant1 + SIETrans.Belop.
        ELSE IF SIETrans.TTId = 150 THEN
          ASSIGN dKontant2 = dKontant2 + SIETrans.Belop.
        NEXT SIE_LOOP.
    END.
  ELSE 
/*    IF SIETrans.KontoNr >= 4000 THEN
/*    IF iKontoNr >= 4000 THEN*/
       NEXT SIE_LOOP.*/
      
/*  FIND FIRST KontoTabell NO-LOCK WHERE
    iKontoNr = KontoTabell.KontoNr NO-ERROR.
  IF NOT AVAILABLE KontoTabell THEN
    ASSIGN iDebKred = 1.
  ELSE IF KontoTabell.DebKred = 0 THEN
    ASSIGN iDebKred = 1.
  ELSE
    ASSIGN iDebKred = KontoTabell.DebKred.

  IF iDebKred = 2 THEN
    DO:
/*         MESSAGE "Kredit" STRING(SIETrans.KontoNr) STRING(iDebKred) SKIP VIEW-AS ALERT-BOX.*/
         dBelop = (-1 * dBelop).
    END.*/

/*    IF dBelop = 0 THEN*/
    DO:
      ASSIGN dBelop  = SIETrans.Belop
           iKontoNr = SIETrans.KontoNr.  
      CREATE ttKonto.
      ASSIGN ttKonto.tKontoNr = SIETrans.KontoNr
             ttKonto.tKasseNr = SIETrans.KasseNr
             ttKonto.tBelopp = SIETrans.Belop.
      IF SIETrans.TTId = 50 OR
         SIETrans.TTId = 67 OR
         SIETrans.TTId = 70 
/*           OR SIETrans.TTId = 78 */
          THEN
        ASSIGN dKontant1 = dKontant1 + SIETrans.Belop.
      ELSE IF SIETrans.TTId = 150 THEN
        ASSIGN dKontant2 = dKontant2 + SIETrans.Belop.
      NEXT SIE_LOOP.     
    END.
         
END. /* SIE_LOOP */

/*MESSAGE "dKontant1: " dKontant1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
MESSAGE "dKontant2: " dKontant2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

  RUN DiffKontroll.

/* Kolla om kortbetalning finns. I så fall 3020. */
  IF lKonto3020 = TRUE THEN
    RUN KortKontroll.

  ASSIGN iAntLas = 0.

  FOR EACH ttKonto NO-LOCK WHERE ttKonto.tKontoNr <> 9999 BY ttKonto.tKontoNr:
      iAntLas = iAntLas + 1.
/*MESSAGE "ttKonto: " ttKonto.tKontoNr ttKonto.tBelopp
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      ASSIGN cSIEBel = TRIM(REPLACE(STRING(ttKonto.tBelopp,"->>>>>>9.99"),",","."))
       cSIEBel = FILL(" ",11 - LENGTH(cSIEBel)) + cSIEBel.

      IF lVissKst = TRUE THEN
      DO:
        IF iKstButik > 9 THEN
          ASSIGN cBlank = "00".
        ELSE 
          ASSIGN cBlank = "0".
/*        IF CAN-DO(cButKonton,STRING(ttKonto.tKontoNr)) THEN 2014-03-04 ghg */
/*      IF ttKonto.tKontoNr = 3010 OR ttKonto.tKontoNr = 6993 THEN*/
          PUT STREAM UtSie UNFORMATTED
            "    #TRANS "
            + STRING(ttKonto.tKontoNr)
            + " "
            + cLbracket
            + cFnutt + "1" + cFnutt + " "
            + cFnutt
            + STRING(iKstButik)
            + cFnutt
            + cRbracket
            + cSIEBel
            SKIP.
/*        ELSE
            PUT STREAM UtSie UNFORMATTED
              "    #TRANS "
              + STRING(ttKonto.tKontoNr)
              + " "
              + cLbracket
              + cFnutt + "1" + cFnutt + " "
              + cFnutt
              + cBlank
              + cFnutt
              + cRbracket
              + cSIEBel
              SKIP. 2014-03-04 ghg */
      END.
      ELSE
        PUT STREAM UtSie UNFORMATTED
          "    #TRANS "
          + STRING(ttKonto.tKontoNr)
          + " "
          + cLbracket
          + cFnutt + "1" + cFnutt + " "
          + cFnutt
          + STRING(iKstButik)
          + cFnutt
          + cRbracket
          + cSIEBel
          SKIP.
      iAntLas = iAntLas + 1.
    END.

  PUT STREAM UtSie UNFORMATTED
  STRING(cRbracket)
  SKIP.

  FOR EACH ttKonto:
    DELETE ttKonto.
  END.

/*OUTPUT STREAM Ut CLOSE.  
IF iAntLas = 0 THEN
  OS-DELETE VALUE(cKatalog + '\' + cFilNavn).*/

END PROCEDURE.

PROCEDURE DiffKontroll:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

  ASSIGN dDiffBel = 0.
  FOR EACH ttKonto:
    IF ttKonto.tBelopp <> 0  AND ttKonto.tKontoNr <> 9999 THEN
    DO:
      FIND FIRST KontoTabell NO-LOCK WHERE
      ttKonto.tKontoNr = KontoTabell.KontoNr NO-ERROR.
       IF NOT AVAILABLE KontoTabell THEN
         ASSIGN iDebKred = 1.
       ELSE IF KontoTabell.DebKred = 0 THEN
         ASSIGN iDebKred = 1.
       ELSE
         ASSIGN iDebKred = KontoTabell.DebKred.

       IF iDebKred = 2 THEN
         DO:
           ttKonto.tBelopp = (-1 * ttKonto.tBelopp).
         END.
    END.
  END.

  FOR EACH ttKonto WHERE ttKonto.tKontoNr <> 9999:
     dDiffBel = dDiffBel + ttKonto.tBelopp.
  END.


  IF dDiffBel <> 0 THEN
    DO:
      ASSIGN dDiffBel = (-1 * dDiffBel).
      FIND ttKonto WHERE ttKonto.tKontoNr = piSIEdiffKonto AND 
                         ttKonto.tKasseNr = 1 NO-ERROR.
      IF AVAILABLE ttKonto THEN
        ttKonto.tBelopp = ttKonto.tBelopp + dDiffBel.
      ELSE
        DO:
          CREATE ttKonto.
          ASSIGN ttKonto.tKontoNr = piSIEdiffKonto
                 ttKonto.tKasseNr = 1
                 ttKonto.tBelopp = dDiffBel.
        END.
    END.

IF NOT CAN-DO(cWebButik,STRING(iButikkNr)) THEN
  IF lTestTyp150 = TRUE THEN
    DO:
    IF dKontant1 <> dkontant2 THEN
      DO:
        ASSIGN dDiffBel = dKontant1 - dKontant2.
        FIND ttKonto WHERE ttKonto.tKontoNr = piSIEdiffKonto AND 
                           ttKonto.tKasseNr = 1 NO-ERROR.
        IF AVAILABLE ttKonto THEN
            ttKonto.tBelopp = ttKonto.tBelopp + dDiffBel.
          ELSE
            DO:
              CREATE ttKonto.
              ASSIGN ttKonto.tKontoNr = piSIEdiffKonto
                     ttKonto.tKasseNr = 1
                     ttKonto.tBelopp = dDiffBel.
            END.
            ASSIGN dDiffBel = -1 * dDiffBel.
            RUN HentKontoNr (iButNr, 50, 1, OUTPUT iKontoNr).  

            FIND ttKonto WHERE ttKonto.tKontoNr = iKontoNr AND 
                               ttKonto.tKasseNr = 1 NO-ERROR.
            IF AVAILABLE ttKonto THEN
                ttKonto.tBelopp = ttKonto.tBelopp + dDiffBel.
              ELSE
                DO:
                  CREATE ttKonto.
                  ASSIGN ttKonto.tKontoNr = iKontoNr
                         ttKonto.tKasseNr = 1
                         ttKonto.tBelopp = dDiffBel.
                END.

      END.
  END.

END PROCEDURE.

PROCEDURE KortKontroll:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
ASSIGN iKortBel = 0.

FOR EACH ttKonto:
    IF SUBSTRING(STRING(ttKonto.tKontoNr), 1, 3) = "158" THEN
      iKortBel = iKortBel + ttKonto.tBelopp.
END.

IF iKortBel <> 0 THEN
DO:
   iKortBel = (0.8 * iKortBel).
   FIND ttKonto WHERE ttKonto.tKontoNr = 3010 AND 
                      ttKonto.tKasseNr = 1 NO-ERROR.
   IF AVAILABLE ttKonto THEN
      ttKonto.tBelopp = ttKonto.tBelopp + iKortBel.
   ELSE
   DO:
     CREATE ttKonto.
     ASSIGN ttKonto.tKontoNr = 3010
            ttKonto.tKasseNr = 1
            ttKonto.tBelopp = (-1 * iKortBel).
   END.

   FIND ttKonto WHERE ttKonto.tKontoNr = 3020 AND 
                      ttKonto.tKasseNr = 1 NO-ERROR.
   IF AVAILABLE ttKonto THEN
      ttKonto.tBelopp = ttKonto.tBelopp - iKortBel.
   ELSE
   DO:
     CREATE ttKonto.
     ASSIGN ttKonto.tKontoNr = 3020
            ttKonto.tKasseNr = 1
            ttKonto.tBelopp = (-1 * iKortBel).
   END.
END.

END PROCEDURE.

PROCEDURE hentKontoNr:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  piButikkNr   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  piTTId       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  piTBId       AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER piSIEKontoNr AS INTEGER NO-UNDO.
  
  IF piTBId = 0 THEN
      piTBId = 1.

    /* Spesifikk kontoplan for butikk. */
    
    FIND SIETransType NO-LOCK WHERE
        SIETransType.ButikkNr = piButikkNr AND
        SIETransType.TTId     = piTTId AND 
        SIETransType.TBId = piTBId NO-ERROR.
        
    /* Standard kontoplan. */
    
    IF NOT AVAILABLE SIETransType THEN
        FIND SIETransType NO-LOCK WHERE
            SIETransType.ButikkNr = 0 AND
            SIETransType.TTId     = piTTId AND
            SIETransType.TBId = piTBId NO-ERROR.
    IF NOT AVAILABLE SIETransType THEN
        piSIEKontoNr = piSIEStdKonto.
    ELSE IF SIETranstype.KontoNr > 0 THEN 
        piSIEKontoNr = SIETransType.KontoNr.
    ELSE piSIEKontoNr = piSIEStdKonto.

/*    IF piSIEKontoNr = 9999 THEN
      MESSAGE "HentK " STRING(piButikkNr) STRING(piTTId) SKIP VIEW-AS ALERT-BOX.*/

END PROCEDURE.
