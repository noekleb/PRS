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
DEF INPUT PARAMETER piJobbNr AS INT NO-UNDO.

{runlib.i}
{xPrint.i}

DEF STREAM UtFil.

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
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Henter jobbrecorden. */
FIND Jobb NO-LOCK where
  Jobb.JobbNr = piJobbNr NO-ERROR.
if NOT AVAILABLE Jobb then
  RETURN.

RUN SkrivUt.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SkrivUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivUt Procedure 
PROCEDURE SkrivUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil      AS CHAR                     NO-UNDO.
  DEF VAR pdDLinjeSum    AS DEC  FORMAT "->>>>>9.99" NO-UNDO.    
  DEF VAR pdKLinjeSum    AS DEC  FORMAT "->>>>>9.99" NO-UNDO.    
  DEF VAR pdDSum         AS DEC  FORMAT "->>>>>9.99" NO-UNDO.    
  DEF VAR pdKSum         AS DEC  FORMAT "->>>>>9.99" NO-UNDO.    
  DEF VAR piLinjeNr      AS INT                      NO-UNDO.
  DEF VAR pcTekst        AS CHAR                     NO-UNDO.
  DEF VAR pdFraDato      AS DATE FORMAT "99/99/99"   NO-UNDO.
  DEF VAR pdTilDato      AS DATE FORMAT "99/99/99"   NO-UNDO.

  DEF VAR pcFirma        AS CHAR                         NO-UNDO.
  DEF VAR pcSideLbl      AS CHAR INITIAL "Side:"         NO-UNDO.
  DEF VAR pcSumLbl       AS CHAR INITIAL "Sum"           NO-UNDO.
  DEF VAR pcSaldoLbl     AS CHAR INITIAL "Saldo"         NO-UNDO.
  DEF VAR pdSaldo        AS DEC                          NO-UNDO.
  DEF VAR pcKundeNrLbl   AS CHAR INITIAL "KundeNr"       NO-UNDO.
  DEF VAR pcKundeNavnLbl AS CHAR INITIAL "Navn"          NO-UNDO.
  DEF VAR pcKundeAdr1Lbl AS CHAR INITIAL "Adresse"       NO-UNDO.
  DEF VAR pcKundeAdr2Lbl AS CHAR INITIAL ""              NO-UNDO.
  DEF VAR pcKundePostLbl AS CHAR INITIAL "Poststed"      NO-UNDO.
  DEF VAR pcLabler       AS CHAR                         NO-UNDO.
  DEF VAR pcRapport      AS CHAR INITIAL "KONTOUTSKRIFT" NO-UNDO.
  DEF VAR pcKriterieLbl  AS CHAR INITIAL "Periode"       NO-UNDO.

  {syspara.i 1 1 101 pcFirma} /* firmanavn */

  {syspara.i 6 620  1 pcRapport} 
  {syspara.i 6 620  2 pcSideLbl} 
  {syspara.i 6 620  3 pcSumLbl} 
  {syspara.i 6 620  4 pcSaldoLbl} 
  {syspara.i 6 620  5 pcKundeNrLbl} 
  {syspara.i 6 620  6 pcKundeNavnLbl} 
  {syspara.i 6 620  7 pcKundeAdr1Lbl} 
  {syspara.i 6 620  8 pcKundeAdr2Lbl} 
  {syspara.i 6 620  9 pcKundePostLbl} 
  {syspara.i 6 620 10 pcKriterieLbl} 

  /* Sjekker datasett */
  if NUM-ENTRIES(Jobb.Kriterier) < 10 then
    DO:
      MESSAGE "Gammelt datasett. Generer en ny liste." skip
              "Det ligger feil parametre i det gamle datasettet." SKIP(1)
              Jobb.Kriterier
              VIEW-AS ALERT-BOX.
      RETURN "AVBRYT".
    END.
  ASSIGN
      pcTekst = ENTRY(8,Jobb.Kriterier)
      pdFraDato = DATE(ENTRY(1,pcTekst,"|"))
      pdTilDato = DATE(ENTRY(2,pcTekst,"|"))
      .

  /* Default */
  IF pcLabler = "" THEN
  ASSIGN
      pcLabler  = "Trans " +
                  " ButNr " + 
                  "Kas " +
                  "Dato     " + 
                  "    Bong " + 
                  "Varetekst         " + 
                  "Str " + 
                  "Ant" + 
                  "         Kjøp " + 
                  "    Betaling "
                  .

  RUN GetTempFileName IN wLibHandle ("utk","xpr", OUTPUT pcRappFil).

  OUTPUT STREAM UtFil TO VALUE(pcRappFil) NO-ECHO PAGED page-size 64.
    
  Put STREAM UtFil control '<PREVIEW=ZoomToWidth>'. /* Preview - Fyll tilgjengelig bredde */
  put STREAM UtFil control "<Printer?>".            /* Skriverdialog */
  put STREAM UtFil control "<TITLE=" + pcRapport + ">".

  JOBBLINJE:
  FOR EACH JobbLinje NO-LOCK WHERE
      JobbLinje.JobbNr = Jobb.JobbNr AND
      NOT CAN-DO("63",JobbLinje.DivX[15])
      BREAK 
      BY JobbLinje.JobbNr
      BY JobbLinje.Char1
      BY JobbLinje.Char2
      BY JobbLinje.DivX[20] /* ButikkNr */
      BY JobbLinje.DivX[21] /* KasseNr  */
      BY date(JobbLinje.DivX[17]) /* Dato     */ 
      BY JobbLinje.DivX[18] /* Tid      */
      BY JobbLinje.DivX[22] /* BongNr   */
      BY JobbLinje.DivX[23] /* BongLinje*/
      :

    /* TN 13/6-05 LAgt inn i For Each for å ikke forstyrre loopen. */
    /* Driter i subtotalrabatten. den er innarbeidet allerede.     */
    /*
    IF INT(JobbLinje.DivX[15]) = 63 THEN
        NEXT JOBBLINJE.
    */

    ASSIGN
      pdDLinjeSum = 0
      pdKLinjeSum = 0
      .

    /* Nullstiller */
    IF FIRST-OF(JobbLinje.Char2) THEN
    DO:
        IF NOT FIRST(JobbLinje.Char2) THEN
        DO:
            /* SideBunn Info */
            PUT STREAM UtFil UNFORMATTED
                "<P10><B><R63><C5><FROM><R63><C79><LINE>" SKIP
                "<C5>" STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " 
                       USERID("DictDb")
                "<C28>" pcFirma
                "<C64>" pcSideLbl FORMAT "x(6)"
                string(PAGE-NUMBER(UtFil)) + "/<#Pages></B>" SKIP
                .
        END.
        PAGE STREAM UtFil.
        ASSIGN
            pdDSum      = 0
            pdKSum      = 0
            .
    END.

    /* SideHeading */
    IF LINE-COUNTER(UtFil) = 1 THEN
    DO:
        PUT STREAM UtFil UNFORMATTED
            SKIP(5)
            "<P28><C5><B>" pcRapport SKIP(2) 
            "<P10><C5><FROM><C79><LINE>" SKIP
            "</B><P12>" 
            "<C5>" pcKundeNrLbl   FORMAT "x(15)" JobbLinje.DivX[1] SKIP
            "<C5>" pcKundeNavnLbl FORMAT "x(15)" JobbLinje.DivX[2] SKIP
            "<C5>" pcKundeAdr1Lbl FORMAT "x(15)" JobbLinje.DivX[3] SKIP
            "<C5>" pcKundeAdr2Lbl FORMAT "x(15)" JobbLinje.DivX[4] SKIP
            "<C5>" pcKundePostLbl FORMAT "x(15)" JobbLinje.DivX[6] SKIP(1)
            "<C5><B>" pcKriterieLbl  FORMAT "x(15)" pdFraDato " - " pdTilDato 
            "<B>" SKIP
            SKIP(1)
            "<P10><C5><B><U>" pcLabler "</U></B>" SKIP
            .
    END.

    PUT STREAM UtFil UNFORMATTED
        "<P10><C5>" 
        substring(JobbLinje.DivX[16],1,5)  FORMAT "x(5)"    /* TransType */
        " " int(JobbLinje.DivX[20]) FORMAT ">>>>>9"   /* ButikkNr  */
        " " int(JobbLinje.DivX[21]) FORMAT ">>9"      /* KasseNr   */
        " " JobbLinje.DivX[17]      FORMAT "xxxxxxxx" /* Dato      */
        " " int(JobbLinje.DivX[22]) FORMAT ">>>>>>>9" /* BongNr    */
        .

    IF INT(JobbLinje.DivX[15]) < 50 OR 
       INT(JobbLinje.DivX[15]) = 134 /* Gavekort ut */ THEN
    VARETRANSER:
    DO:
        /* Subtotalrabatt */
        IF INT(JobbLinje.DivX[15]) = 63 THEN
            ASSIGN
                /*
                pdDLinjeSum = DEC(JobbLinje.DivX[28]) * -1
                pdDSum      = pdDSum + 
                              (DEC(JobbLinje.DivX[28]) * -1)
                */
                .
        /* Varesalg */
        ELSE
            ASSIGN
                pdDLinjeSum = /*int(JobbLinje.DivX[27]) **/ abs(DEC(JobbLinje.DivX[28]))
                pdDSum      = pdDSum + (IF int(JobbLinje.DivX[27]) < 0
                                          THEN abs(DEC(JobbLinje.DivX[28])) * -1
                                          ELSE abs(DEC(JobbLinje.DivX[28])))
                .
        PUT STREAM UtFil UNFORMATTED         
           " " substring(JobbLinje.DivX[ 7],1,16) FORMAT "x(16)" /* Varetekst */
           " " JobbLinje.DivX[26]      FORMAT "xxxx"             /* Str  */
           " " int(JobbLinje.DivX[27]) FORMAT "->>"              /* Ant  */
           " " 
           (IF LAST-OF(JobbLinje.Char2)
              THEN "<U>"
              ELSE "")
           pdDLinjeSum             FORMAT "->>>>>>>9.99"    /* Pris */
           (IF LAST-OF(JobbLinje.Char2)
              THEN string(pdKLinjeSum,"->>>>>>>9.99")
              ELSE "")
           (IF LAST-OF(JobbLinje.Char2)
              THEN "</U>"
              ELSE "")
        SKIP.

    END. /* VARETRANSER */
    ELSE 
    BETALINGSTRANSER:
    DO:
        ASSIGN
            pdKLinjeSum = (IF INT(JobbLinje.DivX[15]) = 70
                             THEN (DEC(JobbLinje.DivX[28]) * -1)
                             ELSE DEC(JobbLinje.DivX[28]))
            .
        IF JobbLinje.DivX[14] = "" THEN
            ASSIGN
            pdKSum      = pdKSum +
                          (IF INT(JobbLinje.DivX[15]) = 70
                             THEN (DEC(JobbLinje.DivX[28]) * -1)
                             ELSE DEC(JobbLinje.DivX[28]))
            .
        PUT STREAM UtFil UNFORMATTED
           " "
           JobbLinje.DivX[ 7] FORMAT "x(16)"
            fill(" ",18)
           "     " 
           (IF LAST-OF(JobbLinje.Char2)
              THEN "<U>"
              ELSE "")
           pdKLinjeSum FORMAT "->>>>>>>9.99"    /* Pris */
           JobbLinje.DivX[14] FORMAT "x(1)"
           (IF LAST-OF(JobbLinje.Char2)
              THEN "</U>"
              ELSE "")
        SKIP.
    END. /* BETALINGSTRANSER */

    /* Totaler for kunde */
    IF LAST-OF(JobbLinje.Char2) THEN
    DO:
        PUT STREAM UtFil UNFORMATTED         
           "<B>"
           fill(" ",55) 
           pcSumLbl FORMAT "x(10)" 
           " " pdDSum FORMAT "->>>>>>>9.99" /* Sum Debet  */
           " " pdKSum FORMAT "->>>>>>>9.99" /* Sum Kredit */
        SKIP.

        ASSIGN
            pdSaldo = pdDSum - pdKSum
            .

        IF pdSaldo > 0 then
            PUT STREAM UtFil UNFORMATTED         
            fill(" ",55) 
            pcSaldoLbl FORMAT "x(10)" 
            " " pdSaldo FORMAT "->>>>>>>9.99" /* Saldo Debet  */
            .

        ELSE IF pdSaldo <= 0 then
            PUT STREAM UtFil UNFORMATTED         
            fill(" ",55) 
            pcSaldoLbl FORMAT "x(10)" 
            FILL(" ",13)
            " " pdSaldo FORMAT "->>>>>>>9.99" /* Saldo Kredit  */
            .

        PUT STREAM UtFil UNFORMATTED         
            "</B><P12>"
            SKIP(1).
    END.
        /* SideHeading */
    IF LINE-COUNTER(UtFil) > 62 THEN
    DO:
        /* SideBunn Info */
        PUT STREAM UtFil UNFORMATTED
            "<P10><R63><C5><FROM><R63><C79><LINE>" SKIP
            "<C5>" STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " 
                   USERID("DictDb")
            "<C28>" pcFirma
            "<C64>" pcSideLbl FORMAT "x(6)"
            string(PAGE-NUMBER(UtFil)) + "/<#Pages>" SKIP
            .

        PAGE STREAM UtFil.

    END.
      
  END. /* JOBBLINJE */

  OUTPUT STREAM UtFil CLOSE.

    /* Klargjør rapportfilnavnet */
  ASSIGN
    FILE-INFO:File-NAME = pcRappFil
    .
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

