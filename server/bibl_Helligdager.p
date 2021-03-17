&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEF INPUT  PARAMETER wAar     AS INTE NO-UNDO.    /* Årests helgedager   */
DEF OUTPUT PARAMETER cHdagNum AS CHAR NO-UNDO.

DEF VAR cLand AS CHAR NO-UNDO.    /* Land det skal finnes helligdager for */

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

FIND Bruker NO-LOCK WHERE
    Bruker.BrukerId = USERID("skotex") NO-ERROR.
IF AVAILABLE Bruker THEN
DO:
    IF Bruker.Lng = '' THEN
        cLand = 'NOR'.
    ELSE IF CAN-DO('SE,SVE',Bruker.Lng) THEN
        cLand = 'SVE'.
    ELSE 
        cLand = 'NOR'.
END.
ELSE 
    cLand = 'NOR'.

RUN FinnHellig.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FinnHellig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnHellig Procedure 
PROCEDURE FinnHellig :
/*------------------------------------------------------------------------------
  Purpose:     Finner årets helgedager. 
               Påskedag ut fra formel av C. Fr. Gauss.
  Parameters:  Outp hdag kommasep sting med helligdager.
               Outp hmnd kommasep string med korresponderende måneder.
  Notes:       
------------------------------------------------------------------------------*/
 

   DEF VAR wHDag     AS CHAR NO-UNDO. /* Årests helgedager   */
   DEF VAR wHMnd     AS CHAR NO-UNDO. /* Korrsponderende mnd */

   DEF VAR iCount    AS INT  NO-UNDO.
   DEF VAR dExtraDat AS DATE NO-UNDO.
   DEF VAR wPdag     AS INTE NO-UNDO. /* Påskedag */
   DEF VAR wPmnd     AS INTE NO-UNDO. /* Måneden til påskedagen */
   DEF VAR dFG31Dec  AS DATE NO-UNDO. /* 31 dec foregående år.  */

   DEF VAR a AS INTE NO-UNDO.
   DEF VAR b AS INTE NO-UNDO.
   DEF VAR c AS INTE NO-UNDO.    
   DEF VAR d AS INTE NO-UNDO.
   DEF VAR e AS INTE NO-UNDO.

   DEF VAR x AS INTE init 24  NO-UNDO.
   DEF VAR y AS INTE init 5   NO-UNDO.

   ASSIGN
       dFG31Dec = DATE (12,31,wAar).

   /* Faste helgedager */
   IF cLand = "NOR" THEN
       ASSIGN wHDag = "1,1,17,25,26"
              wHMnd = "1,5,5,12,12".
   ELSE IF cLand = "SVE" THEN DO:
       ASSIGN wHDag = "1,6,1,6,25,26"
              wHMnd = "1,1,5,6,12,12".
       /* midsommardagen lördag som infaller under tiden den 20--26 juni */
       DO dExtraDat = DATE(06,20,wAar) TO DATE(06,20,wAar) + 6:
           IF WEEKDAY(dExtraDat) = 7 THEN
               LEAVE.
       END.
       ASSIGN wHDag = wHDag + "," + STRING(DAY(dExtraDat))
              wHMnd = wHMnd + "," + STRING(MONTH(dExtraDat)).
       /* alla helgons dag: lördag 31 oktober--den 6 november */
       DO dExtraDat = DATE(10,31,wAar) TO DATE(10,31,wAar) + 6:
           IF WEEKDAY(dExtraDat) = 7 THEN
               LEAVE.
       END.
       ASSIGN wHDag = wHDag + "," + STRING(DAY(dExtraDat))
              wHMnd = wHMnd + "," + STRING(MONTH(dExtraDat)).
   END.

   IF wAar >= 1700 AND wAar <= 1799 THEN ASSIGN x = 23 y = 3. ELSE
   IF wAar >= 1800 AND wAar <= 1899 THEN ASSIGN x = 23 y = 4. ELSE
   IF wAar >= 1900 AND wAar <= 2099 THEN ASSIGN x = 24 y = 5. ELSE
   IF wAar >= 2100 AND wAar <= 2199 THEN ASSIGN x = 24 y = 6. ELSE
   IF wAar >= 2200 AND wAar <= 2299 THEN ASSIGN x = 25 y = 0. 
   ELSE DO:
      ASSIGN 
         wHDag = wHDag + ",,,,,,,,"
         wHMnd = wHMnd + ",,,,,,,,".
      RETURN.
   END.   

   ASSIGN 
      a = wAar MOD 19
      b = wAar MOD 4
      c = wAar MOD 7
      d = (19 * a + x) MOD 30
      e = (2 * b + 4 * c + 6 * d + y) MOD 7
      wPdag = d + e + 22
      wPmnd = 3. 

   IF wPdag > 31 THEN 
      ASSIGN wPdag = (d + e - 9) wPmnd = 4.
   
   /* Unntagelse ihht. Gauss' formel */   
   IF wPmnd = 4 THEN DO:   
      /* Unntak 1? */
      IF wPdag = 26 THEN 
         ASSIGN wPdag = 19. 
      ELSE       
      /* Unntak 2? */
      IF wPdag = 25 AND (a > 10 AND d = 28 AND e = 6) THEN
         ASSIGN wPdag = 18.
   END.      

   ASSIGN 
     wHDag = wHDag 
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 7))     /* Palmesøndag    */
      + (IF cLand <> "SVE" THEN "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 3)) ELSE "")  /* Skjærtorsdag   */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 2))     /* Langfredag     */
      + "," + STRING(wPdag)                               /* Påskedag       */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 1))     /* 2. påskedag    */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 39))    /* Kr. himmelfart */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 49))    /* 1. pinsedag    */
/*       + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 50))    /* 2. pinsedag    */ */
      + (IF cLand <> "SVE" THEN "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 50)) ELSE "")  /* 2. pinsedag   */
     wHmnd = wHmnd
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 7))   /* Palmesøndag    */
      + (IF cLand <> "SVE" THEN "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 3)) ELSE "")  /* Skjærtorsdag   */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 2))   /* Langfredag     */
      + "," + STRING(wPmnd)                               /* Påskedag       */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 1))   /* 2. påskedag    */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 39))  /* Kr. himmelfart */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 49))  /* 1. pinsedag    */
/*       + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 50)). /* 2. pinsedag    */ */
      + (IF cLand <> "SVE" THEN "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 50)) ELSE "").  /* 2. pinsedag   */
    
    cHdagNum = ''.
    DO iCount = 1 TO NUM-ENTRIES(wHmnd):
        ASSIGN
            cHdagNum = cHdagNum + 
                       (IF cHdagNum = '' THEN '' ELSE ',') + 
                       STRING(DATE(INT(ENTRY(iCount,wHmnd)),
                              INT(ENTRY(iCount,wHDag)),
                              wAar),"99/99/99").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

