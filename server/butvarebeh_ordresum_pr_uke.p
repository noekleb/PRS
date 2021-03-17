/* Rapport, sum ordre pr uke fra varehåndteringsbok
   Parametere: 
   Opprettet: 28.04.11 av BHa                          
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO INIT YES.

DEFINE VAR hQuery   AS HANDLE NO-UNDO.
DEFINE VAR iUke     AS INT    NO-UNDO.
DEFINE VAR iPrevUke AS INT    NO-UNDO.
DEFINE VAR fTotSum  AS DEC    NO-UNDO.
DEFINE VAR cSum     AS CHAR   NO-UNDO.

DEF TEMP-TABLE ttUkeSum
    FIELD cUkeNr  AS CHAR LABEL "Lev.uke"
    FIELD fSumUke AS DEC  FORMAT "->>,>>>,>>9,99" LABEL "Sum innkj.pris"
    .
DEF VAR hBuffTtUkeSum AS HANDLE NO-UNDO.
hBuffTtUkeSum = BUFFER ttUkeSum:HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).

hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY LeveringsDato").

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  RUN WeekNum (ihBuffer:BUFFER-FIELD("LeveringsDato"):BUFFER-VALUE,OUTPUT iUke).
  FIND FIRST ttUkeSum 
       WHERE ttUkeSum.cUkeNr = STRING(iUke)
       NO-ERROR.
  IF NOT AVAIL ttUkeSum THEN DO:
    CREATE ttUkeSum.  
    ttUkeSum.cUkeNr = STRING(iUke).
  END.
  ASSIGN ttUkeSum.fSumUke = ttUkeSum.fSumUke + ihBuffer:BUFFER-FIELD("OrdreTotPris"):BUFFER-VALUE
         fTotSum          = fTotSum + ihBuffer:BUFFER-FIELD("OrdreTotPris"):BUFFER-VALUE
         .

  hQuery:GET-NEXT().

END.

DELETE OBJECT hQuery.

CREATE ttUkeSum.
ASSIGN ttUkeSum.cUkeNr  = "Totalt:"
       ttUkeSum.fSumUke = fTotSum.

ocReturn = "Lev.uke~tSum innkj.pris" + CHR(10).
FOR EACH ttUkeSum:
  cSum = STRING(ttUkeSum.fSumUke,"->>,>>>,>>9.99").
  cSum = REPLACE(cSum,"."," ").
/*   cSum = REPLACE(cSum,".",","). */
/*   cSum = REPLACE(cSum,"|"," "). */
  ocReturn = ocReturn 
           + ttUkeSum.cUkeNr + "~t" + cSum + CHR(10).
END.

/* RUN ToExcelViaFile.p(hBuffTtUkeSum,0). */


PROCEDURE WeekNum:    
  DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
  DEFINE OUTPUT PARAMETER yyyyww   AS INT.   /* Output week, eg 9042     */

  DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
  DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
                                /* (01/01/90 is a Monday)      */
  DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
  DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */

  ASSIGN
    yr   = YEAR(indate)
    d1   = WEEKDAY(DATE( 1 , 1 , yr))
    dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                            DATE(1, 10, yr) - d1 )
    wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
    yyyyww = yr * 100 + wn.

  IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
  ASSIGN
    yr     = yr - 1
    d1     = WEEKDAY(DATE( 1 , 1 , yr))
    dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                              DATE(1, 10, yr) - d1 )
    wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
    yyyyww = yr * 100 + wn.

  ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
  ASSIGN
    yr     = yr + 1
    d1     = WEEKDAY(DATE( 1 , 1 , yr))
    yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
                THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.
END PROCEDURE.
