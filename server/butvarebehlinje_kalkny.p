/* Kalkulering av ny artikkel som ikke er registrert i basen enda 
   Parametere: Modifisert felt og verdi
               temp-tabell artikkelinfor
   
   Opprettet: 13.11.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cModField     AS CHAR   NO-UNDO.
DEF VAR fModValue     AS DEC    NO-UNDO.
DEF VAR iProfilNr     AS INT    NO-UNDO.
DEF VAR fMva%         AS DEC    NO-UNDO INIT 25.
DEF VAR fPrisExMva    AS DEC    NO-UNDO.
DEF VAR fVarebehNr    AS DEC    NO-UNDO.
DEF VAR iMesseType    AS INT    NO-UNDO INIT 2. /* 1: Forh: 2: Supp */

DEF TEMP-TABLE ttVarebehLinje LIKE VareBehLinje.

FIND FIRST Moms NO-LOCK
     WHERE Moms.MomsKod = 1
     NO-ERROR.
IF AVAIL Moms THEN fMva% = Moms.MomsProc.

cModField = ENTRY(1,icParam,"|").
IF ihBuffer:BUFFER-FIELD(cModField):DATA-TYPE NE "decimal" THEN RETURN.

ASSIGN fModValue  = DEC(ENTRY(2,icParam,"|"))
       iProfilNr  = INT(ENTRY(3,icParam,"|"))
       fVarebehNr = DEC(ENTRY(4,icParam,"|"))
       .

ihBuffer:FIND-FIRST() NO-ERROR.

IF NOT ihBuffer:AVAILABLE THEN RETURN.

FIND FIRST VareBehHode NO-LOCK
     WHERE VareBehHode.VareBehNr = fVarebehNr
     NO-ERROR.
IF AVAIL VareBehHode THEN
  FIND FIRST messe NO-LOCK
       WHERE messe.MesseNr = VareBehHode.MesseNr
       NO-ERROR.
ELSE RETURN.

IF AVAIL Messe THEN 
  iMesseType = messe.MesseType.

CREATE ttVarebehLinje.
BUFFER ttVarebehLinje:HANDLE:BUFFER-COPY(ihBuffer).

fPrisExMVA = Pris / (1 + Mva% / 100).


CASE cModField:
  WHEN "InnkjopsPris" THEN DO:      
    InnkjopsPris = fModValue.
    IF iMesseType = 1 THEN
      Varekost = InnkjopsPris - InnkjopsPris * forhRab% / 100.
    ELSE
      Varekost = InnkjopsPris - InnkjopsPris * supRab% / 100.
  END.
  WHEN "Varekost"     THEN DO:      
    Varekost    = fModValue.
    IF iMesseType = 1 THEN
      forhRab%    = (InnkjopsPris - Varekost) / InnkjopsPris * 100.
    ELSE
      supRab%    = (InnkjopsPris - Varekost) / InnkjopsPris * 100.
  END.
/*   WHEN "supVarekost"  THEN                                                  */
/*     ASSIGN supVarekost = fModValue                                          */
/*            supRab%     = (InnkjopsPris - supVarekost) / InnkjopsPris * 100. */
  WHEN "forhRab%"     THEN DO:      
    forhRab% = fModValue.
    IF iMesseType = 1 THEN
      Varekost  = InnkjopsPris - InnkjopsPris * forhRab% / 100.
  END.
  WHEN "supRab%"      THEN DO:
    supRab% = fModValue.
    supVarekost = InnkjopsPris - InnkjopsPris * supRab% / 100.           
    IF iMesseType = 2 THEN
      Varekost = InnkjopsPris - InnkjopsPris * supRab% / 100.           
  END.
  WHEN "AnbefaltPris" THEN 
    Pris = AnbefaltPris.
END CASE.

ASSIGN forhKalkyle    = Pris / VareKost
       supKalkyle     = Pris / SupVareKost

       DBkr           = fPrisExMVA - VareKost
       supDBkr        = fPrisExMVA - supVareKost

       DB%            = DBkr / fPrisExMVA * 100
       supDB%         = supDBkr / fPrisExMVA * 100
       .


IF ocReturn = "" THEN DO:
  ihBuffer:BUFFER-COPY(BUFFER ttVarebehLinje:HANDLE).
  obOk = TRUE.
END.
ELSE ocReturn = TRIM(ocReturn,CHR(10)).
