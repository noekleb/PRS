/* Kjør forskjellige operasjoner for et utvalg av ordre 
   Parametere: serverprogram;parameter;brukerid;liste over ordrenr
   
   Brukes til å endre status, registrere varemottak og makulering av restkvt.
   
   Opprettet: 21.11.05 av BHa              
   Endret:    15.05.07 av BHa
              - Tatt vekk transaksjon og endret til å kalles fra processQuery
                (dvs at alle filter-kriterier inkl kalk.felter,etc prosesseres på serveren i et eget utplukk)                
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cServerProg  AS CHAR   NO-UNDO.
DEF VAR cUserId      AS CHAR   NO-UNDO.
DEF VAR cParam       AS CHAR   NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR cReturn      AS CHAR   NO-UNDO.

ASSIGN cServerProg  = ENTRY(1,icParam,";")
       cParam       = ENTRY(2,icParam,";")
       cUserId      = ENTRY(3,icParam,";")
       .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).

hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY CL BY LeveringsDato BY LevNr").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  RUN VALUE(cServerProg) (cParam + ";" + cUserId + ";" + STRING(ihBuffer:BUFFER-FIELD("OrdreNr"):BUFFER-VALUE),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
  IF ocReturn NE "" THEN cReturn = ocReturn.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

IF cReturn = "" THEN 
  obOk = TRUE.
ELSE ocReturn = cReturn.

