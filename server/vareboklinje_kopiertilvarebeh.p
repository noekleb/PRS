/* Kopier varebok varebehandlingsbok 
   Parametere:  Vareboknr;evt liste over artikler;sortering
   
   Opprettet: 17.09.04 av BHa 
      Endret: 27.01.05 av BHa
              - Kan komplettere en eksisterende varehåndteringsbok samt behandle en liste av artikler                 
              - Oppretter registreringsunderlag også iom et det kan være gjort registreringer i varehåndteringsboken
                ved å kalle varebeh_legg_til_farger.p med samme artikkel som kilde og ny
      Endret: 18.01.06 av BHa
              - Oppretter ikke lenger registreringsunderlag siden det nå gjøres "on the fly" i vareh.boken
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hBuffVarebokHode  AS HANDLE NO-UNDO.
DEF VAR hBuffVarebehHode  AS HANDLE NO-UNDO.
DEF VAR hBuffVarebokLinje AS HANDLE NO-UNDO.
DEF VAR hBuffVarebehLinje AS HANDLE NO-UNDO.
DEF VAR fArtikkelNr       AS DEC    NO-UNDO.
DEF VAR bOK               AS LOG    NO-UNDO.
DEF VAR cSortString       AS CHAR   NO-UNDO.
DEF VAR cModSortString    AS CHAR   NO-UNDO.
DEF VAR iFieldSeqNum      AS INT    NO-UNDO.
DEF VAR cQueryString      AS CHAR   NO-UNDO.

cSortString = ENTRY(3,icParam,";").
DO ix = 1 TO NUM-ENTRIES(cSortString," "):
  iFieldSeqNum = INT(SUBSTR(ENTRY(ix,cSortString," "),LENGTH(ENTRY(ix,cSortString," ")))) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
    cModSortString = cModSortString + SUBSTR(ENTRY(ix,cSortString," "),1,LENGTH(ENTRY(ix,cSortString," ")) - 1) + " ".
  ELSE cModSortString = cModSortString + ENTRY(ix,cSortString," ") + " ".
END.
cQueryString = "FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " 
             + ENTRY(1,icParam,";")  /* Vareboknr */
             + ENTRY(2,icParam,";")  /* Evt. liste over artikler (can-do) */
             + ",FIRST ArtBas OF VarebokLinje NO-LOCK,FIRST VareMerke OF ArtBas NO-LOCK OUTER-JOIN"
             + (IF cModSortString NE "" THEN " BY " + cModSortString ELSE "").

CREATE BUFFER hBuffVarebokLinje FOR TABLE "VarebokLinje".
CREATE BUFFER hBuffVarebehLinje FOR TABLE "VarebehLinje".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffVarebokLinje,BUFFER ArtBas:HANDLE,BUFFER VareMerke:HANDLE).
obOk = hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
IF NOT obOk THEN DO:
  ocReturn = "Denne spørringen blir ikke godtatt: " + CHR(10) + cQueryString.
  RETURN.
END.

DO TRANSACTION ON ERROR UNDO, LEAVE:
  FIND FIRST VarebokHode WHERE VarebokNr = DEC(ENTRY(1,icParam,";")) NO-LOCK NO-ERROR.
  IF AVAIL VarebokHode THEN DO: 
    FIND FIRST VarebehHode WHERE kilde = DEC(ENTRY(1,icParam,";")) NO-LOCK NO-ERROR.
    IF NOT AVAIL VarebehHode THEN DO:
      CREATE VarebehHode.
      BUFFER-COPY VarebokHode TO VarebehHode.
      ASSIGN VarebehHode.Kilde              = VarebokHode.VarebokNr
             VarebehHode.VarebehBeskrivelse = VarebokHode.VarebokBeskrivelse
             VarebehHode.VarebehNotat       = VarebokHode.VarebokNotat
             VarebehHode.VarebehType        = 1
             .
    END.
    ELSE DO:
      FOR EACH VarebehLinje
          WHERE VarebehLinje.VarebehNr = VarebehHode.VarebehNr
          BY Sortering DESC:
        ix = VarebehLinje.Sortering.
        LEAVE.
      END.
    END.
  END.
END.

IF NOT ERROR-STATUS:ERROR THEN DO:
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    fArtikkelNr = DEC(hBuffVarebokLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
    IF NOT CAN-FIND(FIRST VarebehLinje
                          WHERE VarebehLinje.VarebehNr  = VarebehHode.VarebehNr
                            AND VarebehLinje.Artikkelnr = fArtikkelNr) THEN DO TRANSACTION:
      ix = ix + 1.

      hBuffVarebehLinje:BUFFER-CREATE().
      hBuffVarebehLinje:BUFFER-COPY(hBuffVarebokLinje).
      ASSIGN hBuffVarebehLinje:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE = VarebehHode.VarebehNr
             hBuffVarebehLinje:BUFFER-FIELD("Sortering"):BUFFER-VALUE = ix
             .
/*       RUN varebeh_legg_til_farger.p (STRING(VarebehHode.VarebehNr) + ","                                     */
/*                                    + STRING(hBuffVarebokLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," */
/*                                    + STRING(hBuffVarebokLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)       */
/*                                      ,?,icSessionId,OUTPUT ocReturn, OUTPUT obOk).                           */
/*       IF NOT obOK THEN                                                                                       */
/*         UNDO, LEAVE.                                                                                         */
    END.
    hQuery:GET-NEXT().
  END.
END.
ELSE ocReturn = "Finner ikke varebok " + ENTRY(1,icParam,";").

DELETE OBJECT hQuery.
DELETE OBJECT hBuffVarebokLinje.
DELETE OBJECT hBuffVarebehLinje.

IF ocReturn = "" THEN obOk = TRUE.

