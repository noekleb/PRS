/* Aktiver VPI for varebok 
   Overfører varebok direkte til HK's VPI register. Slik at det blir unødvendig 
   å gjøre dette via lokalt artikkelregsiter på HK.
   Parametere:  Vareboknr;evt liste over artikler
   
   Opprettet: 18.06.07 av TN 
   Endret:    25.02.08 av BHa
            - Bruker temp-tabell som er resultat av enten valgte eller alle poster i spørring i varebok
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR bOK               AS LOG    NO-UNDO.
DEF VAR hPrisKo           AS HANDLE NO-UNDO.

DEF VAR iAntall           AS INT    NO-UNDO.
DEF VAR i2Antall          AS INT    NO-UNDO.
DEF VAR iUAntall          AS INT    NO-UNDO.

DEF VAR iEkstVPILevNr     AS INT    NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

/* TESt */
/*
MESSAGE "icParam    " icParam      SKIP
        "ihBuffer   " ihBuffer     SKIP
        "icSessionID" icSessionID  SKIP
        "ocReturn   " ocReturn     SKIP
        "obOk       " obOk         SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN "Gurre var her". /* TEST */
*/

FIND FIRST VarebokHode WHERE VarebokHode.VarebokNr = DEC(ENTRY(1,icParam,";")) NO-LOCK NO-ERROR.
IF NOT AVAIL VarebokHode THEN DO:
  ocReturn = "Finner ikke varebok: " + ENTRY(1,icParam,";").
  RETURN.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).

/* hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE).                                                                                                                            */
/* obOk = hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = "                                                                                             */
/*                            + ENTRY(1,icParam,";")  /* Vareboknr */                                                                                                         */
/*                            + (IF NUM-ENTRIES(icParam,";") > 1 THEN ENTRY(2,icParam,";") ELSE "")  /* Evt. liste over artikler (can-do) */                                  */
/*                            ) NO-ERROR.                                                                                                                                     */
/* IF NOT obOk THEN DO:                                                                                                                                                       */
/*   ocReturn = "Denne spørringen blir ikke godtatt: " + CHR(10) + "FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " + ENTRY(1,icParam,";") + " BY " + ENTRY(3,icParam,";"). */
/*   RETURN.                                                                                                                                                                  */
/* END.                                                                                                                                                                       */

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
/*   FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarebokLinje.ArtikkelNr NO-LOCK NO-ERROR. */
  FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN 
    RUN OverforTilERP.
           
/*   IF NOT obOK THEN */
/*     UNDO, LEAVE.   */

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.
IF obOk THEN
DO:
    ocReturn = "Antall artikler overført til ERP er " + STRING(iAntall) + ".".
    IF i2Antall > 0 THEN
        ocReturn = ocReturn + CHR(13) + "Antall ikke overført " + STRING(i2Antall) + " (Ikke HK varer).". 
    IF iUAntall > 0 THEN
        ocReturn = ocReturn + CHR(13) + "Antall uten utpris eller innpris - ikke overført " + STRING(iUAntall) + ".". 
END.

IF VALID-HANDLE(hPrisKo) THEN
    DELETE PROCEDURE hPrisKo.

PROCEDURE OverforTilERP:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Tilpasset fra prosedyre LagreKalkyle i w-kalkyle.w 
------------------------------------------------------------------------------*/

  def var wSkjerm      AS CHAR  NO-UNDO.
  
  DEF VAR pcError      AS CHAR  NO-UNDO.
  DEF VAR wArtBasRecid AS RECID NO-UNDO.
  
  /* Artikler uten innkjøps og/eller salgspris til butikk, skal ikke kunne legges over. */
/*   IF (VarebokLinje.Pris <= 0 OR VarebokLinje.InnkjopsPris <= 0) THEN */
  IF DEC(ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE) <= 0 OR DEC(ihBuffer:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE) <= 0 THEN
  DO:
      iUAntall = iUAntall + 1.
      RETURN.
  END.

  ASSIGN 
      wArtBasRecid = RECID(ArtBas)
      iAntall      = iAntall + 1
      .

  IF AVAILABLE ArtBas THEN
  ERP-LOGG:
  DO TRANSACTION:
    FIND ELogg WHERE
         ELogg.TabellNavn     = "VarebokVPI" AND
         ELogg.EksterntSystem = "ERP"    AND
         ELogg.Verdier        = STRING(VarebokHode.VarebokNr) + CHR(1) + 
                                STRING(ArtBas.ArtikkelNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "VarebokVPI"
               ELogg.EksterntSystem = "ERP"
               ELogg.Verdier        = STRING(VarebokHode.VarebokNr) + CHR(1) + 
                                      STRING(ArtBas.ArtikkelNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
  END. /* TRANSACTION ERP-LOGG */


END PROCEDURE.


