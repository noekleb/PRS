/* Kopier messeregistreringer fra en til flere andre butikker
   Parametere:  fra butikknr
                liste over til-butikker 
                varebehnr
                buffersandfields
                query 
   Opprettet: 24.08.05 av BHa 
   Endret:    17.01.06 av Bha
              - Dersom en leveradør gjør kopieringen så betyr et 3-tall i entry(8,"¤") i parameter-strengen
                at alle nye registreringer blir satt som bestillingsforslag
                (se prosedyre KopierRegRecord i varebehhode.w)              
              - 18.01.06: Genererer kun registreringsunderlag for berørte artikler
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR cDummy            AS CHAR   NO-UNDO.
DEF VAR bForslag          AS LOG    NO-UNDO.

DEF VAR cFileName         AS CHAR   NO-UNDO.
DEF VAR bEksisterer       AS LOG    NO-UNDO.
DEF VAR cButikkListe      AS CHAR   NO-UNDO.
DEF VAR iFraButikkNr      AS INT    NO-UNDO.
DEF VAR fVarebehNr        AS DEC    NO-UNDO.
DEFINE VARIABLE bBrukMal AS LOG NO-UNDO.

DEF BUFFER bVarebehLinjeTrans FOR VarebehLinjeTrans.

cFileName = SESSION:TEMP-DIR + "kopilogg" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

RUN jbserv_gettemptablejoin.p(icSessionId,
                              100000,
                              0,
                              "",
                              ENTRY(4,icParam,"¤"),
                              ENTRY(5,icParam,"¤"),
                              "",
                              "calcfieldproc;" + ENTRY(7,icParam,"¤"),
                              OUTPUT TABLE-HANDLE hTempTable,
                              OUTPUT cDummy,
                              OUTPUT ocReturn).

IF ocReturn NE "" THEN RETURN.
    
ASSIGN iFraButikkNr = INT(ENTRY(1,icParam,"¤"))
       cButikkListe = REPLACE(ENTRY(2,icParam,"¤"),"|",",")
       fVarebehNr   = DEC(ENTRY(3,icParam,"¤"))
       bForslag     = ENTRY(8,icParam,"¤") = "6"
       .

/* DO ix = 1 TO NUM-ENTRIES(cButikkListe):                                                                                        */
/*   IF NOT CAN-FIND(FIRST VarebehLinjeThode                                                                                      */
/*                   WHERE VarebehLinjeThode.VarebehNr = fVarebehNr                                                               */
/*                     AND VarebehLinjeThode.ButikkNr  = INT(ENTRY(ix,cButikkListe))) THEN                                        */
/*     RUN varebehlinje_gentrans.p (STRING(fVarebehNr) + "," + ENTRY(ix,cButikkListe),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk). */
/*   IF ocReturn NE "" THEN RETURN.                                                                                               */
/* END.                                                                                                                           */


hBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

IF hBuffer:AVAIL THEN DO:
  OUTPUT TO VALUE(cFileName).
  PUT UNFORMATTED "Disse bestillingene var allerede lagt inn og ble derfor ikke overskrevet ved kopiering:" SKIP(2).
  PUT UNFORMATTED "Butikk~tArtikkelnr~tStr/innd~tLev.uke1~tAnt~tLev.uke2~tAnt~tLev.uke3~tAnt~tLev.uke4~tAnt" SKIP.
END.
ELSE RETURN.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF CAN-FIND(FIRST VarebehLinjeTrans 
              WHERE VarebehLinjeTrans.VarebehNr  = fVarebehNr
                AND VarebehLinjeTrans.ButikkNr   = iFraButikkNr
                AND VarebehLinjeTrans.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                AND VarebehLinjeTrans.RegistrertBestilling)
     THEN DO:

/*     DO ix = 1 TO NUM-ENTRIES(cButikkListe):                                                   */
/*       IF NOT CAN-FIND(FIRST VarebehLinjeThode                                                 */
/*                       WHERE VarebehLinjeThode.VarebehNr = fVarebehNr                          */
/*                         AND VarebehLinjeThode.ButikkNr  = INT(ENTRY(ix,cButikkListe))) THEN   */
/*         RUN varebehlinje_gentrans.p (STRING(fVarebehNr) + "," +                               */
/*                                      ENTRY(ix,cButikkListe) + "," +                           */
/*                                      STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)  */
/*                                     ,?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).              */
/*       IF ocReturn NE "" THEN RETURN.                                                          */
/*     END.                                                                                      */

    FOR EACH VarebehLinjeTrans NO-LOCK
        WHERE VarebehLinjeTrans.VarebehNr  = fVarebehNr
          AND VarebehLinjeTrans.ButikkNr   = iFraButikkNr
          AND VarebehLinjeTrans.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE):
      BUTIKK_LOOP:    
      DO ix = 1 TO NUM-ENTRIES(cButikkListe):
        RUN varebehlinje_gentrans.p (STRING(fVarebehNr) + "," + 
                                     ENTRY(ix,cButikkListe) + "," +
                                     STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                                    ,?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
        FIND FIRST bVarebehLinjeTrans EXCLUSIVE-LOCK
             WHERE bVarebehLinjeTrans.VarebehNr  = fVarebehNr
               AND bVarebehLinjeTrans.ButikkNr   = INT(ENTRY(ix,cButikkListe))
               AND bVarebehLinjeTrans.Kode       = VarebehLinjeTrans.kode
               AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
             NO-ERROR.
        IF AVAIL bVarebehLinjeTrans THEN 
        KOPIERTRANS:
        DO:
          IF bVarebehLinjeTrans.Bestilt1 = 0 AND
             bVarebehLinjeTrans.Bestilt2 = 0 AND
             bVarebehLinjeTrans.Bestilt3 = 0 AND
             bVarebehLinjeTrans.Bestilt4 = 0 THEN 
          TIL_TOM_RECORD:
          DO:
            BUFFER-COPY VarebehLinjeTrans 
                EXCEPT VarebehNr ButikkNr ArtikkelNr Kode 
                TO bVarebehLinjeTrans
                .
            FIND Butiker NO-LOCK WHERE 
                Butiker.butik = bVarebehLinjeTrans.ButikkNr NO-ERROR.
            /* TN 25/6-18 Bruker mal for kopiering av antall. Overskriver buffer-copy. */    
            IF AVAILABLE Butiker AND Butiker.ButMalNr > 0 THEN
            BRUK_MAL:           
            DO:
                IF AVAILABLE ButMalLinje THEN 
                    RELEASE ButMalLinje.
                FIND ArtBas NO-LOCK WHERE 
                    ArtBas.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr NO-ERROR.
                IF AVAILABLE ArtBas THEN 
                    FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
                IF AVAILABLE HuvGr THEN 
                    FIND ButMalLinje NO-LOCK WHERE 
                        butMalLinje.ButMalNr   = Butiker.ButMalNr AND 
                        butMalLinje.AvdelingNr = HuvGr.AvdelingNr NO-ERROR.
                IF AVAILABLE ButMalLinje AND ButMalLinje.Fordeling% > 0 THEN 
                ASSIGN 
                    bVarebehLinjeTrans.Bestilt1 = ROUND(bVarebehLinjeTrans.Bestilt1 * (ButMalLinje.Fordeling% / 100),0) 
                    bVarebehLinjeTrans.Bestilt2 = ROUND(bVarebehLinjeTrans.Bestilt2 * (ButMalLinje.Fordeling% / 100),0) 
                    bVarebehLinjeTrans.Bestilt3 = ROUND(bVarebehLinjeTrans.Bestilt3 * (ButMalLinje.Fordeling% / 100),0) 
                    bVarebehLinjeTrans.Bestilt4 = ROUND(bVarebehLinjeTrans.Bestilt4 * (ButMalLinje.Fordeling% / 100),0)  
                        .
            END. /* BRUK_MAL*/
            IF bForslag THEN 
                bVarebehLinjeTrans.GodkjentBestilling = FALSE.
          END. /* TIL_TOM_RECORD */
          ELSE IF bVarebehLinjeTrans.Bestilt1 NE VarebehLinjeTrans.Bestilt1 OR 
                  bVarebehLinjeTrans.Bestilt2 NE VarebehLinjeTrans.Bestilt2 OR
                  bVarebehLinjeTrans.Bestilt3 NE VarebehLinjeTrans.Bestilt3 OR
                  bVarebehLinjeTrans.Bestilt4 NE VarebehLinjeTrans.Bestilt4 OR
                  bVarebehLinjeTrans.Levuke1  NE VarebehLinjeTrans.Levuke1  OR
                  bVarebehLinjeTrans.Levuke2  NE VarebehLinjeTrans.Levuke2  OR
                  bVarebehLinjeTrans.Levuke3  NE VarebehLinjeTrans.Levuke3  OR
                  bVarebehLinjeTrans.Levuke4  NE VarebehLinjeTrans.Levuke4  THEN 
          BESTILLING_FINNES_FRA_FOR:
          DO:
            FIND FIRST Strekkode OF VarebehLinjeTrans NO-LOCK NO-ERROR.
            IF AVAIL Strekkode THEN
              FIND FIRST StrKonv OF Strekkode NO-LOCK NO-ERROR.
            ELSE 
              FIND FIRST StrKonv WHERE StrKonv.StrKode = -242984953 NO-LOCK NO-ERROR.
            PUT UNFORMATTED 
                ENTRY(ix,cButikkListe)                + "~t" +
                STRING(VarebehLinjeTrans.ArtikkelNr) + "~t" +
                (IF AVAIL StrKonv THEN TRIM(StrKonv.Storl) ELSE VarebehLinjeTrans.kode) + "~t" +
                STRING(bVarebehLinjeTrans.LevUke1) + "~t" + 
                STRING(bVarebehLinjeTrans.Bestilt1) + "~t" + 
                STRING(bVarebehLinjeTrans.LevUke2) + "~t" + 
                STRING(bVarebehLinjeTrans.Bestilt2) + "~t" + 
                STRING(bVarebehLinjeTrans.LevUke3) + "~t" + 
                STRING(bVarebehLinjeTrans.Bestilt3) + "~t" + 
                STRING(bVarebehLinjeTrans.LevUke4) + "~t" + 
                STRING(bVarebehLinjeTrans.Bestilt4) SKIP. 
            bEksisterer = TRUE.
          END. /* BESTILLING_FINNES_FRA_FOR */             
        END. /* KOPIERTRANS */
      END. /* BUTIKK_LOOP*/
    END.
  END.

  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hQuery.
DELETE OBJECT hTempTable.

IF ocReturn = "" THEN DO:
  obOk = TRUE.
  IF bEksisterer THEN ocReturn = cFileName.
END.

