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
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER iAntLinjer  AS INT    NO-UNDO.
DEF INPUT  PARAMETER iFilType    AS INT    NO-UNDO.
DEF INPUT  PARAMETER cLinje      AS CHAR   NO-UNDO.
DEF OUTPUT PARAMETER iButikkNr   AS INT    NO-UNDO.
DEF OUTPUT PARAMETER iGruppeNr   AS INT    NO-UNDO.
DEF OUTPUT PARAMETER iKasseNr    AS INT    NO-UNDO.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.
DEF VAR cFilNavn        AS CHAR NO-UNDO.
DEF VAR iEntry          AS INTEGER NO-UNDO.
/* Linjen flyttad som INPUT:
      Om iAntlinjer = 0 läses den från fil annars anväds IUNPUT linjen */
/* DEF VAR cLinje          AS CHAR NO-UNDO. */

DEF STREAM InnFil.

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

FIND Filer NO-LOCK WHERE
    Filer.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE Filer THEN
DO:
    RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " ** Ukjent filer post sendt til <koblekasse.p> (" + STRING(lFilId) + ").") NO-ERROR.

    RETURN " ** Ukjent Filer post (" + STRING(lFilId) + ")  sendt til <koblekasse.p>.".
END.

CASE iFilType:
    WHEN 1 THEN RUN KobleElJournal.
    WHEN 2 THEN RUN KobleKvittering.
    WHEN 3 THEN RUN KobleUtskriftskopi.
    WHEN 4 THEN RUN KobleDagsoppgjor.
    WHEN 5 THEN RUN KobleKassererOppgjor.
    OTHERWISE RETURN " ** Ukjent filtype. Kan ikke kobles.".
END CASE.

RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-KobleElJournal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleElJournal Procedure 
PROCEDURE KobleElJournal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcKvitteringId AS CHAR NO-UNDO.

  DEF VAR piButikkNr AS INT NO-UNDO.
  DEF VAR piGruppeNr AS INT NO-UNDO.
  DEF VAR piKasseNr  AS INT NO-UNDO.
  DEFINE VARIABLE iInt AS INTEGER NO-UNDO.

  ASSIGN
      iButikkNr = 0
      iGruppeNr = 0
      iKasseNr  = 0
      .
  /* Om iAntLinjer = 0 Leser første linjen i filen. */
  IF iAntLinjer = 0 THEN DO:
      INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
      IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
      ASSIGN
          iAntLinjer = iAntLinjer + 1
          .
      INPUT STREAM InnFil CLOSE.
  END.
  /* 
   ELSE har vi fått en post från anropande program
   */
  /* Finner ut hvilken kasse filen kommer fra */
  KASSE:
  FOR EACH Kasse NO-LOCK WHERE Kasse.Aktiv = TRUE
    /* TEST */ /*AND Kasse.ButikkNr = INT(ENTRY(2,Filer.FilNavn,'.'))*/ :

    ASSIGN
      pcKvitteringId = Kasse.ElJournalId
      .
    /* Setter inn data i "variable" posisjoner i masken for de kassene som . */
    /* må ha dette.                                                          */
    CASE Kasse.ModellNr:
      WHEN 1 THEN DO:
          /* Legger inn dato i masken. */
          OVERLAY(pcKvitteringId, 5, 6) = SUBSTRING(cLinje,5,6).
          ASSIGN
            pcKvitteringId = "*" + pcKvitteringId + "*"
            .
      END.
      WHEN  5 THEN DO: /* PRSPos 1.0 */
          ASSIGN
            pcKvitteringId = pcKvitteringId + "*"
            .
      END.
      WHEN 10 THEN DO: /* InfoPOS kasse */
          ASSIGN
            pcKvitteringId = pcKvitteringId + "*"
            .
      END.
      WHEN 11 THEN DO: /* PRS Nettbutikk 1.0 */
          ASSIGN
            pcKvitteringId = pcKvitteringId + "*"
            .
      END.
      WHEN 20 THEN DO: /* Hugin/Sveda S9500 kasse */
          ASSIGN
            pcKvitteringId = "*" + pcKvitteringId
            .
      END.
      WHEN 40 THEN DO: /* StorePoint kasse */
          ASSIGN
            pcKvitteringId = pcKvitteringId + "*"
            .
      END.
      WHEN 50 THEN DO: /* Wayne DTL 2.0 ISM Nucleus */
          ASSIGN
            pcKvitteringId = "*" + pcKvitteringId
            .
      END.
      WHEN 51 THEN DO: /* Wayne N9 BOS Nucleus */
          ASSIGN
            pcKvitteringId = "*" + pcKvitteringId
            .
      END.
    END CASE.

    /* PRS Pos kasse */
    IF Kasse.ModellNr = 5 THEN 
    DO:
       ASSIGN iInt = INT(ENTRY(2,Filer.FilNavn,'.')) NO-ERROR.
       IF ERROR-STATUS:ERROR THEN 
       DO:
         pcKvitteringId = ''. /* Dummy */
         NEXT KASSE.
       END.
         
       /* Feil butikk */
       IF Kasse.ButikkNr <> INT(ENTRY(2,Filer.FilNavn,'.')) THEN 
         NEXT KASSE.

       ASSIGN
           iButikkNr      = INT(ENTRY(2,Filer.FilNavn,'.'))
           iGruppeNr      = 1
           iKasseNr       = INT(ENTRY(2,Filer.FilNavn,'_'))
           NO-ERROR.
       RUN NyFilLogg IN h_Parent
                     (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Filen er koblet til butikk/gruppe/kasse: " + 
                      STRING(iButikkNr) + " / " + 
                      STRING(iGruppeNr) + " / " +
                      STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
       IF iButikknr > 0 AND iGruppeNr > 0 AND iKasseNr > 0 
         THEN LEAVE KASSE.
         ELSE NEXT KASSE. 
    END.

    /* InfoPOS 8.0 */
    ELSE IF (Kasse.ModellNr = 10 OR Kasse.ModellNr = 11) THEN 
    DO:
       ASSIGN iInt = INT(ENTRY(NUM-ENTRIES(Filer.FilNavn,'.'),Filer.FilNavn,'.')) NO-ERROR.
       IF ERROR-STATUS:ERROR THEN 
         NEXT KASSE.
       /* Feil butikk */
       IF Kasse.ButikkNr <> INT(ENTRY(NUM-ENTRIES(Filer.FilNavn,'.'),Filer.FilNavn,'.')) THEN
         NEXT KASSE.
       /*-----------------------------*/     
        INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
        FINN-KASSENR:
        REPEAT:
              IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
              IF NUM-ENTRIES(cLinje,";") >= 2 THEN
              DO:

                  IF INT(ENTRY(2,cLinje,';')) = Kasse.KasseNr THEN
                  DO:
                      ASSIGN
                          iButikkNr = Kasse.ButikkNr
                          iGruppeNr = 1
                          iKasseNr  = Kasse.KasseNr 
                          .
                      LEAVE FINN-KASSENR.
                  END.
                  ELSE DO: 
                    INPUT STREAM InnFil CLOSE.
                    NEXT KASSE.
                  END.
              END.
              ELSE DO: 
                INPUT STREAM InnFil CLOSE.
                NEXT KASSE.
              END.
        END. /* FINN-KASSENR */
        INPUT STREAM InnFil CLOSE.
       /*-----------------------------*/    
       RUN NyFilLogg IN h_Parent
                     (INPUT lFilId, STRING(TODAY) + " " + 
                       STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Filen er koblet til butikk/gruppe/kasse: " + 
                      STRING(iButikkNr) + " / " + 
                      STRING(iGruppeNr) + " / " +
                      STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
       IF iButikknr > 0 AND iGruppeNr > 0 AND iKasseNr > 0 
         THEN LEAVE KASSE.
         ELSE NEXT KASSE. 
    END.
    /* Hugin/Sveda kassene kobles til butikk og kasse ved hjelp av katalognavn. */
    ELSE IF Kasse.ModellNr = 20 AND 
       Filer.FilNavn BEGINS "LD" AND
       Filer.FilNavn MATCHES "*.DBF" THEN
    HUGIN-SVEDA:
    DO:
       IF Filer.Katalog MATCHES pcKvitteringId THEN
       DO:
           ASSIGN
               iButikkNr      = Kasse.ButikkNr
               iGruppeNr      = Kasse.GruppeNr
               iKasseNr       = Kasse.KasseNr
               .
           RUN NyFilLogg IN h_Parent
                         (INPUT lFilId, STRING(TODAY) + " " + 
                           STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                          " - Filen er koblet til butikk/gruppe/kasse: " + 
                          STRING(iButikkNr) + " / " + 
                          STRING(iGruppeNr) + " / " +
                          STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
           LEAVE KASSE.
       END.
    END. /* HUGIN-SVEDA */
    /* StorePoint kasse */
    /* Første 40 kassen vi kommer inn her med, dykker ned i transfilen og finner */
    /* butikk og kassenr. ButikkNr er gitt av pcKvitteringsId. Deretter må filen */
    /* leses til den første transkode 16 kommer. I entry 5 i den recorden står   */
    /* kassanummeret. Deretter avsluttes koblingen.                              */
    ELSE IF Kasse.ModellNr = 40 AND
         Filer.FilNavn BEGINS "DTBR" THEN
    STOREPOINT:
    DO:
        INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
        FINN-KASSENR:
        REPEAT:
              IMPORT STREAM InnFil UNFORMATTED cLinje NO-ERROR.
              IF NUM-ENTRIES(cLinje,"+") >= 4 THEN
              DO:
                  IF INT(TRIM(TRIM(ENTRY(4,cLinje),"+"),"-")) = 16 THEN
                  DO:
                      ASSIGN
                          iButikkNr = INT(TRIM(TRIM(ENTRY(1,cLinje),"+"),"-"))
                          iGruppeNr = 1
                          /*
                          iKasseNr  = INT(trim(trim(ENTRY(5,cLinje),"+"),"-"))                      
                          */
                          iKasseNr  = Kasse.KasseNr /* Vi overstyrer kassenummeret her */
                          .
                      LEAVE FINN-KASSENR.
                  END.
              END.
        END. /* FINN-KASSENR */
        INPUT STREAM InnFil CLOSE.
    END. /* STOREPOINT */
    /* Wayne DTL fil kobles ved hjelp av katalognavn. */
    ELSE IF Kasse.ModellNr = 50 AND 
       Filer.FilNavn BEGINS "DTL" AND
       Filer.FilNavn MATCHES "*.TXT" THEN
    WAYNE:
    DO:
       IF Filer.Katalog MATCHES pcKvitteringId THEN
       DO:
           ASSIGN
               iButikkNr      = Kasse.ButikkNr
               iGruppeNr      = Kasse.GruppeNr
               iKasseNr       = Kasse.KasseNr
               .
           RUN NyFilLogg IN h_Parent
                         (INPUT lFilId, STRING(TODAY) + " " + 
                           STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                          " - Filen er koblet til butikk/gruppe/kasse: " + 
                          STRING(iButikkNr) + " / " + 
                          STRING(iGruppeNr) + " / " +
                          STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
           LEAVE KASSE.
       END.
    END. /* WAYNE */
    /* Wayne N9 BOS fil kobles ved hjelp av katalognavn. */
    ELSE IF Kasse.ModellNr = 51 AND 
       Filer.FilNavn BEGINS "POSEvent" AND
       Filer.FilNavn MATCHES "*.xml" THEN
    WAYNE:
    DO:
       /* ButikkNr ligger nederst i katalogstrukturen */
       IF Filer.Katalog MATCHES pcKvitteringId OR 
          SUBSTRING(ENTRY(2,Filer.FilNavn,"_"),1,5) = LEFT-TRIM(pcKvitteringId,'*') THEN
       DO:
           ASSIGN
               iButikkNr      = Kasse.ButikkNr
               iGruppeNr      = Kasse.GruppeNr
               iKasseNr       = Kasse.KasseNr
               .
           RUN NyFilLogg IN h_Parent
                         (INPUT lFilId, STRING(TODAY) + " " + 
                           STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                          " - Filen er koblet til butikk/gruppe/kasse: " + 
                          STRING(iButikkNr) + " / " + 
                          STRING(iGruppeNr) + " / " +
                          STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
           LEAVE KASSE.
       END.
    END. /* WAYNE */
    /*
    /* Kobler kassen. */
    ELSE IF cLinje MATCHES pcKvitteringId THEN
    DO:
        ASSIGN
            iButikkNr      = Kasse.ButikkNr
            iGruppeNr      = Kasse.GruppeNr
            iKasseNr       = Kasse.KasseNr
            .
        RUN NyFilLogg IN h_Parent
                      (INPUT lFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                       " - Filen er koblet til butikk/gruppe/kasse: " + 
                       STRING(iButikkNr) + " / " + 
                       STRING(iGruppeNr) + " / " +
                       STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
        
MESSAGE 'Gurre' SKIP cLinje SKIP pcKvitteringid
VIEW-AS ALERT-BOX.        

        LEAVE KASSE.
    END.
    */
  END. /* KASSE */

  /* Ingen match funnet */
  IF (iButikkNr = 0 OR
      iGruppeNr = 0 OR
      iKasseNr  = 0) THEN
  DO:
      RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " ** Ingen match på kasse funnet for transaksjon: " + cLinje + "). <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
      RETURN "Ingen match på kasse funnet for transaksjon: " + cLinje + "). <RETURN-VALUE koblekasse.p>".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KobleKvittering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleKvittering Procedure 
PROCEDURE KobleKvittering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcKvitteringId AS CHAR NO-UNDO.

  ASSIGN
      iButikkNr = 0
      iGruppeNr = 0
      iKasseNr  = 0
      .

  /* Om iAntLinjer = 0 Leser første linjen i filen. */
  IF iAntLinjer = 0 THEN DO:
      INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
        IMPORT STREAM InnFil UNFORMATTED cLinje.
      INPUT STREAM InnFil CLOSE.
  END.
  /* 
   ELSE har vi fått en post från anropande program
   */

  /* Finner ut hvilken kasse filen kommer fra */
  KASSE:
  FOR EACH Kasse NO-LOCK:
    /* Kun disse kassetypene skal behandles. */
    IF NOT CAN-DO("1,30",STRING(Kasse.ModellNr)) THEN
        NEXT KASSE.

    ASSIGN
      pcKvitteringId = Kasse.KvitteringId
      .

    /* Setter inn data i "variable" posisjoner i masken for de kassene som . */
    /* må ha dette.                                                          */
    CASE Kasse.ModellNr:
      WHEN 1 THEN DO:
          /* Legger inn dato i masken. */
          OVERLAY(pcKvitteringId, 5, 6) = SUBSTRING(cLinje,5,6).
          ASSIGN
            pcKvitteringId = "*" + pcKvitteringId + "*"
            .
      END.
      WHEN 30 THEN DO: /* MegaDisc kasse (Det skal stå katalognavn her) */
          ASSIGN
            pcKvitteringId = "*" + pcKvitteringId + "*"
            .
      END.
    END CASE.

    /* MegaDisk kassene kobles til butikk og kasse ved hjelp av katalognavn. */
    IF Kasse.ModellNr = 30 AND 
       Filer.FilNavn BEGINS "MD" AND
       (Filer.FilNavn MATCHES "*.r1" OR Filer.FilNavn MATCHES "*.txt") THEN
    MEGADISK:
    DO:
       IF Filer.Katalog MATCHES pcKvitteringId THEN
       DO:
           ASSIGN
               iButikkNr      = Kasse.ButikkNr
               iGruppeNr      = Kasse.GruppeNr
               iKasseNr       = Kasse.KasseNr
               .
           RUN NyFilLogg IN h_Parent
                         (INPUT lFilId, STRING(TODAY) + " " + 
                           STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                          " - Filen er koblet til butikk/gruppe/kasse: " + 
                          STRING(iButikkNr) + " / " + 
                          STRING(iGruppeNr) + " / " +
                          STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
           LEAVE KASSE.
       END.
    END. /* MEGADISK */

    /* Kobler kassen. */
    ELSE IF cLinje MATCHES pcKvitteringId AND pcKvitteringId <> "*" THEN
    DO:

        ASSIGN
            iButikkNr      = Kasse.ButikkNr
            iGruppeNr      = Kasse.GruppeNr
            iKasseNr       = Kasse.KasseNr
            .
        RUN NyFilLogg IN h_Parent
                      (INPUT lFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                       " - Filen er koblet til butikk/gruppe/kasse: " + 
                       STRING(iButikkNr) + " / " + 
                       STRING(iGruppeNr) + " / " +
                       STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
        LEAVE KASSE.
    END.
  END. /* KASSE */



  /* Ingen match funnet */
  IF (iButikkNr = 0 AND
      iGruppeNr = 0 AND
      iKasseNr  = 0) THEN
  DO:
      RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " ** Klarte ikke koble: " + cLinje + "). <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
      RETURN "Ingen match på kasse funnet for transaksjon: " + cLinje + "). <RETURN-VALUE koblekasse.p>".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KobleUtskriftskopi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleUtskriftskopi Procedure 
PROCEDURE KobleUtskriftskopi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcUtskriftsKopid AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT  NO-UNDO.
  DEF VAR pcEkstent        AS CHAR NO-UNDO.

  ASSIGN
      iButikkNr = 0
      iGruppeNr = 0
      iKasseNr  = 0
      pcEkstent = ""
      .

  /* Leser igjennom filen til vi finner første linje hvor ID står. */
  INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  LOOPEN:
  REPEAT:

    IMPORT STREAM InnFil UNFORMATTED cLinje.
    IF cLinje = "" THEN
        NEXT LOOPEN.

    ASSIGN
        pcEkstent = (IF NUM-ENTRIES(Filer.FilNavn,".") = 2
                       THEN ENTRY(2,Filer.FilNavn,".")
                       ELSE "XXX")
        .

    /* Finner ut hvilken kasse filen kommer fra.                           */
    /* Benytter oss av at vi vet at denne filtypen har BBK (Butikk, kasse) */
    /* i filekstenten.                                                     */
    IF pcEkstent <> "XXX" THEN
    KASSE:
    FOR EACH Kasse NO-LOCK WHERE
        Kasse.Utskriftskopi[2] = pcEkstent AND
        Kasse.UtskriftsKopiId  <> "*" AND
        Kasse.UtskriftsKopiId  <> "":
   
      /* Kobler kassen. */
      IF cLinje MATCHES "*" + Kasse.UtskriftsKopiId + "*" THEN
      DO:
          ASSIGN
              iButikkNr      = Kasse.ButikkNr
              iGruppeNr      = Kasse.GruppeNr
              iKasseNr       = Kasse.KasseNr
              .
          RUN NyFilLogg IN h_Parent
                        (INPUT lFilId, STRING(TODAY) + " " + 
                          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                         " - Filen er koblet til butikk/gruppe/kasse: " + 
                         STRING(iButikkNr) + " / " + 
                         STRING(iGruppeNr) + " / " +
                         STRING(iKasseNr)  + ". <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
          LEAVE LOOPEN. /* Ferdig. */
      END.
    END. /* KASSE */

    /* Ferdig */
    IF iButikkNr <> 0 THEN
        LEAVE LOOPEN.
  END. /* LOOPEN */
  INPUT STREAM InnFil CLOSE.

  /* Ingen match funnet */
  IF (iButikkNr = 0 AND
      iGruppeNr = 0 AND
      iKasseNr  = 0) THEN
  DO:
      RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " ** Ingen match på kasse funnet for transaksjon: " + cLinje + "). <koblekasse.p linje " + STRING(iantLinjer) + ">") NO-ERROR.
      RETURN "Ingen match på kasse funnet for transaksjon: " + cLinje + "). <RETURN-VALUE koblekasse.p>".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

