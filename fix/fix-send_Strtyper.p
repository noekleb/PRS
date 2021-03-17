
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFilnavn           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c2Filnavn          AS CHAR       NO-UNDO.
DEFINE VARIABLE cNumericFormat     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDateFormat        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButListe          AS CHARACTER  NO-UNDO.
DEFINE VAR      cSentrallagerliste AS CHAR NO-UNDO.
DEFINE VAR      FI-Eksportdir      AS CHAR NO-UNDO.
DEF VAR piAnt AS INT NO-UNDO. 

DEFINE TEMP-TABLE TT_Elogg             NO-UNDO LIKE Elogg.
DEFINE TEMP-TABLE TT_StrKonv          LIKE StrKonv.
DEFINE TEMP-TABLE TT_StrTStr          LIKE StrTStr.
DEFINE TEMP-TABLE TT_StrType          LIKE StrType.

DEF TEMP-TABLE tmpStrType
    FIELD StrtypeId AS INT FORMAT ">>>>>>>>9"
    INDEX StrTypeId StrTypeId.


FUNCTION getAntLevSAnt RETURNS INTEGER
  ( INPUT iLevNr AS INTEGER, INPUT cSortID AS CHARACTER )  FORWARD.

FUNCTION getAntStrTStr RETURNS INTEGER
  ( INPUT iStrTypeID AS INTEGER )  FORWARD.


BYGG_BUTIKK_LISTE:
FOR EACH Butiker NO-LOCK WHERE
    Butiker.CL = Butiker.Butik AND
    Butiker.ApningsDato > 01/01/2000 AND
    Butiker.NedlagtDato = ?:
    
    ASSIGN
      cSentrallagerliste = cSentrallagerliste + (IF cSentrallagerliste = '' THEN '' ELSE ',') + string(Butiker.Butik).

END. /* BYGG_BUTIKK_LISTE */

/* Rense litt */
FOR EACH tmpStrType:
    DELETE tmpStrType.
END.

/* Alle størrelser skal ut en gang. */
FOR EACH StrKonv EXCLUSIVE-LOCK:
    StrKonv.ETid = TIME.
END.

/* Leser bare nye artikler */
ART_BLOKK:
FOR EACH StrType NO-LOCK WHERE
    StrType.StrTypeid >= 70000:
    
    CREATE tmpStrType.
    ASSIGN
        tmpStrType.StrTypeId = StrType.StrTypeId
        piant = piAnt + 1.
END. /* ART_BLOKK */

BLOKK_500:
DO WHILE CAN-FIND(FIRST tmpStrType):
    RUN ByggELogg.

    ASSIGN /*cSentrallagerliste     = '1,176'*/
           cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy"
           FI-Eksportdir          = 'c:\home\lindbak\sendes\test'
           .
    DO:
      IF cSentrallagerliste = "" THEN DO:
          MESSAGE "Ingen butikk registrert som sentrallager."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ASSIGN 
          cFilnavn = FI-Eksportdir + "\" + "HK" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","").
      EMPTY TEMP-TABLE TT_Elogg.

      OUTPUT TO VALUE(cFilnavn).
        RUN SendStrKonv.
        RUN SendStrType.
      OUTPUT CLOSE.
    END.

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
    FILE-INFO:FILE-NAME = cFilnavn.
    IF NOT FILE-INFO:FILE-SIZE > 0 THEN DO:
        MESSAGE "Ingen data for eksport"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO: 
        ASSIGN 
            c2Filnavn = REPLACE(cFilnavn,"HK","HKVPI")
            cButListe = cSentrallagerliste.
        DO iCount = 1 TO NUM-ENTRIES(cButListe):
            OS-COPY VALUE(cFilnavn) VALUE(c2Filnavn + "." + ENTRY(iCount,cButListe)).
        END.
    END.
    OS-DELETE VALUE(cFilnavn).
END. /* BLOKK_500 */

PROCEDURE KopierElogg:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  
  FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND ELogg.EksterntSystem = "POS":
      CREATE TT_Elogg.
      BUFFER-COPY Elogg TO TT_Elogg.
      RELEASE TT_Elogg.
      ASSIGN Elogg.Behandlet = TRUE
             iAntSlett   = iAntSlett   + IF Elogg.EndringsType = 3 THEN 1 ELSE 0
             iAntNyEndre = iAntNyEndre + IF Elogg.EndringsType = 1 AND ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
      DELETE ELogg.
  END.
END PROCEDURE.

PROCEDURE SendStrKonv:

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell  StrKonv
    &SCOPED-DEFINE KeyFelt StrKode
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    
    RUN KopierElogg (cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell}.StrKode       
                                 TT_{&Tabell}.Storl         
                                 TT_{&Tabell}.Merknad       
                                 TT_{&Tabell}.EDato         
                                 TT_{&Tabell}.ETid          
                                 TT_{&Tabell}.BrukerID      
                                 TT_{&Tabell}.RegistrertDato
                                 TT_{&Tabell}.RegistrertTid 
                                 TT_{&Tabell}.RegistrertAv.
        END.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.StrKode       
                                     {&Tabell}.Storl         
                                     {&Tabell}.Merknad       
                                     {&Tabell}.EDato         
                                     {&Tabell}.ETid          
                                     {&Tabell}.BrukerID      
                                     {&Tabell}.RegistrertDato
                                     {&Tabell}.RegistrertTid 
                                     {&Tabell}.RegistrertAv.
        END.
    END.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

PROCEDURE SendStrType:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell_1  StrType
    &SCOPED-DEFINE Tabell_2  StrTStr
    &SCOPED-DEFINE KeyFelt_1 StrTypeID
    &SCOPED-DEFINE KeyFelt_2 SoStorl
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn_1 AS CHARACTER  INIT "{&Tabell_1}" NO-UNDO.
    DEFINE VARIABLE cTabellNavn_2 AS CHARACTER  INIT "{&Tabell_2}" NO-UNDO.

    DEF VAR iAntall AS INT NO-UNDO.
    DEF VAR iLoop   AS INT NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell_1}.
    EMPTY TEMP-TABLE TT_{&Tabell_2}.

    RUN KopierEloggStrType (cTabellNavn_1,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn_1 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell_1}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn_1 AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell_1}.{&KeyFelt_1} = INT(TT_Elogg.Verdier).
            EXPORT TT_{&Tabell_1}.StrTypeID     
                   TT_{&Tabell_1}.Beskrivelse   
                   TT_{&Tabell_1}.Intervall     
                   TT_{&Tabell_1}.Fordeling     
                   TT_{&Tabell_1}.KortNavn      
                   TT_{&Tabell_1}.EDato         
                   TT_{&Tabell_1}.ETid          
                   TT_{&Tabell_1}.BrukerID      
                   TT_{&Tabell_1}.RegistrertDato
                   TT_{&Tabell_1}.RegistrertTid 
                   TT_{&Tabell_1}.RegistrertAv  
                   TT_{&Tabell_1}.AlfaFordeling.
        END.
    END.

    IF iAntNyEndre > 0 THEN DO:
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn_1 AND TT_ELogg.EksterntSystem = "POS"
                            AND TT_Elogg.EndringsType = 1:
            EXPORT "H" cTabellNavn_1 1 "1.0" 1.
            FIND {&Tabell_1} WHERE {&Tabell_1}.{&KeyFelt_1} = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell_1} THEN DO:
                EXPORT {&Tabell_1}.StrTypeID     
                       {&Tabell_1}.Beskrivelse   
                       {&Tabell_1}.Intervall     
                       {&Tabell_1}.Fordeling     
                       {&Tabell_1}.KortNavn      
                       {&Tabell_1}.EDato         
                       {&Tabell_1}.ETid          
                       {&Tabell_1}.BrukerID      
                       {&Tabell_1}.RegistrertDato
                       {&Tabell_1}.RegistrertTid 
                       {&Tabell_1}.RegistrertAv  
                       {&Tabell_1}.AlfaFordeling.
                ASSIGN
                    iAntall = INT(getAntStrTStr({&Tabell_1}.{&KeyFelt_1}))
                    iLoop   = 0.
                IF iAntall > 48 THEN iAntall = 48.
                EXPORT "H" cTabellNavn_2 1 "1.0" STRING(iAntall).
                LOOPEN:
                FOR EACH {&Tabell_2} OF {&Tabell_1} USE-INDEX StrTStr:
                    iLoop = iLoop + 1.
                    EXPORT {&Tabell_2}.StrTypeID     
                           {&Tabell_2}.SoStorl       
                           {&Tabell_2}.SeqNr         
                           {&Tabell_2}.EDato         
                           {&Tabell_2}.ETid          
                           {&Tabell_2}.BrukerID      
                           {&Tabell_2}.RegistrertDato
                           {&Tabell_2}.RegistrertTid 
                           {&Tabell_2}.RegistrertAv.  
                    IF iLoop >= 48 THEN LEAVE LOOPEN.
                END. /* LOOPEN */
            END.
        END.
    END.
    &UNDEFINE Tabell_1
    &UNDEFINE Tabell_2
    &UNDEFINE KeyFelt_1
    &UNDEFINE KeyFelt_2
END PROCEDURE.

PROCEDURE KopierEloggStrType:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  
  FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND ELogg.EksterntSystem = "POS".
      IF Elogg.EndringsType = 3 THEN DO:
          IF NUM-ENTRIES(ELogg.Verdier) = 2 THEN DO:
              IF NOT CAN-FIND(FIRST StrType WHERE StrType.StrTypeID = INT(ENTRY(1,ELogg.Verdier))) THEN DO:
                  IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                                            TT_ELogg.EksterntSystem = "POS" AND
                                            TT_ELogg.Verdier = ENTRY(1,ELogg.Verdier)) THEN DO:
                      CREATE TT_ELogg.
                      BUFFER-COPY ELogg EXCEPT Verdier TO TT_Elogg.
                      ASSIGN TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier).
                      RELEASE TT_Elogg.
                      ASSIGN iAntSlett   = iAntSlett + 1.
                  END.
              END.
              ELSE DO:
                  IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                                            TT_ELogg.EksterntSystem = "POS" AND
                                            TT_ELogg.Verdier = ENTRY(1,ELogg.Verdier)) THEN DO:
                      CREATE TT_ELogg.
                      BUFFER-COPY ELogg EXCEPT Verdier EndringsType TO TT_Elogg.
                      ASSIGN TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier)
                             TT_Elogg.EndringsType = 1.
                      RELEASE TT_Elogg.
                      ASSIGN iAntNyEndre = iAntNyEndre + 1.
                  END.
              END.
          END.
      END.
      ELSE DO:
          IF NOT CAN-FIND(FIRST TT_Elogg WHERE TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier)) THEN DO:
              CREATE TT_ELogg.
              BUFFER-COPY ELogg EXCEPT Verdier TO TT_Elogg.
              ASSIGN TT_Elogg.Verdier = ENTRY(1,ELogg.Verdier).
              RELEASE TT_Elogg.
              ASSIGN iAntNyEndre = iAntNyEndre + IF ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
          END.
      END.
      ASSIGN Elogg.Behandlet = TRUE.
      DELETE Elogg.
  END.
END PROCEDURE.

FUNCTION getAntLevSAnt RETURNS INTEGER
  ( INPUT iLevNr AS INTEGER, INPUT cSortID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAnt AS INTEGER    NO-UNDO.
  FOR EACH LevSAnt WHERE LevSAnt.LevNr = iLevNr AND LevSAnt.SortID = cSortID NO-LOCK.
      ASSIGN iAnt = iAnt + 1.
  END.
  RETURN iAnt.   /* Function return value. */
END FUNCTION.

FUNCTION getAntStrTStr RETURNS INTEGER
  ( INPUT iStrTypeID AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAnt AS INTEGER    NO-UNDO.

  FOR EACH StrTStr WHERE StrTStr.StrTypeID = iStrTypeID NO-LOCK.
      ASSIGN iAnt = iAnt + 1.
  END.
  RETURN iAnt.   /* Function return value. */

END FUNCTION.


PROCEDURE ByggELogg:
  DEF VAR piAntall AS INT NO-UNDO.
  BLOKK:
  FOR EACH tmpStrType NO-LOCK:
      piAntall = piAntall + 1.
      IF piAntall > 500 THEN LEAVE BLOKK.

      FIND ELogg WHERE 
           ELogg.TabellNavn     = "StrType" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(tmpStrType.StrTypeID) NO-ERROR.
      IF NOT AVAIL Elogg THEN DO:
          CREATE Elogg.
          ASSIGN ELogg.TabellNavn     = "StrType"
                 ELogg.EksterntSystem = "POS"   
                 ELogg.Verdier        = STRING(tmpStrType.StrTypeID).
      END.
      ASSIGN ELogg.EndringsType = 1
             ELogg.Behandlet    = FALSE.
      RELEASE ELogg.
      DELETE tmpStrType.
  END. /* BLOKK */
END PROCEDURE.
