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

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cParaString AS CHARACTER INIT "1050000001|" NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
&ENDIF


DEF VAR hHodeTH      AS HANDLE     NO-UNDO.
DEF VAR hLinjeTH     AS HANDLE     NO-UNDO.
DEF VAR hTTHodeBuff  AS HANDLE     NO-UNDO.
DEF VAR hTTLinjeBuff AS HANDLE     NO-UNDO.
DEF VAR iAntLinjer   AS INT        NO-UNDO.
DEF VAR iAlle        AS INT        NO-UNDO.
DEF VAR bStream      AS LOG        NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"    NO-UNDO.
DEF VAR cKatalog   AS CHAR                   NO-UNDO.
DEF VAR cPrefix    AS CHAR                   NO-UNDO.
DEF VAR cEkstent   AS CHAR                   NO-UNDO.
DEF VAR iSekvens   AS INT  FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR cEDBSystem AS CHAR INITIAL "EkspFAK" NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN */
/*     RETURN.                                                      */

{syspara.i 19 101 1 iAlle INT}

RUN PopulateTT.

/* Når det kommer flere formater, håndter dem på    */
/* denne måten. Da kjøres utlegget for de formatene */
/* som er satt aktive.                              */
/* EDB systemene skal være opprettet via sysinit.p  */
/* Bruk Case for å teste hvilken eksportrutine som  */
/* skal kjøres.                                     */
/* For each EkstEDBSystem where                     */
/*   EkstEDBSystem.EDBSystem begins "EkspFAK" and   */
/*   EkstEDBSystem.Aktiv = true:                    */

  RUN EksporterFaktura. /* (EkstEDBSystem.EDBSystem)  */
/* end. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EDBSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EDBSystem Procedure 
PROCEDURE EDBSystem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND EkstEDBSystem WHERE
        EkstEDBSystem.EDBSystem = cEDBSystem EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE EkstEDBSystem THEN DO:
        LEAVE.
    END.
    ELSE DO:
        ASSIGN
            cKatalog           = TRIM(EkstEDBSystem.Filkatalog,"\")
            cPrefix            = EkstEDBSystem.FilPrefix
            cEkstent           = trim(EkstEDBSystem.FilEkstent,".")
            iSekvens           = IF (EkstEDBSystem.SeqNr + 1) > EkstEDBSystem.MaksSeq
                                    THEN 1
                                    ELSE EkstEDBSystem.SeqNr + 1
            EkstEDBSystem.SeqNr = iSekvens
            cFilNavn           = cKatalog + "\" +
                                 cPrefix  + 
                                 STRING(iSekvens,"99999999") + "." + 
                                 cEkstent
            .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterFaktura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFaktura Procedure 
PROCEDURE EksporterFaktura :
/*------------------------------------------------------------------------------
  Purpose:     Utlegg av fakturadata til Visma Global. Opprinnelig for Dampbageriet.
               Et enkelt utlegg som er basert på at priser og rabatter hentes og 
               beregnes fra Visma Global.
               Legg merke til at heller ikke størrelse er lagt ut, da denne forutsettes
               alltid være 1.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.

   DEF VAR cTekst AS CHAR NO-UNDO.

   CREATE QUERY qH.
   CREATE QUERY qL.
   qL:SET-BUFFERS(hTTLinjeBuff).
   qH:SET-BUFFERS(hTTHodeBuff).
   qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
   qH:QUERY-OPEN().


   /* Legger ut data til fil */
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       /* Kun ikke tidligere eksportere faktura legges ut. */
       IF date(hTTHodeBuff:BUFFER-FIELD("EksportertDato"):BUFFER-VALUE) = ? OR
           iAlle = 0 /* Alle */ THEN
       DO:
           /* Leser alle fakturalinjene for gjeldende faktura */
           qL:QUERY-OPEN().
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iAntLinjer = iAntLinjer + 1.

               /* Header */
               IF iantLinjer = 1 THEN
               DO:
                   /* Setter filnavn */
                   RUN EDBSystem.        /* (EkstEDBSystem.EDBSystem)  */
                   IF cFilnavn = "" THEN
                       RETURN.

                   /* Åpner stream */
                   OUTPUT TO VALUE(cFilnavn) NO-ECHO.
                   /* Header */
                   put unformatted
                       "Ordrenr;" + 
                       "KundeNr;" + 
                       "Dato;" + 
                       "Ref.Nr;" + 
                       "Ref.tekst;" +
                       "ArtikkelNr;" + 
                       "Varetekst;" + 
                       "Antall;" + 
                       "LinjeSum;" + 
                       "Rabatt"  
                       SKIP.
                   /* Flagger at stream er åpnet */
                   bStream = TRUE.
               END.

               ASSIGN
                   cTekst = STRING(hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE)
                   .
               if available Strekkode then release Strekkode.
               FIND FIRST Strekkode NO-LOCK WHERE
                   Strekkode.ArtikkelNr = DEC(hTTLinjeBuff:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
               /* Utlegg av rad. */
               PUT UNFORMATTED 
                   hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE ";"
                   hTTHodeBuff:BUFFER-FIELD("Kundekort"):BUFFER-VALUE ";"
                   cTekst ";"
                   hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE ";"
                   hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE ";"
                   (IF AVAILABLE Strekkode 
                      THEN Strekkode.Kode
                      ELSE hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) ";"
                   hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE ";"
                   hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE ";"
                   hTTLinjeBuff:BUFFER-FIELD("LinjeSum"):BUFFER-VALUE ";"
                   "0"
                   SKIP.

               qL:GET-NEXT().
           END.
       END.
       
       /* Neste post */
       qH:GET-NEXT().
   END.

   /* Flagger fakturaene som eksportert. */
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       FIND FakturaHode EXCLUSIVE-LOCK WHERE
           FakturaHode.Faktura_Id = dec(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE) NO-ERROR.
       IF AVAILABLE FakturaHode AND FakturaHode.EksportertDato = ? THEN
       DO:
           ASSIGN
               FakturaHode.EksportertDato = TODAY
               FakturaHode.EksportertAv   = USERID("SkoTex")
               .
       END.
       /* Neste post */
       qH:GET-NEXT().
   END.

   qH:QUERY-CLOSE().
   DELETE OBJECT qH.
   qL:QUERY-CLOSE().
   DELETE OBJECT qL.

   /* Lukker stream */
   IF bStream THEN
       OUTPUT CLOSE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTT Procedure 
PROCEDURE PopulateTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_fakturahode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_fakturalinje.p",cParaString,?).
    hTTHodeBuff  = hHodeTH:DEFAULT-BUFFER-HANDLE.
    hTTLinjeBuff = hLinjeTH:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

