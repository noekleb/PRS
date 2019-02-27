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

DEFINE INPUT  PARAMETER iTelleNr LIKE TelleHode.TelleNr NO-UNDO.
DEFINE INPUT  PARAMETER iHt      AS INTEGER NO-UNDO.

DEFINE VARIABLE cFilnavn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGTINFil   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFil2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAppend    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lVVarekost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iCL        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEkstent   AS CHAR      NO-UNDO.
DEFINE VARIABLE cButListe  AS CHAR      NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER NO-UNDO.

DEFINE STREAM Ut.
DEFINE STREAM Ut2.

DEF TEMP-TABLE PrisUt 
    FIELD ButikkNr AS INT FORMAT ">>9"
    FIELD Ean      AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Varetekst AS CHAR 
    FIELD BestNr    AS INT  
    FIELD Tilbud    AS LOG FORMAT "Ja/Nei"
    FIELD UtprisUt  AS DEC FORMAT ">>,>>9.99"
    FIELD UtprisInn AS DEC FORMAT ">>,>>9.99"
    INDEX Ean Ean.

DEFINE TEMP-TABLE PrisCubUt
    /*  1 */ FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
    /*  2 */ FIELD StrKode AS INTEGER FORMAT ">>>>9"
    /*  3 */ FIELD ModellFarge AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
    /*  4 */ FIELD Bestillingsnr AS CHARACTER FORMAT "x(30)"
    /*  5 */ FIELD VgLopNr AS CHARACTER FORMAT "x(30)"
    /*  6 */ FIELD Varetekst AS CHARACTER FORMAT "x(30)" 
    /*  7 */ FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
    /*  8 */ FIELD VgBeskr AS CHARACTER FORMAT "x(30)"
    /*  9 */ FIELD Vg AS INTEGER FORMAT ">>>>>9"
    /* 10 */ FIELD LopNr AS INTEGER FORMAT ">>>>>9"
    /* 11 */ FIELD Storl AS CHARACTER FORMAT "x(20)"
    /* 12 */ FIELD ButikkNr AS INTEGER FORMAT ">>>>>9"
    /* 13 */ FIELD PrisHK AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 14 */ FIELD PrisLokal AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 15 */ FIELD Varekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 16 */ FIELD VektetVarekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 17 */ FIELD Lagerstyrt AS LOG 
    /* 18 */ FIELD Strekkode AS CHARACTER FORMAT "x(30)"      
    /* 19 */ FIELD Interleave AS CHARACTER FORMAT "x(30)"
    /* 20 */ FIELD MvaKode AS INTEGER FORMAT ">9"
    /* 21 */ FIELD LinkVare AS CHARACTER FORMAT "x(20)"
    /* 22 */ FIELD Antall AS INTEGER FORMAT ">>>9"
    /* 23 */ FIELD AntallBulk AS INTEGER FORMAT ">>>9"
    INDEX Strekkode Strekkode.

DEF BUFFER clButiker FOR Butiker.

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
{syspara.i 5 1 1 iCl INT}

FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
    RETURN.

FIND TelleHode WHERE TelleHode.TelleNr = iTelleNr NO-LOCK NO-ERROR.
IF NOT AVAIL TelleHode THEN DO:
    MESSAGE "Finner ikke telling"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Henter filnavn fra PDA registeret. */
RUN InitFilnavn.

/* InfoPOS PRO hånterminal */
IF CAN-DO("6,8,9",STRING(iHt)) THEN
    RUN LeggUtVarefil.
ELSE IF CAN-DO("7",STRING(iHt)) THEN
    RUN LeggUtBxCentral.
/* CubComm PDA */
ELSE IF CAN-DO("12",STRING(iHt)) THEN
    RUN LeggUtCubVarefil.
ELSE IF CAN-DO("10",STRING(iHt)) THEN
  DO: 
    RUN LeggUtBxMobile.
    RUN eksportHTButikker.p(iHt,
                            cEksportKatalog,
                            'Butiker',
                            cEkstent
                            ).
    RUN eksportHTLev.p(iHt,
                            cEksportKatalog,
                            'Leverandor',
                            cEkstent
                            ).
  END.
/* Sport 1 håndterminal */
ELSE
    RUN LeggUtFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitFilnavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFilnavn Procedure 
PROCEDURE InitFilnavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDir  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLoop AS INTEGER    NO-UNDO.

  FIND HT-Type WHERE HT-Type.TypeId = iHt NO-LOCK.
  ASSIGN
      cEkstent = HT-Type.EkspFilEkstent
      .
  /* Her skal det spørres etter butikk */
  IF CAN-DO("6,7,10,12",STRING(iHt)) AND cEkstent = "<ButNr>" THEN
      cEkstent = TelleHode.ButikkListe.

  ASSIGN 
      cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(HT-Type.EksportKatalog), "\"),"/") + "\" + 
                      HT-Type.EkspFilPrefix + "." + cEkstent
      cGTINFil = REPLACE(cFilnavn,'Varer','GTIN')
      cEksportKatalog = RIGHT-TRIM(RIGHT-TRIM(TRIM(HT-Type.EksportKatalog), "\"),"/")
      .

  /* Oppretter katalogen hvis den ikke finnes. */
  ASSIGN cDir = ENTRY(1,cFilnavn,"\") + "\".
  DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
      cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
      OS-CREATE-DIR VALUE(cDir).
  END.
  
  IF SEARCH(cFilNavn) <> ? AND NOT CAN-DO("6,8,9",STRING(iHt)) THEN 
  DO:
      MESSAGE "Håndterminalfilen finnes fra før." SKIP
          "Ønsker du å legge til i den eksisterende filen?" SKIP
          "(Ja=Legge til, Nei=Erstatte)"
          VIEW-AS ALERT-BOX INFO BUTTONS YES-NO-CANCEL UPDATE lAppend.
      IF lAppend = ? THEN
          RETURN.
  END.
  ELSE 
      lAppend = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeggUtBxCentral) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggUtBxCentral Procedure 
PROCEDURE LeggUtBxCentral :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLoop   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLoopVL AS INTEGER    NO-UNDO.
    DEF VAR pcTekst   AS CHAR NO-UNDO.
    DEF VAR plEan     AS DEC  NO-UNDO.
    DEF VAR cLinje    AS CHAR NO-UNDO.
    DEF VAR cTxt     AS CHAR NO-UNDO.

    IF lAppend = TRUE THEN
        OUTPUT TO VALUE(cFilnavn) APPEND.
    ELSE
        OUTPUT TO VALUE(cFilnavn).
    FOR EACH TelleLinje OF TelleHode NO-LOCK BREAK BY TelleLinje.Butik BY TelleLinje.ArtikkelNr:
        IF FIRST-OF(ArtikkelNr) THEN DO:
            FIND ArtBas WHERE Artbas.Artikkelnr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
                ASSIGN iLoop = iLoop + 1.
                STREKKODE:
                FOR EACH Strekkode OF ArtBas WHERE StrekKode.KodeType = 1 NO-LOCK:
                    ASSIGN
                        pcTekst = TRIM(Strekkode.Kode)
                        plEan   = DEC(Strekkode.Kode)
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        NEXT STREKKODE.
                    IF pcTekst = "" THEN
                        NEXT STREKKODE.
                    IF LENGTH(pcTekst) <> 13 THEN
                        NEXT STREKKODE.
                    ctxt = TRIM(ArtBas.Beskr).
                    cTxt = IF cTxt <> "" THEN cTxt ELSE "Blank varetekst".

                    PUT UNFORMATTED
                        TRIM(ArtBas.LevKod) ';'
                        Strekkode.Kode ';'
                        '"' + cTxt + '"' ';'
                        ArtPris.VareKost[1] ';'
                        '1;'
                        '1;'
                        '1;'
                        '1;'
                        ArtPris.Pris[1]
                        SKIP.

                    ASSIGN iLoopVL = iLoopVL + 1.
                END.
            END.
        END.
    END.
    OUTPUT CLOSE.
    MESSAGE "Varetellingsfil: " cFilnavn + "." SKIP
            "Antall produkter eksportert " + STRING(iLoop) + "." SKIP
            "Antall varelinjer eksportert " + STRING(iLoopVL) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeggUtBxMobile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggUtBxMobile Procedure 
PROCEDURE LeggUtBxMobile :
/*------------------------------------------------------------------------------
  Purpose: Utlegg til BxMobile håndterminalprogram     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iLoop        AS INTEGER    NO-UNDO.
    DEF VAR iLoopVL      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntStrekkoder AS INTEGER NO-UNDO.
    DEF VAR lTilbud   AS LOG FORMAT "yes/no" NO-UNDO.
    DEF VAR lcKatalog AS CHAR NO-UNDO.
    DEF VAR pcTekst   AS CHAR NO-UNDO.
    DEF VAR plEan     AS DEC  NO-UNDO.
    DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTst AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE iMvaKode AS INTEGER NO-UNDO. 
        
    DEFINE BUFFER clArtPris FOR ArtPris.
    
    ASSIGN
      cFilNavn = cFilNavn + '_TMP'
      cGTINFil = cGTINFil + '_TMP'
      .
      
    /* Tømmer temp-table */
    FOR EACH PrisCubUt:
        DELETE PrisCubUt.
    END.

    FOR EACH TelleLinje OF TelleHode NO-LOCK 
        BREAK BY TelleLinje.TelleNr 
              BY TelleLinje.ArtikkelNr:

        IF FIRST-OF (TelleLinje.ArtikkelNr) THEN 
        DO:
            FIND ArtBas WHERE Artbas.Artikkelnr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
                IF AVAILABLE VarGr THEN FIND Moms OF VarGr NO-LOCK NO-ERROR.
                IF AVAILABLE VarGr 
                  THEN iMvaKode = VarGr.MomsKod.
                  ELSE iMvaKode = 3.
                FIND clArtPris NO-LOCK WHERE
                     clArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                     clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                FIND VarGr NO-LOCK OF ArtBas NO-ERROR.
                FIND butiker NO-LOCK WHERE
                    butiker.Butik = TelleLinje.butik NO-ERROR.
                IF AVAILABLE butiker THEN
                    FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = butiker.ProfilNr NO-ERROR.
                IF NOT AVAILABLE ArtPris THEN
                    FIND FIRST ArtPris OF ArtBas NO-ERROR.
                IF AVAILABLE ArtPris THEN
                    ltilbud = ArtPris.Tilbud.
                ELSE
                    lTilbud = FALSE.

                /* Vektet varekost - Kun for lagerstyrte varer. */
                IF AVAILABLE Lager THEN RELEASE Lager.
                IF ArtBas.Lager THEN
                DO:
                    FIND Lager NO-LOCK WHERE
                        Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                        Lager.Butik      = INT(cEkstent) NO-ERROR.
                    IF (AVAILABLE Lager AND Lager.VVareKost <> ? AND Lager.VVarekost > 0) 
                        THEN lVVarekost = Lager.VVareKost.
                        ELSE lVVareKost = (IF AVAILABLE ArtPris THEN ArtPris.VareKost[1] ELSE 0).
                END.
                ELSE lVVarekost = (IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0).    
                
                pcTekst = TRIM(REPLACE(ArtBas.Beskr,'"','')).
                pcTekst = (IF pcTekst = "" THEN "Blank varetekst" ELSE pcTekst).
                
                CREATE PrisCubUt.
                ASSIGN
                /*  1 */ PrisCubUt.ArtikkelNr     = ArtBas.ArtikkelNr 
                /*  2 */ PrisCubUt.Strekkode      = ''       
                /*  3 */ PrisCubUt.Varetekst      = REPLACE(REPLACE(REPLACE(TRIM(ArtBas.Beskr),';',','),CHR(10),''),CHR(13),'')
                /*  4 */ PrisCubUt.Varekost       = ArtPris.Varekost[1]
                /*  5 */ PrisCubUt.MvaKode        = iMvaKode
                /*  6 */ PrisCubUt.LinkVare       = ''
                /*  7 */ PrisCubUt.Antall         = 1
                /*  8 */ PrisCubUt.AntallBulk     = 1
                /*  9 */ PrisCubUt.PrisLokal      = ArtPris.Pris[1] 
                iLoopVL                  = iLoopVL + 1
                NO-ERROR.

            END.
        END.
    END.

    IF lAppend = TRUE THEN
        DO:
          OUTPUT TO VALUE(cFilnavn) APPEND. 
          OUTPUT STREAM Ut TO VALUE(cGTINFil) APPEND.
        END.
    ELSE DO:
          OUTPUT TO VALUE(cFilnavn).
          OUTPUT STREAM Ut TO VALUE(cGTINFil) APPEND.
        END.
 
    iLoopVL = 0.

    /* Legger ut tingene sortert. */
    FOR EACH PrisCubUt 
      BREAK BY PrisCubUt.ArtikkelNr
            BY PrisCubUt.Strekkode:
        PUT UNFORMATTED
            /*  1 */ PrisCubUt.ArtikkelNr ';'
            /*  2 */ PrisCubUt.Strekkode  ';'         
            /*  3 */ SUBSTRING(PriscubUt.Varetekst,1,60) ';'     
            /*  4 */ PrisCubUt.Varekost  ';'   
            /*  5 */ PrisCubUt.MvaKode  ';'        
            /*  6 */ PrisCubUt.LinkVare  ';'      
            /*  7 */ PrisCubUt.Antall  ';'     
            /*  8 */ PrisCubUt.AntallBulk  ';'        
            /*  9 */ PrisCubUt.PrisLokal
        SKIP.            
        
        ASSIGN 
            iLoop      = iLoop + 1
            cStrekkode = ''.
        STREKKODE:
        FOR EACH Strekkode NO-LOCK WHERE 
          Strekkode.ArtikkelNr = PrisCubUt.ArtikkelNr AND 
          TRIM(Strekkode.Kode) > '' 
          BREAK BY Strekkode.ArtikkelNr
                BY Strekkode.StrKode:

            FIND StrKonv NO-LOCK WHERE 
                 StrKonv.StrKode = Strekkode.StrKode NO-ERROR.

            ASSIGN
                pcTekst = TRIM(Strekkode.Kode)
                plEan   = DEC(Strekkode.Kode)
                NO-ERROR.
            IF ERROR-STATUS:ERROR = FALSE AND 
               LENGTH(TRIM(StrekKode.kode)) = 13 AND 
               TRIM(Strekkode.Kode) <> '' THEN 
            DO: 
              /* Trimmer bort ledende nuller i EAN8 koder. */
              cKode = Strekkode.Kode.
              IF cKode BEGINS '00000' THEN 
                cKode = SUBSTRING(cKode,6).  
                
              ASSIGN
              iAntStrekkoder  = iAntStrekkoder + 1
              cStrekkode      = cStrekkode + 
                                (IF cStrekkode = '' THEN '' ELSE ',') + 
                                cKode.
            END.
            IF TRIM(cStrekkode) <> '' THEN 
            DO:
                ASSIGN iLoopVL = iLoopVL + 1.
                PUT STREAM Ut UNFORMATTED
                /*  1 */ PrisCubUt.ArtikkelNr  ';'         
                /*  2 */ cStrekkode ';'
                /*  3 */ (IF AVAILABLE StrKonv THEN TRIM(StrKonv.Storl) ELSE '')        
                SKIP.            
            END.
            ASSIGN 
              cStrekkode  = ''.
        END. /* STREKKODE */
    END. /* PrisCubUt */
    
    OUTPUT STREAM Ut CLOSE.
    OUTPUT CLOSE.

    /* sist så skall ursprungligat tmp-filen byta namn */
    cFil2 = REPLACE(cFilnavn,"_TMP","").
    OS-DELETE VALUE(cFil2).
    OS-RENAME VALUE(cFilnavn) VALUE(cFil2).

    /* sist så skall ursprungligat tmp-filen byta namn */
    cFil2 = REPLACE(cGTINFil,"_TMP","").
    OS-DELETE VALUE(cFil2).
    OS-RENAME VALUE(cGTINFil) VALUE(cFil2).

    MESSAGE 
            "Opprettet varetellingsfil: " REPLACE(cFilnavn,"_TMP","") + "." SKIP
            "          Strekkodefil   : " REPLACE(cGTINFil,"_TMP","") + "." SKIP
            "Antall produkter lest " + STRING(iLoop) + "." SKIP
            "Antall strekkoder lest " + STRING(iAntStrekkoder) + "." SKIP 
            "Antall strekkoder eksportert " + STRING(iLoopVL) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeggUtCubVarefil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggUtCubVarefil Procedure 
PROCEDURE LeggUtCubVarefil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iLoop        AS INTEGER    NO-UNDO.
    DEF VAR iLoopVL      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntStrekkoder AS INTEGER NO-UNDO.
    DEF VAR lTilbud   AS LOG FORMAT "yes/no" NO-UNDO.
    DEF VAR lcKatalog AS CHAR NO-UNDO.
    DEF VAR pcTekst   AS CHAR NO-UNDO.
    DEF VAR plEan     AS DEC  NO-UNDO.
    DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c2av5Interleave AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTst AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.    
        
    DEFINE BUFFER clArtPris FOR ArtPris.
    
    /* Tømmer temp-table */
    FOR EACH PrisCubUt:
        DELETE PrisCubUt.
    END.

    FOR EACH TelleLinje OF TelleHode NO-LOCK 
        BREAK BY TelleLinje.TelleNr 
              BY TelleLinje.ArtikkelNr:

        IF FIRST-OF (TelleLinje.ArtikkelNr) THEN 
        DO:
            FIND ArtBas WHERE Artbas.Artikkelnr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                ASSIGN 
                  iLoop           = iLoop + 1
                  cStrekkode      = ''
                  c2Av5Interleave = ''.

                STREKKODE:
                FOR EACH Strekkode OF ArtBas NO-LOCK WHERE 
                  TRIM(Strekkode.Kode) > '' 
                  BREAK BY Strekkode.ArtikkelNr
                        BY Strekkode.StrKode:
                    ASSIGN
                        pcTekst = TRIM(Strekkode.Kode)
                        plEan   = DEC(Strekkode.Kode)
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR = FALSE AND 
                       LENGTH(TRIM(StrekKode.kode)) = 13 AND 
                       TRIM(Strekkode.Kode) <> '' THEN 
                    DO: 
                      /* Trimmer bort ledende nuller i EAN8 koder. */
                      cKode = Strekkode.Kode.
                      IF cKode BEGINS '00000' THEN 
                        cKode = SUBSTRING(cKode,6).  
                        
                      ASSIGN
                      iAntStrekkoder  = iAntStrekkoder + 1
                      cStrekkode      = cStrekkode + 
                                        (IF cStrekkode = '' THEN '' ELSE ',') + 
                                        cKode
                      c2Av5Interleave = c2Av5Interleave + 
                                       (IF c2Av5Interleave = '' THEN '' ELSE ',') + 
                                        Strekkode.BestillingsNummer.
                    END.
                    
                    /* Legger også ut 2AV5INTERLEAVE i strekkode feltet. */
                    IF LENGTH(TRIM(Strekkode.Bestillingsnummer)) = 12 THEN
                    INTERLEAVE: 
                    DO:
                        dTst = DECI(Strekkode.Bestillingsnummer) NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN
                          LEAVE INTERLEAVE.
                        ASSIGN
                          cStrekkode = cStrekkode + 
                                       (IF cStrekkode = '' THEN '' ELSE ',') + 
                                       Strekkode.Bestillingsnummer.
                    END. /* INTERLEAVE */                   

                    IF LAST-OF(Strekkode.StrKode) THEN
                    STREKKODE: 
                    DO:
                        FIND clArtPris NO-LOCK WHERE
                             clArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                             clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                        FIND StrKonv NO-LOCK WHERE 
                            StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
                        FIND VarGr NO-LOCK OF ArtBas NO-ERROR.
                            
                        FIND butiker NO-LOCK WHERE
                            butiker.Butik = TelleLinje.butik NO-ERROR.
                        IF AVAILABLE butiker THEN
                            FIND ArtPris NO-LOCK WHERE
                            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                            ArtPris.ProfilNr   = butiker.ProfilNr NO-ERROR.
                        IF NOT AVAILABLE ArtPris THEN
                            FIND FIRST ArtPris OF ArtBas NO-ERROR.
                        IF AVAILABLE ArtPris THEN
                            ltilbud = ArtPris.Tilbud.
                        ELSE
                            lTilbud = FALSE.
                            
                        /* Vektet varekost - Kun for lagerstyrte varer. */
                        IF AVAILABLE Lager THEN RELEASE Lager.
                        IF ArtBas.Lager THEN
                        DO:
                            FIND Lager NO-LOCK WHERE
                                Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                                Lager.Butik      = INT(cEkstent) NO-ERROR.
                            IF (AVAILABLE Lager AND Lager.VVareKost <> ? AND Lager.VVarekost > 0) 
                                THEN lVVarekost = Lager.VVareKost.
                                ELSE lVVareKost = (IF AVAILABLE ArtPris THEN ArtPris.VareKost[1] ELSE 0).
                        END.
                        ELSE lVVarekost = (IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0).    
                        
                        pcTekst = TRIM(REPLACE(ArtBas.Beskr,'"','')).
                        pcTekst = (IF pcTekst = "" THEN "Blank varetekst" ELSE pcTekst).
    
                        IF TRIM(cStrekkode) <> '' THEN 
                        DO:
                            CREATE PrisCubUt.
                            ASSIGN
                                /*  1 */ PrisCubUt.ArtikkelNr     = ArtBas.ArtikkelNr 
                                /*  2 */ PrisCubUt.StrKode        = (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0) 
                                /*  3 */ PriscubUt.ModellFarge    = ArtBas.ModellFarge 
                                /*  4 */ PrisCubUt.Bestillingsnr  = (IF Strekkode.Bestillingsnummer <> '' THEN Strekkode.Bestillingsnummer ELSE ArtBas.LevKod) 
                                /*  5 */ PrisCubUt.VgLopNr        = STRING(ArtBas.Vg) + '/' + (IF ArtBas.LopNr <> ? THEN STRING(ArtBas.LopNr) ELSE '0')
                                /*  6 */ PrisCubUt.Varetekst      = REPLACE(REPLACE(REPLACE(TRIM(ArtBas.Beskr),';',','),CHR(10),''),CHR(13),'')
                                /*  7 */ PrisCubUt.LevFargKod     = REPLACE(REPLACE(REPLACE(TRIM(ArtBas.LevFargKod),';',','),CHR(10),''),CHR(13),'')
                                /*  8 */ PrisCubUt.VgBeskr        = REPLACE(REPLACE(REPLACE(TRIM(IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ''),';',','),CHR(10),''),CHR(13),'')
                                /*  9 */ PrisCubUt.Vg             = ArtBas.Vg 
                                /* 10 */ PrisCubUt.LopNr          = (IF ArtBas.LopNr <> ? THEN ArtBas.LopNr ELSE 0)
                                /* 11 */ PrisCubUt.Storl          = (IF AVAILABLE StrKonv THEN REPLACE(TRIM(StrKonv.Storl),';',',') ELSE '')
                                /* 12 */ PrisCubUt.ButikkNr       = TelleLinje.Butik
                                /* 13 */ PrisCubUt.PrisHK         = (IF AVAILABLE clArtPris THEN clArtPris.Pris[1] ELSE 0)
                                /* 14 */ PrisCubUt.PrisLokal      = ArtPris.Pris[1] 
                                /* 15 */ PrisCubUt.Varekost       = ArtPris.Varekost[1]
                                /* 16 */ PrisCubUt.VektetVarekost = lVVareKost
                                /* 17 */ PrisCubUt.Lagerstyrt     = ArtBas.Lager
                                /* 18 */ PrisCubUt.Strekkode      = cStrekkode      
                                /* 19 */ PrisCubUt.Interleave     = c2Av5Interleave 
                                         iLoopVL                  = iLoopVL + 1
                                NO-ERROR.
                        END.
                        ASSIGN 
                        cStrekkode      = ''
                        c2Av5Interleave = ''.
                    END. /* STREKKODE */
                END.
            END.
        END.
    END.

    IF lAppend = TRUE THEN
        OUTPUT TO VALUE(cFilnavn) APPEND.
    ELSE
        OUTPUT TO VALUE(cFilnavn).

    /* Legger ut tingene sortert. */
    FOR EACH PrisCubUt
        BREAK BY PrisCubUt.Strekkode:

        PUT UNFORMATTED
            /*  1 */ PrisCubUt.ArtikkelNr  ';'      
            /*  2 */ PrisCubUt.StrKode  ';'         
            /*  3 */ PriscubUt.ModellFarge  ';'     
            /*  4 */ PrisCubUt.Bestillingsnr  ';'   
            /*  5 */ PrisCubUt.VgLopNr  ';'        
            /*  6 */ PrisCubUt.Varetekst  ';'      
            /*  7 */ PrisCubUt.LevFargKod  ';'     
            /*  8 */ PrisCubUt.VgBeskr  ';'        
            /*  9 */ PrisCubUt.Vg  ';'              
            /* 10 */ PrisCubUt.LopNr  ';'          
            /* 11 */ PrisCubUt.Storl  ';'          
            /* 12 */ PrisCubUt.ButikkNr  ';'       
            /* 13 */ REPLACE(TRIM(STRING(PrisCubUt.PrisHK,">>>>>>9.99")),",",".")  ';'         
            /* 14 */ REPLACE(TRIM(STRING(PrisCubUt.PrisLokal,">>>>>>9.99")),",",".")  ';'       
            /* 15 */ REPLACE(TRIM(STRING(PrisCubUt.Varekost,">>>>>>9.99")),",",".")  ';'       
            /* 16 */ REPLACE(TRIM(STRING(PrisCubUt.VektetVarekost,">>>>>>9.99")),",",".")  ';' 
            /* 17 */ PrisCubUt.Lagerstyrt  ';'     
            /* 18 */ PrisCubUt.Strekkode  ';'          
            /* 19 */ PrisCubUt.Interleave  
            SKIP
            .            
    END.
    OUTPUT CLOSE.

    /* Flytter filen for CubComm til HT katalogen. */
    IF CAN-DO("12",STRING(iHt)) THEN
    DO:
        /* Kopierer konvertert fil til ut katalogen */
        IF SEARCH(lcKatalog + HT-Type.EkspFilPrefix + HT-Type.EkspFilEkstent) <> ? THEN
            OS-COPY VALUE(lcKatalog + HT-Type.EkspFilPrefix + HT-Type.EkspFilEkstent) VALUE(cFilNavn).
    END.

    MESSAGE "Opprettet varetellingsfil: " cFilnavn + "." SKIP
            "Antall produkter lest " + STRING(iLoop) + "." SKIP
            "Antall strekkoder lest " + STRING(iAntStrekkoder) + "." SKIP 
            "Antall strekkoder eksportert " + STRING(iLoopVL) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    /* ---- CubSlutt -----------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeggUtFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggUtFil Procedure 
PROCEDURE LeggUtFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLoop   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLoopVL AS INTEGER    NO-UNDO.
    DEF VAR pcTekst   AS CHAR NO-UNDO.
    DEF VAR plEan     AS DEC  NO-UNDO.
    DEF VAR cLinje    AS CHAR NO-UNDO.
    DEF VAR cTxt     AS CHAR NO-UNDO.

    IF lAppend = TRUE THEN
        OUTPUT TO VALUE(cFilnavn) APPEND.
    ELSE
        OUTPUT TO VALUE(cFilnavn).
    FOR EACH TelleLinje OF TelleHode NO-LOCK BREAK BY TelleLinje.Butik BY TelleLinje.ArtikkelNr:
        IF FIRST-OF(ArtikkelNr) THEN DO:
            FIND ArtBas WHERE Artbas.Artikkelnr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                ASSIGN iLoop = iLoop + 1.
                STREKKODE:
                FOR EACH Strekkode OF ArtBas WHERE StrekKode.KodeType = 1 NO-LOCK:
                    ASSIGN
                        pcTekst = TRIM(Strekkode.Kode)
                        plEan   = DEC(Strekkode.Kode)
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        NEXT STREKKODE.
                    IF pcTekst = "" THEN
                        NEXT STREKKODE.
                    IF LENGTH(pcTekst) <> 13 THEN
                        NEXT STREKKODE.
                    ctxt = TRIM(ArtBas.Beskr).
                    cTxt = IF cTxt <> "" THEN cTxt ELSE "Blank varetekst".
                    cLinje = Strekkode.Kode + FILL(" ",13 - LENGTH(Strekkode.Kode)) + cTxt + FILL(" ",20 - length(cTxt)).
/*                              (IF ArtBas.Beskr = "" THEN "Blank varetekst" ELSE trim(substring(ArtBas.Beskr,1,20))) + FILL(" ",20 - length(trim(substring(ArtBas.Beskr,1,20)))). */
                    IF cLinje <> "" AND LENGTH(cLinje) = 33 THEN
                        PUT UNFORMATTED cLinje SKIP.

                    ASSIGN iLoopVL = iLoopVL + 1.
                END.
            END.
        END.
    END.
    OUTPUT CLOSE.
    MESSAGE "Varetellingsfil: " cFilnavn + "." SKIP
            "Antall produkter eksportert " + STRING(iLoop) + "." SKIP
            "Antall varelinjer eksportert " + STRING(iLoopVL) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeggUtVarefil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggUtVarefil Procedure 
PROCEDURE LeggUtVarefil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

DEF TEMP-TABLE PrisUt 
    FIELD ButikkNr AS INT FORMAT ">>9"
    FIELD Ean      AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Varetekst AS CHAR 
    FIELD BestNr    AS INT  
    FIELD Tilbud    AS LOG FORMAT "Ja/Nei"
    FIELD UtprisUt  AS DEC FORMAT ">>,>>9.99"
    FIELD UtprisInn AS DEC FORMAT ">>,>>9.99"
    .

------------------------------------------------------------------------------*/
    DEF VAR iLoop        AS INTEGER    NO-UNDO.
    DEF VAR iLoopVL      AS INTEGER    NO-UNDO.

    DEF VAR lTilbud   AS LOG FORMAT "yes/no" NO-UNDO.
    DEF VAR lcKatalog AS CHAR NO-UNDO.
    DEF VAR pcTekst   AS CHAR NO-UNDO.
    DEF VAR plEan     AS DEC  NO-UNDO.

    /* Tømmer temp-table */
    FOR EACH PrisUt:
        DELETE PrisUt.
    END.

    /* Symbol PPT 8800.                                 */
    /* Må konverteres i en temp katalog pga aktiv sync. */
/*     IF CAN-DO("6",STRING(iHt)) THEN                                                                  */
/*     DO:                                                                                              */
/*         ASSIGN /* Overstyrer filnavn for symbol */                                                   */
/*             lcKatalog    = "c:\home\lindbak\kasse\"                                                  */
/*             cFilnavn     = RIGHT-TRIM(RIGHT-TRIM(TRIM(HT-Type.EksportKatalog), "\"),"/") + "\" +     */
/*                            HT-Type.EkspFilPrefix + "c." + HT-Type.EkspFilEkstent                     */
/*             .                                                                                        */
/*                                                                                                      */
/*         IF lAppend = TRUE THEN                                                                       */
/*             OUTPUT TO VALUE(lcKatalog + HT-Type.EkspFilPrefix + "." + HT-Type.EkspFilEksten) APPEND. */
/*         ELSE                                                                                         */
/*             OUTPUT TO VALUE(lcKatalog + HT-Type.EkspFilPrefix + "." + HT-Type.EkspFilEksten).        */
/*     END.                                                                                             */
/*     /* Andre HT typer skal ha filen lagt ut direkte. */                                              */
/*     ELSE DO:                                                                                         */
/*         IF lAppend = TRUE THEN                                                                       */
/*             OUTPUT TO VALUE(cFilnavn) APPEND.                                                        */
/*         ELSE                                                                                         */
/*             OUTPUT TO VALUE(cFilnavn).                                                               */
/*     END.                                                                                             */
    IF lAppend = TRUE THEN
        OUTPUT TO VALUE(cFilnavn) APPEND.
    ELSE
        OUTPUT TO VALUE(cFilnavn).

    FOR EACH TelleLinje OF TelleHode NO-LOCK 
        BREAK BY TelleLinje.TelleNr BY TelleLinje.ArtikkelNr:

        IF FIRST-OF (TelleLinje.ArtikkelNr) THEN DO:
            FIND ArtBas WHERE Artbas.Artikkelnr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                ASSIGN iLoop = iLoop + 1.

                STREKKODE:
                FOR EACH Strekkode OF ArtBas WHERE StrekKode.KodeType = 1 NO-LOCK:
                    ASSIGN
                        pcTekst = TRIM(Strekkode.Kode)
                        plEan   = DEC(Strekkode.Kode)
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                        NEXT STREKKODE.
                    IF pcTekst = "" THEN
                        NEXT STREKKODE.
                    IF LENGTH(pcTekst) <> 13 THEN
                        NEXT STREKKODE.
/*                     IF LENGTH(pcTekst) = 13 AND             */
/*                        SUBSTRING(pcTekst,1,2) = "02" AND    */
/*                        SUBSTRING(pcTekst,10,3) = "000" THEN */
/*                         NEXT STREKKODE.                     */

                    ASSIGN iLoopVL = iLoopVL + 1.

                    FIND butiker NO-LOCK WHERE
                        butiker.Butik = TelleLinje.butik NO-ERROR.
                    IF AVAILABLE butiker THEN
                        FIND ArtPris NO-LOCK WHERE
                        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                        ArtPris.ProfilNr   = butiker.ProfilNr NO-ERROR.
                    IF NOT AVAILABLE ArtPris THEN
                        FIND FIRST ArtPris OF ArtBas NO-ERROR.
                    IF AVAILABLE ArtPRis THEN
                        ltilbud = ArtPris.Tilbud.
                    ELSE
                        lTilbud = FALSE.
                    
                    pcTekst = TRIM(REPLACE(ArtBas.Beskr,'"','')).
                    pcTekst = (IF pcTekst = "" THEN "Blank varetekst" ELSE pcTekst).


                    CREATE PrisUt.
                    ASSIGN
                        PrisUt.ButikkNr  = TelleLinje.Butik
                        PrisUt.Ean       = DEC(Strekkode.Kode)
                        PrisUt.Varetekst = SUBSTRING(pcTekst,1,20)
                        PrisUt.BestNr    = ?
                        PrisUt.Tilbud    = lTilbud
                        PrisUt.UtprisUt  = (IF AVAILABLE ArtPris
                                              THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                              ELSE 0)
                        PrisUt.UtprisInn = 0 
                        NO-ERROR.

                    /*
                    /*PUT UNFORMATTED StrekKode.Kode + STRING(ArtBas.Beskr,"x(20)") SKIP.*/
                    PUT UNFORMATTED  
                        TelleLinje.Butik " "
                        DEC(Strekkode.Kode) " " 
                        '"' + ArtBas.Beskr + '" '
                        ? " "
                        lTilbud " "
                        (IF AVAILABLE ArtPris
                           THEN replace(trim(string(ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1],">>>>9.99")),",",".")
                           ELSE "0.00") " "
                        "0.00" SKIP
                        .
                    */
                END.
            END.
        END.
    END.
    /* Legger ut tingene sortert. */
    FOR EACH PrisUt
        BREAK BY PrisUt.Ean:

        PUT UNFORMATTED
            PrisUt.ButikkNr " " 
            PrisUt.Ean " "      
            '"' + substring(PrisUt.Varetekst,1,20) + '"' " "
            PrisUt.BestNr " "    
            PrisUt.Tilbud " "   
            REPLACE(TRIM(STRING(PrisUt.UtprisUt,">>>>9.99")),",",".") " "
            REPLACE(TRIM(STRING(PrisUt.UtprisInn,">>>>9.99")),",",".")
            SKIP
            .
        
/*         EXPORT                                  */
/*             PrisUt.ButikkNr                     */
/*             PrisUt.Ean                          */
/*             PrisUt.Varetekst                    */
/*             PrisUt.BestNr                       */
/*             PrisUt.Tilbud                       */
/*             PrisUt.UtprisUt  FORMAT ">>,>>9.99" */
/*             PrisUt.UtprisInn FORMAT ">>,>>9.99" */
/*             .                                   */
    END.
    OUTPUT CLOSE.

    /* Flytter filen for PPT 8800 til HT katalogen. */
    IF CAN-DO("6",STRING(iHt)) THEN
    DO:
        /* Konverterer til pakket format. */
        IF SEARCH(lcKAtalog + HT-Type.EkspFilPrefix + "." + HT-Type.EkspFilEkstent) <> ? THEN
            OS-COMMAND SILENT VALUE('var2sym.bat ' + HT-Type.EkspFilEkstent).
        PAUSE 3 NO-MESSAGE.
        /* Kopierer konvertert fil til ut katalogen */
        IF SEARCH(lcKatalog + HT-Type.EkspFilPrefix + "c." + HT-Type.EkspFilEkstent) <> ? THEN
            OS-COPY VALUE(lcKatalog + HT-Type.EkspFilPrefix + "c." + HT-Type.EkspFilEkstent) VALUE(cFilNavn).
        /* Sletter temporære filer. */
        IF SEARCH(cFilNavn) <> ? THEN
        DO:
            /*
            OS-DELETE VALUE(lcKatalog + HT-Type.EkspFilPrefix + "c." + HT-Type.EkspFilEkstent).
            */
            /*OS-DELETE VALUE(ctmpFilNavn).*/
        END.
    END.

    MESSAGE "Varetellingsfil: " cFilnavn + "." SKIP
            "Antall produkter eksportert " + STRING(iLoop) + "." SKIP
            "Antall varelinjer eksportert " + STRING(iLoopVL) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

