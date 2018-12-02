
 /*

	Last change:  BO    3 Jun 99    3:25 pm
*/

DEFINE INPUT PARAMETER wQY          AS CHAR     NO-UNDO.
DEFINE INPUT PARAMETER iHt          AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER gcTabell     AS CHAR     NO-UNDO.

DEF VAR hField1  AS HANDLE NO-UNDO.
DEF VAR hField2  AS HANDLE NO-UNDO.
DEF VAR hField3  AS HANDLE NO-UNDO.
DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR hBuffer  AS HANDLE NO-UNDO.
DEF VAR cFilnavn AS CHAR   NO-UNDO.
DEF VAR iLoop    AS INT    NO-UNDO.
DEF VAR iLoopVl  AS INTE   NO-UNDO.
DEF VAR cDir     AS CHAR   NO-UNDO.
DEF VAR iCl      AS INT    NO-UNDO.
DEF VAR ltilbud  AS LOG    NO-UNDO.
DEF VAR lcKatalog AS CHAR  NO-UNDO.
DEF VAR pcTekst   AS CHAR  NO-UNDO.
DEF VAR plEan     AS DEC   NO-UNDO.
DEF VAR cBeskr    AS CHAR  NO-UNDO.
DEF VAR cLinje    AS CHAR  NO-UNDO.
DEF VAR cEkstent  AS CHAR  NO-UNDO.
DEF VAR cButListe AS CHAR  NO-UNDO.

CREATE QUERY  hQuery.
CREATE BUFFER hBuffer FOR TABLE gcTabell.

DEF TEMP-TABLE PrisUt 
    FIELD ButikkNr          AS INT FORMAT ">>9"
    FIELD Ean               AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Varetekst         AS CHAR 
    FIELD BestNr            AS INT  
    FIELD Tilbud            AS LOG FORMAT "Ja/Nei"
    FIELD UtprisUt          AS DEC FORMAT "->>>,>>9.99"
    FIELD UtprisInn         AS DEC FORMAT "->>>,>>9.99"
    FIELD LevKod            AS CHAR FORMAT "x(20)"
    FIELD MvaKode    AS INT FORMAT "9"
    FIELD LinkVareNr AS CHAR
    FIELD Antall     AS DEC FORMAT "->>>,>>9.999"
    FIELD AntIPakn   AS DEC FORMAT "->>>,>>9.999"
    FIELD cEAN       AS CHAR 
    .

DEFINE TEMP-TABLE BxCentral NO-UNDO 
    FIELD VareNr AS CHARACTER FORMAT "x(40)"
    FIELD EAN AS CHARACTER FORMAT "x(40)"
    FIELD Beskrivelse AS CHARACTER FORMAT "x(100)"
    FIELD KjopsPris AS CHARACTER FORMAT "x(12)"
    FIELD MvaKode AS CHARACTER FORMAT "x(1)"
    FIELD BindingsVareNr AS CHARACTER FORMAT "x(20)"
    FIELD Antall AS CHARACTER FORMAT "x(10)"
    FIELD AntallBulk AS CHARACTER FORMAT "x(10)"
    FIELD NettoPris AS CHARACTER FORMAT "x(12)"
    .

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
    INDEX ArtikkelNr ArtikkelNr.

DEFINE BUFFER clArtPris FOR ArtPris.
DEFINE BUFFER clButiker FOR Butiker.

DEF STREAM Ut.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
    RETURN.

{tmp2artbasdef.i}

FIND HT-Type WHERE HT-Type.TypeId = iHt NO-LOCK.
ASSIGN
    cEkstent = HT-Type.EkspFilEkstent
    .
/* Her skal det spørres etter butikk */
IF CAN-DO("6,7,10,12",STRING(iHt)) AND cEkstent = "<ButNr>" THEN
DO:
    cButListe = "".
    FOR EACH Butiker NO-LOCK:
        ASSIGN
            cButListe = cButListe + (IF cButListe = "" THEN "" ELSE ",") +
                        STRING(Butiker.Butik,">>>999") + " " + Butiker.ButNamn + "," + STRING(Butiker.Butik).
    END.
    RUN d-VelgGenerellCombo.w ("Velg butikk",cButListe, INPUT-OUTPUT cEkstent).
    IF cEkstent = "" THEN RETURN.
    IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = INT(cEkstent)) THEN RETURN.
END.

ASSIGN 
    cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(HT-Type.EksportKatalog), "\"),"/") + "\" + 
                    HT-Type.EkspFilPrefix + "." + cEkstent
    cDir = ENTRY(1,cFilnavn,"\") + "\"
    .
DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
    cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
    OS-CREATE-DIR VALUE(cDir).
END.

IF SEARCH(cFilNavn) <> ? AND NOT CAN-DO("6,7,8,9",STRING(iHt)) THEN DO:
    MESSAGE "Hådterminalfilen finnes fra før." SKIP
        "Ønsker du å legge til i den eksisterende filen?" SKIP
        "(Ja=Legge til, Nei=Erstatte)"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO-CANCEL UPDATE lOk AS LOGICAL.
    IF lOk = ? THEN
        RETURN.
END.
ELSE
    lOk = FALSE.

/* Tømmer temp-table */
FOR EACH PrisUt:
    DELETE PrisUt.
END.
FOR EACH PrisCubUt:
    DELETE PrisCubUt.    
END.
FOR EACH BxCentral:
    DELETE BxCentral.
END.
    
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY).
hQuery:QUERY-OPEN().

/*
 /* Symbol PPT 8800.                                 */
/* Må konverteres i en temp katalog pga aktiv sync. */
IF CAN-DO("6",STRING(iHt)) THEN
DO:
    ASSIGN /* Overstyrer filnavn for symbol */
        lcKatalog    = "c:\home\lindbak\kasse\"
        cFilnavn     = RIGHT-TRIM(RIGHT-TRIM(TRIM(HT-Type.EksportKatalog), "\"),"/") + "\" + 
                       HT-Type.EkspFilPrefix + "c." + HT-Type.EkspFilEkstent
        .

    IF lOk = TRUE THEN
        OUTPUT STREAM Ut TO VALUE(lcKatalog + HT-Type.EkspFilPrefix + "." + HT-Type.EkspFilEksten) APPEND.
    ELSE
        OUTPUT STREAM Ut TO VALUE(lcKatalog + HT-Type.EkspFilPrefix + "." + HT-Type.EkspFilEksten).
END.
/* Andre HT typer skal ha filen lagt ut direkte. */
ELSE DO:
    IF lOk = TRUE THEN
        OUTPUT STREAM Ut TO VALUE(cFilnavn) APPEND.
    ELSE
        OUTPUT STREAM Ut TO VALUE(cFilnavn).
END.
*/
IF lOk = TRUE THEN
    OUTPUT STREAM Ut TO VALUE(cFilnavn) APPEND.
ELSE
    OUTPUT STREAM Ut TO VALUE(cFilnavn).

STATUS DEFAULT "".
REPEAT:
   hQuery:GET-NEXT() NO-ERROR.
   IF NOT hBuffer:AVAILABLE THEN 
   DO:
       LEAVE.
   END.
   ASSIGN hField1 = hBuffer:BUFFER-FIELD("Artikkelnr")
          hField2 = hBuffer:BUFFER-FIELD("Beskr")
          hField3 = hBuffer:BUFFER-FIELD("lager").
/*       IF hField3:BUFFER-VALUE() = FALSE THEN */
/*           NEXT. */
   IF CAN-DO("6,8,9",STRING(iHt)) THEN
       RUN LeggUtVareFil.
   ELSE IF CAN-DO("7",STRING(iHt)) THEN
       RUN LeggUtBxCentral.
   ELSE IF CAN-DO('10',STRING(iHT)) THEN
       RUN LeggUtBxMobile.
   ELSE IF CAN-DO('12',STRING(iHT)) THEN
       RUN LeggUtCubComm.
   ELSE
       RUN LeggUtFil.

   ASSIGN iLoop = iLoop + 1.
   IF iLoop MODULO 50 = 0 THEN
     STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
END.
STATUS DEFAULT "Eksporterer til håndterminalfil...".

/* Legger ut tingene sortert. */
IF CAN-DO("6,8,9",STRING(iHt)) THEN
FOR EACH PrisUt
    BREAK BY PrisUt.Ean:
/*     EXPORT STREAM Ut     */
/*         PrisUt.ButikkNr  */
/*         PrisUt.Ean       */
/*         PrisUt.Varetekst */
/*         PrisUt.BestNr    */
/*         PrisUt.Tilbud    */
/*         PrisUt.UtprisUt  */
/*         PrisUt.UtprisInn */
/*         .                */
    PUT STREAM Ut UNFORMATTED
        PrisUt.ButikkNr " " 
        PrisUt.Ean " "      
        '"' + substring(PrisUt.Varetekst,1,20) + '"' " "
        PrisUt.BestNr " "    
        PrisUt.Tilbud " "   
        REPLACE(TRIM(STRING(PrisUt.UtprisUt,">>>>9.99")),",",".") " "
        REPLACE(TRIM(STRING(PrisUt.UtprisInn,">>>>9.99")),",",".")
        SKIP
        .
END.
ELSE IF CAN-DO('7',STRING(iHT)) THEN
    FOR EACH BxCentral
        BREAK BY BxCentral.Ean:
      PUT STREAM Ut UNFORMATTED
        BxCentral.VareNr ';'
        BxCentral.EAN ';'
        '"' + BxCentral.Beskrivelse + '"' ';'
        BxCentral.KjopsPris ';'
        BxCentral.MvaKode ';'
        BxCentral.BindingsVareNr ';'
        BxCentral.Antall ';'
        BxCentral.AntallBulk ';'
        BxCentral.NettoPris
      SKIP.
    END.

ELSE IF CAN-DO('10',STRING(iHT)) THEN
    FOR EACH PrisUt
        BREAK BY PrisUt.Ean:
    /*     EXPORT STREAM Ut     */
    /*         PrisUt.ButikkNr  */
    /*         PrisUt.Ean       */
    /*         PrisUt.Varetekst */
    /*         PrisUt.BestNr    */
    /*         PrisUt.Tilbud    */
    /*         PrisUt.UtprisUt  */
    /*         PrisUt.UtprisInn */
    /*         .                */
        PUT STREAM Ut UNFORMATTED
            (IF PrisUt.LevKod = '' 
              THEN PrisUt.cEAN
              ELSE PrisUt.LevKod) ";"    
            PrisUt.cEan ";"      
            SUBSTRING(PrisUt.Varetekst,1,30) ";"
            REPLACE(TRIM(STRING(PrisUt.UtprisInn,"->>>>>>9.99")),".",",") ";"
            PrisUt.MvaKode  ";"
            PrisUt.LinkVareNr ";"
            PrisUt.Antall ";"
            PrisUt.Antall /*PrisUt.AntIPakn */ ";"
            REPLACE(TRIM(STRING(PrisUt.UtprisUt,"->>>>>>9.99")),".",",")
            SKIP
            .
    END.
ELSE IF CAN-DO("12",STRING(iHt)) THEN
FOR EACH PrisCubUt
    BREAK BY PrisCubUt.ArtikkelNr:

    PUT STREAM Ut UNFORMATTED
    /*  1 */ PrisCubUt.ArtikkelNr ';'
    /*  2 */ PrisCubUt.StrKode ';'
    /*  3 */ PrisCubUt.ModellFarge ';'
    /*  4 */ PrisCubUt.Bestillingsnr ';'
    /*  5 */ PrisCubUt.VgLopNr ';'
    /*  6 */ PrisCubUt.Varetekst ';' 
    /*  7 */ PrisCubUt.LevFargKod ';'
    /*  8 */ PrisCubUt.VgBeskr ';'
    /*  9 */ PrisCubUt.Vg ';'
    /* 10 */ PrisCubUt.LopNr ';'
    /* 11 */ PrisCubUt.Storl ';'
    /* 12 */ PrisCubUt.ButikkNr ';'
    /* 13 */ TRIM(REPLACE(STRING(PrisCubUt.PrisHK,">>>>>>9.99"),",",".")) ';'
    /* 14 */ TRIM(REPLACE(STRING(PrisCubUt.PrisLokal,">>>>>>9.99"),",",".")) ';'
    /* 15 */ TRIM(REPLACE(STRING(PrisCubUt.Varekost,">>>>>>9.99"),",",".")) ';'
    /* 16 */ TRIM(REPLACE(STRING(PrisCubUt.VektetVarekost,">>>>>>9.99"),",",".")) ';'
    /* 17 */ PrisCubUt.Lagerstyrt ';' 
    /* 18 */ PrisCubUt.Strekkode ';'      
    /* 19 */ PrisCubUt.Interleave
    SKIP. 
END.

OUTPUT STREAM Ut CLOSE.

/* Flytter filen for PPT 8800 til HT katalogen. */
IF CAN-DO("6",STRING(iHt)) THEN
DO:
    /* TN 28/4-05 Dette skal ikke lenger gjøres.
       Filen skal bare legges ut på vanlig område.
    /* Konverterer til pakket format. */
    IF SEARCH(lcKatalog + HT-Type.EkspFilPrefix + "." + HT-Type.EkspFilEkstent) <> ? THEN
        OS-COMMAND SILENT value('var2sym.bat ' + HT-Type.EkspFilEkstent).
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
    */
END.

MESSAGE "Varetellingsfil: " cFilnavn + "." SKIP
        "Antall produkter eksportert " + STRING(iLoop) + "."
        "Antall varelinjer eksportert " + STRING(iLoopVL) + "."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/* **********************  Internal Procedures  *********************** */


PROCEDURE LeggUtBxCentral:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  	
		    																  
------------------------------------------------------------------------------*/

    STREKKODE:
    FOR EACH StrekKode WHERE 
        StrekKode.Artikkelnr = hField1:BUFFER-VALUE()  
        AND StrekKode.KodeType = 1
        /* AND StrekKode.StrKode > 0 */ NO-LOCK.

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
/*         IF LENGTH(pcTekst) = 13 AND             */
/*            SUBSTRING(pcTekst,1,2) = "02" AND    */
/*            SUBSTRING(pcTekst,10,3) = "000" THEN */
/*             NEXT STREKKODE.                     */

        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF ArtBas.OPris = TRUE THEN
            NEXT.
        FIND butiker NO-LOCK WHERE
            butiker.Butik = int(cEkstent) NO-ERROR.
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

        CREATE BxCentral.
        ASSIGN 
            BxCentral.VareNr = TRIM(ArtBas.LevKod)
            BxCentral.EAN = Strekkode.Kode
            BxCentral.Beskrivelse = SUBSTRING(pcTekst,1,40)
            BxCentral.KjopsPris = STRING(ArtPris.Varekost[1])
            BxCentral.MvaKode = '1'
            BxCentral.BindingsVareNr = ''
            BxCentral.Antall = '1'
            BxCentral.AntallBulk = '1'
            BxCentral.NettoPris = STRING(ArtPris.Pris[1])
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE BxCentral.

        ASSIGN iLoopVL = iLoopVL + 1.
        IF iLoopVL MODULO 100 = 0 THEN
          STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
    END.


END PROCEDURE.

PROCEDURE LeggUtCubComm:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lVVarekost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cStrekkode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c2av5Interleave AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTst            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cKode           AS CHARACTER NO-UNDO.
    
    STREKKODE:
    FOR EACH StrekKode NO-LOCK WHERE 
        StrekKode.Artikkelnr = hField1:BUFFER-VALUE() AND   
        StrekKode.KodeType = 1 
        BREAK BY Strekkode.ArtikkelNr 
              BY Strekkode.StrKode:

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

        /* Trimmer bort ledende nuller i EAN8 koder. */
        cKode = Strekkode.Kode.
        IF cKode BEGINS '00000' THEN 
          cKode = SUBSTRING(cKode,6).  
            
        ASSIGN
          cStrekkode      = cStrekkode + 
                            (IF cStrekkode = '' THEN '' ELSE ',') + 
                            cKode
          c2Av5Interleave = c2Av5Interleave + 
                           (IF c2Av5Interleave = '' THEN '' ELSE ',') + 
                            Strekkode.BestillingsNummer.
                            
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
        DO:
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
            IF ArtBas.OPris = TRUE THEN
                NEXT.
            FIND butiker NO-LOCK WHERE
                butiker.Butik = int(cEkstent) NO-ERROR.
            IF AVAILABLE butiker THEN
                FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
                FIND FIRST ArtPris OF ArtBas NO-ERROR.
            FIND clArtPris NO-LOCK WHERE
                clArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
    
            /* Vektet varekost - Kun for lagerstyrte varer. */
            IF AVAILABLE Lager THEN RELEASE Lager.
            IF ArtBas.Lager THEN
            DO:
                FIND Lager NO-LOCK WHERE
                    Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                    Lager.Butik      = Butiker.Butik NO-ERROR.
                IF (AVAILABLE Lager AND Lager.VVareKost <> ? AND Lager.VVarekost > 0) 
                    THEN lVVarekost = Lager.VVareKost.
                    ELSE lVVareKost = (IF AVAILABLE ArtPris THEN ArtPris.VareKost[1] ELSE 0).
            END.
            ELSE lVVarekost = (IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0).    
    
            CREATE PrisCubUt.
            ASSIGN 
                /*  1 */ PrisCubUt.ArtikkelNr     = ArtBas.ArtikkelNr                                                                                                  
                /*  2 */ PrisCubUt.StrKode        = (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0)                                                             
                /*  3 */ PrisCubUt.ModellFarge    = ArtBas.ModellFarge                                                                                                 
                /*  4 */ PrisCubUt.Bestillingsnr  = TRIM((IF Strekkode.Bestillingsnummer <> '' THEN Strekkode.Bestillingsnummer ELSE ArtBas.LevKod))                   
                /*  5 */ PrisCubUt.VgLopNr        = STRING(ArtBas.Vg) + '/' + (IF ArtBas.LopNr <> ? THEN STRING(ArtBas.LopNr) ELSE '0')
                /*  6 */ PrisCubUt.Varetekst      = REPLACE(REPLACE(REPLACE(TRIM(ArtBas.Beskr),';',','),CHR(10),''),CHR(13),'')
                         PrisCubUt.Varetekst      = (IF PrisCubUt.Varetekst = "" THEN "Blank varetekst" ELSE PrisCubUt.Varetekst)
                /*  7 */ PrisCubUt.LevFargKod     = TRIM(REPLACE(REPLACE(REPLACE(TRIM(ArtBas.LevFargKod),';',','),CHR(10),''),CHR(13),''))                             
                /*  8 */ PrisCubUt.VgBeskr        = TRIM(REPLACE(REPLACE(REPLACE(TRIM(IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ''),';',','),CHR(10),''),CHR(13),'')) 
                /*  9 */ PrisCubUt.Vg             = ArtBas.Vg
                /* 10 */ PrisCubUt.LopNr          = (IF ArtBas.LopNr <> ? THEN ArtBas.LopNr ELSE 0)                                                                    
                /* 11 */ PrisCubUt.Storl          = (IF AVAILABLE StrKonv THEN REPLACE(TRIM(StrKonv.Storl),';',',') ELSE '')                                           
                /* 12 */ PrisCubUt.ButikkNr       = Butiker.Butik                                                                             
                /* 13 */ PrisCubUt.PrisHK         = (IF AVAILABLE clArtPris THEN clArtPris.Pris[1] ELSE 0)                   
                /* 14 */ PrisCubUt.PrisLokal      = ArtPris.Pris[1]                                                        
                /* 15 */ PrisCubUt.Varekost       = ArtPris.Varekost[1]                                                    
                /* 16 */ PrisCubUt.VektetVarekost = lVVareKost                                                             
                /* 17 */ PrisCubUt.Lagerstyrt     = ArtBas.Lager                                                                                                       
                /* 18 */ PrisCubUt.Strekkode      = cStrekkode                                                                                                         
                /* 19 */ PrisCubUt.Interleave     = c2Av5Interleave
                         cStrekkode               = ''
                         c2Av5Interleave          = ''
                NO-ERROR.                                                                                                       
        END.
        ASSIGN iLoopVL = iLoopVL + 1.
        IF iLoopVL MODULO 100 = 0 THEN
          STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
    END.

END PROCEDURE.

PROCEDURE LeggUtFil:
    STREKKODE:
    FOR EACH StrekKode WHERE 
        StrekKode.Artikkelnr = hField1:BUFFER-VALUE() 
        AND StrekKode.KodeType = 1
        /* AND StrekKode.StrKode > 0 */ NO-LOCK.
        FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas OR ArtBas.lager = FALSE THEN
            NEXT.
        ASSIGN
            pcTekst = TRIM(Strekkode.Kode)
            plEan   = DEC(Strekkode.Kode)
            cBeskr  = TRIM(SUBSTRING(REPLACE(REPLACE(hField2:BUFFER-VALUE(),",","."),'"',' '),1,20))
            cBeskr  = (IF cBeskr = "" THEN "Blank varetekst" ELSE cBeskr)
            cBeskr  = SUBSTRING(cBeskr,1,20)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT STREKKODE.
        IF pcTekst = "" THEN
            NEXT STREKKODE.
        IF LENGTH(pcTekst) <> 13 THEN
            NEXT STREKKODE.
        cLinje = Strekkode.Kode + FILL(" ",13 - LENGTH(Strekkode.Kode)) +
                 cBeskr + FILL(" ",20 - length(cBeskr)).
        IF cLinje <> "" AND LENGTH(cLinje) = 33 THEN
            PUT STREAM Ut UNFORMATTED cLinje SKIP.
        /*
        ASSIGN
            pcTekst = trim(Strekkode.Kode)
            plEan   = DEC(Strekkode.Kode)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT STREKKODE.
        IF pcTekst = "" THEN
            NEXT STREKKODE.
        IF LENGTH(pcTekst) <> 13 THEN
            NEXT STREKKODE.
        IF LENGTH(pcTekst) = 13 AND
           SUBSTRING(pcTekst,1,2) = "02" AND
           SUBSTRING(pcTekst,10,3) = "000" THEN
            NEXT STREKKODE.

        PUT STREAM Ut UNFORMATTED StrekKode.Kode REPLACE(REPLACE(hField2:BUFFER-VALUE(),",","."),'"',' ') FORMAT "X(20)" SKIP.
        */
        ASSIGN iLoopVL = iLoopVL + 1.
        IF iLoopVL MODULO 100 = 0 THEN
          STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
    END.
END PROCEDURE.

PROCEDURE LeggUtVarefil:

    STREKKODE:
    FOR EACH StrekKode WHERE 
        StrekKode.Artikkelnr = hField1:BUFFER-VALUE()  
        AND StrekKode.KodeType = 1
        /* AND StrekKode.StrKode > 0 */ NO-LOCK.

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
/*         IF LENGTH(pcTekst) = 13 AND             */
/*            SUBSTRING(pcTekst,1,2) = "02" AND    */
/*            SUBSTRING(pcTekst,10,3) = "000" THEN */
/*             NEXT STREKKODE.                     */

        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF ArtBas.OPris = TRUE THEN
            NEXT.
        FIND butiker NO-LOCK WHERE
            butiker.Butik = int(cEkstent) NO-ERROR.
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
            PrisUt.ButikkNr  = Butiker.Butik
            PrisUt.Ean       = DEC(Strekkode.Kode)
            PrisUt.Varetekst = SUBSTRING(pcTekst,1,20)
            PrisUt.Varetekst = IF PrisUt.VareTekst = "" 
                                 THEN "Mangler tekst" 
                                 ELSE PrisUt.Varetekst
            PrisUt.BestNr    = ?
            PrisUt.Tilbud    = lTilbud
            PrisUt.UtprisUt  = (IF AVAILABLE ArtPris
                                  THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                  ELSE 0)
            PrisUt.UtprisInn = 0 
            NO-ERROR.

        ASSIGN iLoopVL = iLoopVL + 1.
        IF iLoopVL MODULO 100 = 0 THEN
          STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
    END.

END PROCEDURE.

PROCEDURE LeggUtBxMobile:
    STREKKODE:
    FOR EACH StrekKode WHERE 
        StrekKode.Artikkelnr = hField1:BUFFER-VALUE()  
        AND StrekKode.KodeType = 1
        /* AND StrekKode.StrKode > 0 */ NO-LOCK.

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
/*         IF LENGTH(pcTekst) = 13 AND             */
/*            SUBSTRING(pcTekst,1,2) = "02" AND    */
/*            SUBSTRING(pcTekst,10,3) = "000" THEN */
/*             NEXT STREKKODE.                     */

        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF ArtBas.OPris = TRUE THEN
            NEXT.
        FIND butiker NO-LOCK WHERE
            butiker.Butik = int(cEkstent) NO-ERROR.
        IF NOT AVAILABLE Butiker THEN
            FIND Butiker NO-LOCK WHERE
            Butiker.Butik = iCL NO-ERROR.
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
        pcTekst = REPLACE(pcTekst,";"," ").

        CREATE PrisUt.
        ASSIGN
            PrisUt.ButikkNr  = Butiker.Butik
            PrisUt.Ean       = DEC(Strekkode.Kode)
            PrisUt.cEAN      = TRIM(STRING(PrisUt.Ean,">9999999999999"))
            PrisUt.Varetekst = SUBSTRING(pcTekst,1,30)
            PrisUt.Varetekst = IF PrisUt.VareTekst = "" 
                                 THEN "Mangler tekst" 
                                 ELSE PrisUt.Varetekst
            PrisUt.Varetekst = REPLACE(PrisUt.Varetekst,"'","")
            PrisUt.LevKod    = IF Strekkode.BestillingsNummer <> ''
                                     THEN SUBSTRING(Strekkode.Bestillingsnummer,1,20)
                               ELSE ArtBas.LevKod
            PrisUt.Tilbud    = lTilbud
            PrisUt.UtprisUt  = (IF AVAILABLE ArtPris
                                  THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                  ELSE 0)
            PrisUt.UtprisInn = (IF AVAILABLE ArtPris
                                  THEN ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                  ELSE 0) 
            PrisUt.Antall    = 1
            PrisUt.AntIPakn  = ArtBas.AntIPakn
            PrisUt.AntIPakn  = IF PrisUt.AntIPakn < PrisUt.Antall
                                     THEN PrisUt.Antall
                               ELSE PrisUt.AntIPakn
        NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE '* Feil ved utlegg av artikkel.' SKIP 
                    '  ArtikkelNr: ' STRING(ArtBas.ArtikkelNr)
            VIEW-AS ALERT-BOX.
        END.
        /* Håndtering av UPDA koder. */
        IF LENGTH(PrisUt.cEAN) = 13 AND
          PrisUt.cEAN BEGINS '00' THEN
          PrisUt.cEan = SUBSTRING(PrisUt.cEan,2,13).
        
        IF AVAILABLE ArtPris THEN DO:
            IF INT(ArtPris.Mva%[1]) = 0 THEN
                PrisUt.MvaKode = 1.
            ELSE IF INT(ArtPris.Mva%[1]) = 14 THEN
                PrisUt.MvaKode = 2.
            ELSE IF INT(ArtPris.Mva%[1]) = 25 THEN
                PrisUt.MvaKode = 3.
            ELSE
                PrisUt.MvaKode = 3.
        END.
        ELSE
            PrisUt.MvaKode = 3.

        ASSIGN iLoopVL = iLoopVL + 1.
        IF iLoopVL MODULO 100 = 0 THEN
          STATUS DEFAULT "Antall eksporterte produkter/varelinjer " + STRING(iLoop) + "/" + STRING(iLoopVL) + ".".
    END.

END PROCEDURE. /* LeggUtBxMobile */
