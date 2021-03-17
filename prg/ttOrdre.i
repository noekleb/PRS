&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

DEFINE {1} {2} TEMP-TABLE ttOrdre NO-UNDO
  FIELD RecType         AS INT       FORMAT ">>99"
  FIELD OrdreNr         AS INTEGER   FORMAT ">>>>>>>9" LABEL "OrdreNr"
  FIELD LevNr           AS INTEGER   FORMAT "zzzzz9" LABEL "Leverandør"
  FIELD OrdreStatus     AS INTEGER   FORMAT ">9" INITIAL 1 LABEL "Status"
  FIELD EkstId          AS CHARACTER FORMAT "X(15)" LABEL "Eksternt Id" COLUMN-LABEL "EkstId"
  FIELD LevMerknad      AS CHARACTER FORMAT "X(50)" LABEL "Merknad"
  FIELD SendtDato       AS DATE      FORMAT "99/99/9999" LABEL "Sendt dato"
  FIELD HMerknad        AS CHARACTER FORMAT "X(40)" LABEL "Merknad"
  FIELD LevAdresse1     AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD LevAdresse2     AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD LevPostNr       AS CHARACTER FORMAT "X(10)" LABEL "PostNr"
  FIELD LevPostBoks     AS CHARACTER FORMAT "X(40)" LABEL "Postboks"
  FIELD LevTelefon      AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD LevKontakt      AS CHARACTER FORMAT "X(30)" LABEL "Kontakt"
  FIELD Notat           AS CHARACTER FORMAT "X(8)"  LABEL "Notat"
  FIELD Hasteordre      AS LOG                      LABEL "Hasteordre"

  FIELD BestNr          AS INTEGER   FORMAT ">>>>>>>9" LABEL "Bestilling" COLUMN-LABEL "BestNr"
  FIELD BestillingsDato AS DATE      FORMAT "99/99/9999" INITIAL TODAY LABEL "Bestillingsdato" COLUMN-LABEL "BestDat"
  FIELD BestStat        AS INTEGER   FORMAT ">9" LABEL "Bestillingsstatus" COLUMN-LABEL "BS"
  FIELD Merknad         AS CHARACTER FORMAT "x(30)" LABEL "Merknad"
  FIELD Beskrivelse     AS CHARACTER FORMAT "x(50)" LABEL "Beskrivelse"
  FIELD DirekteLev      AS LOGICAL   LABEL "Direktelevert" COLUMN-LABEL "DL"
  FIELD ArtikkelNr      AS DECIMAL   DECIMALS 0 FORMAT "zzzzzzzzzzzz9" LABEL "Artikkelnummer"
  field Beskr           as char      format "x(30)"
  field LevKod          as char      format "x(30)"
  field LevFargKod      as char      format "x(30)"
  FIELD OrdreNr2        AS INTEGER   FORMAT ">>>>>>>9" LABEL "OrdreNr"
  FIELD BestType        AS INTEGER   FORMAT "9" INITIAL 1 LABEL "Best.type"
  FIELD SendtDato2      AS DATE      FORMAT "99/99/9999" LABEL "Sendt dato"
  FIELD LevDato         AS DATE      FORMAT "99/99/9999" LABEL "Leveringsdato"
  FIELD KjedeAvtale     AS LOGICAL   LABEL "Kjedeavtale"
  FIELD EkstOrdreNr     AS INTEGER   FORMAT ">>>>>>>9" LABEL "OrdreNr"

  FIELD ProfilNr        AS INTEGER   FORMAT "->>>>>>9"
  FIELD ValPris         AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Valutapris"
  FIELD InnkjopsPris    AS DECIMAL   DECIMALS 2 LABEL "Innkjøpspris"
  FIELD Rab1Kr          AS DECIMAL   DECIMALS 2 LABEL "Rabatt 1"
  FIELD Rab1%           AS DECIMAL   DECIMALS 2 FORMAT "->>9.99" LABEL "%Rabatt 1"
  FIELD Rab2Kr          AS DECIMAL   DECIMALS 2 LABEL "Rabatt 2"
  FIELD Rab2%           AS DECIMAL   DECIMALS 2 FORMAT "->>9.99" LABEL "%Rabatt 2"
  FIELD Frakt           AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Frakt"
  FIELD Frakt%          AS DECIMAL   DECIMALS 2 FORMAT "->>9.99" LABEL "Frakt%"
  FIELD DivKostKr       AS DECIMAL   DECIMALS 2 LABEL "Div.kost"
  FIELD DivKost%        AS DECIMAL   DECIMALS 2 FORMAT "->>9.99" LABEL "Div.Kost%"
  FIELD Rab3Kr          AS DECIMAL   DECIMALS 2 LABEL "Rabatt 3"
  FIELD Rab3%           AS DECIMAL   DECIMALS 2 FORMAT "->>9.99" LABEL "%Rabatt 3"
  FIELD DBKr            AS DECIMAL   DECIMALS 2 LABEL "DB"
  FIELD DB%             AS DECIMAL   DECIMALS 2 LABEL "DB%"
  FIELD Pris            AS DECIMAL   DECIMALS 2 LABEL "Pris"
  FIELD KjedeInnkPris   AS DECIMAL   DECIMALS 2 LABEL "KjedeInnkPris"
                       
  FIELD Butik           AS INTEGER   FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
  FIELD Storl           AS CHARACTER FORMAT "x(4)" LABEL "Størrelse"
  FIELD Bestilt         AS DECIMAL   DECIMALS 2 FORMAT "->>,>>9" LABEL "Bestilt"
  FIELD Strekkode       AS CHAR FORMAT "x(20)"      
  FIELD Bestillingsnummer AS CHAR FORMAT "x(30)" LABEL "Bestillingsnummer"
  FIELD Varekost        AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Varekost"
  FIELD StrKode         AS INT FORMAT "999"

  FIELD pksdlOrdreNr    AS INT FORMAT ">>>>>>>9" 
  FIELD pksdlDato       AS DATE 
  FIELD pksdlTid        AS CHAR 
  FIELD pksdlBruker     AS CHAR 
  FIELD pksdlNr         AS INT FORMAT ">>>>>>>9" 
  FIELD pksdlLinjeNr    AS INT FORMAT ">>>>>9"
  FIELD pksdlLevert     AS DEC DECIMALS 2 FORMAT "->>,>>9" LABEL "Plukket"
  FIELD pksdlLinjeSeq   AS INT 
  FIELD Funnet          AS INT 
  FIELD Vg              AS INT FORMAT ">>>>>9"
  FIELD VmId            LIKE ArtBas.VmId
  FIELD ProdNr          LIKE ArtBas.ProdNr
  FIELD Sasong          LIKE ArtBas.Sasong
  FIELD Farg            LIKE ArtBas.Farg
  FIELD AntIPakn        LIKE ArtBas.AntIPakn
  FIELD RAvdNr          LIKE ArtBas.RAvdNr
  FIELD forhRab%        LIKE ArtBas.forhRab%     
  FIELD supRab%         LIKE ArtBas.supRab%      
  FIELD VPIBildeKode    LIKE ArtBas.VPIBildeKode 
  FIELD KjedeRab%       LIKE ArtBas.KjedeRab%    
  FIELD PkSdlLevNr      LIKE LevBas.LevNr
  FIELD PkSdlLevNamn    LIKE LevBas.LevNamn
  FIELD UkjentVareNotat AS CHAR FORMAT "x(30)"
  FIELD PlListeId       LIKE plListeHode.PlListeId
  FIELD PkSdlId         LIKE PkSdlHode.PkSdlId 
  FIELD PkSdlLinjeId    LIKE PkSdlLinje.PkSdlLinjeId
    
  INDEX oabs OrdreNr ArtikkelNr Butik Storl
  INDEX Funnet Ordrenr Funnet EkstId Butik LevNr ArtikkelNr LevDato Storl
  INDEX LEGGTILVRE RecType ArtikkelNr
  INDEX PKSDL RecType EkstId pksdlLinjeSeq
  INDEX PKSDLLINE PkSdlId PkSdlLinjeId
  INDEX PAKK2 ArtikkelNr StrKode Butik EkstId PkSdlNr
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


