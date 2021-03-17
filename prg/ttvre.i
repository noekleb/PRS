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

/*     VAL,,,,E,3595961485845,00002, */
/*     VAL,,,,E,3595961485746,00002, */
/*     VAL,,,,E,3595961485852,00002, */
/*     VAL,,,,E,3595961485753,00002, */
/*     VAL,,,,E,3595961485869,00002, */
DEFINE {&NEW} {&SHARED} TEMP-TABLE ttVre
  FIELD EkstVPILevNr AS INT
  FIELD LinjeNr      AS INT
  FIELD ButikkNr     AS INT
  FIELD OrdreNr      AS CHARACTER 
  FIELD PakkseddelNr AS CHARACTER 
  FIELD ArtikkelNr   AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD LevModellNr  AS CHAR
  FIELD VareTekst    AS CHAR 
  FIELD FargeTekst   AS CHAR 
  FIELD Str          AS CHAR 
  FIELD Felt1        AS CHAR /* Alltid VAL */
  FIELD Felt2        AS CHAR
  FIELD Felt3        AS CHAR
  FIELD Felt4        AS CHAR
  FIELD Felt5        AS CHAR /* Alltid E   */
  FIELD Felt6        AS CHAR
  FIELD Felt7        AS CHAR
  FIELD Felt8        AS CHAR
  FIELD Felt9        AS CHAR /* Størrelsen seqnr. */
  FIELD Kode         AS CHARACTER 
  FIELD BestNr       AS INTEGER FORMAT ">>>>>>>>9"
  FIELD Antall       AS INTEGER
  FIELD LevFargKod   AS CHARACTER  
  FIELD LevNr        AS INTEGER FORMAT ">>>>>9"
  FIELD StrKode      AS INTEGER FORMAT ">>>>9"
  FIELD Salgsenhet   AS CHARACTER 
  FIELD LevPrisEngros AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD forhRab%     AS DECIMAL FORMAT "->,>>9.99"
  FIELD VeilPris     AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD nettoForh    AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD MarkedsPris  AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD InnkjopsPris AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD MvaKr        AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD PkSdl_Id     AS CHARACTER FORMAT "x(13)"
  FIELD PrisRab%     AS DECIMAL FORMAT "->,>>9.99"  
  FIELD Sesong       AS CHARACTER FORMAT "x(10)"
  FIELD Ordretype    AS CHARACTER FORMAT "x(5)"
  FIELD LandedCost   AS DECIMAL FORMAT "->>,>>>,>>9.99"
  INDEX ixPkSdlNr PakkseddelNr
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


