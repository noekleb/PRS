&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : kordre_sjekkartnettbutikk.p
    Purpose     : Sjekker om kundeordren kommer fra en nettbutikk.
                  Gjør den det og artikkelen som legges inn 
                  ikke er aktivert for nettbutikk, skal artikkelen aktiveres.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER lKOrdre_Id  AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEF BUFFER bufArtBas FOR ArtBas.

FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
IF NOT AVAILABLE KOrdreHode THEN
    RETURN.
/* Skal bare gjøres for kundeordre som kommer fra nettbutikk når */
/* disse redigeres i butikksystemet.                             */
IF KORdreHode.Opphav <> 10 THEN
    RETURN.

FOR EACH KOrdreLinje OF KOrdreHode:
    FIND bufArtBas NO-LOCK WHERE
        bufArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
    IF AVAILABLE bufArtBas AND bufArtBas.WebButikkArtikkel = FALSE THEN
    DO:
        FIND ArtBas EXCLUSIVE-LOCK WHERE
             ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
        ASSIGN
            ArtBas.WebButikkArtikkel = TRUE.
        RELEASE ArtBas.
    END.
END.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


