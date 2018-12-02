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


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnDb% Include 
PROCEDURE FinnDb% :
/*------------------------------------------------------------------------------
  Purpose:     Finner DB% når beløp, mva% og varekost er kjent.
  Parameters:  <Beløp>,<Mva%>,<VareKost>
  Syntaks:     RUN FinnDb%(500,25,267.84).
  Notes:       returnerer db% i return-value avrundet til to decimaler.
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipPris     AS DEC NO-UNDO.
    DEF INPUT PARAMETER ipMva%     AS DEC NO-UNDO.
    DEF INPUT PARAMETER ipVarekost AS DEC NO-UNDO.

    DEF VAR wMvaKr AS DEC NO-UNDO.
    DEF VAR wDb%   AS DEC NO-UNDO.

    RUN FinnMva(ipPris,ipMva%).
    ASSIGN
        wMvaKr = DEC(RETURN-VALUE)
        wDb%   = (((ipPris - wMvaKr) - ipVareKost) / (ipPris - wMvaKr)) * 100
        .
    RETURN STRING(ROUND(wDb%,2)).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnEksMva Include 
PROCEDURE FinnEksMva :
/*------------------------------------------------------------------------------
  Purpose:     Finner beløpet eksklusive mva når beløp inklusiv mva og mva%
               er gitt.
  Parameters:  <Beløp>,<Mva%>
  Syntak:      run FinnEksMva(120,23).
  Notes:       Svaret returneres i return-value avrundet til to decimaler.
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipPris   AS DEC NO-UNDO.
    DEF INPUT PARAMETER ipMva%   AS DEC NO-UNDO.

    DEF VAR wMvaKr AS DEC NO-UNDO.

    RUN FinnMva(ipPris,ipMva%).
    ASSIGN
        wMvaKr = DEC(RETURN-VALUE).
    RETURN STRING(DEC(ROUND(ipPris - wMvaKr,2))).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnMva Include 
PROCEDURE FinnMva :
/*------------------------------------------------------------------------------
  Purpose:     
            Returnerer verdi av mva.                                        
            Gjelder beløpet en pris, må eventuell rabatt trekkes fra først. 
            Det returnerte beløp er avrundet til to decimaler etter komma.  
  Parameters:  
            <Beløp>,<Mva%>
  Syntax:
            RUN FinnMva(120,23).
  Notes:    
            Svar returneres i RETURN-VALUE avrundet til to decimaler.
               
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipPris   AS DEC NO-UNDO.
  DEF INPUT PARAMETER ipMva%   AS DEC NO-UNDO.

  DEF VAR wWork as DEC NO-UNDO.

    wWork = ipPris -
            (ipPris ) / (1 + (ipMva% / 100)).
    if wWork = ? THEN wWork = 0.

  RETURN string(round(wWork,2)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

