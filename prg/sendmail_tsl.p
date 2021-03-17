&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEFINE INPUT  PARAMETER cMailtyp    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cMailrubrik AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cAttachment AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cBody       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cOpt2       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cOpt3       AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c_command   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p1_from   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p2_to     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p3_hub    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p4_usr    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p5_pwd    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p6_title  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p7_msg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p8_attach AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCommandstring AS CHARACTER   NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

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
ASSIGN 
  cLogg       = 'SendMail_tsl' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).

c_p6_title = cMailrubrik.
IF cBody <> "" THEN
    c_p7_msg   = cBody.
ELSE
    c_p7_msg   = "Automatiskt genererad".
c_p8_attach = cAttachment.

RUN getsyspara.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Mailtype: ' + cMailtyp 
    ).

CASE cMailtyp:
    WHEN "OVERFORERROR" OR WHEN "SASONGLISTA" THEN DO:
        
        {syspara.i 50 50 25 c_p2_to}

/* c_p2_to = "ken1@polygonsoftware.no;svenor.voxo@gmail.com". */

                
/*                 OS-COMMAND SILENT VALUE(c_command   + ' '  +   */
/*                                         c_p1_from   + ' '  +   */
/*                                         c_p2_to     + ' '  +   */
/*                                         c_p3_hub    + ' '  +   */
/*                                         c_p4_usr    + ' '  +   */
/*                                         c_p5_pwd    + ' "' +   */
/*                                         c_p6_title  + '" "'  + */
/*                                 c_p7_msg    + '" ' +           */
/*                                 c_p8_attach).                  */
        
/*         cmd\sendEmail -q -f %1 -t %2 -s %3 -xu %4 -xp %5 -u %6 -m %7 -a %8 */

        /*  cmd\sendEmail -f info@polygon.se 
                          -t g.jansbo@polygon.se;ken1@polygonsoftware.no 
                          -s smtp.office365.com:587 
                          -xu info@polygon.se 
                          -xp Uddeva11a 
                          -u "Test email" 
                          -m "Hi buddy, this is a test email."
                          -a filnamn
        */

    END.
    WHEN "ECCORAPPORT" THEN DO:
        {syspara.i 50 50 26 c_p2_to}
    END.
    WHEN "PLOCKBUTIK" THEN DO:
        {syspara.i 50 50 27 c_p2_to}
    END.
    WHEN "WEBKOMMKONTROLL" THEN DO:
        {syspara.i 50 50 34 c_p2_to}
    END.
    WHEN "HITRATE" THEN DO:
        {syspara.i 210 275 2 c_p2_to}
    END.
    WHEN "NOIMAGE" THEN DO:
        {syspara.i 50 50 24 c_p2_to}
/*             cCommandstring = c_command   + ' '  +          */
/*                                 c_p1_from   + ' "'  +      */
/*                                 c_p2_to     + '" '  +      */
/*                                 c_p3_hub    + ' '  +       */
/*                                 c_p4_usr    + ' '  +       */
/*                                 c_p5_pwd    + ' "' +       */
/*                                 c_p6_title  + '" "'  +     */
/*                                 c_p7_msg    + '" ' + '"' + */
/*                                 c_p8_attach + '"'          */
/*                                 .                          */
            .
            cCommandstring = 'c:\appdir\se\cmd\sendEmail.exe'        +
                        REPLACE(' -q -f FROM',"FROM",c_p1_from) +
                        REPLACE(' -t "TO"',"TO",c_p2_to) +
                        REPLACE(' -s HUB',"HUB",c_p3_hub) +
                        REPLACE(' -xu USR',"USR",c_p4_usr)       +
                        REPLACE(' -xp PWD',"PWD",c_p5_pwd)             +
                        REPLACE(' -u "TITLE"',"TITLE",c_p6_title) +
                        REPLACE(' -m "MSG"',"MSG",c_p7_msg).
                              

    END.
    WHEN "SUPPLORDRE" THEN 
    DO:
        {syspara.i 50 50 30 c_p2_to}
        ASSIGN
            c_p1_from      = 'support@polygon.se'                 
            c_p3_hub       = 'smtp.office365.com:587'
            c_p4_usr       = 'support@polygon.se'
            c_p5_pwd       = 'Tenn1s39'
            cCommandstring = c_command   + ' '  +
                                c_p1_from   + ' "'  +
                                c_p2_to     + '" '  +            
                                c_p3_hub    + ' '  +
                                c_p4_usr    + ' '  +
                                c_p5_pwd    + ' "' +
                                c_p6_title  + '" "'  +
                                c_p7_msg    + '" ' + '"' + 
                                c_p8_attach + '"'
        .                         
    END.
    WHEN "PAKKSEDDEL" OR WHEN "VPI" OR WHEN "TimeGrip" THEN 
    DO:
        /* TN Endret 13/2-19 ved sync med Kenneth.
            c_p1_from      = 'info@polygon.se'                 
            c_p3_hub       = 'smtp.office365.com:587'
            c_p4_usr       = 'info@polygon.se'
            c_p5_pwd       = 'Uddeva11a'        
        */
        {syspara.i 50 50 28 c_p2_to}
        ASSIGN
            c_p1_from      = 'support@polygon.se'
            c_p3_hub       = 'smtp.office365.com:587'
            c_p4_usr       = 'support@polygon.se'
            c_p5_pwd       = 'Tenn1s39'
            cCommandstring = c_command   + ' '  +
                                c_p1_from   + ' "'  +
                                c_p2_to     + '" '  +            
                                c_p3_hub    + ' '  +
                                c_p4_usr    + ' '  +
                                c_p5_pwd    + ' "' +
                                c_p6_title  + '" "'  +
                                c_p7_msg    + '" ' + /*'"' +*/ 
                                c_p8_attach /*+ '"'*/
        .   
        
/*        MESSAGE 'cMailtyp:' cMailtyp SKIP*/
/*            c_p1_from SKIP               */
/*            c_p2_to   SKIP               */
/*            c_p3_hub  SKIP               */
/*            c_p4_usr  SKIP               */
/*            c_p5_pwd  SKIP               */
/*        VIEW-AS ALERT-BOX.               */
    END.
    OTHERWISE
        RETURN.
END CASE.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Cmd: ' + cCommandstring 
    ).

IF cCommandstring = '' THEN 
    cCommandstring = c_command   + ' '  +
                     c_p1_from   + ' "'  +
                     c_p2_to     + '" '  +
                     c_p3_hub    + ' '  +
                     c_p4_usr    + ' '  +
                     c_p5_pwd    + ' "' +
                     c_p6_title  + '" "'  +
                     c_p7_msg    + '" ' + '"' + 
                     c_p8_attach + '"'.

/* TN 28/9-18 Skaper logg over utførte mail kommandoer. */  
OS-COMMAND SILENT mkdir VALUE('log') NO-ERROR.                 

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  skriv til logg' + 'log\mail' + cMailtyp + REPLACE(STRING(TODAY),'/','') + '.txt' 
    ).

OUTPUT TO VALUE('log\mail' + cMailtyp + REPLACE(STRING(TODAY),'/','') + '.txt') APPEND.
   PUT UNFORMATTED 
    STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + cCommandstring 
    SKIP.
OUTPUT CLOSE.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Utfører kommando: ' + cCommandstring 
    ).
    
/* Sender mailen */                 
OS-COMMAND SILENT VALUE(cCommandstring).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getsyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getsyspara Procedure 
PROCEDURE getsyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{syspara.i 50 50 6 c_command}
{syspara.i 50 50 1 c_p3_hub}
{syspara.i 50 50 4 c_p4_usr}
{syspara.i 50 50 5 c_p5_pwd}
{syspara.i 50 50 4 c_p1_from}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

