DEFINE INPUT  PARAMETER c_p2_to AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER c_p6_title  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER c_p7_msg    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER c_p8_attach AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c_command   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p1_from   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p3_hub    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p4_usr    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_p5_pwd    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCommandstring AS CHARACTER   NO-UNDO.

c_command = ".\cmd\sendmail_proc.cmd".
c_p3_hub  = "smtp.office365.com:587".
c_p4_usr  = "support@polygon.se".
c_p5_pwd  = "Tenn1s39".
c_p1_from = "support@polygon.se".

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
                     
OUTPUT TO c:\tmp\mail.txt.
   PUT UNFORMATTED cCommandstring SKIP.
OUTPUT CLOSE.
                 
OS-COMMAND SILENT VALUE(cCommandstring).
OS-DELETE VALUE("c:\tmp\mail.txt").
