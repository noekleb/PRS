 
 /*------------------------------------------------------------------------
    File        : SendEMail
    Purpose     : Forenkle sending av eMail fra progress programmer.
    Syntax      : 
    Description : Klassen inneholder nødvendige metoder for sending av eMail ved hjelp av programmet SendEMail.exe.
    Author(s)   : tny
    Created     : Mon Feb 18 15:57:22 CET 2019
    Notes       : Benytter SendEMail.exe som ligger på cmd katalogen. 
                  Bruken av programmet forutsetter at det kjøres fra PRS arbeidskatalog og at cmd og log katalogene ligger under denne.
        
sendemail-1.56 by Brandon Zehm <caspian@dotconf.net>

Synopsis:  sendemail -f ADDRESS [options]

  Required:
    -f ADDRESS                from (sender) email address
    * At least one recipient required via -t, -cc, or -bcc
    * Message body required via -m, STDIN, or -o message-file=FILE

  Common:
    -t ADDRESS [ADDR ...]     to email address(es)
    -u SUBJECT                message subject
    -m MESSAGE                message body
    -s SERVER[:PORT]          smtp mail relay, default is localhost:25

  Optional:
    -a   FILE [FILE ...]      file attachment(s)
    -cc  ADDRESS [ADDR ...]   cc  email address(es)
    -bcc ADDRESS [ADDR ...]   bcc email address(es)
    -xu  USERNAME             username for SMTP authentication
    -xp  PASSWORD             password for SMTP authentication

  Paranormal:
    -b BINDADDR[:PORT]        local host bind address
    -l LOGFILE                log to the specified file
    -v                        verbosity, use multiple times for greater effect
    -q                        be quiet (i.e. no STDOUT output)
    -o NAME=VALUE             advanced options, for details try: --help misc
        -o message-content-type=<auto|text|html>
        -o message-file=FILE         -o message-format=raw
        -o message-header=HEADER     -o message-charset=CHARSET
        -o reply-to=ADDRESS          -o timeout=SECONDS
        -o username=USERNAME         -o password=PASSWORD
        -o tls=<auto|yes|no>         -o fqdn=FQDN


  Help:
    --help                    the helpful overview you're reading now
    --help addressing         explain addressing and related options
    --help message            explain message body input and related options
    --help networking         explain -s, -b, etc
    --help output             explain logging and other output options
    --help misc               explain -o options, TLS, SMTP auth, and more

MISC DOCUMENTATION

Misc Options
Options that don't fit anywhere else:
    -a   ATTACHMENT [ATTACHMENT ...]
    -xu  USERNAME
    -xp  PASSWORD
    -o   username=USERNAME
    -o   password=PASSWORD
    -o   tls=<auto|yes|no>
    -o   timeout=SECONDS
    -o   fqdn=FQDN

-a   ATTACHMENT [ATTACHMENT ...]
    This option allows you to attach any number of files to your email message.
    To specify more than one attachment, simply separate each filename with a
    space.  Example: -a file1.txt file2.txt file3.txt

-xu  USERNAME
    Alias for -o username=USERNAME

-xp  PASSWORD
    Alias for -o password=PASSWORD

-o   username=USERNAME (synonym for -xu)
    These options allow specification of a username to be used with SMTP
    servers that require authentication.  If a username is specified but a
    password is not, you will be prompted to enter one at runtime.

-o   password=PASSWORD (synonym for -xp)
    These options allow specification of a password to be used with SMTP
    servers that require authentication.  If a username is specified but a
    password is not, you will be prompted to enter one at runtime.

-o   tls=<auto|yes|no>
    This option allows you to specify if TLS (SSL for SMTP) should be enabled
    or disabled.  The default, auto, will use TLS automatically if your perl
    installation has the IO::Socket::SSL and Net::SSLeay modules available,
    and if the remote SMTP server supports TLS.  To require TLS for message
    delivery set this to yes.  To disable TLS support set this to no.  A debug
    level of one or higher will reveal details about the status of TLS.

-o   timeout=SECONDS
    This option sets the timeout value in seconds used for all network reads,
    writes, and a few other things.

-o   fqdn=FQDN
    This option sets the Fully Qualified Domain Name used during the initial
    SMTP greeting.  Normally this is automatically detected, but in case you
    need to manually set it for some reason or get a warning about detection
    failing, you can use this to override the default.

MESSAGE DOCUMENTATION

Message Options
Options related to the email message body:
    -u  SUBJECT
    -m  MESSAGE
    -o  message-file=FILE
    -o  message-content-type=<auto|text|html>
    -o  message-header=EMAIL HEADER
    -o  message-charset=CHARSET
    -o  message-format=raw

-u SUBJECT
    This option allows you to specify the subject for your email message.
    It is not required (anymore) that the subject be quoted, although it
    is recommended.  The subject will be read until an argument starting
    with a hyphen (-) is found.
    Examples:
      -u "Contact information while on vacation"
      -u New Microsoft vulnerability discovered

-m MESSAGE
    This option is one of three methods that allow you to specify the message
    body for your email.  The message may be specified on the command line
    with this -m option, read from a file with the -o message-file=FILE
    option, or read from STDIN if neither of these options are present.

    It is not required (anymore) that the message be quoted, although it is
    recommended.  The message will be read until an argument starting with a
    hyphen (-) is found.
    Examples:
      -m "See you in South Beach, Hawaii.  -Todd"
      -m Please ensure that you upgrade your systems right away

    Multi-line message bodies may be specified with the -m option by putting
    a "\n" into the message.  Example:
      -m "This is line 1.\nAnd this is line 2."

    HTML messages are supported, simply begin your message with "<html>" and
    sendEmail will properly label the mime header so MUAs properly render
    the message.  It is currently not possible without "-o message-format=raw"
    to send a message with both text and html parts with sendEmail.

-o message-file=FILE
    This option is one of three methods that allow you to specify the message
    body for your email.  To use this option simply specify a text file
    containing the body of your email message. Examples:
      -o message-file=/root/message.txt
      -o message-file="C:\Program Files\output.txt"

-o message-content-type=<auto|text|html>
    This option allows you to specify the content-type of the email. If your
    email message is an html message but is being displayed as a text message
    just add "-o message-content-type=html" to the command line to force it
    to display as an html message. This actually just changes the Content-Type:
    header. Advanced users will be happy to know that if you specify anything
    other than the three options listed above it will use that as the vaule
    for the Content-Type header.

-o message-header=EMAIL HEADER
    This option allows you to specify additional email headers to be included.
    To add more than one message header simply use this option on the command
    line more than once.  If you specify a message header that sendEmail would
    normally generate the one you specified will be used in it's place.
    Do not use this unless you know what you are doing!
    Example:
      To scare a Microsoft Outlook user you may want to try this:
      -o message-header="X-Message-Flag: Message contains illegal content"
    Example:
      To request a read-receipt try this:
      -o message-header="Disposition-Notification-To: <user@domain.com>"
    Example:
      To set the message priority try this:
      -o message-header="X-Priority: 1"
      Priority reference: 1=highest, 2=high, 3=normal, 4=low, 5=lowest

-o message-charset=CHARSET
    This option allows you to specify the character-set for the message body.
    The default is iso-8859-1.

-o message-format=raw
    This option instructs sendEmail to assume the message (specified with -m,
    read from STDIN, or read from the file specified in -o message-file=FILE)
    is already a *complete* email message.  SendEmail will not generate any
    headers and will transmit the message as-is to the remote SMTP server.
    Due to the nature of this option the following command line options will
    be ignored when this one is used:
      -u SUBJECT
      -o message-header=EMAIL HEADER
      -o message-charset=CHARSET
      -a ATTACHMENT


The Message Body
The email message body may be specified in one of three ways:
 1) Via the -m MESSAGE command line option.
    Example:
      -m "This is the message body"

 2) By putting the message body in a file and using the -o message-file=FILE
    command line option.
    Example:
      -o message-file=/root/message.txt

 3) By piping the message body to sendEmail when nither of the above command
    line options were specified.
    Example:
      grep "ERROR" /var/log/messages | sendEmail -t you@domain.com ...

If the message body begins with "<html>" then the message will be treated as
an HTML message and the MIME headers will be written so that a HTML capable
email client will display the message in it's HTML form.
Any of the above methods may be used with the -o message-format=raw option
to deliver an already complete email message.

ADDRESSING DOCUMENTATION

Addressing Options
Options related to addressing:
    -f   ADDRESS
    -t   ADDRESS [ADDRESS ...]
    -cc  ADDRESS [ADDRESS ...]
    -bcc ADDRESS [ADDRESS ...]
    -o   reply-to=ADDRESS

-f ADDRESS
    This required option specifies who the email is from, I.E. the sender's
    email address.

-t ADDRESS [ADDRESS ...]
    This option specifies the primary recipient(s).  At least one recipient
    address must be specified via the -t, -cc. or -bcc options.

-cc ADDRESS [ADDRESS ...]
    This option specifies the "carbon copy" recipient(s).  At least one
    recipient address must be specified via the -t, -cc. or -bcc options.

-bcc ADDRESS [ADDRESS ...]
    This option specifies the "blind carbon copy" recipient(s).  At least
    one recipient address must be specified via the -t, -cc. or -bcc options.

-o reply-to=ADDRESS
    This option specifies that an optional "Reply-To" address should be
    written in the email's headers.


Email Address Syntax
Email addresses may be specified in one of two ways:
    Full Name:     "John Doe <john.doe@gmail.com>"
    Just Address:  "john.doe@gmail.com"

The "Full Name" method is useful if you want a name, rather than a plain
email address, to be displayed in the recipient's From, To, or Cc fields
when they view the message.


Multiple Recipients
The -t, -cc, and -bcc options each accept multiple addresses.  They may be
specified by separating them by either a white space, comma, or semi-colon
separated list.  You may also specify the -t, -cc, and -bcc options multiple
times, each occurance will append the new recipients to the respective list.

Examples:
(I used "-t" in these examples, but it can be "-cc" or "-bcc" as well)

  * Space separated list:
    -t jane.doe@yahoo.com "John Doe <john.doe@gmail.com>"

  * Semi-colon separated list:
    -t "jane.doe@yahoo.com; John Doe <john.doe@gmail.com>"

  * Comma separated list:
    -t "jane.doe@yahoo.com, John Doe <john.doe@gmail.com>"

  * Multiple -t, -cc, or -bcc options:
    -t "jane.doe@yahoo.com" -t "John Doe <john.doe@gmail.com>"


  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/*BLOCK-LEVEL ON ERROR UNDO, THROW.*/

CLASS cls.SendEMail.SendEMail: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Gurre tester itt.
     
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest AS LOG NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
    
    DEFINE PUBLIC EVENT fakturaFilNavn  SIGNATURE VOID ().

    /* Definerer Properties */
    DEFINE PROPERTY parFromADDRESS AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parToADDRESS   AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parccADDRESS   AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parbccADDRESS  AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parSUBJECT     AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parMESSAGE     AS LONGCHAR NO-UNDO GET. SET.
    DEFINE PROPERTY parSERVER      AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parFILE        AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parUSERNAME    AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parPASSWORD    AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parBINDADDR    AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parLOGFILE     AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parQUIET       AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parTLS         AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parCommandString AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parMessage-Content-Type AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parMessage-File AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parMailType    AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY parMessage-Charset AS CHARACTER NO-UNDO GET. SET.
    
    /* Standard funksjoner for logging */
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    
    DEFINE STREAM Inn.
    DEFINE STREAM UtFil.
        
    /* Klassen startes uten loggparameter. */    
    CONSTRUCTOR PUBLIC SendEMail (  ):
        SUPER ().

        
        ASSIGN 
            cLogg = 'SendEMail' + REPLACE(STRING(TODAY),'/','')
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        FIX-CODEPAGE ( parMESSAGE ) = 'UTF-8'.

    END CONSTRUCTOR.

    /* Klassen startes med loggparameter. */    
    CONSTRUCTOR PUBLIC SendEMail ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        FIX-CODEPAGE ( parMESSAGE ) = 'UTF-8'.
        
        ASSIGN 
            cLogg = pcLogg
            .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID leggInnLoggen( INPUT pparLOGFILE AS CHARACTER ):
        
        DEFINE VARIABLE pcRecord AS CHARACTER NO-UNDO.
        
        IF SEARCH(pparLOGFILE) <> ? THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  eMailLOGG:' 
                ).
            INPUT STREAM Inn FROM VALUE(pparLOGFILE).
            REPEAT:
                IMPORT STREAM Inn UNFORMATTED 
                    pcRecord.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '    ' + pcRecord 
                    ).
            END.
            INPUT STREAM Inn CLOSE.
        END.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL parameteroppsett(  ):

        DEFINE VARIABLE pbResult AS LOG NO-UNDO.
        DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
        
        ASSIGN 
            bTest    = TRUE /* Settes til false i produksjon */ 
            pbResult = TRUE 
            cLogg = IF cLogg = '' THEN 'SendEMail' + REPLACE(STRING(TODAY),'/','') ELSE cLogg
            .
            
        /* Karraktersett */
        IF parMessage-Charset = '' THEN 
            parMessage-Charset = 'UTF-8'.
            
        /* tekstfil som standard. */
        IF parMessage-Content-Type = '' THEN 
            parMessage-Content-Type = 'text'.    
            
        /* Legger inn meldingsbody i longchar variabelen. */    
        IF parMessage-File <> '' AND SEARCH(parMessage-File) <> ? AND parMESSAGE = '' THEN 
            COPY-LOB FROM FILE parMessage-File TO parMESSAGE.
                 
        CASE parMailType:
            WHEN "PAKKSEDDEL" OR 
            WHEN "PKSDLFaktura" OR 
            WHEN "VPI" OR 
            WHEN "TimeGrip" OR 
            WHEN 'FakturaMail' OR 
            WHEN 'PRISMODELL' OR 
            WHEN 'PLUKKLISTE' OR 
            WHEN 'KOrdreMail' THEN
            DO:
              IF (parToADDRESS = '' AND parMailType = 'FakturaMail') THEN
                  {syspara.i 50 50 34 parToADDRESS}
              ELSE IF (parToADDRESS <> '' AND parMailType = 'PKSDLFaktura') THEN
              DO: 
                  pctekst = parToADDRESS.                   
                  {syspara.i 50 50 34 parToADDRESS}
                  parToADDRESS = parToADDRESS + 
                                 (IF parToADDRESS = '' THEN '' ELSE ';') + 
                                 pcTekst.
              END.
              ELSE IF parToADDRESS = '' THEN  
                  {syspara.i 50 50 30 parToADDRESS}

              {syspara.i 50 50 60 pcTekst}
              IF parMailType = 'KOrdreMail' THEN 
              DO:
                ASSIGN 
                  parFromADDRESS = IF parFromADDRESS = '' THEN 'support@polygon.se' ELSE parFromADDRESS                 
                  parSERVER      = IF parSERVER      = '' THEN 'smtp.office365.com:587' ELSE parSERVER
                  parUSERNAME    = IF parUSERNAME    = '' THEN 'support@polygon.se' ELSE parUSERNAME
                  parPASSWORD    = IF parPASSWORD    = '' THEN 'Tenn1s39' ELSE parPASSWORD
                  .
              END.
              ELSE IF pcTekst = '1'  THEN 
              DO:
                IF parFromADDRESS = '' THEN {syspara.i 50 50 61 parFromADDRESS}                         
                IF parSERVER      = '' THEN {syspara.i 50 50 62 parSERVER}
                IF parUSERNAME    = '' THEN {syspara.i 50 50 63 parUSERNAME}
                IF parPASSWORD    = '' THEN {syspara.i 50 50 64 parPASSWORD}
              END.
              ELSE DO:
                ASSIGN 
                  parFromADDRESS = IF parFromADDRESS = '' THEN 'support@polygon.se' ELSE parFromADDRESS                 
                  parSERVER      = IF parSERVER      = '' THEN 'smtp.office365.com:587' ELSE parSERVER
                  parUSERNAME    = IF parUSERNAME    = '' THEN 'support@polygon.se' ELSE parUSERNAME
                  parPASSWORD    = IF parPASSWORD    = '' THEN 'Tenn1s39' ELSE parPASSWORD
                  .
              END.
              
              IF parSUBJECT = '' THEN DO:
                  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  ** Mangler SUBJECT.' 
                      ).
                  pbResult = FALSE.    
              END.
              
              IF parMessage-File = '' AND parMESSAGE = '' THEN DO:
                  rStandardFunksjoner:SkrivTilLogg(cLogg,
                      '  ** Mangler MESSAGE BODY.' 
                      ).
                  pbResult = FALSE.    
              END.
            END.
            WHEN 'SMS' THEN 
            DO:
              
            END.    
        END CASE.
        
        IF bTest THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  PARAMETRE:'          
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parSERVER:' + parSERVER               
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parUSERNAME:' + parUSERNAME             
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parPASSWORD:' + parPASSWORD             
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parFromADDRESS:' + parFromADDRESS          
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parToADDRESS:' + parToADDRESS            
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parccADDRESS:' + parccADDRESS            
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parbccADDRESS:' + parbccADDRESS           
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parSUBJECT:' + parSUBJECT              
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parFILE:' + parFILE                 
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parBINDADDR:' + parBINDADDR             
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parLOGFILE:' + parLOGFILE              
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parQUIET:' + parQUIET                
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parTLS:' + parTLS                
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parMessage-Content-Type:' + parMessage-Content-Type 
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parMessage-File:' + parMessage-File         
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    parMailType:' + parMailType             
                ).
        END.
        
        RETURN pbResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Kommando syntaks. Allt må stå på en linje.
            Med dette oppsettet sendes ingen, ett eler flere vedlegg og message body hentes fra en fil.
            cmd\sendEmail.exe   -f FromADDRESS  support@polygon.se 
                                -t ToADDRESS    tomn@nsoft.no 
                                -s SERVER       smtp.office365.com:587 
                                -xu USERNAME    support@polygon.se 
                                -xp PASSWORD    Tenn1s39 
                                -u SUBJECT      "Test email" 
                                -o NAME=VALUE   message-file=log\message_body.txt 
                                -q QUIET
                                -l LOGFILE      log\sendemail.log 
                                -a ATTACHMENT   C:\NSoft\Polygon\PRS\utskrift\But_20_Faktura_20900001-180219-64893.pdf C:\NSoft\Polygon\PRS\utskrift\But_20_Faktura_20900001-180219-64034.pdf C:\NSoft\Polygon\PRS\utskrift\But_10_PkSdl_180219135646_180219-18660.pdf

    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL send(  ):
        
        DEFINE VARIABLE pbResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
        DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
        DEFINE VARIABLE pcCmdFile AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcBodyTekstFile AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcLogFile AS CHARACTER NO-UNDO.
        DEFINE VARIABLE piAktiv AS INTEGER NO-UNDO.
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Start SendeMail.cls:' 
            ).

        /* Oppsett av parametre. */
        pbResult = parameteroppsett( ).

        /* Henter temprært filnavn til filene. */
        pcCmdFile       = rStandardFunksjoner:getTempFileName(  ).
        pcBodyTekstFile = rStandardFunksjoner:getTempFileName(  ).
        pcLogFile       = rStandardFunksjoner:getTempFileName(  ).
        IF parLOGFILE = '' THEN 
            parLOGFILE = pcLogFile. 

        IF bTest THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Vellykket parameteroppsett: ' + STRING(pbResult) 
                ).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  LogFile: ' + pcLogFile 
                ).
        END.
        
        IF pbResult THEN 
        DO:
            /* Legger ut MessageBody til en fil. Sjekker på parMessage-Content-Type om det er til en xml eller txt fil. */
            utleggMessageBody( INPUT-OUTPUT pcBodyTekstFile ).
            IF parMessage-File = '' THEN 
                parMessage-File = pcBodyTekstFile. 
            IF bTest THEN 
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  BodyTekstFile: ' + pcBodyTekstFile 
                    ).
            
            ASSIGN
                pbResult  = TRUE 
                parFILE   = REPLACE(parFILE,';',' ')
                parFILE   = REPLACE(parFILE,',',' ')
                parFILE   = REPLACE(parFILE,'|',' ')
                parCommandstring = 'cmd\sendEmail ' + 
                            (IF parFromADDRESS  <> '' THEN ' -f '   + QUOTER(parFromADDRESS) ELSE '') +
                            (IF parToADDRESS    <> '' THEN ' -t '   + QUOTER(parToADDRESS) ELSE '') + 
                            (IF parccADDRESS    <> '' THEN ' -cc '  + QUOTER(parccADDRESS) ELSE '') + 
                            (IF parbccADDRESS   <> '' THEN ' -bcc ' + QUOTER(parbccADDRESS) ELSE '') + 
                            (IF parSERVER       <> '' THEN ' -s '   + parSERVER ELSE '') + 
                            (IF parUSERNAME     <> '' THEN ' -xu '  + parUSERNAME ELSE '') + 
                            (IF parPASSWORD     <> '' THEN ' -xp '  + parPASSWORD ELSE '') + 
                            (IF parSUBJECT      <> '' THEN ' -u '   + QUOTER(parSUBJECT) ELSE '') + 
                            (IF parTLS          <> '' THEN ' -o '   + parTLS ELSE '') + 
                            (IF parmessage-content-type <> '' THEN ' -o message-content-type='   + parmessage-content-type ELSE '') + 
                            (IF parMessage-File <> '' THEN ' -o message-file='   + parMessage-File ELSE '') + 
                            (IF parMessage-Charset <> '' THEN ' -o message-charset=' + parMessage-Charset ELSE '') + 
                            (IF parQUIET        <> '' THEN ' -q ' ELSE '') + 
                            (IF parLOGFILE      <> '' THEN ' -l '   + parLOGFILE ELSE '') +
                            (IF parFILE         <> '' THEN ' -a '   + parFILE ELSE '') 
                .
                    
            /* Legger ut kommandostrengen. */
            utleggCommandString( INPUT-OUTPUT pcCmdFile ).        
            IF bTest THEN 
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  CmdFile: ' + pcCmdFile 
                    ).
            /* OS kommando */
            cCmd = pcCmdFile.
            IF bTest THEN
            DO: 
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Kommando: ' + cCmd 
                    ).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Kommandostreng: ' + parCommandstring 
                    ).
            END.
                                
            /* Sikrer at log katalogen finnes. */
            OS-COMMAND SILENT mkdir VALUE('log') NO-ERROR.                 
            /* Utfører kommando og sender mailen */                 
            OS-COMMAND SILENT VALUE(cCmd).
            
            /* Legger loggen inn i loggen */
            leggInnLoggen( parLOGFILE ).
            
            /* rydder opp */
            IF NOT bTest THEN 
            DO:
                OS-DELETE VALUE( pcCmdFile ).
                OS-DELETE VALUE( pcBodyTekstFile ).
            END.
        END.
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Slutt SendeMail.cls:' 
            ).
        RETURN pbResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL sendFakturaVaremottak( INPUT plFaktura_Id AS DECIMAL ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID utleggCommandString( INPUT-OUTPUT pcCmdFile AS CHARACTER ):
        
        pcCmdFile = REPLACE(pcCmdFile,'.tmp','.bat').
        OUTPUT STREAM UtFil TO VALUE( pcCmdfile ).
            PUT STREAM UtFil UNFORMATTED 
                parCommandString
                SKIP.
        OUTPUT STREAM UtFil CLOSE.
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID utleggMessageBody( INPUT-OUTPUT pcBodyTekstFile AS CHARACTER ):
        
        /* message-content-type=<auto|text|html> */
        CASE parMessage-Content-Type:
            WHEN 'auto' THEN. /* Gjør ingenting. */
            WHEN 'text' THEN pcBodyTekstFile = REPLACE( pcBodyTekstFile,'.tmp','.txt' ).
            WHEN 'html' THEN pcBodyTekstFile = REPLACE( pcBodyTekstFile,'.tmp','.html' ).
        END CASE.
        
        /* Legger ut teksten til fil. */
        COPY-LOB FROM parMESSAGE TO FILE pcBodyTekstFile. 
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC SendEMail ( ):

    END DESTRUCTOR.

END CLASS.