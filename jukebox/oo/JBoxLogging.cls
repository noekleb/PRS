 
/*------------------------------------------------------------------------
   File        : JBoxLogging
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : goo_000
   Created     : Tue Jan 19 09:26:18 CET 2016
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxLogging: 
    def public variable LogLevelForPrinting as int init 3 no-undo.
    DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS JBoxLogging NO-UNDO
        GET():
            IF Instance = ? THEN Instance = NEW JBoxLogging().
            RETURN Instance.
        END GET.
        PRIVATE SET.

    METHOD PUBLIC VOID LOG(input icSubsystem as char, INPUT icMessage AS CHARACTER):       
        IF LOG-MANAGER:LOGGING-LEVEL GE LogLevelForPrinting THEN
            LOG-MANAGER:WRITE-MESSAGE(icMessage,icSubsystem). 
    END METHOD.

    METHOD PUBLIC VOID LOG(INPUT icMessage AS CHARACTER):       
        IF LOG-MANAGER:LOGGING-LEVEL GE LogLevelForPrinting THEN
            LOG-MANAGER:WRITE-MESSAGE(icMessage,''). 
    END METHOD.


    DESTRUCTOR PUBLIC JBoxLogging():

    END DESTRUCTOR.

END CLASS.