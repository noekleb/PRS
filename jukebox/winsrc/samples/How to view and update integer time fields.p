To view the time fields as hh:mm in browse, add fields like this (for hhmmss use jbserv_int_to_hhmm_time.p):

+ ";+cFromTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (CalenderTimeFrom)|From"
+ ";+cToTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (CalenderTimeTo)|To"


Add the fill-ins to the FieldMap definition, last parameter.
(cFromTime and cToTime are fill-ins defined as CHAR, FORMAT "99:99")

hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                              hBrowse,
                              FRAME {&FRAME-NAME}:HANDLE,
                              "<updateable fields in viewer>","",
                              "Display fields in viewer","",
                              "cFromTime,cToTime"). /* <-- Fill ins */


Add the original integer fields to the "bufferextrafields" parameter:

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","CalenderTimeFrom,CalenderTimeTo").


DisplayRecord (after super):

IF hFieldMap:AVAIL THEN 
  ASSIGN cFromTime:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("cFromTime"):BUFFER-VALUE
         cToTime:SCREEN-VALUE   = hFieldMap:BUFFER-FIELD("cToTime"):BUFFER-VALUE
         cFromTime:MODIFIED	= FALSE
	 cToTime:MODIFIED	= FALSE
         .



Save record (before super):

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                  STRING(INT(ENTRY(1,cFromTime:SCREEN-VALUE,":")) * 3600 + INT(ENTRY(2,cFromTime:SCREEN-VALUE,":")) * 60) + "|" +
                  STRING(INT(ENTRY(1,cToTime:SCREEN-VALUE,":")) * 3600 + INT(ENTRY(2,cToTime:SCREEN-VALUE,":")) * 60) 
                 ).
