DEFINE VARIABLE objOutlook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookMsg AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookAttach AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookRecip AS COM-HANDLE NO-UNDO.
CREATE "Outlook.Application" objOutlook.
objoutlookMsg = objOutlook:CreateItem(0).
objOutlookRecip = objOutlookMsg:Recipients:Add("noekleb@online.no").
objOutlookRecip:Type = 1.
objOutlookMsg:Subject = "TEST Progress".
objOutlookMsg:Body = "The Body".
objoutlookMsg:ReadReceiptRequested = FALSE.
objoutlookMsg:OriginatorDeliveryReportRequested = FALSE.
objoutlookMsg:SentOnBehalfOfName = "tomn@nsoft.no".
objOutlookMsg:Attachments:Add("C:\tmp\debug.txt").
objOutlookRecip:Resolve.
objOutlookMsg:Send.
RELEASE OBJECT objOutlook.
RELEASE OBJECT objOutlookMsg.
RELEASE OBJECT objOutlookRecip
