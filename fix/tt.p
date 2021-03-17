DEF VAR APIUri AS CHAR NO-UNDO.
DEFINE VARIABLE wc AS System.Net.WebClient NO-UNDO.
DEFINE VARIABLE WebRespons AS LONGCHAR NO-UNDO.

System.Net.ServicePointManager:SecurityProtocol = System.Net.SecurityProtocolType:Tls12.

ASSIGN 
        APIUri = 'https://checkout.dintero.com/v1/sessions'.

wc = NEW System.Net.WebClient().
wc:Encoding = System.Text.Encoding:UTF8.

wc:BaseAddress = APIUri.

wc:Headers:Add("Accept", "application/json").
wc:Headers:Add("Content-Type", "application/json;charset=utf-8").
wc:Headers:Add("Authentication", "Bearer " + "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJ3cml0ZTpjaGVja291dCByZWFkOmNoZWNrb3V0IHdyaXRlOnJlY2VpcHRzIHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6ZGlzY291bnRzOi9hdmFpbGFibGVfZm9yX3JlY2VpcHQiLCJ2ZXJzaW9uIjoyLCJpYXQiOjE2MDE5MDU2MTgsImV4cCI6MTYwMTkyMDAxOH0.doeBfH3PsIhkKXt4SEF3_F8mkbtx5QqbNvFJpDLohEx21NUY0iMcEgAMEvsOK-5hrW3urLk7-VniUnoTrcCEvIowIV7XtkcWLipRJIu3j7aAweLZ2ypr9SFB4r2JWu8goYDAHpcwyY9f1k3sAacQCoYNGgr3kYtIa64n9Gdl4Kz5HLCpB6glApxy3XN4yWe-1YEM5EVABtJR5P-uP7q8ksMFcqw3g-fWr1gY7VZwRWFBgMs_fhFvUtgNcCHL_sKxWgWlugxfzB7_scxkG0nxfhx0m5rf7hUjQ0kjOMHDJLZY-n3icJyFKpUdtvtp6MzecPIWDyElZ-dEg28Q1H5HFg").

WebRespons = wc:DownloadString(APIUri).

MESSAGE STRING(WebRespons)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

