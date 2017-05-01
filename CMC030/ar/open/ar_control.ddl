DEFINE RECORD CDD$TOP.AR.AR_CONTROL

        DESCRIPTION IS /*Accounts Receivable Control File*/.

        AR_CONTROL_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = AR account */
        AR_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Retention cycle */
        RETAIN                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Last Period Closed */
        LASTPERCLOSE            DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Current Year */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Closing flag 0-nostate,1-close,2-reset */
        CLOSEFLAG               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Number of days in a period */
        AGEPER                  ARRAY 0:4 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Names of periods */
        AGENAM                  ARRAY 0:4 DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = Customer Title (Cust, client, patient) */
        CTITLE                  DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = Default Method for AR */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        END AR_CONTROL_CDD STRUCTURE.

END AR_CONTROL.
