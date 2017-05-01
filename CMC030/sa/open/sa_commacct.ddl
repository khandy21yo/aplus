DEFINE RECORD CDD$TOP.SA.SA_COMMACCT

        DESCRIPTION IS /*Commision GL Accounts Table*/.

        SA_COMMACCT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = SALTYPE
        Description = Salesman Type */
        SALTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = Commision Expanse GL Account */
        EXPACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Commision Payable GL Account */
        PAYACCT                 DATATYPE IS TEXT SIZE IS 18.

        END SA_COMMACCT_CDD STRUCTURE.

END SA_COMMACCT.
