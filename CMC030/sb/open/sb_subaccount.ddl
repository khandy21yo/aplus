DEFINE RECORD CDD$TOP.SB.SB_SUBACCOUNT

        DESCRIPTION IS /*Subaccount Description File*/.

        SB_SUBACCOUNT_CDD STRUCTURE.

        /* Element =
        Description = Subject type */
        SUBJECT                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCOUNT              DATATYPE IS TEXT SIZE IS 10.

        /* Element = DESCRIPTION6
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = CLASS
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        EDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Extra Fields for a Subaccount */
        EXTRAFIELDS             DATATYPE IS TEXT SIZE IS 110.

        END SB_SUBACCOUNT_CDD STRUCTURE.

END SB_SUBACCOUNT.
