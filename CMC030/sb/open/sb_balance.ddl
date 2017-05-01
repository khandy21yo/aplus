DEFINE RECORD CDD$TOP.SB.SB_BALANCE

        DESCRIPTION IS /*Subaccount Balance File*/.

        SB_BALANCE_CDD STRUCTURE.

        /* Element = SYSTEM
        Description = System name */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = SUBACCT
        Description = Sub Account Number */
        SUBACCOUNT              DATATYPE IS TEXT SIZE IS 10.

        /* Element = OPERATION
        Description = */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = GL Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = PERIOD
        Description = YYYYPP */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Units */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Hours */
        HOURS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Beginning Amount */
        BEG_AMOUNT              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Beginning Units */
        BEG_UNITS               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Beginning Hours */
        BEG_HOURS               DATATYPE IS G_FLOATING.

        END SB_BALANCE_CDD STRUCTURE.

END SB_BALANCE.
