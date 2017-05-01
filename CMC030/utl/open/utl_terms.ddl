DEFINE RECORD CDD$TOP.UTL.UTL_TERMS

        DESCRIPTION IS /*Terms Definition File*/.

        UTL_TERMS_CDD STRUCTURE.

        /* Element =
        Description = Terms Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Discount percentage */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Days until become due */
        DUEDAYS                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Day of month becomes due */
        DUEDATE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Days discount is good for */
        DISCOUNTDAYS            DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Day discount becomes due */
        DISCOUNTDATE            DATATYPE IS TEXT SIZE IS 2.

        END UTL_TERMS_CDD STRUCTURE.

END UTL_TERMS.
