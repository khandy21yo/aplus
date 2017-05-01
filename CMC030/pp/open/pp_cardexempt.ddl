DEFINE RECORD CDD$TOP.PP.PP_CARDEXEMPT

        DESCRIPTION IS /*Pacific Pride Card Tax Exemption File*/.

        PP_CARDEXEMPT_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CARD
        Description = Card Number */
        CARD                    DATATYPE IS TEXT SIZE IS 8.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Authority */
        AUTHORITY               DATATYPE IS TEXT SIZE IS 5.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        END PP_CARDEXEMPT_CDD STRUCTURE.

END PP_CARDEXEMPT.
