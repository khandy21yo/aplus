DEFINE RECORD CDD$TOP.AR.AR_CUSTYPE

        DESCRIPTION IS /*Customer Type Description File*/.

        AR_CUSTYPE_CDD STRUCTURE.

        /* Element =
        Description = Customer Type Code */
        CUSTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Customer Type Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        END AR_CUSTYPE_CDD STRUCTURE.

END AR_CUSTYPE.
