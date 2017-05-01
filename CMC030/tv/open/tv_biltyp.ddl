DEFINE RECORD CDD$TOP.TV.TV_BILTYP

        DESCRIPTION IS /*TV Billing Type*/.

        TV_BILTYP_CDD STRUCTURE.

        /* Element =
        Description = Bill type */
        BTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END TV_BILTYP_CDD STRUCTURE.

END TV_BILTYP.
