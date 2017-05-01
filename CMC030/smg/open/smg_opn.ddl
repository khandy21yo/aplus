DEFINE RECORD CDD$TOP.SMG.SMG_OPN

        DESCRIPTION IS /*PASS OPEN FILE DATA BETWEEN PROGRAMS*/.

        SMG_OPN_CDD STRUCTURE.

        /* Element =
        Description = File organization */
        ORGNIZATION             DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = File structure */
        STRCTURE                DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = File open name */
        FILE_NAME               DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = File extension */
        EXTENSION               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Keys */
        KEYS                    ARRAY 0:31 DATATYPE IS TEXT SIZE IS 255.

        /* Element =
        Description = Number of keys */
        KEYS_NUM                DATATYPE IS SIGNED WORD.

        END SMG_OPN_CDD STRUCTURE.

END SMG_OPN.
