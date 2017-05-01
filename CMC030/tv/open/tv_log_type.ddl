DEFINE RECORD CDD$TOP.TV.TV_LOG_TYPE

        DESCRIPTION IS /*TV Log Type Table*/.

        TV_LOG_TYPE_CDD STRUCTURE.

        /* Element =
        Description = Log type code */
        LTYPE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Type description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END TV_LOG_TYPE_CDD STRUCTURE.

END TV_LOG_TYPE.
