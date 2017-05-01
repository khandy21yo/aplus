DEFINE RECORD CDD$TOP.TV.TV_LOG_CLASS

        DESCRIPTION IS /*TV Log Class Table*/.

        TV_LOG_CLASS_CDD STRUCTURE.

        /* Element =
        Description = Class code */
        CLASS                   DATATYPE IS TEXT SIZE IS 04.

        /* Element =
        Description = Class description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END TV_LOG_CLASS_CDD STRUCTURE.

END TV_LOG_CLASS.
