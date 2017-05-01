DEFINE RECORD CDD$TOP.MO.MO_OPTGROUP

        DESCRIPTION IS /*Option Group Description Table*/.

        MO_OPTGROUP_CDD STRUCTURE.

        /* Element =
        Description = Option Group Code */
        OPTGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Group Code Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Sequence Number */
        SEQUENCE                DATATYPE IS TEXT SIZE IS 4.

        END MO_OPTGROUP_CDD STRUCTURE.

END MO_OPTGROUP.
