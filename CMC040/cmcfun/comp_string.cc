// %TITLE "String Comparison Function"
// %SBTTL "COMP_STRING"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/comp_string.bas
// Translated from Basic to C++ using btran
// on Wednesday, November 01, 2017 at 09:21:43
//

#include <cstring>
#include <regex>
#include "basicfun.h"
#include "cmcfun.h"





long comp_string(const std::string &test_string, const std::string &wildcard_string)
{
	long is_match;
	std::string str2;
	std::string test_stringa;

	// ++
	//
	// Abstract:HELP
	//	.b
	//	.lm +5
	//	This function compares a string to a pattern.
	//	It returns 0 if there is no match, and -1
	//	if there is.
	//	.lm -5
	//
	// Index:
	//
	// Parameters:
	//
	//	TEST_STRING$
	//		The passed string to be tested.
	//
	//	WILDCARD_STRING$
	//		The passed pattern to compare against.
	//		This value can be anything that is allowed in the
	//		"Wildcard" as defined in other manuels.
	//		('*'s, ",", "/", "?", ...)
	//
	//		A blank wildcard string matches nothing,not even
	//		blank strings.
	//
	//		Replaced with Unix Regex strings, but still
	//		allows '*' as a match for anything (ie. ".*").
	//
	//	Returned value
	//		An integer value containing the result of the
	//		comparison.
	//		(0 if match fails and -1 it succeeds.)
	//
	// --

	//
	// Trim the test string
	//
//	str2 = boost::trim_right_copy(wildcard_string);
	str2 = wildcard_string;

	// No match
	is_match = 0;
	if (str2 == "")
	{
		return 0;
	}
	// Yes match all
	if ((str2 == "*") || (str2 == ".*"))
	{
		return -1;
	}
	test_stringa = boost::trim_right_copy(test_string);

	//
	// Since '*' is so commonly used, convert it to a real regex
	//
	if (str2 == "*")
	{
		str2 = ".*";
	}
	//
	// Force it tomatch the full string instead of any part
	//
	if (str2[0] != '^')
	{
		str2 = "^(" + str2 + ")$";
	}

	//
	// Wildcard match
	//
	try
	{
std::cerr << "comp_string('" << test_stringa << "', '" <<
	str2 << "')" << std::endl;

		std::regex re(str2);
		if (std::regex_match(test_stringa, re))
		{
			return -1;
		}
		else
		{
			return 0;
		}
	}
	catch (...)
	{
		//
		// If we get an error, it probably means a bad
		// wildcard string has been passed.
		//
std::cerr << "comp_string: Bad regex on ('" << str2 << "', '" <<
	test_stringa << "'" << std::endl;
		return 0;
	}
}
