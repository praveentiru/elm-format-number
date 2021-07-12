module FormatNumber.Parser exposing
    ( Category(..)
    , FormattedNumber
    , addZerosToFit
    , classify
    , joinParts
    , parse
    , parseString
    , removeZeros
    , splitInParts
    , splitNumberStringToParts
    , splitThousands
    , validateNumber
    )

import Array exposing (Array)
import Char
import FormatNumber.Locales exposing (Decimals(..), Locale)
import Regex
import Round
import String
import Test.Html.Event exposing (check)


{-| `Category` is a helper type and constructor to classify numbers in positive
(should use positive prefix and/or suffix), negative (should use negative
prefix and/or suffix), or zero (discard any prefix or suffix).
-}
type Category
    = Positive
    | Zero
    | Negative


{-| `FormattedNumber` type and constructor.
-}
type alias FormattedNumber =
    { original : Float
    , integers : List String
    , decimals : String
    , prefix : String
    , suffix : String
    }


{-| Identify if the formatted version of a number is negative:

    classify (FormattedNumber 1.2 ["1"] "2" "" "")
    --> Positive

    classify (FormattedNumber 0 ["0"] "" "" "")
    --> Zero

    classify (FormattedNumber -1 ["1"] "0" "" "")
    --> Negative

    classify (FormattedNumber 0 ["0"] "000" "" "")
    --> Zero

    classify (FormattedNumber -0.01 ["0"] "0" "" "")
    --> Zero

    classify (FormattedNumber -0.01 ["0"] "01" "" "")
    --> Negative

    classify (FormattedNumber 0.01 ["0"] "01" "" "")
    --> Positive

-}
classify : FormattedNumber -> Category
classify formatted =
    let
        onlyZeros : Bool
        onlyZeros =
            formatted.decimals
                |> List.singleton
                |> List.append formatted.integers
                |> String.concat
                |> String.all (\char -> char == '0')
    in
    if onlyZeros then
        Zero

    else if formatted.original < 0 then
        Negative

    else
        Positive


{-| Split a `String` in `List String` grouping by thousands digits:

    splitThousands "12345" --> [ "12", "345" ]

    splitThousands "12" --> [ "12" ]

-}
splitThousands : String -> List String
splitThousands integers =
    let
        reversedSplitThousands : String -> List String
        reversedSplitThousands value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplitThousands
                    |> (::) (String.right 3 value)

            else
                [ value ]
    in
    integers
        |> reversedSplitThousands
        |> List.reverse


{-| Given a `Locale` and a `Float`, returns a tuple with the integer and the
decimal parts as strings.

    import FormatNumber.Locales exposing (Decimals(..), usLocale)

    splitInParts usLocale 3.1415
    --> ("3", "14")

    splitInParts { usLocale | decimals = Exact 0 } 3.1415
    --> ("3", "")

-}
splitInParts : Locale -> Float -> ( String, String )
splitInParts locale value =
    let
        toString : Float -> String
        toString =
            case locale.decimals of
                Max max ->
                    Round.round max

                Min _ ->
                    String.fromFloat

                Exact exact ->
                    Round.round exact

        asList : List String
        asList =
            value |> toString |> String.split "."

        integers : String
        integers =
            asList |> List.head |> Maybe.withDefault ""

        decimals : String
        decimals =
            case List.tail asList of
                Just values ->
                    values |> List.head |> Maybe.withDefault ""

                Nothing ->
                    ""
    in
    ( integers, decimals )


{-| Remove all zeros from the tail of a string.

    removeZeros "100"
    --> "1"

-}
removeZeros : String -> String
removeZeros decimals =
    if String.right 1 decimals /= "0" then
        decimals

    else
        decimals
            |> String.dropRight 1
            |> removeZeros


{-| Given a `String` adds zeros to its tail until it reaches `desiredLength`.

    addZerosToFit 3 "1"
    --> "100"

-}
addZerosToFit : Int -> String -> String
addZerosToFit desiredLength value =
    let
        length : Int
        length =
            String.length value

        missing : Int
        missing =
            if length < desiredLength then
                abs <| desiredLength - length

            else
                0
    in
    value ++ String.repeat missing "0"


{-| Given a `Locale`, and the decimals as `String`, this function handles the
length of the string, removing or adding zeros as needed.
-}
getDecimals : Locale -> String -> String
getDecimals locale digits =
    case locale.decimals of
        Max _ ->
            removeZeros digits

        Exact _ ->
            digits

        Min min ->
            addZerosToFit min digits



{- Joins parts into `Maybe Float`. It expects parts to be strings with only
   digits.

      joinParts ("100", "235")
      --> Just 100.235

      joinParts ("", "534")
      --> Just 0.534

      joinParts ("243", "")
      --> Just 243

      joinParts ("", "")
      --> Nothing

      joinParts ("100,00", "243")
      --> Nothing

      joinParts ("10000", "24,3")
      --> Nothing

-}


joinParts : ( String, String ) -> Maybe Float
joinParts parts =
    let
        decimalCount : Float
        decimalCount =
            parts
                |> Tuple.second
                |> String.length
                |> toFloat

        integers : Maybe Float
        integers =
            parts
                |> Tuple.first
                |> String.toFloat

        decimalValue : Maybe Float
        decimalValue =
            parts
                |> Tuple.second
                |> String.toFloat
                |> Maybe.map (\n -> n / (10 ^ decimalCount))
    in
    case parts of
        ( "", "" ) ->
            Nothing

        ( _, "" ) ->
            integers

        ( "", _ ) ->
            decimalValue

        _ ->
            Maybe.map2 (+) integers decimalValue



{- Assumes that it is receiving a list of size two and builds a tuple of first
   and last item of the list
-}


buildNumberTuple : List String -> ( String, String )
buildNumberTuple parts =
    List.map2 Tuple.pair parts (List.reverse parts)
        |> List.head
        |> Maybe.withDefault ( "", "" )



{- Splits number string into its integers and decimal parts. Assumes valid
   number string. Use `validateNumber` to ensure number validity before calling
   this function. See case 2 for failure when bad strings are used

       import FormatNumber.Locales exposing (base)

       splitNumberStringToParts {base | thousandSeparator = ","} "100,000.234"
       --> ("100000", "234")

       splitNumberStringToParts {base | thousandSeparator = ","} "100,000.23.4"
       --> ("100000", "4")

       splitNumberStringToParts {base | thousandSeparator = ","} "1,00,000.234"
       --> ("100000", "234")

       splitNumberStringToParts {base | thousandSeparator = ","} "1#00#000.234"
       --> ("100000", "234")

-}


splitNumberStringToParts : Locale -> String -> ( String, String )
splitNumberStringToParts locale number =
    let
        notDigits : Regex.Regex
        notDigits =
            "\\D"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        cleanString : String -> String
        cleanString string =
            Regex.replace notDigits (\_ -> "") string
    in
    number
        |> String.split locale.decimalSeparator
        |> List.map cleanString
        |> buildNumberTuple


{- Check is used in number validation for prefix and suffix checking
 -}
type StringEdge
    = Prefix
    | Suffix


{- Validates that `String` matches `Locale` and returns `Bool`.
   TODO: Add tests for numericSystem

       import FormatNumber.Locales exposing (base, Decimals(..))

       validateNumber {base | thousandSeparator = ",", negativePrefix = "-"} "-100,000.234"
       --> Ok True

       validateNumber {base | thousandSeparator = ",", negativePrefix = "(", negativeSuffix = ")"} "(100,000.234)"
       --> Ok True

       validateNumber {base | thousandSeparator = ","} "(100,000.234)"
       --> Err "Negative or, Zero prefix or, suffix does not match locale"

       validateNumber {base | thousandSeparator = ","} "100#000.234"
       --> Err "Thousands separtor does not match locale"

       validateNumber {base | thousandSeparator = ","} "100,000.234"
       --> Ok True

       validateNumber {base | decimals = Max 0, thousandSeparator = ","} "100,000.2"
       --> Err "Number of digits in decimal does not match locale"

       validateNumber {base | decimals = Exact 2, thousandSeparator = ","} "100,000.234"
       --> Err "Number of digits in decimal does not match locale"

       validateNumber {base | decimals = Min 4, thousandSeparator = ","} "100,000.234"
       --> Err "Number of digits in decimal does not match locale"

       validateNumber {base | thousandSeparator = ","} "100,000.23.4"
       --> Err "Number of decimal separators in string is more than 1"

-}
validateNumber : Locale -> String -> Result String Bool
validateNumber locale value =
    -- TODO
    -- 4. Check thousands separator matches
    -- Change to manage multiple instances of thousand separator
    -- tCnt = nbr |> String.indices "," |> List.length
    -- nbr |> String.split "." |> List.head |> Maybe.withDefault "" |> String.dropLeft (String.length np) |> Regex.replace reg (\_ -> "") |> (==) (String.repeat tCnt ",")
    -- 5. Check thousands separator gap matches numericSystem
    -- Borrow from Eduardo code to figure out calculation
    -- Sequence of testing
    -- Check for deviation rather than valid format
    -- Check negative suffix and prefix
    -- Check zero suffix and prefix
    -- Check thousand separator match
    -- Check gap between thousand separators for numericSystem
    let
        noOfDecimalSepartors : String -> Int
        noOfDecimalSepartors valueString =
            valueString
                |> String.indices locale.decimalSeparator
                |> List.length

        splitToParts : String -> ( String, String )
        splitToParts valueString =
            valueString
                |> String.split locale.decimalSeparator
                |> buildNumberTuple

        -- Assumption: It is already validated that there is one or, less
        -- decimalSeparator so, String.split will result in list of size two
        -- or, one. Logic works in either case
        decimalCount : String -> Int
        decimalCount valueString =
            valueString
                |> splitToParts
                |> Tuple.second
                |> String.length

        validateDecimalCount : String -> Bool
        validateDecimalCount valueString =
            case locale.decimals of
                Min count ->
                    decimalCount valueString >= count

                Max count ->
                    decimalCount valueString <= count

                Exact count ->
                    decimalCount valueString == count

        notDigits : Regex.Regex
        notDigits =
            "\\D"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        hasNonDigits : String -> Bool
        hasNonDigits valueString =
            valueString
                |> Debug.log "Has non digit input"
                |> Regex.findAtMost 1 notDigits
                |> List.length
                |> (==) 1
                |> Debug.log "Has non digit output"

        getStringEndFunction : StringEdge -> (Int -> String -> String)
        getStringEndFunction check =
            case check of
                Prefix ->
                    String.left

                Suffix ->
                    String.right

        isCharAtStringEnd : StringEdge -> String -> Bool
        isCharAtStringEnd check string =
            string
                |> (getStringEndFunction check) 1
                |> hasNonDigits

        matchPrefixOrSuffix : StringEdge -> String -> String -> Bool
        matchPrefixOrSuffix check substring string =
            string
                |> Debug.log "Match fn input"
                |> getStringEndFunction check (String.length substring)
                |> (==) substring
                |> Debug.log "Match fn output"

        validatePrefixAndSuffix : (String, String) -> String -> Bool
        validatePrefixAndSuffix prefixSuffix valueString =
            let
                prefix : String
                prefix =
                    Tuple.first prefixSuffix

                suffix : String
                suffix =
                    Tuple.second prefixSuffix

            in
                case ((String.length prefix) == 0, 
                    (String.length suffix) == 0) of
                    (False, True) -> 
                        matchPrefixOrSuffix Prefix prefix valueString
                            && not (isCharAtStringEnd Suffix valueString)

                    (True, False) ->
                        not (isCharAtStringEnd Prefix valueString)
                            && matchPrefixOrSuffix Suffix suffix
                            valueString
                    (True, True) ->
                        not (isCharAtStringEnd Prefix valueString)
                            && not (isCharAtStringEnd Suffix valueString)
                    (False, False) ->
                        matchPrefixOrSuffix Prefix prefix valueString 
                            && matchPrefixOrSuffix Suffix suffix valueString

        validateNegativePrefixAndSuffix : String -> Bool
        validateNegativePrefixAndSuffix valueString =
            validatePrefixAndSuffix (locale.negativePrefix, 
            locale.negativeSuffix) valueString

        validateZeroPrefixAndSuffix : String -> Bool
        validateZeroPrefixAndSuffix valueString =
            validatePrefixAndSuffix (locale.zeroPrefix, locale.zeroSuffix)
            valueString
    in
    if noOfDecimalSepartors value > 1 then
        Err "Number of decimal separators in string is more than 1"

    else if not (validateDecimalCount value) then
        Err "Number of digits in decimal does not match locale"

    else if
        (hasNonDigits (String.left 1 value)
            || hasNonDigits (String.right 1 value)
        )
            && (not (validateNegativePrefixAndSuffix value)
                    && not (validateZeroPrefixAndSuffix value)
               )
    then
        Err "Negative or, Zero prefix or, suffix does not match locale"

    else
        Ok True


{-| Given a `Locale` parses a `Float` into a `FormattedNumber`:

    import FormatNumber.Locales exposing (Decimals(..), usLocale)

    parse { usLocale | decimals = Exact 3 } 3.1415
    --> { original = 3.1415
    --> , integers = ["3"]
    --> , decimals = "142"
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 3 } -3.1415
    --> { original = -3.1415
    --> , integers = ["3"]
    --> , decimals = "141"
    --> , prefix = "−"
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 3, positiveSuffix = "+" } 3.1415
    --> { original = 3.1415
    --> , integers = ["3"]
    --> , decimals = "142"
    --> , prefix = ""
    --> , suffix = "+"
    --> }

    parse { usLocale | negativePrefix = "(", negativeSuffix = ")", positiveSuffix = " ", zeroSuffix = " " } -12.34
    --> { original = -12.34
    --> , integers = ["12"]
    --> , decimals = "34"
    --> , prefix = "("
    --> , suffix = ")"
    --> }

    parse { usLocale | negativePrefix = "(", negativeSuffix = ")", positiveSuffix = " ", zeroSuffix = " " } 12.34
    --> { original = 12.34
    --> , integers = ["12"]
    --> , decimals = "34"
    --> , prefix = ""
    --> , suffix = " "
    --> }

    parse { usLocale | negativePrefix = "(", negativeSuffix = ")", positiveSuffix = " ", zeroSuffix = " " } 0.0
    --> { original = 0.0
    --> , integers = ["0"]
    --> , decimals = "00"
    --> , prefix = ""
    --> , suffix = " "
    --> }

    parse { usLocale | decimals = Exact 0 } 1234567.89
    --> { original = 1234567.89
    --> , integers = ["1", "234", "568"]
    --> , decimals = ""
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 0 } -1234567.89
    --> { original = -1234567.89
    --> , integers = ["1", "234", "568"]
    --> , decimals = ""
    --> , prefix = "−"
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 1 } 999.9
    --> { original = 999.9
    --> , integers = ["999"]
    --> , decimals = "9"
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 1 } -999.9
    --> { original = -999.9
    --> , integers = ["999"]
    --> , decimals = "9"
    --> , prefix = "−"
    --> , suffix = ""
    --> }

    parse usLocale 0.001
    --> { original = 0.001
    --> , integers = ["0"]
    --> , decimals = "00"
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse usLocale 0.001
    --> { original = 0.001
    --> , integers = ["0"]
    --> , decimals = "00"
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse usLocale -0.001
    --> { original = -0.001
    --> , integers = ["0"]
    --> , decimals = "00"
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 1 } ((2 ^ 39) / 100)
    --> { original = 5497558138.88
    --> , integers = ["5", "497", "558", "138"]
    --> , decimals = "9"
    --> , prefix = ""
    --> , suffix = ""
    --> }

    parse { usLocale | decimals = Exact 1 } ((-2 ^ 39) / 100)
    --> { original = -5497558138.88
    --> , integers = ["5", "497", "558", "138"]
    --> , decimals = "9"
    --> , prefix = "−"
    --> , suffix = ""
    --> }

-}
parse : Locale -> Float -> FormattedNumber
parse locale original =
    let
        parts : ( String, String )
        parts =
            splitInParts locale original

        integers : List String
        integers =
            parts
                |> Tuple.first
                |> String.filter Char.isDigit
                |> splitThousands

        decimals : String
        decimals =
            parts
                |> Tuple.second
                |> getDecimals locale

        partial : FormattedNumber
        partial =
            FormattedNumber original integers decimals "" ""
    in
    case classify partial of
        Negative ->
            { partial
                | prefix = locale.negativePrefix
                , suffix = locale.negativeSuffix
            }

        Positive ->
            { partial
                | prefix = locale.positivePrefix
                , suffix = locale.positiveSuffix
            }

        Zero ->
            { partial
                | prefix = locale.zeroPrefix
                , suffix = locale.zeroSuffix
            }


{-| Given a `Locale` parses a `String` into a `Maybe Float`
-}
parseString : Locale -> String -> Maybe Float
parseString locale value =
    let
        isNegative : Bool
        isNegative =
            String.startsWith locale.negativePrefix value
                && String.endsWith
                    locale.negativeSuffix
                    value

        notDigits : Regex.Regex
        notDigits =
            "\\D"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        onlyDigits : String -> String
        onlyDigits =
            Regex.replace notDigits (\_ -> "")

        splitValue : String -> List String
        splitValue number =
            number
                |> String.split locale.decimalSeparator
                |> List.map onlyDigits
    in
    case splitValue value of
        integers :: decimals :: [] ->
            if isNegative then
                ( integers, decimals )
                    |> joinParts
                    |> Maybe.map negate

            else
                joinParts ( integers, decimals )

        integers :: [] ->
            if isNegative then
                ( integers, "" )
                    |> joinParts
                    |> Maybe.map negate

            else
                joinParts ( integers, "" )

        _ ->
            Nothing
