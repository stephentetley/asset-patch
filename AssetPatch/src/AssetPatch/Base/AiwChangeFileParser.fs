// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module AiwChangeFileParser =

    open FParsec
    open System

    open AssetPatch.Base.Common
    open AssetPatch.Base.AiwChangeFile

    type AiwChangeFileParser<'ans> = Parser<'ans, unit>

    // ************************************************************************
    // Lexer


    let intOfLength (len : int) : AiwChangeFileParser<int> = 
        let after (chars : char []) = chars |> System.String |> int
        parray len digit |>> after

    let lexeme (parser : AiwChangeFileParser<'a>) : AiwChangeFileParser<'a> = 
        let softSpaces = many (anyOf [' '; '\t'])
        parser .>> softSpaces

    let token (str : string) : AiwChangeFileParser<string> = 
        lexeme (pstring str)

    let charToken (ch : char) : AiwChangeFileParser<char> = 
        lexeme (pchar ch)

    let directive (parser : AiwChangeFileParser<'a>) : AiwChangeFileParser<'a> = 
        charToken '*' >>. parser .>> newline

    let named (name : string) 
              (parser : AiwChangeFileParser<'a>) : AiwChangeFileParser<'a> = 
        token (name + ":") >>. parser

    //let cellValue : AiwChangeFileParser<string> =
    //    manyChars (noneOf ['\t'; '\r'; '\n' ])


    // ************************************************************************
    // Parser

    let pIntegerString : AiwChangeFileParser<IntegerString> = 
        many1Chars digit |>> IntegerString |> lexeme


    let pFuncLoc : AiwChangeFileParser<string> = 
        many1Chars (satisfy (not << Char.IsWhiteSpace)) |> lexeme


    let pFileType : AiwChangeFileParser<FileType> =
        let inner = 
            choice [ token "Download" >>. preturn Download 
                   ; token "Upload" >>. preturn Upload
                   ]
        directive inner

    let pDataModel : AiwChangeFileParser<DataModel> =
        let inner = 
            choice [ token "U1" >>. preturn U1 ]
        directive (named "Data Model" inner)


    let pEntityType : AiwChangeFileParser<EntityType> =
        let inner = 
            choice 
                [ token "FUNCLOC"   >>. preturn FuncLoc 
                ; token "CLASSFLOC" >>. preturn ClassFloc
                ; token "VALUAFLOC" >>. preturn ValuaFloc
                ; token "IFLOTX"    >>. preturn Iflotx
                ; token "EQUI"      >>. preturn Equi
                ; token "CLASSEQUI" >>. preturn ClassEqui
                ; token "VALUAEQUI" >>. preturn ValuaEqui
                ; token "EQMLTXT"   >>. preturn Eqmltxt

                ]
        directive (named "Entity Type" inner)

    let pVariant : AiwChangeFileParser<string> =
        let inner = restOfLine false |>> (fun s -> s.Trim())
        directive (named "Variant" inner)

    let pUser : AiwChangeFileParser<string> =
        let inner = restOfLine false |>> (fun s -> s.Trim())
        directive (named "User" inner)


    let pDate : AiwChangeFileParser<int * int * int> =
        let inner = tuple3 (intOfLength 4) (intOfLength 2) (intOfLength 2)
        named "Date" (lexeme inner)
    
    let pTime : AiwChangeFileParser<int * int * int> =
        let inner = tuple3 (intOfLength 2) (intOfLength 2) (intOfLength 2)
        named "Time" (lexeme inner)

    

    let pDateTime : AiwChangeFileParser<DateTime> = 
        let inner = 
            parse { 
                let! (yr,mon,day) = pDate
                let! _ = charToken '/'
                let! (hr,mins,sec) = pTime
                return DateTime(year=yr, month=mon, day=day, hour=hr, 
                                minute=mins, second=sec)
            }
        directive inner
    

    let pSelectionItem : AiwChangeFileParser<Selection> = 
        let line = regex ".*\|.*\|" |>> SelectionLine
        attempt (directive line)

    let pSelectionHeader : AiwChangeFileParser<unit> =
        let inner = preturn ()
        directive (named "Selection" inner)


    let pSelection : AiwChangeFileParser<Selection list> = 
        pSelectionHeader >>. many pSelectionItem

    let pHeaderRow : AiwChangeFileParser<HeaderRow> = 
        let decode (str : string) = str.Split([| '\t' |]) |> HeaderRow
        let inner = restOfLine false |>> decode
        directive inner 


    let pDataRow (size :int) : AiwChangeFileParser<DataRow> = 
        let decode (str : string) = 
            let arr = str.Split([| '\t' |]) 
            try 
                Array.take size arr |> DataRow |> preturn 
            with 
            | _ -> fail "bad row"
        restOfLine true >>= decode

    let pDataRows (size : int) : AiwChangeFileParser<DataRow list> = 
        manyTill (pDataRow size) eof

    let pFileHeader : AiwChangeFileParser<FileHeader> = 
        parse {
            let! ptype = pFileType
            let! dmodel = pDataModel
            let! etype = pEntityType
            let! variant  = pVariant
            let! user = pUser
            let! date = pDateTime
            return { FileType = ptype
                     DataModel = dmodel
                     EntityType = etype
                     Variant = variant
                     User = user
                     DateTime = date }
        }

    let parseAiwChangeFile () : AiwChangeFileParser<AiwChangeFile> = 
        parse {
            let! fileHeader = pFileHeader
            let! selection = 
                match fileHeader.FileType with
                | Download -> pSelection |>> Some
                | _ -> preturn None
            let! headerDescs = 
                match fileHeader.FileType with
                | Upload -> pHeaderRow |>> Some
                | _ -> preturn None
            let! headerRow = pHeaderRow
            let size = headerRow.Columns |> List.length
            let! datas = pDataRows size
            return { Header = fileHeader
                     Selection = selection
                     HeaderDescriptions = headerDescs
                     HeaderRow = headerRow
                     DataRows = datas }
        }


    let readAiwChangeFile (inputPath : string) : Result<AiwChangeFile, ErrMsg> = 
        match runParserOnFile (parseAiwChangeFile ()) () inputPath Text.Encoding.UTF8 with
        | Failure (str,_,_) -> Result.Error str
        | Success (ans,_,_) -> Result.Ok ans

